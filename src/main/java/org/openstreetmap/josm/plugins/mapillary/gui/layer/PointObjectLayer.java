// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.AlphaComposite;
import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.event.ActionEvent;
import java.awt.geom.Area;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonException;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.stream.JsonParser;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.actions.RenameLayerAction;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.DataSource;
import org.openstreetmap.josm.data.coor.EastNorth;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.DataSourceChangeEvent;
import org.openstreetmap.josm.data.osm.DataSourceListener;
import org.openstreetmap.josm.data.osm.DownloadPolicy;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.UploadPolicy;
import org.openstreetmap.josm.data.osm.visitor.paint.AbstractMapRenderer;
import org.openstreetmap.josm.data.osm.visitor.paint.MapRendererFactory;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.MapViewState.MapViewPoint;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.gui.mappaint.MapPaintStyles;
import org.openstreetmap.josm.gui.mappaint.loader.MapPaintStyleLoader;
import org.openstreetmap.josm.gui.mappaint.mapcss.MapCSSStyleSource;
import org.openstreetmap.josm.gui.preferences.display.DrawingPreference;
import org.openstreetmap.josm.gui.progress.NullProgressMonitor;
import org.openstreetmap.josm.io.GeoJSONReader;
import org.openstreetmap.josm.io.IllegalDataException;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.OpenBrowser;

/**
 * Mapillary Point Object layer
 */
public class PointObjectLayer extends OsmDataLayer implements DataSourceListener {
  private final Collection<DataSource> dataSources = new HashSet<>();
  private static final String NAME = marktr("Mapillary Point Objects");
  private static final int HATCHED_SIZE = 15;
  private final DataSet followDataSet;
  private static final String PAINT_STYLE_SOURCE = "resource://mapcss/Mapillary.mapcss";
  private static MapCSSStyleSource mapcss;

  private static MapCSSStyleSource getMapCSSStyle() {
    List<MapCSSStyleSource> styles = MapPaintStyles.getStyles().getStyleSources().parallelStream().filter(
      MapCSSStyleSource.class::isInstance
    ).map(MapCSSStyleSource.class::cast).filter(s -> PAINT_STYLE_SOURCE.equals(s.url))
      .collect(Collectors.toList());
    mapcss = styles.isEmpty() ? new MapCSSStyleSource(PAINT_STYLE_SOURCE, "Mapillary", "Mapillary Point Objects")
      : styles.get(0);
    return mapcss;
  }

  /**
   * a texture for non-downloaded area Copied from OsmDataLayer
   */
  private static volatile BufferedImage hatched;

  static {
    createHatchTexture();
  }

  /**
   * Initialize the hatch pattern used to paint the non-downloaded area
   */
  public static void createHatchTexture() {
    BufferedImage bi = new BufferedImage(HATCHED_SIZE, HATCHED_SIZE, BufferedImage.TYPE_INT_ARGB);
    Graphics2D big = bi.createGraphics();
    big.setColor(getBackgroundColor());
    Composite comp = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f);
    big.setComposite(comp);
    big.fillRect(0, 0, HATCHED_SIZE, HATCHED_SIZE);
    big.setColor(getOutsideColor());
    big.drawLine(-1, 6, 6, -1);
    big.drawLine(4, 16, 16, 4);
    hatched = bi;
  }

  public PointObjectLayer() {
    super(new DataSet(), tr(NAME), null);
    data.setUploadPolicy(UploadPolicy.BLOCKED);
    data.setDownloadPolicy(DownloadPolicy.BLOCKED);
    data.lock();
    followDataSet = MainApplication.getLayerManager().getActiveDataSet();
    followDataSet.addDataSourceListener(this);
    this.setName(NAME + ": " + MainApplication.getLayerManager().getActiveDataLayer().getName());
    MainApplication.worker.execute(() -> followDataSet.getDataSources().forEach(this::getData));
    getMapCSSStyle();
    if (!MapPaintStyles.getStyles().getStyleSources().contains(mapcss)) {
      MapPaintStyles.addStyle(mapcss);
      mapcss.active = true;
      MapPaintStyleLoader.reloadStyle(mapcss);
      MapPaintStyles.fireMapPaintStyleEntryUpdated(MapPaintStyles.getStyles().getStyleSources().indexOf(mapcss));
    }
  }

  @Override
  public void dataSourceChange(DataSourceChangeEvent event) {
    if (SwingUtilities.isEventDispatchThread()) {
      MainApplication.worker.execute(() -> event.getAdded().forEach(this::getData));
    } else {
      event.getAdded().forEach(this::getData);
    }
  }

  public void getData(DataSource dataSource) {
    if (dataSources.add(dataSource)) {
      try {
        DataSet ds = realGetData(dataSource.bounds);
        synchronized (this) {
          data.unlock();
          data.mergeFrom(ds);
          data.addDataSource(dataSource);
        }
      } catch (IllegalDataException | IOException e) {
        Logging.error(e);
        dataSources.remove(dataSource);
      } finally {
        data.lock();
      }
    }
  }

  private static DataSet realGetData(Bounds bound) throws IllegalDataException, IOException {
    URL url = MapillaryURL.APIv3.searchMapPointObjects(bound);
    HttpClient client = HttpClient.create(url);
    OAuthUtils.addAuthenticationHeader(client);
    client.connect();
    try (InputStream stream = client.getResponse().getContent()) {
      return GeoJSONReader.parseDataSet(stream, NullProgressMonitor.INSTANCE);
    } finally {
      client.disconnect();
    }
  }

  @Override
  public Icon getIcon() {
    return MapillaryPlugin.LOGO.setSize(ImageSizes.LAYER).get();
  }

  @Override
  public void paint(final Graphics2D g, final MapView mv, Bounds box) {
    boolean active = mv.getLayerManager().getActiveLayer() == this;
    boolean inactive = false;
    boolean virtual = !inactive && mv.isVirtualNodesEnabled();

    // draw the hatched area for non-downloaded region. only draw if we're the active
    // and bounds are defined; don't draw for inactive layers or loaded GPX files etc
    if (active && DrawingPreference.SOURCE_BOUNDS_PROP.get() && !data.getDataSources().isEmpty()) {
      // initialize area with current viewport
      Rectangle b = mv.getBounds();
      // on some platforms viewport bounds seem to be offset from the left,
      // over-grow it just to be sure
      b.grow(100, 100);
      Path2D p = new Path2D.Double();

      // combine successively downloaded areas
      for (Bounds bounds : data.getDataSourceBounds()) {
        if (bounds.isCollapsed()) {
          continue;
        }
        p.append(mv.getState().getArea(bounds), false);
      }
      // subtract combined areas
      Area a = new Area(b);
      a.subtract(new Area(p));

      // paint remainder
      MapViewPoint anchor = mv.getState().getPointFor(new EastNorth(0, 0));
      Rectangle2D anchorRect = new Rectangle2D.Double(
        anchor.getInView().getX() % HATCHED_SIZE, anchor.getInView().getY() % HATCHED_SIZE, HATCHED_SIZE, HATCHED_SIZE
      );
      if (hatched != null) {
        g.setPaint(new TexturePaint(hatched, anchorRect));
      }
      try {
        g.fill(a);
      } catch (ArrayIndexOutOfBoundsException e) {
        // #16686 - AIOOBE in java.awt.TexturePaintContext$Int.setRaster
        Logging.error(e);
      }
    }

    AbstractMapRenderer painter = MapRendererFactory.getInstance().createActiveRenderer(g, mv, inactive);
    painter.enableSlowOperations(
      mv.getMapMover() == null || !mv.getMapMover().movementInProgress() || !PROPERTY_HIDE_LABELS_WHILE_DRAGGING.get()
    );
    painter.render(data, virtual, box);
    MainApplication.getMap().conflictDialog.paintConflicts(g, mv);
  }

  @Override
  public void selectionChanged(SelectionChangeEvent event) {
    super.selectionChanged(event);
    if (!event.getSelection().isEmpty() && MapillaryLayer.hasInstance()) {
      // I don't want to deal with multiple objects right now. TODO?
      OsmPrimitive prim = event.getSelection().iterator().next();
      List<Map<String, String>> detections = new ArrayList<>();
      try (JsonParser parser = Json
        .createParser(new ByteArrayInputStream(prim.get("detections").getBytes(StandardCharsets.UTF_8)))) {
        while (parser.hasNext() && JsonParser.Event.START_ARRAY == parser.next()) {
          JsonArray array = parser.getArray();
          for (JsonObject obj : array.getValuesAs(JsonObject.class)) {
            Map<String, String> detection = new HashMap<>();
            for (Map.Entry<String, JsonValue> entry : obj.entrySet()) {
              if (entry.getValue().getValueType().equals(JsonValue.ValueType.STRING)) {
                detection.putIfAbsent(entry.getKey(), ((JsonString) entry.getValue()).getString());
              } else {
                detection.putIfAbsent(entry.getKey(), entry.getValue().toString());
              }
            }
            detections.add(detection);
          }
        }
      } catch (JsonException e) {
        Logging.error(e);
      }
      MapillaryData data = MapillaryLayer.getInstance().getData();
      MapillaryImage selectedImage = data.getSelectedImage() instanceof MapillaryImage
          ? (MapillaryImage) data.getSelectedImage()
              : null;
      List<MapillaryImage> images = getImagesForDetections(data, detections);
      SwingUtilities.invokeLater(() -> data.getAllDetections(images));
      MapillaryImage toSelect = images.isEmpty() ? null : images.get(0);
      boolean inDetections = selectedImage != null && images.contains(selectedImage);

      if (!inDetections && (selectedImage == null || !selectedImage.equals(toSelect))) {
        data.setSelectedImage(toSelect);
      }
    }
    SwingUtilities.invokeLater(() -> MapillaryMainDialog.getInstance().mapillaryImageDisplay.repaint());
  }

  private static List<MapillaryImage> getImagesForDetections(MapillaryData data, List<Map<String, String>> detections) {
    return detections.stream().filter(m -> m.containsKey("image_key")).map(m -> m.get("image_key")).map(data::getImage)
        .collect(Collectors.toList());
  }

  @Override
  public Action[] getMenuEntries() {
      List<Action> actions = new ArrayList<>();
      actions.addAll(Arrays.asList(
        LayerListDialog.getInstance().createActivateLayerAction(this),
        LayerListDialog.getInstance().createShowHideLayerAction(),
        LayerListDialog.getInstance().createDeleteLayerAction(),
        SeparatorLayerAction.INSTANCE,
        LayerListDialog.getInstance().createMergeLayerAction(this)));
      actions.addAll(Arrays.asList(
        SeparatorLayerAction.INSTANCE,
        new RenameLayerAction(getAssociatedFile(), this),
        SeparatorLayerAction.INSTANCE,
        new RequestDataAction()));
      return actions.toArray(new Action[0]);
  }

  static class RequestDataAction extends AbstractAction {
    private static final long serialVersionUID = 8823333297547249069L;

    public RequestDataAction() {
      super(tr("Request Data"));
      MapillaryPlugin.LOGO.getResource().attachImageIcon(this, true);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      OpenBrowser.displayUrl("https://mapillary.github.io/mapillary_solutions/data-request/");
    }
  }

  @Override
  public synchronized void destroy() {
    super.destroy();
    followDataSet.removeDataSourceListener(this);
    List<? extends PointObjectLayer> layers = MainApplication.getLayerManager().getLayersOfType(this.getClass());
    if (layers.isEmpty() || (layers.size() == 1 && this.equals(layers.get(0))))
      MapPaintStyles.removeStyle(mapcss);
  }
}
