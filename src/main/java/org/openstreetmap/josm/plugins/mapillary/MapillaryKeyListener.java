// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonReader;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import javax.swing.AbstractAction;
import javax.swing.JPopupMenu;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.OsmData;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.dialogs.properties.PropertiesDialog;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;

/**
 * Listen for Mapillary keys
 *
 * @author Taylor Smock
 */
public class MapillaryKeyListener implements PopupMenuListener, Destroyable {

  private JPopupMenu menu;
  private PropertiesDialog properties;
  private final Map<JPopupMenu, List<String>> addedValues = new HashMap<>();
  private final Map<JPopupMenu, List<Component>> addedComponents = new HashMap<>();

  private static final String IMAGE_KEY = "image_key";

  public MapillaryKeyListener(PropertiesDialog properties, JPopupMenu menu) {
    this.menu = menu;
    this.properties = properties;
    menu.addPopupMenuListener(this);
  }

  @Override
  public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
    JPopupMenu popup = (JPopupMenu) e.getSource();
    properties.visitSelectedProperties((primitive, key, value) -> addAction(popup, key, value));
  }

  @Override
  public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
    JPopupMenu popup = (JPopupMenu) e.getSource();
    if (addedValues.containsKey(popup) && addedComponents.containsKey(popup)) {
      addedValues.remove(popup);
      addedComponents.get(popup).forEach(popup::remove);
      addedComponents.remove(popup);
    }
  }

  @Override
  public void popupMenuCanceled(PopupMenuEvent e) {
    // Do nothing
  }

  private void addAction(JPopupMenu popup, String key, String value) {
    if (value == null || value.trim().isEmpty() || !MapillaryLayer.hasInstance()) {
      return;
    }
    String lowerKey = key.toLowerCase(Locale.ENGLISH);
    if (Arrays.asList("mapillary", "mapillary:image").contains(lowerKey)) {
      List<String> strings = addedValues.computeIfAbsent(popup, p -> new ArrayList<>());
      List<Component> components = addedComponents.computeIfAbsent(popup, p -> new ArrayList<>());
      if (!strings.contains(value)) {
        strings.add(value);
        components.add(popup.add(new MapillaryImageKeyAction(value)));
      }
      // TODO only if not filtering map features
    } else if ("mapillary:map_feature".equals(lowerKey)
      && !MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).isEmpty()) {
      List<String> strings = addedValues.computeIfAbsent(popup, p -> new ArrayList<>());
      List<Component> components = addedComponents.computeIfAbsent(popup, p -> new ArrayList<>());
      if (!strings.contains(value)) {
        strings.add(value);
        components.add(popup.add(new MapillaryDetectionKeyAction(value)));
      }
    }
  }

  @Override
  public void destroy() {
    if (menu != null) {
      menu.removePopupMenuListener(this);
    }
    properties = null;
    menu = null;
  }

  private static class MapillaryImageKeyAction extends AbstractAction {
    private static final long serialVersionUID = -4129937925620244251L;
    private final String imageKey;

    MapillaryImageKeyAction(String imageKey) {
      new ImageProvider("mapillary-logo").getResource().attachImageIcon(this, true);
      this.imageKey = imageKey;
      putValue(NAME, tr("Select Mapillary Image ({0})", this.imageKey));
      putValue(SHORT_DESCRIPTION, tr("Select the Mapillary Image {0} on the {1} layer (downloads if needed)",
        this.imageKey, MapillaryLayer.getInstance().getName()));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      VectorDataSet data = MapillaryLayer.getInstance().getData();
      Map<String, VectorNode> map = data.getNodes().stream().filter(image -> MapillaryImageUtils.getKey(image) != null)
        .collect(Collectors.toMap(MapillaryImageUtils::getKey, i -> i));
      Collection<String> missingImages = Stream.of(this.imageKey.split(";", 0)).filter(i -> !map.containsKey(i))
        .collect(Collectors.toList());
      map.clear(); // deallocate map
      if (!missingImages.isEmpty()) {
        MapillaryDownloader.downloadSequences(
          MapillaryDownloader.downloadImages(missingImages.toArray(new String[0])).keySet().toArray(new String[0]));
      }
      Map<String, VectorNode> newMap = data.getNodes().stream()
        .filter(image -> MapillaryImageUtils.getKey(image) != null)
        .collect(Collectors.toMap(MapillaryImageUtils::getKey, i -> i));
      GuiHelper.runInEDT(() -> MapillaryLayer.getInstance()
        .setSelected(Stream.of(this.imageKey.split(";", 0)).map(newMap::get).collect(Collectors.toSet())));
    }
  }

  private static class MapillaryDetectionKeyAction extends AbstractAction {
    private static final long serialVersionUID = -4129937925620244252L;
    private final String[] detection;

    MapillaryDetectionKeyAction(String detection) {
      new ImageProvider("mapillary-logo").getResource().attachImageIcon(this, true);
      this.detection = detection.split(";", 0);
      putValue(NAME, tr("Select Mapillary Detection ({0})", detection));
      putValue(SHORT_DESCRIPTION, tr("Select the Mapillary Detection {0}", detection));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      final VectorDataSet data = MapillaryLayer.getInstance().getData();
      final Collection<IPrimitive> detections = MainApplication.getLayerManager()
        .getLayersOfType(PointObjectLayer.class).stream().map(PointObjectLayer::getData)
        .flatMap(d -> d.allNonDeletedPrimitives().stream())
        .filter(p -> p.hasKey("key") && Stream.of(this.detection).anyMatch(d -> d.equals(p.get("key"))))
        .collect(Collectors.toSet());
      final Collection<String> images = detections.stream()
        .flatMap(d -> detectionsKeyToImages(d.get("detections")).stream()).collect(Collectors.toList());
      final Map<String, VectorNode> dataImages = data.getNodes().stream()
        .filter(i -> MapillaryImageUtils.getKey(i) != null)
        .collect(Collectors.toMap(MapillaryImageUtils::getKey, i -> i));
      Collection<String> missingImages = images.stream().filter(i -> !dataImages.containsKey(i))
        .collect(Collectors.toList());
      if (!missingImages.isEmpty()) {
        MapillaryDownloader.downloadSequences(
          MapillaryDownloader.downloadImages(missingImages.toArray(new String[0])).keySet().toArray(new String[0]));
      }
      Map<OsmData<?, ?, ?, ?>, List<IPrimitive>> selections = detections.stream()
        .collect(Collectors.groupingBy(IPrimitive::getDataSet));
      GuiHelper.runInEDT(() -> {
        for (Map.Entry<OsmData<?, ?, ?, ?>, List<IPrimitive>> selection : selections.entrySet()) {
          selection.getKey().setSelected(selection.getValue());
        }
      });
    }

    private static Collection<String> detectionsKeyToImages(String key) {
      try (InputStream keyStream = new ByteArrayInputStream(key.getBytes(StandardCharsets.UTF_8));
        JsonReader reader = Json.createReader(keyStream)) {
        JsonStructure structure = reader.read();
        Set<String> imageKeys = new TreeSet<>();
        if (structure.getValueType() == JsonValue.ValueType.ARRAY) {
          JsonArray array = structure.asJsonArray();
          for (JsonValue obj : array) {
            if (obj.getValueType() == JsonValue.ValueType.OBJECT && obj.asJsonObject().containsKey(IMAGE_KEY)
              && obj.asJsonObject().get(IMAGE_KEY).getValueType() == JsonValue.ValueType.STRING) {
              imageKeys.add(obj.asJsonObject().getString(IMAGE_KEY));
            }
          }
        }
        return imageKeys;
      } catch (IOException e) {
        Logging.error(e);
      }
      return Collections.emptyList();
    }
  }
}
