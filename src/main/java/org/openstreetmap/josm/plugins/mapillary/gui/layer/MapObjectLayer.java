// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.ImageIcon;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.NavigatableComponent;
import org.openstreetmap.josm.gui.NavigatableComponent.ZoomChangeListener;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapObjectDownloadRunnable;
import org.openstreetmap.josm.plugins.mapillary.model.MapObject;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;

public final class MapObjectLayer extends Layer implements ZoomChangeListener {

  public enum STATUS {
    DOWNLOADING(I18n.marktr("Downloading map objects…"), Color.YELLOW),
    COMPLETE(I18n.marktr("All map objects loaded."), Color.GREEN),
    INCOMPLETE(I18n.marktr("Too many map objects, zoom in to see all."), Color.ORANGE),
    FAILED(I18n.marktr("Downloading map objects failed!"), Color.RED);

    private final int colorValue;
    public final String message;

    STATUS(final String message, final Color color) {
      this.colorValue = color.getRGB();
      this.message = message;
    }

    /**
     * Note: The color is stored as int and each time returned as new {@link Color} instance,
     * because the class {@link Color} is not strictly immutable.
     * @return the color associated with the current status
     */
    public Color getColor() {
      return new Color(colorValue, true);
    }
  }

  private static MapObjectLayer instance;

  private STATUS status = STATUS.COMPLETE;
  private MapObjectDownloadRunnable downloadRunnable;
  private MapObjectDownloadRunnable nextDownloadRunnable;
  private final Collection<MapObject> objects = new HashSet<>();

  private final Map<String, ImageIcon> scaledIcons = new HashMap<>();

  private MapObjectLayer() {
    super(I18n.tr("Mapillary objects"));
    NavigatableComponent.addZoomChangeListener(this);
    MapillaryProperties.MAPOBJECT_ICON_SIZE.addListener(val -> {
      scaledIcons.clear();
      finishDownload(false);
    });
    zoomChanged();
  }

  private static void clearInstance() {
    synchronized (MapObjectLayer.class) {
      instance = null;
    }
  }

  public static MapObjectLayer getInstance() {
    synchronized (MapObjectLayer.class) {
      if (instance == null) {
        instance = new MapObjectLayer();
      }
      return instance;
    }
  }

  public boolean isDownloadRunnableScheduled() {
    synchronized (this) {
      return nextDownloadRunnable != null;
    }
  }

  public void finishDownload(boolean replaceMapObjects) {
    synchronized (this) {
      final MapObjectDownloadRunnable currentRunnable = downloadRunnable;
      if (currentRunnable != null) {
        synchronized (objects) {
          if (replaceMapObjects) {
            objects.clear();
          }
          objects.addAll(currentRunnable.getMapObjects());
        }
      }
      downloadRunnable = null;
      if (nextDownloadRunnable != null) {
        downloadRunnable = nextDownloadRunnable;
        nextDownloadRunnable = null;
        new Thread(downloadRunnable, "downloadMapObjects").start();
      }
    }
    new Thread(() -> {
      synchronized (objects) {
        for (MapObject object : objects) {
          if (!scaledIcons.containsKey(object.getValue())) {
            scaledIcons.put(
              object.getValue(),
              ImageUtil.scaleImageIcon(MapObject.getIcon(object.getValue()), MapillaryProperties.MAPOBJECT_ICON_SIZE.get())
            );
          }
        }
      }
      invalidate();
    }, "downloadMapObjectIcons").start();
  }

  public int getObjectCount() {
    return objects.size();
  }

  public void setStatus(STATUS status) {
    this.status = status;
    invalidate();
  }

  @Override
  public void paint(Graphics2D g, MapView mv, Bounds bbox) {
    final long startTime = System.currentTimeMillis();
    final Collection<MapObject> displayedObjects = new HashSet<>();
    synchronized (this) {
      final MapObjectDownloadRunnable currentRunnable = downloadRunnable;
      if (currentRunnable != null) {
        displayedObjects.addAll(currentRunnable.getMapObjects());
      }
    }
    displayedObjects.addAll(objects);

    for (MapObject object : displayedObjects) {
      final ImageIcon icon = scaledIcons.get(object.getValue());
      if (icon != null) {
        final Point p = mv.getPoint(object.getCoordinate());
        g.drawImage(
          icon.getImage(),
          p.x - icon.getIconWidth() / 2,
          p.y - icon.getIconHeight() / 2,
          null
        );
      }
    }

    final STATUS currentStatus = status;
    g.setFont(g.getFont().deriveFont(Font.PLAIN).deriveFont(12f));
    g.setColor(currentStatus.getColor());
    final FontMetrics fm = g.getFontMetrics();
    g.fillRect(0, mv.getHeight() - fm.getAscent() - fm.getDescent(), fm.stringWidth(I18n.tr(currentStatus.message)), fm.getAscent() + fm.getDescent());
    g.setColor(Color.BLACK);
    g.drawString(I18n.tr(currentStatus.message), 0, mv.getHeight() - fm.getDescent());
    Logging.debug("{0} painted in {1} milliseconds.", MapObjectLayer.class.getName(), System.currentTimeMillis() - startTime);
  }

  @Override
  public Icon getIcon() {
    return MapillaryPlugin.LOGO.setSize(ImageSizes.LAYER).get();
  }

  @Override
  public String getToolTipText() {
    return I18n.tr("Displays objects detected by Mapillary from their street view imagery");
  }

  @Override
  public void mergeFrom(Layer from) {
    // Not mergeable
  }

  @Override
  public boolean isMergable(Layer other) {
    return false;
  }

  @Override
  public void visitBoundingBox(BoundingXYVisitor v) {
    // Unused method enforced by the Layer class
  }

  @Override
  public Object getInfoComponent() {
    return null;
  }

  @Override
  public Action[] getMenuEntries() {
    return new Action[0];
  }

  public void scheduleDownload(final Bounds bounds) {
    synchronized(this) {
      if (downloadRunnable == null) {
        downloadRunnable = new MapObjectDownloadRunnable(this, bounds);
        new Thread(downloadRunnable).start();
      } else {
        nextDownloadRunnable = new MapObjectDownloadRunnable(this, bounds);
      }
    }
  }

  /* (non-Javadoc)
   * @see org.openstreetmap.josm.gui.layer.Layer#destroy()
   */
  @Override
  public synchronized void destroy() {
    clearInstance();
    super.destroy();
  }

  @Override
  public void zoomChanged() {
    MapView mv = MapillaryPlugin.getMapView();
    if (mv != null) {
      scheduleDownload(mv.getState().getViewArea().getLatLonBoundsBox());
    }
  }
}
