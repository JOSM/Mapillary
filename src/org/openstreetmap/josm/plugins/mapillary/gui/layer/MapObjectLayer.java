// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.util.Collection;
import java.util.HashSet;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.ImageIcon;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.NavigatableComponent;
import org.openstreetmap.josm.gui.NavigatableComponent.ZoomChangeListener;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapObjectDownloadRunnable;
import org.openstreetmap.josm.plugins.mapillary.objects.MapObject;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

public final class MapObjectLayer extends Layer implements ZoomChangeListener {
  public enum STATUS {
    DOWNLOADING("Downloading map objects…", Color.YELLOW),
    COMPLETE("All map objects loaded.", Color.GREEN),
    INCOMPLETE("Too many map objects, zoom in to see all.", Color.ORANGE),
    FAILED("Downloading map objects failed!", Color.RED);

    public final Color color;
    public final String message;

    private STATUS(final String message, final Color color) {
      this.color = color;
      this.message = message;
      I18n.marktr(message);
    }
  }

  private static MapObjectLayer instance;

  private STATUS status;
  private MapObjectDownloadRunnable downloadRunnable;
  private MapObjectDownloadRunnable nextDownloadRunnable;
  private final Collection<MapObject> objects = new HashSet<>();

  private MapObjectLayer() {
    super(I18n.tr("Mapillary objects"));
    NavigatableComponent.addZoomChangeListener(this);
    zoomChanged();
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
    return nextDownloadRunnable != null;
  }

  public void finishDownload(boolean replaceMapObjects) {
    synchronized (objects) {
      if (replaceMapObjects) {
        objects.clear();
      }
      objects.addAll(downloadRunnable.getMapObjects());
    }
    synchronized (this) {
      downloadRunnable = null;
      if (nextDownloadRunnable != null) {
        downloadRunnable = nextDownloadRunnable;
        nextDownloadRunnable = null;
        new Thread(downloadRunnable).start();
      }
    }
    new Thread(() -> {
      synchronized (objects) {
        for (MapObject object : objects) {
          object.getIcon(true);
        }
      }
      invalidate();
    }).start();
  }

  public int getObjectCount() {
    return objects.size();
  }

  public void setStatus(STATUS status) {
    synchronized (this) {
      this.status = status;
    }
    invalidate();
  }

  @Override
  public void paint(Graphics2D g, MapView mv, Bounds bbox) {
    final Collection<MapObject> displayedObjects = new HashSet<>();
    synchronized (this) {
      if (downloadRunnable != null) {
        displayedObjects.addAll(downloadRunnable.getMapObjects());
      }
    }
    displayedObjects.addAll(objects);

    for (MapObject object : displayedObjects) {
      final ImageIcon icon = object.getIcon(false);
      if (icon != null) {
        final Point p = mv.getPoint(object.getCoordinate());
        g.drawImage(
          ImageUtil.scaleImageIcon(icon, MapillaryProperties.MAPOBJECT_ICON_SIZE.get()).getImage(),
          p.x - MapillaryProperties.MAPOBJECT_ICON_SIZE.get() / 2,
          p.y - MapillaryProperties.MAPOBJECT_ICON_SIZE.get() / 2,
          null
        );
      }
    }

    synchronized (this) {
      if (status != null) {
        g.setFont(g.getFont().deriveFont(Font.PLAIN).deriveFont(12f));
        g.setColor(status.color);
        final FontMetrics fm = g.getFontMetrics();
        g.fillRect(0, mv.getHeight() - fm.getAscent() - fm.getDescent(), fm.stringWidth(status.message), fm.getAscent() + fm.getDescent());
        g.setColor(Color.BLACK);
        g.drawString(status.message, 0, mv.getHeight() - fm.getDescent());
      }
    }
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

  @Override
  public void zoomChanged() {
    synchronized (Main.class) {
      if (Main.isDisplayingMapView()) {
        scheduleDownload(Main.map.mapView.getState().getViewArea().getLatLonBoundsBox());
      }
    }
  }
}
