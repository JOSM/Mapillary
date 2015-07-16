package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Zooms to the currently selected image.
 *
 * @author nokutu
 *
 */
public class MapillaryZoomAction extends JosmAction implements
    MapillaryDataListener {

  private static final long serialVersionUID = -6050566219765623059L;

  /**
   * Main constructor.
   */
  public MapillaryZoomAction() {
    super(tr("Zoom to selected image"), new ImageProvider("icon24.png"),
        tr("Zoom to selected image"), Shortcut.registerShortcut(
            "Zoom Mapillary",
            tr("Zoom to the currently selected Mapillary image"),
            KeyEvent.CHAR_UNDEFINED, Shortcut.NONE), false, "mapillaryZoom",
        false);
    MapillaryData.getInstance().addListener(this);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    if (MapillaryData.getInstance().getSelectedImage() == null)
      throw new IllegalStateException();
    Main.map.mapView.zoomTo(MapillaryData.getInstance().getSelectedImage()
        .getLatLon());
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage,
      MapillaryAbstractImage newImage) {
    if (oldImage == null && newImage != null)
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.ZOOM_MENU, true);
    else if (oldImage != null && newImage == null)
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.ZOOM_MENU, false);
  }

  @Override
  public void imagesAdded() {
  }
}
