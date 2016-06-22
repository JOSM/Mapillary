// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
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
    super(tr("Zoom to selected image"), MapillaryPlugin
        .getProvider("icon24.png"), tr("Zoom to selected image"),
        Shortcut.registerShortcut("Zoom Mapillary", tr("Zoom to the currently selected Mapillary image"),
            KeyEvent.CHAR_UNDEFINED, Shortcut.NONE), false, "mapillaryZoom",
        false);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    if (MapillaryLayer.getInstance().getData().getSelectedImage() == null)
      throw new IllegalStateException();
    Main.map.mapView.zoomTo(MapillaryLayer.getInstance().getData()
        .getSelectedImage().getLatLon());
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
    if (oldImage == null && newImage != null) {
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getZoomMenu(), true);
    } else if (oldImage != null && newImage == null) {
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getZoomMenu(), false);
    }
  }

  @Override
  public void imagesAdded() {
    // Nothing
  }
}
