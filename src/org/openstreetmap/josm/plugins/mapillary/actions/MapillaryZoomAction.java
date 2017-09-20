// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

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
    super(
      tr("Zoom to selected image"),
      new ImageProvider(MapillaryPlugin.LOGO).setSize(ImageSizes.DEFAULT),
      tr("Zoom to the currently selected Mapillary image"),
      null,
      false,
      "mapillaryZoom",
      true
    );
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    if (MapillaryLayer.getInstance().getData().getSelectedImage() == null) {
      throw new IllegalStateException();
    }
    MainApplication.getMap().mapView.zoomTo(MapillaryLayer.getInstance().getData()
        .getSelectedImage().getMovingLatLon());
  }

  @Override
  public void imagesAdded() {
    // Nothing
  }

  @Override
  protected boolean listenToSelectionChange() {
    return false;
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
    if (oldImage == null && newImage != null) {
      setEnabled(true);
    } else if (oldImage != null && newImage == null) {
      setEnabled(false);
    }
  }

  @Override
  protected void updateEnabledState() {
    super.updateEnabledState();
    setEnabled(MapillaryLayer.hasInstance() && MapillaryLayer.getInstance().getData().getSelectedImage() != null);
  }
}
