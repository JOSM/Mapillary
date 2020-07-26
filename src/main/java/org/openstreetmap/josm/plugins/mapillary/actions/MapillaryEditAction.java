// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.mode.EditMode;
import org.openstreetmap.josm.plugins.mapillary.mode.SelectMode;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * Changes mode of layer to Edit mode, if already in edit mode then changes to select Mode.
 *
 * @author Kishan
 */
public class MapillaryEditAction extends JosmAction {

  public MapillaryEditAction() {
    super(
      tr("Edit mode"),
      new ImageProvider("mapmode", "mapillary-edit").setSize(ImageProvider.ImageSizes.DEFAULT),
      tr("Edit pictures"),
      null,
      false,
      "mapillaryEdit",
      true
    );
    MapillaryProperties.DEVELOPER.addListener(
      valueChange -> updateEnabledState());
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    if (MapillaryLayer.getInstance().mode instanceof EditMode) {
      MapillaryLayer.getInstance().setMode(new SelectMode());
    } else {
      MapillaryLayer.getInstance().setMode(new EditMode());
    }
  }

  @Override
  protected boolean listenToSelectionChange() {
    return false;
  }

  /**
   * Enabled when mapillary layer is the active layer
   */
  @Override
  protected void updateEnabledState() {
    super.updateEnabledState();
    setEnabled(MainApplication.getLayerManager().getActiveLayer() instanceof MapillaryLayer
      && MapillaryProperties.DEVELOPER.get());
  }
}
