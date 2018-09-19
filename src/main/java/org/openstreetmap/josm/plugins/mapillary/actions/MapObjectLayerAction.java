// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import java.awt.event.ActionEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapObjectLayer;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

public class MapObjectLayerAction extends JosmAction {
  private static final long serialVersionUID = -8388752916891634738L;

  public MapObjectLayerAction() {
    super(
      I18n.tr("Mapillary object layer"),
      MapillaryPlugin.LOGO.setSize(ImageSizes.DEFAULT),
      I18n.tr("Displays the layer displaying the map objects detected by Mapillary"),
      null,
      false,
      "mapillaryObjectLayer",
      false
    );
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    GuiHelper.runInEDTAndWait(() -> {
      // Synchronization lock must be held by EDT thread
      // See {@link LayerManager#addLayer(org.openstreetmap.josm.gui.layer.Layer, boolean)}.
      synchronized (MainApplication.getLayerManager()) {
        if (!MainApplication.getLayerManager().containsLayer(MapObjectLayer.getInstance())) {
          MainApplication.getLayerManager().addLayer(MapObjectLayer.getInstance());
        }
      }
    });
  }
}
