// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Shortcut;

public class MapObjectLayerAction extends JosmAction {
  private static final long serialVersionUID = -8388752916891634738L;

  public MapObjectLayerAction() {
    super(
      tr("Mapillary traffic signs layer"),
      MapillaryPlugin.LOGO.setSize(ImageSizes.DEFAULT),
      tr("Displays the layer displaying the traffic sign objects detected by Mapillary"),
      // @formatter:off
      Shortcut.registerShortcut("mapillary:trafficSignLayer", tr("Mapillary traffic signs layer"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
      // @formatter:on
      false, "mapillary:trafficSignLayer", false);
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    GuiHelper.runInEDTAndWait(() -> {
      // Synchronization lock must be held by EDT thread
      // See {@link LayerManager#addLayer(org.openstreetmap.josm.gui.layer.Layer, boolean)}.
      synchronized (MainApplication.getLayerManager()) {
        DataSet followDataSet = MainApplication.getLayerManager().getActiveDataSet();
        if (MainApplication.getLayerManager().getActiveDataSet() != null
          && MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).parallelStream()
            .filter(PointObjectLayer::hasTrafficSigns)
            .noneMatch(p -> p.followDataSet != null && p.followDataSet.equals(followDataSet))) {
          MainApplication.getLayerManager().addLayer(new PointObjectLayer(true), false);
        }
      }
    });
  }
}
