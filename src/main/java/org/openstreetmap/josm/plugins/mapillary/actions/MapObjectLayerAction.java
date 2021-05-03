// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryKeys;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Shortcut;

public class MapObjectLayerAction extends JosmAction {
  private static final long serialVersionUID = -8388752916891634738L;
  private static final String ACTION_NAME = I18n.marktr("Mapillary traffic signs layer");
  private static final String DESCRIPTION = I18n
    .marktr("Displays the layer displaying the traffic sign objects detected by Mapillary");

  public MapObjectLayerAction() {
    super(I18n.tr(ACTION_NAME), MapillaryPlugin.LOGO.setSize(ImageSizes.DEFAULT), I18n.tr(DESCRIPTION),
      Shortcut.registerShortcut("mapillary:trafficSignLayer", ACTION_NAME, KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
      false, "mapillary:trafficSignLayer", true);
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    GuiHelper.runInEDTAndWait(() -> {
      // Synchronization lock must be held by EDT thread
      // See {@link LayerManager#addLayer(org.openstreetmap.josm.gui.layer.Layer, boolean)}.
      synchronized (MainApplication.getLayerManager()) {
        if (MainApplication.getLayerManager().getActiveDataSet() != null
          && MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).parallelStream()
            .noneMatch(p -> MapillaryKeys.MAPILLARY_TRAFFIC_SIGNS.equals(p.getInfo()))) {
          MainApplication.getLayerManager().addLayer(new PointObjectLayer(MapillaryKeys.MAPILLARY_TRAFFIC_SIGNS),
            false);
        }
      }
    });
  }

  @Override
  public void updateEnabledState() {
    super.setEnabled(MainApplication.isDisplayingMapView());
  }
}
