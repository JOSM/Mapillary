// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Shortcut;

/**
 *
 */
public class MapPointObjectLayerAction extends JosmAction {
  private static final long serialVersionUID = 5780337309290262545L;
  private static final String ACTION_NAME = marktr("Mapillary point object layer");
  private static final String TOOLTIP = marktr(
    "Displays the layer displaying the map point objects detected by Mapillary"
  );

  public MapPointObjectLayerAction() {
    super(
      tr(ACTION_NAME), MapillaryPlugin.LOGO.setSize(ImageSizes.DEFAULT), tr(TOOLTIP), Shortcut.registerShortcut(tr(ACTION_NAME), tr(TOOLTIP), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE), false, "mapillary:pointObjectLayer", false
    );
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    GuiHelper.runInEDTAndWait(
      () -> {
        // Synchronization lock must be held by EDT thread
        // See {@link LayerManager#addLayer(org.openstreetmap.josm.gui.layer.Layer, boolean)}.
        synchronized (MainApplication.getLayerManager()) {
          MainApplication.getLayerManager().addLayer(new PointObjectLayer(), false);
        }
      }
    );
  }

}
