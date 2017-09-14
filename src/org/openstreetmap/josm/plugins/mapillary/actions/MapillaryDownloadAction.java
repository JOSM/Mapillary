// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Action that triggers the plugin. If in automatic mode, it will automatically
 * download the images in the areas where there is OSM data.
 *
 * @author nokutu
 *
 */
public class MapillaryDownloadAction extends JosmAction {

  private static final long serialVersionUID = 325060354730454948L;

  /**
   * Main constructor.
   */
  public MapillaryDownloadAction() {
    super(
        tr("Mapillary"),
        new ImageProvider(MapillaryPlugin.LOGO).setSize(ImageSizes.DEFAULT),
        tr("Create Mapillary layer"),
        Shortcut.registerShortcut("Mapillary", tr("Start Mapillary layer"), KeyEvent.VK_COMMA, Shortcut.SHIFT),
        false,
        "mapillaryDownload",
        false
    );
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    if (!MapillaryLayer.hasInstance()) {
      // A new mapillary layer is created, so the active layer is not changed
      MapillaryLayer.getInstance();
      return;
    }
    // Successive calls to this action toggle the active layer between the OSM data layer and the mapillary layer
    OsmDataLayer editLayer = MainApplication.getLayerManager().getEditLayer();
    if (MainApplication.getLayerManager().getActiveLayer() != MapillaryLayer.getInstance()) {
      MainApplication.getLayerManager().setActiveLayer(MapillaryLayer.getInstance());
    } else if (editLayer != null) {
      MainApplication.getLayerManager().setActiveLayer(editLayer);
    }
  }
}
