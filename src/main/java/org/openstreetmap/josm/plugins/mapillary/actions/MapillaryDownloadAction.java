// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.List;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog.LayerListModel;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;
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
  public static final Shortcut SHORTCUT = Shortcut.registerShortcut("Mapillary", tr("Open Mapillary layer"), KeyEvent.VK_COMMA, Shortcut.SHIFT);

  /**
   * Main constructor.
   */
  public MapillaryDownloadAction() {
    super(
        tr("Mapillary"),
        new ImageProvider(MapillaryPlugin.LOGO).setSize(ImageSizes.DEFAULT),
        tr("Open Mapillary layer"),
        SHORTCUT,
        false,
        "mapillaryDownload",
        false
    );
  }

  @Override
  public void actionPerformed(ActionEvent ae) {
    if (!MapillaryLayer.hasInstance() || !MainApplication.getLayerManager().containsLayer(MapillaryLayer.getInstance())) {
      LayerListModel model = LayerListDialog.getInstance().getModel();
      model.getLayerManager().addLayer(MapillaryLayer.getInstance());
      List<Layer> selected = model.getSelectedLayers();
      int index = model.getLayers().indexOf(model.getLayerManager().getActiveDataLayer());
      model.setSelectedLayer(MapillaryLayer.getInstance());
      int mapillaryLayerIndex = model.getLayers().indexOf(MapillaryLayer.getInstance());
      while (mapillaryLayerIndex < index && mapillaryLayerIndex < model.getLayers().size()) {
        model.moveDown(mapillaryLayerIndex);
        mapillaryLayerIndex++;
      }
      selected.forEach(model::setSelectedLayer);
      return;
    }

    try {
      // Successive calls to this action toggle the active layer between the OSM data layer and the mapillary layer
      OsmDataLayer editLayer = MainApplication.getLayerManager().getEditLayer();
      if (MainApplication.getLayerManager().getActiveLayer() != MapillaryLayer.getInstance()) {
        MainApplication.getLayerManager().setActiveLayer(MapillaryLayer.getInstance());
      } else if (editLayer != null) {
        MainApplication.getLayerManager().setActiveLayer(editLayer);
      }
    } catch (IllegalArgumentException e) {
      // If the MapillaryLayer is not managed by LayerManager but you try to set it as active layer
      Logging.warn(e);
    }
  }
}
