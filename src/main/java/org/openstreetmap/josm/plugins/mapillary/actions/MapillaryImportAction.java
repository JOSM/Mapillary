// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.ImportMethodDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Shortcut;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.List;
import java.util.concurrent.ConcurrentSkipListSet;

import static org.openstreetmap.josm.tools.I18n.tr;

/**
 * Imports a set of picture files into JOSM. They must be in jpg or png format.
 */
public class MapillaryImportAction extends JosmAction {
  private static final long serialVersionUID = -6902666084216980921L;

  /**
   * Create a new MapillaryImportAction
   *
   * @see JosmAction#JosmAction(String, String, String, Shortcut, boolean, String, boolean)
   */
  public MapillaryImportAction() {
    super(tr("Import pictures"), new ImageProvider(MapillaryPlugin.LOGO).setSize(ImageSizes.DEFAULT),
      tr("Import local pictures"), Shortcut.registerShortcut("mapillary:mapillaryImportImages",
        tr("Import pictures into Mapillary layer"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
      false, "mapillary:mapillaryImportImages", false);
  }

  /**
   * Records in the history, that the given images were loaded
   *
   * @param addedImages the images that have been loaded
   */
  public static void recordChanges(List<VectorNode> addedImages) {
    VectorDataSet dataSet = MapillaryLayer.getInstance().getData();
    new ConcurrentSkipListSet<>(addedImages).forEach(dataSet::addPrimitive);
    MapillaryUtils.showAllPictures();
  }

  @Override
  public void actionPerformed(ActionEvent event) {
    new ImportMethodDialog(MainApplication.getMainFrame()).setVisible(true);
  }
}
