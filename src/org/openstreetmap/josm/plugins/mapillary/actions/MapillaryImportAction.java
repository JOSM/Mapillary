// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;

import javax.swing.JFileChooser;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandImport;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Imports a set of picture files into JOSM. They must be in jpg or png format.
 *
 * @author nokutu
 *
 */
public class MapillaryImportAction extends JosmAction {

  private static final long serialVersionUID = 4995924098228081806L;

  /**
   * Main constructor.
   */
  public MapillaryImportAction() {
    super(
        tr("Import pictures"),
        MapillaryPlugin.getProvider("icon24.png"),
        tr("Import local pictures"),
        Shortcut.registerShortcut("Import Mapillary", tr("Import pictures into Mapillary layer"),
            KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
        false,
        "mapillaryImport",
        false);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent event) {
    JFileChooser chooser = new JFileChooser();
    File startDirectory = new File(Main.pref.get("mapillary.start-directory",
        System.getProperty("user.home")));
    chooser.setCurrentDirectory(startDirectory);
    chooser.setDialogTitle(tr("Select pictures"));
    chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
    chooser.setAcceptAllFileFilterUsed(false);
    chooser.addChoosableFileFilter(ImageUtil.IMAGE_FILE_FILTER);
    chooser.setMultiSelectionEnabled(true);
    if (chooser.showOpenDialog(Main.parent) == JFileChooser.APPROVE_OPTION) {
      Set<MapillaryAbstractImage> images = new ConcurrentSkipListSet<>();
      for (File file : chooser.getSelectedFiles()) {
        Main.pref.put("mapillary.start-directory", file.getParent());
        try {
          images.addAll(ImageUtil.readImagesFrom(
              file,
              Main.map.mapView.getProjection().eastNorth2latlon(Main.map.mapView.getCenter())
          ));
        } catch (IOException e) {
          Main.error("Could not read image(s) from "+file.getAbsolutePath());
        }
      }
      MapillaryRecord.getInstance().addCommand(new CommandImport(images));
      MapillaryUtils.showAllPictures();
    }
  }
}
