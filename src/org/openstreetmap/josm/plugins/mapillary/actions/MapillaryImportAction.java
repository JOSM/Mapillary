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
import javax.swing.filechooser.FileNameExtensionFilter;

import org.apache.commons.imaging.ImageReadException;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandImport;
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
    chooser.addChoosableFileFilter(new FileNameExtensionFilter("images", "jpg", "jpeg", "png"));
    chooser.setMultiSelectionEnabled(true);
    if (chooser.showOpenDialog(Main.parent) == JFileChooser.APPROVE_OPTION) {
      Set<MapillaryAbstractImage> images = new ConcurrentSkipListSet<>();
      for (File file : chooser.getSelectedFiles()) {
        Main.pref.put("mapillary.start-directory", file.getParent());
        MapillaryLayer.getInstance();
        if (file.isDirectory()) {
          if (file.listFiles() == null)
            continue;
          for (File innerFile : file.listFiles()) {
            String extension = MapillaryUtils.getExtension(innerFile);
            try {
              if ("jpg".equals(extension) || "jpeg".equals(extension))
                images.add(MapillaryUtils.readJPG(innerFile));
              else if ("png".equals(extension))
                images.add(MapillaryUtils.readPNG(innerFile));
            } catch (ImageReadException | IOException | NullPointerException e1) {
              Main.error(e1);
            }
          }
        } else {
          String extension = MapillaryUtils.getExtension(file);
          if ("jpg".equals(extension) || "jpeg".equals(extension)) {
            try {
              images.add(MapillaryUtils.readJPG(file));
            } catch (ImageReadException ex) {
              Main.error(ex);
            } catch (IOException ex) {
              Main.error(ex);
            }
          } else if (".png".equals(file.getPath().substring(file.getPath().length() - 4))) {
            images.add(MapillaryUtils.readPNG(file));
          }
        }
      }
      MapillaryRecord.getInstance().addCommand(new CommandImport(images));
      MapillaryUtils.showAllPictures();
    }
  }
}
