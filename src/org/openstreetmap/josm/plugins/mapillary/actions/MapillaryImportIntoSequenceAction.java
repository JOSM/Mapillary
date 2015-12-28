// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.ConcurrentSkipListSet;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.apache.commons.imaging.ImageReadException;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandImport;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Imports a set of images and puts them in a single {@link MapillarySequence}.
 *
 * @author nokutu
 *
 */
public class MapillaryImportIntoSequenceAction extends JosmAction {

  private static final long serialVersionUID = -9190217809965894878L;

  private List<MapillaryAbstractImage> images;

  /**
   * Main constructor.
   */
  public MapillaryImportIntoSequenceAction() {
    super(tr("Import pictures into sequence"), MapillaryPlugin
        .getProvider("icon24.png"), tr("Import local pictures"), Shortcut
        .registerShortcut("Import Mapillary Sequence",
            tr("Import pictures into Mapillary layer in a sequence"),
            KeyEvent.CHAR_UNDEFINED, Shortcut.NONE), false,
        "mapillaryImportSequence", false);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    this.images = new ArrayList<>();

    JFileChooser chooser = new JFileChooser();
    File startDirectory = new File(Main.pref.get("mapillary.start-directory",
        System.getProperty("user.home")));
    chooser.setCurrentDirectory(startDirectory);
    chooser.setDialogTitle(tr("Select pictures"));
    chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
    chooser.setAcceptAllFileFilterUsed(false);
    chooser.addChoosableFileFilter(new FileNameExtensionFilter("images", "jpg", "jpeg"));
    chooser.setMultiSelectionEnabled(true);

    if (chooser.showOpenDialog(Main.parent) == JFileChooser.APPROVE_OPTION) {
      for (File file : chooser.getSelectedFiles()) {
        if (file == null)
          break;
        Main.pref.put("mapillary.start-directory", file.getParent());
        MapillaryLayer.getInstance();
        if (file.isDirectory()) {
          for (File file2 : file.listFiles()) {
            String extension = MapillaryUtils.getExtension(file2);
            try {
              if ("jpg".equals(extension) || "jpeg".equals(extension))
                MapillaryUtils.readJPG(file2, true);
            } catch (ImageReadException | NullPointerException | IOException e) {
              Main.error(e);
            }
          }
        } else {
          String extension = MapillaryUtils.getExtension(file);
          if ("jpg".equals(extension) || "jpeg".equals(extension)) {
            try {
              this.images.add(MapillaryUtils.readJPG(file, true));
            } catch (ImageReadException ex) {
              Main.error(ex);
            } catch (IOException ex) {
              Main.error(ex);
            } catch (IllegalArgumentException ex) {
              // Ignored image.
            }
          }
        }
      }
      joinImages();
      MapillaryRecord.getInstance().addCommand(new CommandImport(new ConcurrentSkipListSet(images)));
    }
    MapillaryUtils.showAllPictures();
  }

  /**
   * Joins all the images in a unique {@link MapillarySequence}.
   */
  public void joinImages() {
    Collections.sort(this.images, new MapillaryEpochComparator());
    MapillarySequence seq = new MapillarySequence();
    for (MapillaryAbstractImage img : this.images) {
      seq.add(img);
      img.setSequence(seq);
    }
  }

  /**
   * Comparator that compares two {@link MapillaryAbstractImage} objects
   * depending on the time they were taken.
   *
   * @author nokutu
   *
   */
  public class MapillaryEpochComparator implements
      Comparator<MapillaryAbstractImage> {

    @Override
    public int compare(MapillaryAbstractImage arg0, MapillaryAbstractImage arg1) {
      if (arg0.getCapturedAt() < arg1.getCapturedAt())
        return -1;
      if (arg0.getCapturedAt() > arg1.getCapturedAt())
        return 1;
      return 0;
    }
  }
}
