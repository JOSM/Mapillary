// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.ConcurrentSkipListSet;

import javax.swing.JFileChooser;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandImport;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageUtil;
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
        .getProvider("icon24.png"), tr("Import local pictures"),
        Shortcut.registerShortcut("Import Mapillary Sequence", tr("Import pictures into Mapillary layer in a sequence"),
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
    chooser.addChoosableFileFilter(ImageUtil.IMAGE_FILE_FILTER);
    chooser.setMultiSelectionEnabled(true);

    if (chooser.showOpenDialog(Main.parent) == JFileChooser.APPROVE_OPTION) {
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
      joinImages();
      MapillaryRecord.getInstance().addCommand(new CommandImport(new ConcurrentSkipListSet<>(images)));
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
  public static class MapillaryEpochComparator implements
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
