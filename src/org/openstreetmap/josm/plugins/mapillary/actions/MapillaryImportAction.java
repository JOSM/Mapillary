// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentSkipListSet;

import javax.swing.JFileChooser;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.preferences.StringProperty;
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
  private static final long serialVersionUID = 4086660991261961490L;
  private final StringProperty startDirProp =
    new StringProperty("mapillary.start-directory", System.getProperty("user.home"));

  public MapillaryImportAction() {
    this(
      tr("Import pictures"),
      tr("Import local pictures"),
      "Import Mapillary",
      tr("Import pictures into Mapillary layer"),
      "mapillaryImport"
    );
  }

  /**
   * Convenience constructor, which calls the super-constructor and disables the action.
   * @param name the name of the {@link JosmAction}, as displayed in the menu
   * @param tooltip a longer description of the {@link JosmAction}, that is displayed as tooltip
   * @param shortcutId an ID for the {@link Shortcut} as described in
   *   {@link Shortcut#registerShortcut(String, String, int, int)} (the passage about the second parameter)
   * @param shortcutName a name for the shortcut (displayed to the user, so use a self-explanatory and translated
   *   {@link String}). See the description of the second parameter in
   *   {@link Shortcut#registerShortcut(String, String, int, int)}
   * @param toolbarId identifier for the toolbar preferences
   * @see JosmAction#JosmAction(String, String, String, Shortcut, boolean, String, boolean)
   * @see Shortcut#registerShortcut(String, String, int, int)
   */
  protected MapillaryImportAction(
    String name, String tooltip, String shortcutId, String shortcutName, String toolbarId
  ) {
    super(
      name,
      MapillaryPlugin.getProvider("icon24.png"),
      tooltip,
      Shortcut.registerShortcut(shortcutId, shortcutName, KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
      false,
      toolbarId,
      false
    );
    setEnabled(false);
  }

  /**
   * Shows the {@link JFileChooser} for selecting images and loads the chosen images into a {@link List}.
   * @return A {@link List} containing the loaded {@link MapillaryAbstractImage}s (might be empty but never null).
   */
  public List<MapillaryAbstractImage> chooseImages() {
    final JFileChooser chooser = new JFileChooser();
    chooser.setCurrentDirectory(new File(startDirProp.get()));
    chooser.setDialogTitle(tr("Select pictures"));
    chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
    chooser.setAcceptAllFileFilterUsed(false);
    chooser.addChoosableFileFilter(ImageUtil.IMAGE_FILE_FILTER);
    chooser.setMultiSelectionEnabled(true);

    final List<MapillaryAbstractImage> images = new ArrayList<>();

    if (chooser.showOpenDialog(Main.parent) == JFileChooser.APPROVE_OPTION) {
      for (File file : chooser.getSelectedFiles()) {
        try {
          images.addAll(ImageUtil.readImagesFrom(
              file,
              Main.map.mapView.getProjection().eastNorth2latlon(Main.map.mapView.getCenter())
          ));
        } catch (IOException e) {
          Main.error("Could not read image(s) from "+file.getAbsolutePath());
        }
      }
      if (chooser.getSelectedFiles().length >= 1) {
        final File lastSelectedFile = chooser.getSelectedFiles()[chooser.getSelectedFiles().length - 1];
        startDirProp.put(
          lastSelectedFile.getParent() == null ? lastSelectedFile.getAbsolutePath() : lastSelectedFile.getParent()
        );
      }
    }
    return images;
  }

  /**
   * Records in the history, that the given images were loaded
   * @param addedImages the images that have been loaded
   */
  public void recordChanges(List<MapillaryAbstractImage> addedImages) {
    MapillaryRecord.getInstance().addCommand(new CommandImport(new ConcurrentSkipListSet<>(addedImages)));
    MapillaryUtils.showAllPictures();
  }

  @Override
  public void actionPerformed(ActionEvent event) {
    recordChanges(chooseImages());
  }
}
