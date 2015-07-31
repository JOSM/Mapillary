package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.downloads.MapillaryDownloader;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * If in manual mode, downloads all the images in the current view.
 *
 * @author nokutu
 *
 */
public class MapillaryDownloadViewAction extends JosmAction {

  private static final long serialVersionUID = -6837073336175123503L;

  /**
   * Main constructor.
   */
  public MapillaryDownloadViewAction() {
    super(tr("Download Mapillary images in current view"), new ImageProvider(
        MapillaryPlugin.directory + "images" + MapillaryPlugin.SEPARATOR + "icon24.png"), tr("Download Mapillary images in current view"),
        Shortcut.registerShortcut("Mapillary area",
            tr("Download Mapillary images in current view"),
            KeyEvent.VK_PERIOD, Shortcut.SHIFT), false, "mapillaryArea", false);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    MapillaryDownloader.completeView();
  }
}
