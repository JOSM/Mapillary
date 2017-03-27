// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * If in "download images in visible area" mode, downloads all the images in the current view.
 *
 * @author nokutu
 *
 */
public class MapillaryDownloadViewAction extends JosmAction {

  private static final long serialVersionUID = -6837073336175123503L;
  private static final String DESCRIPTION = I18n.marktr("Download Mapillary images in current view");

  /**
   * Main constructor.
   */
  public MapillaryDownloadViewAction() {
    super(
      I18n.tr(DESCRIPTION),
      new ImageProvider(MapillaryPlugin.LOGO).setSize(ImageSizes.DEFAULT),
      I18n.tr(DESCRIPTION),
      Shortcut.registerShortcut(
        "Mapillary area", I18n.tr(DESCRIPTION),
        KeyEvent.VK_PERIOD, Shortcut.SHIFT
      ),
      false,
      "mapillaryArea",
      false
    );
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    MapillaryDownloader.downloadVisibleArea();
  }
}
