// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.preferences.AbstractProperty.ValueChangeEvent;
import org.openstreetmap.josm.data.preferences.AbstractProperty.ValueChangeListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
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
public class MapillaryDownloadViewAction extends JosmAction implements ValueChangeListener<String> {

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
      Shortcut.registerShortcut("Mapillary area", I18n.tr(DESCRIPTION), KeyEvent.VK_PERIOD, Shortcut.SHIFT),
      false,
      "mapillaryArea",
      true
    );
    MapillaryProperties.DOWNLOAD_MODE.addListener(this);
    initEnabledState();
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    MapillaryDownloader.downloadVisibleArea();
  }

  @Override
  protected boolean listenToSelectionChange() {
    return false;
  }

  /**
   * Enabled when the Mapillary layer is instantiated and download mode is either "osm area" or "manual".
   */
  @Override
  protected void updateEnabledState() {
    super.updateEnabledState();
    setEnabled(
      MapillaryLayer.hasInstance() && (
        MapillaryDownloader.getMode() == MapillaryDownloader.DOWNLOAD_MODE.OSM_AREA
        || MapillaryDownloader.getMode() == MapillaryDownloader.DOWNLOAD_MODE.MANUAL_ONLY
      )
    );
  }

  @Override
  public void valueChanged(ValueChangeEvent<? extends String> e) {
    updateEnabledState();
  }
}
