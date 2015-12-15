// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;

/**
 * Dialog shown when a set of images is uploaded.
 *
 * @author nokutu
 *
 */
public class FinishedUploadDialog extends JPanel {

  private static final long serialVersionUID = -2180924089016037840L;

  /**
   * Main constructor.
   */
  public FinishedUploadDialog() {
    this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    JLabel text = new JLabel(tr("Uploaded {0} images", PluginState.imagesUploaded));
    text.setAlignmentX(Component.CENTER_ALIGNMENT);
    this.add(text);
    JButton web = new JButton(tr("Approve upload on the website"));
    web.addActionListener(new WebAction());
    web.setAlignmentX(Component.CENTER_ALIGNMENT);
    this.add(web, Component.CENTER_ALIGNMENT);
  }

  private class WebAction implements ActionListener {
    @Override
    public void actionPerformed(ActionEvent e) {
      try {
        MapillaryUtils.browse(MapillaryURL.browseUploadImageURL());
      } catch (IOException e1) {
        Main.error(e1);
      }
    }
  }
}
