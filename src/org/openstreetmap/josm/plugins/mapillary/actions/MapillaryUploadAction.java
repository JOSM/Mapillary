// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryUploadDialog;
import org.openstreetmap.josm.plugins.mapillary.oauth.UploadUtils;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Action called when an upload to the Mapillary servers is going to be
 * performed. It lets you select a couple of options.
 *
 * @author nokutu
 *
 */
public class MapillaryUploadAction extends JosmAction implements
    MapillaryDataListener {

  private static final long serialVersionUID = -1405641273676919943L;

  /**
   * Main constructor.
   */
  public MapillaryUploadAction() {
    super(tr("Upload pictures"), MapillaryPlugin.getProvider("icon24.png"),
        tr("Upload pictures"), Shortcut.registerShortcut("Upload Mapillary",
            tr("Upload Mapillary pictures"), KeyEvent.CHAR_UNDEFINED,
            Shortcut.NONE), false, "mapillaryUpload", false);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    MapillaryUploadDialog dialog = new MapillaryUploadDialog();
    JOptionPane pane = new JOptionPane(dialog, JOptionPane.PLAIN_MESSAGE,
        JOptionPane.OK_CANCEL_OPTION);
    JDialog dlg = pane.createDialog(Main.parent, tr("Upload pictures."));
    dlg.setMinimumSize(new Dimension(400, 150));
    dlg.setVisible(true);

    if (pane.getValue() != null
        && (int) pane.getValue() == JOptionPane.OK_OPTION
        && dialog.getDelete() != null
        && dialog.getSequence().isSelected()) {
      UploadUtils.uploadSequence(
          MapillaryLayer.getInstance().getData().getSelectedImage().getSequence(),
          dialog.getDelete().isSelected()
      );
    }
  }

  @Override
  public void imagesAdded() {
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage,
      MapillaryAbstractImage newImage) {
    if (oldImage == null && newImage != null)
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getUploadMenu(), true);
    else if (oldImage != null && newImage == null)
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.getUploadMenu(), false);
  }
}
