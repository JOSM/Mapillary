// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import java.awt.Dimension;
import java.awt.event.ActionEvent;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryUploadDialog;
import org.openstreetmap.josm.plugins.mapillary.oauth.UploadUtils;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

/**
 * Action called when an upload to the Mapillary servers is going to be
 * performed. It lets you select a couple of options.
 *
 * @author nokutu
 *
 */
public class MapillaryUploadAction extends JosmAction implements MapillaryDataListener {

  private static final long serialVersionUID = -1405641273676919943L;
  private static final String TITLE = I18n.tr("Upload Mapillary images");

  /**
   * Main constructor.
   */
  public MapillaryUploadAction() {
    super(TITLE, new ImageProvider(MapillaryPlugin.LOGO).setSize(ImageSizes.DEFAULT),
        TITLE, null, false, "mapillaryUpload", true);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    MapillaryUploadDialog dialog = new MapillaryUploadDialog();
    JOptionPane pane = new JOptionPane(dialog, JOptionPane.PLAIN_MESSAGE,
        JOptionPane.OK_CANCEL_OPTION);
    JDialog dlg = pane.createDialog(MainApplication.getMainFrame(), TITLE);
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

  /**
   * Enabled if a mapillary image is selected.
   */
  @Override
  protected void updateEnabledState() {
    super.updateEnabledState();
    setEnabled(MapillaryLayer.hasInstance() && MapillaryLayer.getInstance().getData().getSelectedImage() != null);
  }

  @Override
  protected boolean listenToSelectionChange() {
    return false;
  }

  @Override
  public void imagesAdded() {
    // Enforced by {@link MapillaryDataListener}
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
    if (oldImage == null && newImage != null) {
      setEnabled(true);
    } else if (oldImage != null && newImage == null) {
      setEnabled(false);
    }
  }
}
