// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;

/**
 * JPanel used when uploading pictures.
 *
 * @author nokutu
 *
 */
public class MapillaryUploadDialog extends JPanel {
  private static final long serialVersionUID = 2517368588113991767L;

  /** Upload the whole sequence. */
  private JRadioButton sequence;
  /** Whether the images must be deleted after upload or not */
  private JCheckBox delete;

  /**
   * Creates the JPanel and adds the needed elements.
   */
  public MapillaryUploadDialog() {
    setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
    if (MapillaryUser.getUsername() == null) {
      this.add(new JLabel(tr("Go to setting and log in to Mapillary before uploading.")));
    } else {
      ButtonGroup group = new ButtonGroup();
      this.sequence = new JRadioButton(tr("Upload selected sequence"));
      if (!(MapillaryLayer.getInstance().getData().getSelectedImage() instanceof MapillaryImportedImage)) {
        this.sequence.setEnabled(false);
      }
      group.add(this.sequence);
      add(this.sequence);
      group.setSelected(this.sequence.getModel(), true);
      this.delete = new JCheckBox(tr("Delete after upload"));
      this.delete.setSelected(MapillaryProperties.DELETE_AFTER_UPLOAD.get());
      add(this.delete);
    }
  }

  /**
   * @return the delete checkbox of the dialog
   */
  public JCheckBox getDelete() {
    return delete;
  }

  /**
   * @return the sequence radio button of the dialog
   */
  public JRadioButton getSequence() {
    return sequence;
  }
}
