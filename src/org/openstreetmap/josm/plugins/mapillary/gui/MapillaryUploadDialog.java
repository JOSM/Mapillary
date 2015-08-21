package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;

/**
 * @author nokutu
 *
 */
public class MapillaryUploadDialog extends JPanel {

  private static final long serialVersionUID = 2517368588113991767L;

  /** Button group for upload options. */
  public ButtonGroup group;
  /** Upload the whole sequence. */
  public JRadioButton sequence;
  /** Whether the images must be deleted after upload or not */
  public JCheckBox delete;

  /**
   * Main constructor.
   */
  public MapillaryUploadDialog() {
    setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
    if (MapillaryUser.getUsername() != null) {
      this.group = new ButtonGroup();

      this.sequence = new JRadioButton(tr("Upload selected sequence"));
      if (MapillaryLayer.getInstance().getData().getSelectedImage() == null
          || !(MapillaryLayer.getInstance().getData().getSelectedImage() instanceof MapillaryImportedImage))
        this.sequence.setEnabled(false);
      this.group.add(this.sequence);
      add(this.sequence);
      this.group.setSelected(this.sequence.getModel(), true);

      this.delete = new JCheckBox(tr("Delete after upload"));
      this.delete.setSelected(Main.pref.getBoolean(
          "mapillary.delete-after-upload", true));
      add(this.delete);
    } else {
      this.add(new JLabel("Go to setting and log in to Mapillary before uploading."));
    }
  }
}
