package org.openstreetmap.josm.plugins.mapillary.gui;

import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;

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

  /**
   * Main constructor.
   */
  public MapillaryUploadDialog() {
    this.group = new ButtonGroup();

    this.sequence = new JRadioButton("Upload selected sequence.");
    if (MapillaryData.getInstance().getSelectedImage() == null
        || !(MapillaryData.getInstance().getSelectedImage() instanceof MapillaryImportedImage))
      this.sequence.setEnabled(false);
    this.group.add(this.sequence);
    add(this.sequence);

    this.group.setSelected(this.sequence.getModel(), true);

  }
}
