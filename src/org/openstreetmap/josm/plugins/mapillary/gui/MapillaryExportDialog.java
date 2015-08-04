package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.AbstractAction;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;

/**
 * GUI for exporting images.
 *
 * @author nokutu
 *
 */
public class MapillaryExportDialog extends JPanel implements ActionListener {

  private static final long serialVersionUID = 8675637080225099248L;
  protected JOptionPane optionPane;
  /** Button to export all downloaded images. */
  public JRadioButton all;
  /**
   * Button to export all images in the sequence of the selected MapillaryImage.
   */
  public JRadioButton sequence;
  /**
   * Button to export all images belonging to the selected
   * {@link MapillaryImage} objects.
   */
  public JRadioButton selected;
  /** Button to rewrite all imported images. */
  public JRadioButton rewrite;
  /** Group of button containing all the options. */
  public ButtonGroup group;
  private JButton choose;
  private JLabel path;
  /** File chooser. */
  public JFileChooser chooser;
  protected String exportDirectory;
  private JButton ok;

  /**
   * Main constructor.
   *
   * @param ok
   *          The button for to OK option.
   */
  public MapillaryExportDialog(JButton ok) {
    this.ok = ok;
    ok.setEnabled(false);

    setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));

    RewriteButtonAction action = new RewriteButtonAction(this);
    this.group = new ButtonGroup();
    this.all = new JRadioButton(action);
    this.all.setText(tr("Export all images"));
    this.sequence = new JRadioButton(action);
    this.sequence.setText(tr("Export selected sequence"));
    this.selected = new JRadioButton(action);
    this.selected.setText(tr("Export selected images"));
    this.rewrite = new JRadioButton(action);
    this.rewrite.setText(tr("Rewrite imported images"));
    this.group.add(this.all);
    this.group.add(this.sequence);
    this.group.add(this.selected);
    this.group.add(this.rewrite);
    // Some options are disabled depending on the circumstances
    if (MapillaryData.getInstance().getSelectedImage() == null
        || !(MapillaryData.getInstance().getSelectedImage() instanceof MapillaryImage && ((MapillaryImage) MapillaryData
            .getInstance().getSelectedImage()).getSequence() != null)) {
      this.sequence.setEnabled(false);
    }
    if (MapillaryData.getInstance().getMultiSelectedImages().isEmpty()) {
      this.selected.setEnabled(false);
    }
    this.rewrite.setEnabled(false);
    for (MapillaryAbstractImage img : MapillaryData.getInstance().getImages())
      if (img instanceof MapillaryImportedImage)
        this.rewrite.setEnabled(true);

    this.path = new JLabel(tr("Select a folder"));
    this.choose = new JButton(tr("Explore"));
    this.choose.addActionListener(this);

    // All options belong to the same jpanel so the are in line.
    JPanel jpanel = new JPanel();
    jpanel.setLayout(new BoxLayout(jpanel, BoxLayout.PAGE_AXIS));
    jpanel.add(this.all);
    jpanel.add(this.sequence);
    jpanel.add(this.selected);
    jpanel.add(this.rewrite);
    jpanel.setAlignmentX(Component.CENTER_ALIGNMENT);
    this.path.setAlignmentX(Component.CENTER_ALIGNMENT);
    this.choose.setAlignmentX(Component.CENTER_ALIGNMENT);

    add(jpanel);
    add(this.path);
    add(this.choose);
  }

  /**
   * Creates the folder chooser GUI.
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    this.chooser = new JFileChooser();
    this.chooser.setCurrentDirectory(new java.io.File(System
        .getProperty("user.home")));
    this.chooser.setDialogTitle(tr("Select a directory"));
    this.chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    this.chooser.setAcceptAllFileFilterUsed(false);

    if (this.chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
      this.path.setText(this.chooser.getSelectedFile().toString());
      this.updateUI();
      this.ok.setEnabled(true);
    }
  }

  /**
   * Enables/disables some parts of the panel depending if the rewrite button is
   * active.
   *
   * @author nokutu
   *
   */
  public class RewriteButtonAction extends AbstractAction {

    private static final long serialVersionUID = -9103251296651015563L;
    private String lastPath;
    private MapillaryExportDialog dlg;

    /**
     * Main constructor.
     *
     * @param dlg
     *          Parent dialog.
     */
    public RewriteButtonAction(MapillaryExportDialog dlg) {
      this.dlg = dlg;
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
      MapillaryExportDialog.this.choose.setEnabled(!MapillaryExportDialog.this.rewrite.isSelected());
      if (MapillaryExportDialog.this.rewrite.isSelected()) {
        this.lastPath = this.dlg.path.getText();
        this.dlg.path.setText(" ");
      } else if (this.lastPath != null) {
        this.dlg.path.setText(this.lastPath);
      }

    }

  }
}
