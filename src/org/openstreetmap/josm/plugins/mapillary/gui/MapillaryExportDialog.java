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
  /** Button to export all images in the sequence of the selected MapillaryImage. */
  public JRadioButton sequence;
  /** Button to export all images belonging to the selected {@link MapillaryImage} objects. */
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
   * @param ok 
   */
  public MapillaryExportDialog(JButton ok) {
    this.ok = ok;
    ok.setEnabled(false);
    
    setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));

    RewriteButtonAction action = new RewriteButtonAction(this);
    group = new ButtonGroup();
    all = new JRadioButton(action);
    all.setText(tr("Export all images"));
    sequence = new JRadioButton(action);
    sequence.setText(tr("Export selected sequence"));
    selected = new JRadioButton(action);
    selected.setText(tr("Export selected images"));
    rewrite = new JRadioButton(action);
    rewrite.setText(tr("Rewrite imported images"));
    group.add(all);
    group.add(sequence);
    group.add(selected);
    group.add(rewrite);
    // Some options are disabled depending on the circumstances
    if (MapillaryData.getInstance().getSelectedImage() == null
        || !(MapillaryData.getInstance().getSelectedImage() instanceof MapillaryImage && ((MapillaryImage) MapillaryData
            .getInstance().getSelectedImage()).getSequence() != null)) {
      sequence.setEnabled(false);
    }
    if (MapillaryData.getInstance().getMultiSelectedImages().isEmpty()) {
      selected.setEnabled(false);
    }
    rewrite.setEnabled(false);
    for (MapillaryAbstractImage img : MapillaryData.getInstance().getImages())
      if (img instanceof MapillaryImportedImage)
        rewrite.setEnabled(true);

    path = new JLabel(tr("Select a folder"));
    choose = new JButton(tr("Explore"));
    choose.addActionListener(this);

    // All options belong to the same jpanel so the are in line.
    JPanel jpanel = new JPanel();
    jpanel.setLayout(new BoxLayout(jpanel, BoxLayout.PAGE_AXIS));
    jpanel.add(all);
    jpanel.add(sequence);
    jpanel.add(selected);
    jpanel.add(rewrite);
    jpanel.setAlignmentX(Component.CENTER_ALIGNMENT);
    path.setAlignmentX(Component.CENTER_ALIGNMENT);
    choose.setAlignmentX(Component.CENTER_ALIGNMENT);

    add(jpanel);
    add(path);
    add(choose);
  }

  /**
   * Creates the folder chooser GUI.
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    chooser = new JFileChooser();
    chooser.setCurrentDirectory(new java.io.File(System
        .getProperty("user.home")));
    chooser.setDialogTitle(tr("Select a directory"));
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    chooser.setAcceptAllFileFilterUsed(false);

    if (chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
      path.setText(chooser.getSelectedFile().toString());
      this.updateUI();
      ok.setEnabled(true);
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
      choose.setEnabled(!rewrite.isSelected());
      if (rewrite.isSelected()) {
        lastPath = dlg.path.getText();
        dlg.path.setText(" ");
      } else if (lastPath != null) {
        dlg.path.setText(lastPath);
      }

    }

  }
}
