// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;

/**
 * GUI for exporting images.
 *
 * @author nokutu
 */
public class MapillaryExportDialog extends JPanel implements ActionListener {

    private static final long serialVersionUID = 8675637080225099248L;
    /** Button to export all downloaded images. */
    public final JRadioButton all;
    /**
     * Button to export all images in the sequence of the selected MapillaryImage.
     */
    public final JRadioButton sequence;
    /**
     * Button to export all images belonging to the selected
     * {@link INode} objects.
     */
    public final JRadioButton selected;
    /** Group of button containing all the options. */
    public final ButtonGroup group;
    final JButton choose;
    final JLabel path;
    /** File chooser. */
    public JFileChooser chooser;
    private final JButton ok;

    /**
     * Main constructor.
     *
     * @param ok
     *        The button for to OK option.
     */
    public MapillaryExportDialog(JButton ok) {
        this.ok = ok;
        ok.setEnabled(false);

        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));

        this.group = new ButtonGroup();
        this.all = new JRadioButton();
        this.all.setText(tr("Export all images"));
        this.sequence = new JRadioButton();
        this.sequence.setText(tr("Export selected sequence"));
        this.selected = new JRadioButton();
        this.selected.setText(tr("Export selected images"));
        this.group.add(this.all);
        this.group.add(this.sequence);
        this.group.add(this.selected);
        // Some options are disabled depending on the circumstances
        VectorDataSet data = MapillaryLayer.getInstance().getData();
        this.sequence.setEnabled(data.getAllSelected().stream().filter(INode.class::isInstance).map(INode.class::cast)
            .anyMatch(node -> MapillaryImageUtils.getSequenceKey(node) != null));
        this.selected.setEnabled(data.getAllSelected().stream().filter(INode.class::isInstance).map(INode.class::cast)
            .anyMatch(MapillaryImageUtils::isImage));

        this.path = new JLabel(tr("Select a directory"));
        this.choose = new JButton(tr("Explore"));
        this.choose.addActionListener(this);

        // All options belong to the same JPanel so the are in line.
        JPanel jpanel = new JPanel();
        jpanel.setLayout(new BoxLayout(jpanel, BoxLayout.PAGE_AXIS));
        jpanel.add(this.all);
        jpanel.add(this.sequence);
        jpanel.add(this.selected);
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
        this.chooser.setCurrentDirectory(new java.io.File(System.getProperty("user.home")));
        this.chooser.setDialogTitle(tr("Select a directory"));
        this.chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        this.chooser.setAcceptAllFileFilterUsed(false);

        if (this.chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
            this.path.setText(this.chooser.getSelectedFile().toString());
            this.updateUI();
            this.ok.setEnabled(true);
        }
    }
}
