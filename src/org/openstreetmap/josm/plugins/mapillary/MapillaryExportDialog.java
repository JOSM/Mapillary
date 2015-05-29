package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

/**
 * GUI for exporting images.
 * 
 * @author nokutu
 *
 */
public class MapillaryExportDialog extends JPanel implements ActionListener {

	protected JOptionPane optionPane;
	protected JRadioButton all;
	protected JRadioButton sequence;
	protected JRadioButton selected;
	protected ButtonGroup group;
	protected JButton choose;
	protected JLabel path;
	protected JFileChooser chooser;
	protected String exportDirectory;

	public MapillaryExportDialog() {
	setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));


		group = new ButtonGroup();
		all = new JRadioButton(tr("Export all images"));
		sequence = new JRadioButton(tr("Export selected sequence"));
		selected = new JRadioButton(tr("Export selected images"));
		group.add(all);
		group.add(sequence);
		group.add(selected);
		if (MapillaryData.getInstance().getSelectedImage() == null
				|| MapillaryData.getInstance().getSelectedImage().getSequence() == null) {
			sequence.setEnabled(false);
		}
		if (MapillaryData.getInstance().getMultiSelectedImages().isEmpty()){
			selected.setEnabled(false);
		}
		path = new JLabel("Select a folder");
		choose = new JButton(tr("Explore"));
		choose.addActionListener(this);
		
		JPanel jpanel = new JPanel();
		jpanel.setLayout(new BoxLayout(jpanel, BoxLayout.PAGE_AXIS));
		jpanel.add(all);
		jpanel.add(sequence);
		jpanel.add(selected);
		//all.setAlignmentX(Component.CENTER_ALIGNMENT);
		//sequence.setAlignmentX(Component.CENTER_ALIGNMENT);
		jpanel.setAlignmentX(Component.CENTER_ALIGNMENT);
		path.setAlignmentX(Component.CENTER_ALIGNMENT);
		choose.setAlignmentX(Component.CENTER_ALIGNMENT);
		//container.setAlignmentX(Component.CENTER_ALIGNMENT);

		
		add(jpanel);
		add(path);
		add(choose);


	}

	/**
	 * Has to be called after this dialog has been added to a JOptionPane.
	 * 
	 * @param optionPane
	 */
	public void setOptionPane(JOptionPane optionPane) {
		this.optionPane = optionPane;
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		chooser = new JFileChooser();
		chooser.setCurrentDirectory(new java.io.File(System.getProperty("user.home")));
		chooser.setDialogTitle("Select a directory");
		chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		chooser.setAcceptAllFileFilterUsed(false);

		if (chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			path.setText(chooser.getSelectedFile().toString());
			this.updateUI();
		}
	}

}
