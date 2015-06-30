package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Arrays;
import java.util.Calendar;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * ToggleDialog that lets you filter the images that are being shown.
 * 
 * @author nokutu
 *
 */
public class MapillaryFilterDialog extends ToggleDialog implements
		MapillaryDataListener {

	public static MapillaryFilterDialog INSTANCE;

	private final static String[] TIME_LIST = { tr("All time"),
			tr("This year"), tr("This month"), tr("This week") };

	private final static int ROWS = 0;
	private final static int COLUMNS = 3;

	private final JPanel panel = new JPanel(new GridLayout(ROWS, COLUMNS));

	public final JCheckBox imported = new JCheckBox("Imported images");
	public final JCheckBox downloaded = new JCheckBox(
			new downloadCheckBoxAction());
	public final JCheckBox onlySigns = new JCheckBox(new OnlySignsAction());
	public final JComboBox<String> time;
	public final JTextField user;

	public final SideButton updateButton = new SideButton(new UpdateAction());
	public final SideButton resetButton = new SideButton(new ResetAction());
	public final JButton signChooser = new JButton(new SignChooserAction());

	public final MapillaryFilterChooseSigns signFilter = MapillaryFilterChooseSigns
			.getInstance();

	private final String[] SIGN_TAGS = { "prohibitory_speed_limit",
			"priority_stop", "other_give_way", "mandatory_roundabout",
			"other_no_entry", "prohibitory_no_traffic_both_ways",
			"danger_intersection", "mandatory_go", "mandatory_keep",
			"danger_priority_next_intersection", "danger_uneven_road",
			"prohibitory_no_parking", "prohibitory_on_overtaking",
			"danger_pedestrian_crossing", "prohibitory_no_u_turn",
			"prohibitory_noturn" };
	private final JCheckBox[] SIGN_CHECKBOXES = { signFilter.maxspeed,
			signFilter.stop, signFilter.giveWay, signFilter.roundabout,
			signFilter.access, signFilter.access, signFilter.intersection,
			signFilter.direction, signFilter.direction,
			signFilter.intersection, signFilter.uneven, signFilter.noParking,
			signFilter.noOvertaking, signFilter.crossing, signFilter.noTurn,
			signFilter.noTurn };

	public MapillaryFilterDialog() {
		super(tr("Mapillary filter"), "mapillaryfilter.png",
				tr("Open Mapillary filter dialog"), Shortcut.registerShortcut(
						tr("Mapillary filter"),
						tr("Open Mapillary filter dialog"), KeyEvent.VK_M,
						Shortcut.NONE), 200);

		signChooser.setEnabled(false);
		JPanel signChooserPanel = new JPanel();
		signChooserPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
		signChooserPanel.add(signChooser);

		JPanel fromPanel = new JPanel();
		fromPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
		fromPanel.add(new JLabel("From"));
		time = new JComboBox<>(TIME_LIST);
		fromPanel.add(time);

		JPanel userSearchPanel = new JPanel();
		userSearchPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
		user = new JTextField(10);
		user.addActionListener(new UpdateAction());
		userSearchPanel.add(new JLabel("User"));
		userSearchPanel.add(user);

		imported.setSelected(true);
		downloaded.setSelected(true);

		panel.add(downloaded);
		panel.add(imported);
		panel.add(onlySigns);
		panel.add(fromPanel);
		panel.add(userSearchPanel);
		panel.add(signChooserPanel);

		createLayout(panel, true,
				Arrays.asList(new SideButton[] { updateButton, resetButton }));
	}

	public static MapillaryFilterDialog getInstance() {
		if (INSTANCE == null)
			INSTANCE = new MapillaryFilterDialog();
		return INSTANCE;
	}

	@Override
	public void imagesAdded() {
		refresh();
	}

	@Override
	public void selectedImageChanged(MapillaryAbstractImage oldImage,
			MapillaryAbstractImage newImage) {
	}

	public void reset() {
		imported.setSelected(true);
		downloaded.setSelected(true);
		onlySigns.setEnabled(true);
		onlySigns.setSelected(false);
		user.setText("");
		time.setSelectedItem(TIME_LIST[0]);
		refresh();
	}

	public synchronized void refresh() {
		boolean imported = this.imported.isSelected();
		boolean downloaded = this.downloaded.isSelected();
		boolean onlySigns = this.onlySigns.isSelected();

		for (MapillaryAbstractImage img : MapillaryData.getInstance()
				.getImages()) {
			img.setVisible(true);
			if (img instanceof MapillaryImportedImage) {
				if (!imported)
					img.setVisible(false);
				continue;
			} else if (img instanceof MapillaryImage) {
				if (!downloaded) {
					img.setVisible(false);
					continue;
				}
				if (onlySigns) {
					if (((MapillaryImage) img).getSigns().isEmpty()) {
						img.setVisible(false);
						continue;
					}
					if (!checkSigns((MapillaryImage) img)) {
						img.setVisible(false);
						continue;
					}
				}
				if (!user.getText().equals("")
						&& !user.getText().equals(
								((MapillaryImage) img).getUser())) {
					img.setVisible(false);
					continue;
				}
			}
			// Calculates the amount of days since the image was taken
			Long currentTime = currentTime();
			if (time.getSelectedItem() == TIME_LIST[1]) {
				if ((currentTime - img.getCapturedAt()) / (24 * 60 * 60 * 1000) > 365) {
					img.setVisible(false);
					continue;
				}
			}
			if (time.getSelectedItem() == TIME_LIST[2]) {
				if ((currentTime - img.getCapturedAt()) / (24 * 60 * 60 * 1000) > 30) {
					img.setVisible(false);
					continue;
				}
			}
			if (time.getSelectedItem() == TIME_LIST[3]) {
				if ((currentTime - img.getCapturedAt()) / (24 * 60 * 60 * 1000) > 7) {
					img.setVisible(false);
					continue;
				}
			}
		}
		Main.map.repaint();
	}

	private boolean checkSigns(MapillaryImage img) {
		for (int i = 0; i < SIGN_TAGS.length; i++) {
			if (checkSign(img, SIGN_CHECKBOXES[i], SIGN_TAGS[i]))
				return true;
		}
		return false;
	}

	private boolean checkSign(MapillaryImage img, JCheckBox signCheckBox,
			String singString) {
		boolean contains = false;
		for (String sign : img.getSigns()) {
			if (sign.contains(singString))
				contains = true;
		}
		if (contains == signCheckBox.isSelected() && contains)
			return true;
		return false;
	}

	private long currentTime() {
		Calendar cal = Calendar.getInstance();
		return cal.getTimeInMillis();
	}

	private class downloadCheckBoxAction extends AbstractAction {

		public downloadCheckBoxAction() {
			putValue(NAME, tr("Downloaded images"));
		}

		@Override
		public void actionPerformed(ActionEvent arg0) {
			onlySigns.setEnabled(downloaded.isSelected());
		}
	}

	private class UpdateAction extends AbstractAction {
		public UpdateAction() {
			putValue(NAME, tr("Update"));
		}

		@Override
		public void actionPerformed(ActionEvent arg0) {
			MapillaryFilterDialog.getInstance().refresh();
		}
	}

	private class ResetAction extends AbstractAction {
		public ResetAction() {
			putValue(NAME, tr("Reset"));
		}

		@Override
		public void actionPerformed(ActionEvent arg0) {
			MapillaryFilterDialog.getInstance().reset();
		}
	}

	private class OnlySignsAction extends AbstractAction {
		public OnlySignsAction() {
			putValue(NAME, tr("Only images with signs"));
		}

		@Override
		public void actionPerformed(ActionEvent arg0) {
			signChooser.setEnabled(onlySigns.isSelected());
		}
	}

	/**
	 * Opens a new window where you can specifically filter signs.
	 * 
	 * @author nokutu
	 *
	 */
	private class SignChooserAction extends AbstractAction {
		public SignChooserAction() {
			putValue(NAME, tr("Choose signs"));
		}

		@Override
		public void actionPerformed(ActionEvent arg0) {
			JPanel dialog = MapillaryFilterChooseSigns.getInstance();
			JOptionPane pane = new JOptionPane(dialog,
					JOptionPane.PLAIN_MESSAGE, JOptionPane.OK_CANCEL_OPTION);
			JDialog dlg = pane.createDialog(Main.parent, tr("Choose signs"));
			dlg.setVisible(true);
			if ((int) pane.getValue() == JOptionPane.OK_OPTION)
				MapillaryFilterDialog.getInstance().refresh();
			dlg.dispose();
		}
	}

	public static void destroyInstance() {
		MapillaryFilterDialog.INSTANCE = null;
	}
}
