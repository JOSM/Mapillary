// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Dimension;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.openstreetmap.josm.tools.ImageProvider;

/**
 * UI to choose which signs must be displayed.
 *
 * @author nokutu
 *
 */
public final class MapillaryFilterChooseSigns extends JPanel {

  private static final long serialVersionUID = -3513805549022406720L;

  /** Max speed signs */
  public final JCheckBox maxSpeed = new JCheckBox();
  /** Stop signs */
  public final JCheckBox stop = new JCheckBox();
  /** Give way signs */
  public final JCheckBox giveWay = new JCheckBox();
  /** Roundabout sings */
  public final JCheckBox roundabout = new JCheckBox();
  /** Forbidden access or forbidden direction signs */
  public final JCheckBox access = new JCheckBox();
  /** Intersection danger signs */
  public final JCheckBox intersection = new JCheckBox();
  /** Mandatory direction signs */
  public final JCheckBox direction = new JCheckBox();
  /** Uneven pavement signs */
  public final JCheckBox uneven = new JCheckBox();
  /** No parking signs */
  public final JCheckBox noParking = new JCheckBox();
  /** Forbidden overtaking signs */
  public final JCheckBox noOvertaking = new JCheckBox();
  /** Pedestrian crossing signs */
  public final JCheckBox crossing = new JCheckBox();
  /** Forbidden turn signs */
  public final JCheckBox noTurn = new JCheckBox();

  private static MapillaryFilterChooseSigns instance;

  private MapillaryFilterChooseSigns() {
    addCheckBoxWithLabel(this, maxSpeed, true, "signs/speed.png", tr("Speed limit"));
    addCheckBoxWithLabel(this, stop, true, "signs/stop.png", tr("Stop"));
    addCheckBoxWithLabel(this, giveWay, true, "signs/right_of_way.png", tr("Give way"));
    addCheckBoxWithLabel(this, roundabout, true, "signs/roundabout_right.png", tr("Roundabout"));
    addCheckBoxWithLabel(this, access, true, "signs/no_entry.png", tr("No entry"));
    addCheckBoxWithLabel(this, intersection, true, "signs/intersection_danger.png", tr("Intersection danger"));
    addCheckBoxWithLabel(this, direction, true, "signs/only_straight_on.png", tr("Mandatory direction (any)"));
    addCheckBoxWithLabel(this, uneven, true, "signs/uneven.png", tr("Uneven road"));
    addCheckBoxWithLabel(this, noParking, true, "signs/no_parking.png", tr("No parking"));
    addCheckBoxWithLabel(this, noOvertaking, true, "signs/no_overtaking.png", tr("No overtaking"));
    addCheckBoxWithLabel(this, crossing, true, "signs/crossing.png", tr("Pedestrian crossing"));
    addCheckBoxWithLabel(this, noTurn, true, "signs/no_turn.png", tr("No turn"));

    setPreferredSize(new Dimension(600, 150));
  }

  private void addCheckBoxWithLabel(final JPanel parentPanel, final JCheckBox checkBox,
      final boolean isSelected, final String iconPath,  final String labelText) {
    final JPanel checkBoxPanel = new JPanel();
    final JLabel checkBoxLabel = new JLabel(labelText);

    checkBoxLabel.setIcon(new ImageProvider(iconPath).get());
    checkBoxLabel.setLabelFor(checkBox);
    checkBoxPanel.add(checkBoxLabel);
    checkBox.setSelected(isSelected);
    checkBoxPanel.add(checkBox);
    parentPanel.add(checkBoxPanel);
  }

  /**
   * Return the unique instance of the class.
   *
   * @return the unique instance of the class.
   */
  public static synchronized MapillaryFilterChooseSigns getInstance() {
    if (instance == null)
      instance = new MapillaryFilterChooseSigns();
    return instance;
  }
}
