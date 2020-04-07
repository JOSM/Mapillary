// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;
import static org.openstreetmap.josm.tools.I18n.trc;

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

  /**
   * The {@link JCheckBox}es where the respective tag should be searched
   */
  public final JCheckBox[] signCheckboxes = new JCheckBox[]{maxSpeed, stop, giveWay, roundabout, access, intersection, direction, uneven, noParking, noOvertaking, crossing, noTurn};
  public static final String[] SIGN_TAGS = {
    "(complementary)|(regulatory)--maximum-speed-limit", "(regulatory)|(warning)--stop",
    "(regulatory)|(warning)--yield", "(warning)|(regulatory)--roundabout", "regulatory--no-entry",
    "warning--(crossroads)|(junction)", "regulatory--(turn)|(go-straight)", "warning--(uneven)|(slippery)",
    "regulatory--no-parking", "regulatory--no-overtaking", "information--pedestrians-crossing", "regulatory--no-.+-turn"
  };

  private static MapillaryFilterChooseSigns instance;

  private MapillaryFilterChooseSigns() {
    // i18n: traffic sign
    addCheckBoxWithLabel(this, maxSpeed, true, "mapillary_sprite_source/package_signs/regulatory--maximum-speed-limit-60--g1", tr("Speed limit"));
    // i18n: traffic sign
    addCheckBoxWithLabel(this, stop, true, "mapillary_sprite_source/package_signs/regulatory--stop--g1", trc("name of the traffic sign", "Stop"));
    // i18n: traffic sign
    addCheckBoxWithLabel(this, giveWay, true, "mapillary_sprite_source/package_signs/regulatory--yield--g1", tr("Give way"));
    // i18n: traffic sign
    addCheckBoxWithLabel(this, roundabout, true, "mapillary_sprite_source/package_signs/regulatory--roundabout--g1", tr("Roundabout"));
    // i18n: traffic sign
    addCheckBoxWithLabel(this, access, true, "mapillary_sprite_source/package_signs/regulatory--no-entry--g1", tr("No entry"));
    // i18n: traffic sign
    addCheckBoxWithLabel(this, intersection, true, "mapillary_sprite_source/package_signs/warning--junction-with-side-roads--g1", tr("Intersection danger"));
    // i18n: traffic sign
    addCheckBoxWithLabel(this, direction, true, "mapillary_sprite_source/package_signs/regulatory--go-straight--g1", tr("Mandatory direction (any)"));
    // i18n: traffic sign
    addCheckBoxWithLabel(this, uneven, true, "mapillary_sprite_source/package_signs/warning--uneven-road--g1", tr("Uneven road"));
    // i18n: traffic sign
    addCheckBoxWithLabel(this, noParking, true, "mapillary_sprite_source/package_signs/regulatory--no-stopping--g1", tr("No parking"));
    // i18n: traffic sign
    addCheckBoxWithLabel(this, noOvertaking, true, "mapillary_sprite_source/package_signs/regulatory--no-overtaking--g1", tr("No overtaking"));
    // i18n: traffic sign
    addCheckBoxWithLabel(this, crossing, true, "mapillary_sprite_source/package_signs/information--pedestrians-crossing--g1", tr("Pedestrian crossing"));
    // i18n: traffic sign
    addCheckBoxWithLabel(this, noTurn, true, "mapillary_sprite_source/package_signs/regulatory--no-right-turn--g1", tr("No turn"));

    setPreferredSize(new Dimension(600, 250));
  }

  private static void addCheckBoxWithLabel(final JPanel parentPanel, final JCheckBox checkBox,
      final boolean isSelected, final String iconPath, final String labelText) {
    final JPanel checkBoxPanel = new JPanel();
    final JLabel checkBoxLabel = new JLabel(labelText);

    checkBoxLabel.setIcon(ImageProvider.get(iconPath, ImageProvider.ImageSizes.SMALLICON));
    checkBoxLabel.setLabelFor(checkBox);
    checkBoxPanel.add(checkBoxLabel);
    checkBox.setSelected(isSelected);
    checkBoxPanel.add(checkBox);
    parentPanel.add(checkBoxPanel);
  }

  /**
   * Reset the sign filters
   */
  public static void reset() {
    for (int i = 0; i < MapillaryFilterChooseSigns.SIGN_TAGS.length; i++) {
      MapillaryFilterChooseSigns.getInstance().signCheckboxes[i].setSelected(true);
    }
  }

  /**
   * Return the unique instance of the class.
   *
   * @return the unique instance of the class.
   */
  public static synchronized MapillaryFilterChooseSigns getInstance() {
    if (instance == null) {
      instance = new MapillaryFilterChooseSigns();
    }
    return instance;
  }
}
