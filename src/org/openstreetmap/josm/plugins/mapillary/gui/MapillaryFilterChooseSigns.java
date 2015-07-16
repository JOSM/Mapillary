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
public class MapillaryFilterChooseSigns extends JPanel {

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

  private static MapillaryFilterChooseSigns INSTANCE;

  private MapillaryFilterChooseSigns() {
    maxSpeed.setSelected(true);
    stop.setSelected(true);
    giveWay.setSelected(true);
    roundabout.setSelected(true);
    access.setSelected(true);
    intersection.setSelected(true);
    direction.setSelected(true);
    uneven.setSelected(true);
    noParking.setSelected(true);
    noOvertaking.setSelected(true);
    crossing.setSelected(true);
    noTurn.setSelected(true);

    // Max speed sign
    JPanel maxspeedPanel = new JPanel();
    JLabel maxspeedLabel = new JLabel(tr("Speed limit"));
    maxspeedLabel.setIcon(new ImageProvider("signs/speed.png").get());
    maxspeedPanel.add(maxspeedLabel);
    maxspeedPanel.add(maxSpeed);
    this.add(maxspeedPanel);

    // Stop sign
    JPanel stopPanel = new JPanel();
    JLabel stopLabel = new JLabel(tr("Stop"));
    stopLabel.setIcon(new ImageProvider("signs/stop.png").get());
    stopPanel.add(stopLabel);
    stopPanel.add(stop);
    this.add(stopPanel);

    // Give way sign
    JPanel giveWayPanel = new JPanel();
    JLabel giveWayLabel = new JLabel(tr("Give way"));
    giveWayLabel.setIcon(new ImageProvider("signs/right_of_way.png").get());
    giveWayPanel.add(giveWayLabel);
    giveWayPanel.add(giveWay);
    this.add(giveWayPanel);

    // Roundabout sign
    JPanel roundaboutPanel = new JPanel();
    JLabel roundaboutLabel = new JLabel(tr("Give way"));
    roundaboutLabel.setIcon(new ImageProvider("signs/roundabout_right.png")
        .get());
    roundaboutPanel.add(roundaboutLabel);
    roundaboutPanel.add(roundabout);
    this.add(roundaboutPanel);

    // No entry sign
    JPanel noEntryPanel = new JPanel();
    JLabel noEntryLabel = new JLabel(tr("No entry"));
    noEntryLabel.setIcon(new ImageProvider("signs/no_entry.png").get());
    noEntryPanel.add(noEntryLabel);
    noEntryPanel.add(access);
    this.add(noEntryPanel);

    // Danger intersection
    JPanel intersectionPanel = new JPanel();
    JLabel intersectionLabel = new JLabel(tr("Intersection danger"));
    intersectionLabel
        .setIcon(new ImageProvider("signs/intersection_danger.png").get());
    intersectionPanel.add(intersectionLabel);
    intersectionPanel.add(intersection);
    this.add(intersectionPanel);

    // Mandatory direction
    JPanel directionPanel = new JPanel();
    JLabel directionLabel = new JLabel(tr("Mandatory direction (any)"));
    directionLabel.setIcon(new ImageProvider("signs/only_straight_on.png")
        .get());
    directionPanel.add(directionLabel);
    directionPanel.add(direction);
    this.add(directionPanel);

    // No turn
    JPanel noTurnPanel = new JPanel();
    JLabel noTurnLabel = new JLabel(tr("No turn"));
    noTurnLabel.setIcon(new ImageProvider("signs/no_turn.png").get());
    noTurnPanel.add(noTurnLabel);
    noTurnPanel.add(noTurn);
    this.add(noTurnPanel);

    // Uneven road
    JPanel unevenPanel = new JPanel();
    JLabel unevenLabel = new JLabel(tr("Uneven road"));
    unevenLabel.setIcon(new ImageProvider("signs/uneaven.png").get());
    unevenPanel.add(unevenLabel);
    unevenPanel.add(uneven);
    this.add(unevenPanel);

    // No parking
    JPanel noParkingPanel = new JPanel();
    JLabel noParkingLabel = new JLabel(tr("No parking"));
    noParkingLabel.setIcon(new ImageProvider("signs/no_parking.png").get());
    noParkingPanel.add(noParkingLabel);
    noParkingPanel.add(noParking);
    this.add(noParkingPanel);

    // No overtaking
    JPanel noOvertakingPanel = new JPanel();
    JLabel noOvertakingLabel = new JLabel(tr("No overtaking"));
    noOvertakingLabel.setIcon(new ImageProvider("signs/no_overtaking.png")
        .get());
    noOvertakingPanel.add(noOvertakingLabel);
    noOvertakingPanel.add(noOvertaking);
    this.add(noOvertakingPanel);

    // Pedestrian crossing
    JPanel crossingPanel = new JPanel();
    JLabel crossingLabel = new JLabel(tr("Pedestrian crossing"));
    crossingLabel.setIcon(new ImageProvider("signs/crossing.png").get());
    crossingPanel.add(crossingLabel);
    crossingPanel.add(crossing);
    this.add(crossingPanel);

    this.setPreferredSize(new Dimension(600, 150));
  }

  /**
   * Return the unique instance of the class.
   *
   * @return THe unique instance of the class.
   */
  public static MapillaryFilterChooseSigns getInstance() {
    if (INSTANCE == null)
      INSTANCE = new MapillaryFilterChooseSigns();
    return INSTANCE;
  }
}
