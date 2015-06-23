package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.openstreetmap.josm.tools.ImageProvider;

public class MapillaryFilterChooseSigns extends JPanel implements
        ActionListener {

    private final JCheckBox maxspeed = new JCheckBox();
    private final JCheckBox stop = new JCheckBox();
    private final JCheckBox giveWay = new JCheckBox();
    private final JCheckBox roundabout = new JCheckBox();
    private final JCheckBox access = new JCheckBox();

    private static MapillaryFilterChooseSigns INSTANCE;

    public MapillaryFilterChooseSigns() {
        // Max speed sign
        JPanel maxspeedPanel = new JPanel();
        JLabel maxspeedLabel = new JLabel(tr("Speed limit"));
        maxspeedLabel.setIcon(new ImageProvider(
                "styles/standard/vehicle/restriction/speed.png").get());
        maxspeedPanel.add(maxspeedLabel);
        maxspeedPanel.add(maxspeed);
        this.add(maxspeedPanel);

        // Stop sign
        JPanel stopPanel = new JPanel();
        JLabel stopLabel = new JLabel(tr("Stop"));
        stopLabel.setIcon(new ImageProvider(
                "styles/standard/vehicle/restriction/stop.png").get());
        stopPanel.add(stopLabel);
        stopPanel.add(stop);
        this.add(stopPanel);

        // Give way sign
        JPanel giveWayPanel = new JPanel();
        JLabel giveWayLabel = new JLabel(tr("Give way"));
        giveWayLabel.setIcon(new ImageProvider(
                "styles/standard/vehicle/restriction/right_of_way.png").get());
        giveWayPanel.add(giveWayLabel);
        giveWayPanel.add(giveWay);
        this.add(giveWayPanel);

        // Roundabout sign
        JPanel roundaboutPanel = new JPanel();
        JLabel roundaboutLabel = new JLabel(tr("Give way"));
        roundaboutLabel.setIcon(new ImageProvider(
                "styles/standard/vehicle/restriction/roundabout_right.png")
                .get());
        roundaboutPanel.add(roundaboutLabel);
        roundaboutPanel.add(roundabout);
        this.add(roundaboutPanel);

        // No entry sign
        // TODO need icon
        JPanel noEntryPanel = new JPanel();
        JLabel noEntryLabel = new JLabel(tr("No entry"));
        noEntryLabel.setIcon(new ImageProvider(
                "styles/standard/vehicle/restriction/access.png").get());
        noEntryPanel.add(noEntryLabel);
        noEntryPanel.add(access);
        this.add(noEntryPanel);
    }

    public static MapillaryFilterChooseSigns getInstance() {
        if (INSTANCE == null)
            INSTANCE = new MapillaryFilterChooseSigns();
        return INSTANCE;
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
        // TODO Auto-generated method stub

    }

}
