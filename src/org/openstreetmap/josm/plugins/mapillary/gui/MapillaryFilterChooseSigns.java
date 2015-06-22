package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.openstreetmap.josm.tools.ImageProvider;

public class MapillaryFilterChooseSigns extends JPanel implements ActionListener {

    private JCheckBox maxspeed = new JCheckBox();
    
    private static MapillaryFilterChooseSigns INSTANCE;
    
    public MapillaryFilterChooseSigns() {
        JPanel maxspeedPanel = new JPanel();
        JLabel maxspeedLabel = new JLabel(tr("Speed limit"));
        maxspeedLabel.setIcon(new ImageProvider("styles/standard/vehicle/restriction/speed.png").get());
        maxspeedPanel.add(maxspeedLabel);
        maxspeedPanel.add(maxspeed);
        this.add(maxspeedPanel);
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
