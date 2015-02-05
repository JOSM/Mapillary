/**
 * 
 */
package org.openstreetmap.josm.plugins.sumoconvert;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.util.Enumeration;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;

import org.openstreetmap.josm.Main;

/**
 * Main export dialog
 * @author ignacio_palermo
 *
 */
public class SumoExportDialog extends JPanel {
	// the JOptionPane that contains this dialog. required for the closeDialog() method.
    private JOptionPane optionPane;
    private JCheckBox delete;
    private JComboBox portCombo;
    

    public SumoExportDialog() {
        GridBagConstraints c = new GridBagConstraints();
        JButton refreshBtn, configBtn;

        setLayout(new GridBagLayout());

        portCombo = new JComboBox();
        
        refreshPorts();
        c.insets = new Insets(4,4,4,4);
        c.gridwidth = 1;
        c.weightx = 0.8;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 0;
        c.gridy = 0;
        add(new JLabel(tr("Port:")), c);

        c.gridwidth = 1;
        c.gridx = 1;
        c.gridy = 0;
        c.weightx = 1.5;
        add(portCombo, c);

        refreshBtn = new JButton(tr("Refresh"));
        refreshBtn.addActionListener(new ActionListener(){
                public void actionPerformed(java.awt.event.ActionEvent e){
                    refreshPorts();
                }
            });
        refreshBtn.setToolTipText(tr("refresh the port list"));
        c.gridwidth = 1;
        c.weightx = 1.0;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 2;
        c.gridy = 0;
        add(refreshBtn, c);

        configBtn = new JButton(tr("Configure"));
        configBtn.addActionListener(new ActionListener(){
                public void actionPerformed(java.awt.event.ActionEvent e){
                    System.out.println("configureing the device");
                    try{

                       
                       
                      

                    }catch(Exception ex){
                        JOptionPane.showMessageDialog(Main.parent, tr("Connection Error.") + " " + ex.toString());
                    }
                    System.out.println("configureing the device finised");
                }
            });
        configBtn.setToolTipText(tr("configure the connected DG100"));
        c.gridwidth = 1;
        c.weightx = 1.0;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 2;
        c.gridy = 1;
        add(configBtn, c);


        delete = new JCheckBox(tr("delete data after import"));
        delete.setSelected(Main.pref.getBoolean("globalsat.deleteAfterDownload", false));

        c.gridwidth = 3;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 0;
        c.gridy = 2;
        add(delete, c);
    }

    public void refreshPorts(){
        String sel = Main.pref.get("globalsat.portIdentifier");
        portCombo.setVisible(false);
        portCombo.removeAllItems();
        portCombo.setVisible(true);

    }

    public boolean deleteFilesAfterDownload(){
        return delete.isSelected();
    }

    /**
     * Has to be called after this dialog has been added to a JOptionPane.
     * @param optionPane
     */
    public void setOptionPane(JOptionPane optionPane) {
        this.optionPane = optionPane;
    }
}
