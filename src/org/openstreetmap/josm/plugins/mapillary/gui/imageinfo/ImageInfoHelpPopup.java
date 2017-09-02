// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.IllegalComponentStateException;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextPane;

import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.MapillaryButton;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.SelectableLabel;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;

/**
 *
 */
public class ImageInfoHelpPopup extends JPopupMenu {
  private final Component invokerComp;
  private boolean alreadyDisplayed;

  public ImageInfoHelpPopup(Component invoker) {
    this.invokerComp = invoker;
    removeAll();
    setLayout(new BorderLayout());

    JPanel topBar = new JPanel();
    topBar.add(new JLabel(ImageProvider.get("mapillary-logo-white")));
    topBar.setBackground(MapillaryColorScheme.TOOLBAR_DARK_GREY);
    add(topBar, BorderLayout.NORTH);

    JTextPane mainText = new JTextPane();
    mainText.setContentType("text/html");
    mainText.setFont(SelectableLabel.DEFAULT_FONT);
    mainText.setText(
      "<html><div style='width:250px'>" +
      I18n.tr("The Mapillary plugin now uses a separate panel to display extra information (like the image key) and actions for the currently selected Mapillary image (like viewing it in a browser).") +
      "<br><br>" +
      I18n.tr("It can be activated by clicking the left button at the bottom of this message or the button in the toolbar on the left, which uses the same icon.") +
      "</div></html>"
    );
    add(mainText, BorderLayout.CENTER);

    JPanel bottomBar = new JPanel();
    bottomBar.setBackground(new Color(0x00FFFFFF, true));
    MapillaryButton infoButton = new MapillaryButton(ImageInfoPanel.getInstance().getToggleAction());
    infoButton.addActionListener(e -> setVisible(false));
    bottomBar.add(infoButton);
    MapillaryButton closeBtn = new MapillaryButton(new AbstractAction() {
      @Override
      public void actionPerformed(ActionEvent e) {
        setVisible(false);
        MapillaryProperties.IMAGEINFO_HELP_COUNTDOWN.put(0);
      }
    });
    closeBtn.setText(I18n.tr("I got it, close this."));
    bottomBar.add(closeBtn);
    add(bottomBar, BorderLayout.SOUTH);

    setBackground(Color.WHITE);
  }

  /**
   * @return <code>true</code> if the popup is displayed
   */
  public boolean showPopup() {
    if (!alreadyDisplayed && invokerComp.isShowing()) {
      try {
        show(invokerComp, invokerComp.getWidth(), 0);
        alreadyDisplayed = true;
        return true;
      } catch (IllegalComponentStateException e) {
        Logging.log(Logging.LEVEL_WARN, "Could not show ImageInfoHelpPopup, because probably the invoker component has disappeared from screen.", e);
      }
    }
    return false;
  }
}
