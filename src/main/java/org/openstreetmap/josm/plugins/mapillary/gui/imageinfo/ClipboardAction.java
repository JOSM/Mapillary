// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Toolkit;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Popup;
import javax.swing.PopupFactory;

import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

public class ClipboardAction extends AbstractAction {
  private static final long serialVersionUID = 3323536079627210533L;
  /**
   * The duration in milliseconds for which the popup will be shown
   */
  private static final long POPUP_DURATION = 3000;
  /**
   * A small popup that shows up when the text has been moved to the clipboard
   */
  private final JComponent popupContent;
  /**
   * The component which is used as parent of the shown popup.
   * If this is <code>null</code>, no popup will be shown.
   */
  private Component popupParent;
  /**
   * The UNIX epoch time when the popup for this action was shown the last time
   */
  private long lastCopyTime;
  /**
   * The contents that are transfered into the clipboard when the action is executed.
   * If this is <code>null</code>, the clipboard won't be changed.
   */
  private Transferable contents;

  public ClipboardAction(final String name, final String successMessage, final Transferable contents) {
    super(name, ImageProvider.get("copy", ImageSizes.SMALLICON));
    this.contents = contents;

    // Init popup
    this.popupContent = new JPanel();
    popupContent.setLayout(new FlowLayout(FlowLayout.CENTER, 10, 10));
    popupContent.setBackground(MapillaryColorScheme.TOOLBAR_DARK_GREY);
    final JLabel label = new JLabel(successMessage);
    label.setForeground(Color.WHITE);
    popupContent.add(label);
  }

  /**
   * @param contents the contents, which should be copied to the clipboard when the {@link Action} is executed
   */
  public void setContents(Transferable contents) {
    this.contents = contents;
    setEnabled(contents != null);
  }

  /**
   * Sets the component, under which the popup will be shown, which indicates that the text was copied to the clipboard.
   * @param popupParent the component to set as parent of the popup
   */
  public void setPopupParent(Component popupParent) {
    this.popupParent = popupParent;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (contents != null) {
      Toolkit.getDefaultToolkit().getSystemClipboard().setContents(contents, null);
      if (popupParent != null && lastCopyTime + POPUP_DURATION < System.currentTimeMillis()) {
        final PopupFactory popupFactory = new PopupFactory();
        final Popup popup = popupFactory.getPopup(popupParent, popupContent, popupParent.getLocationOnScreen().x, popupParent.getLocationOnScreen().y + popupParent.getHeight() + 2);
        popup.show();
        new Thread(() -> {
          try {
            while (lastCopyTime + POPUP_DURATION >= System.currentTimeMillis()) {
              Thread.sleep(1000);
            }
          } catch (InterruptedException e1) {
            // Ignore interruptions, continue with closing the popup
          } finally {
            popup.hide();
          }
        }).start();
      }
      lastCopyTime = System.currentTimeMillis();
    }
  }

}
