// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import java.awt.Color;
import java.awt.Component;
import java.awt.Toolkit;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPopupMenu;

import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

public class ClipboardAction extends AbstractAction {
  private static final long serialVersionUID = 3323536079627210533L;
  /**
   * The duration in milliseconds for which the popup will be shown
   */
  private static final long POPUP_DURATION = 3000;
  /**
   * A small popup that shows up when the key has been moved to the clipboard
   */
  private final JPopupMenu popup;
  /**
   * The component which is used as parent of the shown popup.
   * If this is <code>null</code>, no popup will be shown.
   */
  private Component popupParent;
  /**
   * The UNIX epoch time when the popup for this action was shown the last time
   */
  private long lastPopupShowTime;
  /**
   * The contents that are transfered into the clipboard when the action is executed.
   * If this is <code>null</code>, the clipboard won't be changed.
   */
  private Transferable contents;

  public ClipboardAction(final String name, final Transferable contents) {
    super(name, ImageProvider.get("copy", ImageSizes.SMALLICON));
    this.contents = contents;

    // Init popup
    popup = new JPopupMenu();
    JLabel label = new JLabel(I18n.tr("Key copied to clipboard…"));
    label.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    popup.add(label);
    popup.setBackground(new Color(0f, 0f, 0f, .5f));
    label.setForeground(Color.WHITE);
  }

  /**
   * @param contents the contents, which should be copied to the clipboard when the {@link Action} is executed
   */
  public void setContents(Transferable contents) {
    this.contents = contents;
    setEnabled(contents != null);
  }

  /**
   * Sets the component, under which the popup will be shown, which indicates that the key was copied to the clipboard.
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
      if (popupParent != null) {
        popup.show(popupParent, 0, popupParent.getHeight() + 2);
        new Thread(() -> {
          long threadStart = System.currentTimeMillis();
          lastPopupShowTime = threadStart;
          try {
            Thread.sleep(POPUP_DURATION);
          } catch (InterruptedException e1) {
            if (threadStart == lastPopupShowTime) {
              popup.setVisible(false);
            }
          }
          if (System.currentTimeMillis() >= lastPopupShowTime + POPUP_DURATION) {
            popup.setVisible(false);
          }
        }).start();
      }
    }
  }

}
