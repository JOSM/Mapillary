package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.AWTEvent;
import java.awt.Cursor;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.SwingConstants;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;

/**
 * {@link JLabel} that acts as a hyperlink.
 *
 * @author nokutu
 *
 */
public class HyperlinkLabel extends JLabel implements ActionListener {

  private static final long serialVersionUID = -8965989079294159405L;
  private String text;
  private URL url;
  private String key;

  /**
   * Creates a new HyperlinlLabel.
   */
  public HyperlinkLabel() {
    super(tr("View in website"), SwingConstants.RIGHT);
    this.addActionListener(this);
    setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));

    enableEvents(AWTEvent.MOUSE_EVENT_MASK);
  }

  /**
   * Sets the text of the label.
   */
  @Override
  public void setText(String text) {
    super
        .setText("<html><font color=\"#0000CF\" size=\"2\">" + text + "</font></html>"); //$NON-NLS-1$ //$NON-NLS-2$
    this.text = text;
  }

  /**
   * Sets a new URL, just pass the key of the image or null if there is none.
   *
   * @param key
   *          The key of the image that the hyperlink will point to.
   */
  public void setURL(String key) {
    this.key = key;
    if (key == null) {
      this.url = null;
      return;
    }
    try {
      this.url = new URL("http://www.mapillary.com/map/im/" + key);
    } catch (MalformedURLException e) {
      Main.error(e);
    }
  }

  /**
   * Returns the text set by the user.
   *
   * @return The plain-text written in the label.
   */
  public String getNormalText() {
    return this.text;
  }

  /**
   * Processes mouse events and responds to clicks.
   */
  @Override
  protected void processMouseEvent(MouseEvent e) {
    super.processMouseEvent(e);
    if (e.getID() == MouseEvent.MOUSE_CLICKED) {
      if (e.getButton() == MouseEvent.BUTTON1)
        fireActionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED,
            getNormalText()));
      if (e.getButton() == MouseEvent.BUTTON3) {
        LinkPopUp menu = new LinkPopUp();
        menu.show(e.getComponent(), e.getX(), e.getY());
      }
    }
  }

  /**
   * PopUp shown when right click on the label.
   *
   * @author nokutu
   *
   */
  private class LinkPopUp extends JPopupMenu {

    private static final long serialVersionUID = 1384054752970921552L;

    private JMenuItem copy;
    private JMenuItem copyTag;
    private JMenuItem edit;

    public LinkPopUp() {
      this.copy = new JMenuItem(tr("Copy key"));
      this.copy.addActionListener(new copyAction());
      add(this.copy);

      this.copyTag = new JMenuItem(tr("Copy key tag"));
      this.copyTag.addActionListener(new copyTagAction());
      add(this.copyTag);

      this.edit = new JMenuItem(tr("Edit on website"));
      this.edit.addActionListener(new editAction());
      add(this.edit);
    }

    private class copyAction implements ActionListener {

      @Override
      public void actionPerformed(ActionEvent arg0) {
        StringSelection stringSelection = new StringSelection(
            ((MapillaryImage) MapillaryMainDialog.getInstance().getImage())
                .getKey());
        Clipboard clpbrd = Toolkit.getDefaultToolkit().getSystemClipboard();
        clpbrd.setContents(stringSelection, null);
      }
    }

    private class copyTagAction implements ActionListener {

      @Override
      public void actionPerformed(ActionEvent arg0) {
        StringSelection stringSelection = new StringSelection(
            "mapillary="
                + ((MapillaryImage) MapillaryMainDialog.getInstance()
                    .getImage()).getKey());
        Clipboard clpbrd = Toolkit.getDefaultToolkit().getSystemClipboard();
        clpbrd.setContents(stringSelection, null);
      }
    }

    private class editAction implements ActionListener {

      @Override
      public void actionPerformed(ActionEvent arg0) {
        try {
          MapillaryUtils.browse(new URL("http://www.mapillary.com/map/e/"
              + HyperlinkLabel.this.key));
        } catch (MalformedURLException e) {
          Main.error(e);
        }
      }
    }
  }

  /**
   * Adds an ActionListener to the list of listeners receiving notifications
   * when the label is clicked.
   *
   * @param listener
   *          The listener to be added.
   */
  public void addActionListener(ActionListener listener) {
    this.listenerList.add(ActionListener.class, listener);
  }

  /**
   * Removes the given ActionListener from the list of listeners receiving
   * notifications when the label is clicked.
   *
   * @param listener
   *          The listener to be added.
   */
  public void removeActionListener(ActionListener listener) {
    this.listenerList.remove(ActionListener.class, listener);
  }

  /**
   * Fires an ActionEvent to all interested listeners.
   *
   * @param evt
   */
  protected void fireActionPerformed(ActionEvent evt) {
    Object[] listeners = this.listenerList.getListenerList();
    for (int i = 0; i < listeners.length; i += 2) {
      if (listeners[i] == ActionListener.class) {
        ActionListener listener = (ActionListener) listeners[i + 1];
        listener.actionPerformed(evt);
      }
    }
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    if (this.url == null)
      return;
    MapillaryUtils.browse(this.url);
  }
}
