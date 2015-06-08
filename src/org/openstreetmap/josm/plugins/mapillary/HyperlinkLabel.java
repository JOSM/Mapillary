package org.openstreetmap.josm.plugins.mapillary;

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;

import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import java.awt.Desktop;

public class HyperlinkLabel extends JLabel implements ActionListener {

	/**
	 * The normal text set by the user.
	 */

	private String text;

	private URL url;

	/**
	 * Creates a new LinkLabel with the given text.
	 */

	public HyperlinkLabel() {
		super("View in website", SwingUtilities.RIGHT);
		this.addActionListener(this);
		setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));

		enableEvents(MouseEvent.MOUSE_EVENT_MASK);
	}

	/**
	 * Sets the text of the label.
	 */

	public void setText(String text) {
		super.setText("<html><font color=\"#0000CF\" size=\"2\">" + text + "</font></html>"); //$NON-NLS-1$ //$NON-NLS-2$
		this.text = text;
	}

	public void setURL(String key) {
		try {
			this.url = new URL("http://www.mapillary.com/map/im/" + key);
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Returns the text set by the user.
	 */

	public String getNormalText() {
		return text;
	}

	/**
	 * Processes mouse events and responds to clicks.
	 */

	protected void processMouseEvent(MouseEvent evt) {
		super.processMouseEvent(evt);
		if (evt.getID() == MouseEvent.MOUSE_CLICKED)
			fireActionPerformed(new ActionEvent(this,
					ActionEvent.ACTION_PERFORMED, getNormalText()));
	}

	/**
	 * Adds an ActionListener to the list of listeners receiving notifications
	 * when the label is clicked.
	 */

	public void addActionListener(ActionListener listener) {
		listenerList.add(ActionListener.class, listener);
	}

	/**
	 * Removes the given ActionListener from the list of listeners receiving
	 * notifications when the label is clicked.
	 */

	public void removeActionListener(ActionListener listener) {
		listenerList.remove(ActionListener.class, listener);
	}

	/**
	 * Fires an ActionEvent to all interested listeners.
	 */

	protected void fireActionPerformed(ActionEvent evt) {
		Object[] listeners = listenerList.getListenerList();
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
		Desktop desktop = Desktop.getDesktop();
		try {
			desktop.browse(url.toURI());
		} catch (IOException | URISyntaxException ex) {
			ex.printStackTrace();
		} catch (UnsupportedOperationException ex) {
			Runtime runtime = Runtime.getRuntime();
			try {
				runtime.exec("xdg-open " + url);
			} catch (IOException exc) {
				exc.printStackTrace();
			}
		}
	}
}