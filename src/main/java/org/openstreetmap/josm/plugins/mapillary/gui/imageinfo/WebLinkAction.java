// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.Serial;
import java.net.URI;
import java.net.URISyntaxException;

import javax.swing.JOptionPane;

import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.OpenBrowser;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * An action to open web links
 */
public class WebLinkAction extends MapillaryAction {
    @Serial
    private static final long serialVersionUID = 2397830510179013823L;

    private URI uri;

    /**
     * Create a new action
     */
    public WebLinkAction() {
        super(tr("View in browser"), "link", tr("Open in browser"),
            Shortcut.registerShortcut("mapillary:open_in_browser_View_in_browser",
                tr("Mapillary: Open image in browser"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
            false, "mapillary:open_in_browser_View_in_browser", false);
    }

    /**
     * Set the URL to be opened
     *
     * @param url the url to set
     */
    public final void setURI(URI url) {
        this.uri = url;
        setEnabled(url != null);
    }

    /*
     * (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        try {
            if (uri != null) {
                OpenBrowser.displayUrl(uri);
            } else {
                throw new URISyntaxException("‹null›", "The URL is null");
            }
        } catch (URISyntaxException e1) {
            String msg = I18n.tr("Could not open the URL {0} in a browser", "‹null›");
            Logging.log(Logging.LEVEL_WARN, msg, e1);
            new Notification(msg).setIcon(JOptionPane.WARNING_MESSAGE).show();
        }
    }

}
