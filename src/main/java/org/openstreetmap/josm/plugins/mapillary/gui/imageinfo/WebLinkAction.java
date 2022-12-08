// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import java.awt.event.ActionEvent;
import java.net.URI;
import java.net.URISyntaxException;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.OpenBrowser;

/**
 * An action to open web links
 */
public class WebLinkAction extends AbstractAction {
    private static final long serialVersionUID = 2397830510179013823L;

    private URI uri;

    /**
     * Create a new action
     *
     * @param name The name to show users
     * @param uri The original URI to open
     */
    public WebLinkAction(final String name, final URI uri) {
        super(name, ImageProvider.get("link", ImageSizes.SMALLICON));
        setURI(uri);
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
