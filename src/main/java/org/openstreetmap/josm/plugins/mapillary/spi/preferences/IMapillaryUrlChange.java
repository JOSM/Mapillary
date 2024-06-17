// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.spi.preferences;

/**
 * Used to indicate that urls have changed
 *
 * @author Taylor Smock
 */
public interface IMapillaryUrlChange {
    /**
     * Called when {@link MapillaryConfig} changes url implementation
     *
     * @param oldUrls The old instance
     * @param newUrls The new instance
     */
    void urlsChanged(final IMapillaryUrls oldUrls, final IMapillaryUrls newUrls);
}
