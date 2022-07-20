package org.openstreetmap.josm.plugins.mapillary.spi.preferences;

/**
 * An interface with necessary base urls
 *
 * @author Taylor Smock
 */
public interface IMapillaryUrls {
    /**
     * Get the base metadata URL
     *
     * @return The base metadata URl
     */
    String getBaseMetaDataUrl();

    /**
     * Get the base tile url
     *
     * @return The base tile url
     */
    String getBaseTileUrl();
}