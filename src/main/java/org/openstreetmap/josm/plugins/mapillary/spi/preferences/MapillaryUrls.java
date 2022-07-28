// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.spi.preferences;

/**
 * The default URL implementation
 *
 * @author Taylor Smock
 */
public class MapillaryUrls implements IMapillaryUrls {
    /** The base URL to get metadata */
    private static final String BASE_META_DATA_URL = "https://graph.mapillary.com/";
    /** The base URL for tiles */
    private static final String BASE_TILE_URL = "https://tiles.mapillary.com/maps/vtp/";
    /** The paint style url */
    private static final String PAINT_STYLE_URL = "https://josm.openstreetmap.de/josmfile?page=Styles/MapillaryDetections&zip=1";

    @Override
    public String getBaseMetaDataUrl() {
        return BASE_META_DATA_URL;
    }

    @Override
    public String getBaseTileUrl() {
        return BASE_TILE_URL;
    }

    @Override
    public String getPaintStyleUrl() {
        return PAINT_STYLE_URL;
    }
}
