// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.spi.preferences;

import static org.openstreetmap.josm.plugins.mapillary.spi.preferences.ApiKeyReader.readMapillaryClientId;
import static org.openstreetmap.josm.plugins.mapillary.spi.preferences.ApiKeyReader.readValue;

/**
 * The default URL implementation
 *
 * @author Taylor Smock
 */
public class MapillaryUrls implements IMapillaryUrls {
    private static final String BASE_URL = "https://www.mapillary.com/";
    /**
     * The API key for v4
     */
    private static final String ACCESS_ID = readValue("MAPILLARY_CLIENT_TOKEN");
    /**
     * This is the client id for the application, used solely for authentication
     */
    private static final long CLIENT_ID = readMapillaryClientId();

    /**
     * This is the client secret for the application, used solely for authentication
     */
    private static final String CLIENT_SECRET = readValue("MAPILLARY_CLIENT_SECRET");

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

    @Override
    public String getAccessId() {
        return ACCESS_ID;
    }

    @Override
    public long getClientId() {
        return CLIENT_ID;
    }

    @Override
    public String getClientSecret() {
        return CLIENT_SECRET;
    }

    @Override
    public String getBaseUrl() {
        return BASE_URL;
    }
}
