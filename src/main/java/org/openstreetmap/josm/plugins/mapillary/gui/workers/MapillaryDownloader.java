// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.workers;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.json.JsonObject;
import javax.swing.SwingWorker;

import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.tools.Logging;

/**
 * A common class for downloaders
 *
 * @param <T> The primary return type
 * @param <V> The updating type
 * @see SwingWorker
 */
abstract class MapillaryDownloader<T, V> extends SwingWorker<T, V> {

    @Nullable
    static JsonObject getUrlResponse(@Nonnull URL url) {
        try {
            return OAuthUtils.getWithHeader(url);
        } catch (IOException e) {
            Logging.trace(e);
            return null;
        }
    }

    @Nullable
    static JsonObject getUrlResponse(@Nonnull String url) {
        try {
            return getUrlResponse(new URL(url));
        } catch (MalformedURLException e) {
            Logging.error(e);
        }
        return null;
    }
}
