package org.openstreetmap.josm.plugins.mapillary.spi.preferences;

import org.openstreetmap.josm.tools.ListenerList;

/**
 * A class holding configuration information
 *
 * @author Taylor Smock
 */
public final class MapillaryConfig {
    private MapillaryConfig() {
        // hide constructor
    }

    private static final ListenerList<IMapillaryUrlChange> MAPILLARY_URL_CHANGE_LISTENER_LIST = ListenerList.create();

    private static IMapillaryUrls urls;

    /**
     * Add a listener for URL changes
     *
     * @param listener The listener to add
     */
    public static void addUrlChangeListener(final IMapillaryUrlChange listener) {
        if (!MAPILLARY_URL_CHANGE_LISTENER_LIST.containsListener(listener)) {
            MAPILLARY_URL_CHANGE_LISTENER_LIST.addListener(listener);
        }
    }

    /**
     * Set the current URL provider
     *
     * @param urlsProvider The provider to use
     */
    public static void setUrlsProvider(final IMapillaryUrls urlsProvider) {
        final IMapillaryUrls oldUrls = urls;
        urls = urlsProvider;
        MAPILLARY_URL_CHANGE_LISTENER_LIST.fireEvent(target -> target.urlsChanged(oldUrls, urls));
    }

    /**
     * The current URL provider
     *
     * @return get the provider
     */
    public static IMapillaryUrls getUrls() {
        return urls;
    }
}
