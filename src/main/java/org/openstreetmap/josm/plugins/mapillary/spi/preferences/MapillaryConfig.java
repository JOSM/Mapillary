// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.spi.preferences;

import org.openstreetmap.josm.spi.preferences.IBaseDirectories;
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
    private static IBaseDirectories pluginDirs;

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

    /**
     * Set the plugin directory provider
     *
     * @param pluginDirs The plugin directory provider
     */
    public static void setDirectoriesProvider(IBaseDirectories pluginDirs) {
        MapillaryConfig.pluginDirs = pluginDirs;
    }

    /**
     * Get the base directories for the plugin
     *
     * @return The base directories
     */
    public static IBaseDirectories getPluginDirs() {
        return pluginDirs;
    }
}
