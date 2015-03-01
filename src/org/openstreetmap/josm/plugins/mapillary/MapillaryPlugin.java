package org.openstreetmap.josm.plugins.mapillary;

import org.openstreetmap.josm.gui.MapFrame;
import org.openstreetmap.josm.plugins.Plugin;
import org.openstreetmap.josm.plugins.PluginInformation;

/**
 * This is the main class for the mapillary plugin.
 *
 */
public class MapillaryPlugin extends Plugin{

    public MapillaryPlugin(PluginInformation info) {
        super(info);
    }

    /**
     * Called when the JOSM map frame is created or destroyed.
     */
    @Override
    public void mapFrameInitialized(MapFrame oldFrame, MapFrame newFrame) {
        if (oldFrame == null && newFrame != null) { // map frame added

        }
    }
}
