package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.function.Supplier;

import org.openstreetmap.josm.data.imagery.ImageryInfo;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.IMapillaryUrls;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;

/**
 * This class holds common keys for Mapillary objects
 */
public final class MapillaryKeys {
    // Misc
    public static final String PANORAMIC_TRUE = Boolean.TRUE.toString();
    public static final String PANORAMIC_FALSE = Boolean.FALSE.toString();

    // Detection specific
    public static final String DETECTIONS = "detections";
    public static final String IMAGE_KEY = "image_key";

    // Vector layers
    public static final String IMAGE_LAYER = "mapillary-images";
    public static final String IMPORTED_LAYER = "mapillary-imported-images";
    public static final String SEQUENCE_LAYER = "mapillary-sequences";
    public static final String EMPTY_LAYER = "mapillary-empty";

    // ImageInfos
    public static final ImageryInfo MAPILLARY_IMAGES = new ImageryInfo(tr("Mapillary Images"),
        MapillaryURL.APIv4.getImages());
    public static final ImageryInfo MAPILLARY_TRAFFIC_SIGNS = new ImageryInfo(tr("Mapillary Traffic Signs"),
        MapillaryURL.APIv4.getTrafficSigns());
    public static final ImageryInfo MAPILLARY_POINT_OBJECTS = new ImageryInfo(tr("Mapillary Point Objects"),
        MapillaryURL.APIv4.getObjectDetections());

    private static void updateExtendedUrl(final IMapillaryUrls urlProvider, final ImageryInfo info,
        final Supplier<String> supplier) {
        if (urlProvider == null) {
            info.setExtendedUrl("");
        } else {
            info.setExtendedUrl(supplier.get());
        }
    }

    static {
        // Add URL change listeners
        MapillaryConfig.addUrlChangeListener(
            (oldUrls, newUrls) -> updateExtendedUrl(newUrls, MAPILLARY_IMAGES, MapillaryURL.APIv4::getImages));
        MapillaryConfig.addUrlChangeListener((oldUrls, newUrls) -> updateExtendedUrl(newUrls, MAPILLARY_TRAFFIC_SIGNS,
            MapillaryURL.APIv4::getTrafficSigns));
        MapillaryConfig.addUrlChangeListener((oldUrls, newUrls) -> updateExtendedUrl(newUrls, MAPILLARY_POINT_OBJECTS,
            MapillaryURL.APIv4::getObjectDetections));

        // Set types to MVT
        MAPILLARY_IMAGES.setSourceType(ImageryInfo.ImageryType.MVT);
        MAPILLARY_TRAFFIC_SIGNS.setSourceType(ImageryInfo.ImageryType.MVT);
        MAPILLARY_POINT_OBJECTS.setSourceType(ImageryInfo.ImageryType.MVT);
        // Set min/max zooms
        MAPILLARY_IMAGES.setDefaultMaxZoom(14);
        MAPILLARY_IMAGES.setDefaultMinZoom(12);
        MAPILLARY_TRAFFIC_SIGNS.setDefaultMaxZoom(14);
        MAPILLARY_TRAFFIC_SIGNS.setDefaultMinZoom(14);
        MAPILLARY_POINT_OBJECTS.setDefaultMaxZoom(14);
        MAPILLARY_POINT_OBJECTS.setDefaultMinZoom(14);

        // Set as overlays
        MAPILLARY_IMAGES.setOverlay(true);
        MAPILLARY_TRAFFIC_SIGNS.setOverlay(true);
        MAPILLARY_POINT_OBJECTS.setOverlay(true);
    }

    private MapillaryKeys() {
        // No-op
    }
}
