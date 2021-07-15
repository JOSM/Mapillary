package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.openstreetmap.josm.tools.I18n.tr;

import org.openstreetmap.josm.data.imagery.ImageryInfo;

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

  static {
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
