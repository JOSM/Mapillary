package org.openstreetmap.josm.plugins.mapillary.utils;

import org.openstreetmap.josm.data.imagery.ImageryInfo;

import static org.openstreetmap.josm.tools.I18n.tr;

/**
 * This class holds common keys for Mapillary objects
 */
public final class MapillaryKeys {
  private MapillaryKeys() {
    // No-op
  }

  // Common
  public static final String CAPTURED_AT = "captured_at";
  public static final String KEY = "key";
  public static final String PANORAMIC = "pano";
  public static final String USER_KEY = "userkey";

  // Sequence specific
  public static final String INITIAL_IMAGE = "ikey";
  public static final String ORGANIZATION_KEY = "organization_key";

  // Misc
  public static final String PANORAMIC_TRUE = "1";
  public static final String PANORAMIC_FALSE = "0";

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
    MAPILLARY_TRAFFIC_SIGNS.setDefaultMaxZoom(20);
    MAPILLARY_TRAFFIC_SIGNS.setDefaultMinZoom(14);
    MAPILLARY_POINT_OBJECTS.setDefaultMaxZoom(20);
    MAPILLARY_POINT_OBJECTS.setDefaultMinZoom(14);
  }
}
