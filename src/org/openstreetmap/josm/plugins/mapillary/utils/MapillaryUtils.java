package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.openstreetmap.josm.tools.I18n.tr;

import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;

/**
 * Set of utilities.
 *
 * @author nokutu
 *
 */
public class MapillaryUtils {
  /**
   * Updates the help text at the bottom of the window.
   */
  public static void updateHelpText() {
    String ret = "";
    if (PluginState.isDownloading())
      ret += tr("Downloading");
    else if (MapillaryLayer.getInstance().getData().size() > 0)
      ret += tr("Total images: {0}", MapillaryLayer.getInstance().getData()
          .size());
    else
      ret += tr("No images found");
    if (MapillaryLayer.getInstance().mode != null)
      ret += " -- " + tr(MapillaryLayer.getInstance().mode.toString());
    if (PluginState.isUploading())
      ret += " -- " + PluginState.getUploadString();
    Main.map.statusLine.setHelpText(ret);
  }

  /**
   * Calculates the decimal degree-value from a degree value given in
   * degrees-minutes-seconds-format
   *
   * @param degMinSec
   *          an array of length 3, the values in there are (in this order)
   *          degrees, minutes and seconds
   * @param ref
   *          the latitude or longitude reference determining if the given value
   *          is:
   *          <ul>
   *          <li>north (
   *          {@link GpsTagConstants#GPS_TAG_GPS_LATITUDE_REF_VALUE_NORTH}) or
   *          south (
   *          {@link GpsTagConstants#GPS_TAG_GPS_LATITUDE_REF_VALUE_SOUTH}) of
   *          the equator</li>
   *          <li>east (
   *          {@link GpsTagConstants#GPS_TAG_GPS_LONGITUDE_REF_VALUE_EAST}) or
   *          west ({@link GpsTagConstants#GPS_TAG_GPS_LONGITUDE_REF_VALUE_WEST}
   *          ) of the equator</li>
   *          </ul>
   * @return the decimal degree-value for the given input, negative when west of
   *         0-meridian or south of equator, positive otherwise
   * @throws IllegalArgumentException
   *           if {@code degMinSec} doesn't have length 3 or if {@code ref} is
   *           not one of the values mentioned above
   */
  // TODO: Maybe move into a separate utility class?
  public static double degMinSecToDouble(RationalNumber[] degMinSec, String ref) {
    if (degMinSec == null || degMinSec.length != 3) {
      throw new IllegalArgumentException("Array's length must be 3.");
    }
    for (int i = 0; i < 3; i++)
      if (degMinSec[i] == null)
        throw new IllegalArgumentException("Null value in array.");

    switch (ref) {
      case GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_NORTH:
      case GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_SOUTH:
      case GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF_VALUE_EAST:
      case GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF_VALUE_WEST:
        break;
      default:
        throw new IllegalArgumentException("Invalid ref.");
    }

    double result = degMinSec[0].doubleValue(); // degrees
    result += degMinSec[1].doubleValue() / 60; // minutes
    result += degMinSec[2].doubleValue() / 3600; // seconds

    if (GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_SOUTH.equals(ref)
        || GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF_VALUE_WEST.equals(ref)) {
      result *= -1;
    }

    result = 360 * ((result + 180) / 360 - Math.floor((result + 180) / 360)) - 180;
    return result;
  }
}
