// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Desktop;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Locale;
import java.util.Set;

import javax.swing.SwingUtilities;

import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;

/**
 * Set of utilities.
 *
 * @author nokutu
 */
public final class MapillaryUtils {

  private static final double MIN_ZOOM_SQUARE_SIDE = 0.002;

  private MapillaryUtils() {
    // Private constructor to avoid instantiation
  }

  /**
   * Open the default browser in the given URL.
   *
   * @param url The (not-null) URL that is going to be opened.
   * @throws IOException when the URL could not be opened
   */
  public static void browse(URL url) throws IOException {
    if (url == null) {
      throw new IllegalArgumentException();
    }
    Desktop desktop = Desktop.getDesktop();
    if (desktop.isSupported(Desktop.Action.BROWSE)) {
      try {
        desktop.browse(url.toURI());
      } catch (URISyntaxException e1) {
        throw new IOException(e1);
      }
    } else {
      Runtime runtime = Runtime.getRuntime();
      runtime.exec("xdg-open " + url);
    }
  }

  /**
   * Returns the current date formatted as EXIF timestamp.
   * As timezone the default timezone of the JVM is used ({@link java.util.TimeZone#getDefault()}).
   *
   * @return A {@code String} object containing the current date.
   */
  public static String currentDate() {
    return new SimpleDateFormat("yyyy:MM:dd HH:mm:ss", Locale.UK).format(Calendar.getInstance().getTime());
  }

  /**
   * Returns current time in Epoch format (milliseconds since 1970-01-01T00:00:00+0000)
   *
   * @return The current date in Epoch format.
   */
  public static long currentTime() {
    return Calendar.getInstance().getTimeInMillis();
  }

  /**
   * Parses a string with a given format and returns the Epoch time.
   * If no timezone information is given, the default timezone of the JVM is used
   * ({@link java.util.TimeZone#getDefault()}).
   *
   * @param date   The string containing the date.
   * @param format The format of the date.
   * @return The date in Epoch format.
   * @throws ParseException if the date cannot be parsed with the given format
   */
  public static long getEpoch(String date, String format) throws ParseException {
    return new SimpleDateFormat(format, Locale.UK).parse(date).getTime();
  }

  /**
   * Calculates the decimal degree-value from a degree value given in
   * degrees-minutes-seconds-format
   *
   * @param degMinSec an array of length 3, the values in there are (in this order)
   *                  degrees, minutes and seconds
   * @param ref       the latitude or longitude reference determining if the given value
   *                  is:
   *                  <ul>
   *                  <li>north (
   *                  {@link GpsTagConstants#GPS_TAG_GPS_LATITUDE_REF_VALUE_NORTH}) or
   *                  south (
   *                  {@link GpsTagConstants#GPS_TAG_GPS_LATITUDE_REF_VALUE_SOUTH}) of
   *                  the equator</li>
   *                  <li>east (
   *                  {@link GpsTagConstants#GPS_TAG_GPS_LONGITUDE_REF_VALUE_EAST}) or
   *                  west ({@link GpsTagConstants#GPS_TAG_GPS_LONGITUDE_REF_VALUE_WEST}
   *                  ) of the equator</li>
   *                  </ul>
   * @return the decimal degree-value for the given input, negative when west of
   * 0-meridian or south of equator, positive otherwise
   * @throws IllegalArgumentException if {@code degMinSec} doesn't have length 3 or if {@code ref} is
   *                                  not one of the values mentioned above
   */
  public static double degMinSecToDouble(RationalNumber[] degMinSec, String ref) {
    if (degMinSec == null || degMinSec.length != 3) {
      throw new IllegalArgumentException("Array's length must be 3.");
    }
    for (int i = 0; i < 3; i++) {
      if (degMinSec[i] == null)
        throw new IllegalArgumentException("Null value in array.");
    }

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

  /**
   * Joins two images into the same sequence. One of them must be the last image of a sequence, the other one the beginning of a different one.
   *
   * @param mapillaryAbstractImage the first image, into whose sequence the images from the sequence of the second image are merged
   * @param mapillaryAbstractImage2 the second image, whose sequence is merged into the sequence of the first image
   */
  public static synchronized void join(
          MapillaryAbstractImage mapillaryAbstractImage,
          MapillaryAbstractImage mapillaryAbstractImage2) {
    MapillaryAbstractImage firstImage = mapillaryAbstractImage;
    MapillaryAbstractImage secondImage = mapillaryAbstractImage2;

    if (mapillaryAbstractImage.next() != null) {
      firstImage = mapillaryAbstractImage2;
      secondImage = mapillaryAbstractImage;
    }
    if (firstImage.getSequence() == null) {
      MapillarySequence seq = new MapillarySequence();
      seq.add(firstImage);
      firstImage.setSequence(seq);
    }
    if (secondImage.getSequence() == null) {
      MapillarySequence seq = new MapillarySequence();
      seq.add(secondImage);
      mapillaryAbstractImage2.setSequence(seq);
    }

    for (MapillaryAbstractImage img : secondImage.getSequence().getImages()) {
      firstImage.getSequence().add(img);
      img.setSequence(firstImage.getSequence());
    }
    if (Main.main != null)
      MapillaryData.dataUpdated();
  }

  /**
   * Zooms to fit all the {@link MapillaryAbstractImage} objects stored in the
   * database.
   */
  public static void showAllPictures() {
    showPictures(MapillaryLayer.getInstance().getData().getImages(), false);
  }

  /**
   * Zooms to fit all the given {@link MapillaryAbstractImage} objects.
   *
   * @param images The images your are zooming to.
   * @param select Whether the added images must be selected or not.
   */
  public static void showPictures(final Set<MapillaryAbstractImage> images, final boolean select) {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater( new Runnable() {
        @Override
        public void run() {
          showPictures(images, select);
        }
      } );
    } else {
      Bounds zoomBounds;
      if (images.isEmpty()) {
        zoomBounds = new Bounds(new LatLon(0, 0));
      } else {
        zoomBounds = new Bounds(images.iterator().next().getMovingLatLon());
        for (MapillaryAbstractImage img : images) {
          zoomBounds.extend(img.getMovingLatLon());
        }
      }

      // The zoom rectangle must have a minimum size.
      double latExtent = Math.max(zoomBounds.getMaxLat() - zoomBounds.getMinLat(), MIN_ZOOM_SQUARE_SIDE);
      double lonExtent = Math.max(zoomBounds.getMaxLon() - zoomBounds.getMinLon(), MIN_ZOOM_SQUARE_SIDE);
      zoomBounds = new Bounds(zoomBounds.getCenter(), latExtent, lonExtent);

      Main.map.mapView.zoomTo(zoomBounds);
      MapillaryLayer.getInstance().getData().setSelectedImage(null);
      if (select)
        MapillaryLayer.getInstance().getData().addMultiSelectedImage(images);
      if (Main.main != null)
        MapillaryData.dataUpdated();
    }

  }

  /**
   * Separates two images belonging to the same sequence. The two images have to be consecutive in the same sequence.
   * Two new sequences are created and all images up to (and including) either {@code imgA} or {@code imgB} (whichever appears first in the sequence) are put into the first of the two sequences.
   * All others are put into the second new sequence.
   *
   * @param mapillaryAbstractImage one of the images marking where to split the sequence
   * @param mapillaryAbstractImage2 the other image marking where to split the sequence, needs to be a direct neighbour of {@code mapillaryAbstractImage} in the sequence.
   */
  public static synchronized void unjoin(
          MapillaryAbstractImage mapillaryAbstractImage,
          MapillaryAbstractImage mapillaryAbstractImage2) {
    MapillaryAbstractImage firstImage = mapillaryAbstractImage;
    MapillaryAbstractImage secondImage = mapillaryAbstractImage2;

    if (mapillaryAbstractImage.next() != mapillaryAbstractImage2) {
      firstImage = mapillaryAbstractImage2;
      secondImage = mapillaryAbstractImage;
    }

    ArrayList<MapillaryAbstractImage> firstHalf = new ArrayList<>(
            firstImage.getSequence().getImages().subList(0,
                    firstImage.getSequence().getImages().indexOf(secondImage)));
    ArrayList<MapillaryAbstractImage> secondHalf = new ArrayList<>(
            firstImage.getSequence().getImages().subList(
                    firstImage.getSequence().getImages().indexOf(secondImage),
                    firstImage.getSequence().getImages().size()));

    MapillarySequence seq1 = new MapillarySequence();
    MapillarySequence seq2 = new MapillarySequence();

    for (MapillaryAbstractImage img : firstHalf) {
      img.setSequence(seq1);
      seq1.add(img);
    }
    for (MapillaryAbstractImage img : secondHalf) {
      img.setSequence(seq2);
      seq2.add(img);
    }
    if (Main.main != null)
      MapillaryData.dataUpdated();
  }

  /**
   * Updates the help text at the bottom of the window.
   */
  public static void updateHelpText() {
    StringBuilder ret = new StringBuilder();
    if (PluginState.isDownloading()) {
      ret.append(tr("Downloading Mapillary images"));
    } else if (MapillaryLayer.getInstance().getData().size() > 0) {
      ret.append(tr("Total Mapillary images: {0}", MapillaryLayer.getInstance().getData().size()));
    } else if (PluginState.isSubmittingChangeset()) {
        ret.append(tr("Submitting Mapillary Changeset"));
    } else {
      ret.append(tr("No images found"));
    }
    if (MapillaryLayer.getInstance().mode != null) {
      ret.append(" — ").append(tr(MapillaryLayer.getInstance().mode.toString()));
    }
    if (PluginState.isUploading()) {
      ret.append(" — ").append(PluginState.getUploadString());
    }
    synchronized (MapillaryUtils.class) {
      Main.map.statusLine.setHelpText(ret.toString());
    }
  }
}
