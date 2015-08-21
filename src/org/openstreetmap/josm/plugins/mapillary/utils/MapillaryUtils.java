package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Desktop;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

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
 *
 */
public class MapillaryUtils {

  private static double MIN_ZOOM_SQUARE_SIDE = 0.002;

  /**
   * Updates the help text at the bottom of the window.
   */
  public static void updateHelpText() {
    String ret = "";
    if (PluginState.isDownloading())
      ret += tr("Downloading Mapillary images");
    else if (MapillaryLayer.getInstance().getData().size() > 0)
      ret += tr("Total Mapillary images: {0}", MapillaryLayer.getInstance()
          .getData().size());
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

  /**
   * Open the default browser in the given URL.
   *
   * @param url
   *          The URL that is going to be opened.
   */
  public static void browse(URL url) {
    Desktop desktop = Desktop.getDesktop();
    if (desktop.isSupported(Desktop.Action.BROWSE)) {
      try {
        desktop.browse(url.toURI());
      } catch (IOException | URISyntaxException e1) {
        Main.error(e1);
      }
    } else {
      Runtime runtime = Runtime.getRuntime();
      try {
        runtime.exec("xdg-open " + url);
      } catch (IOException exc) {
        exc.printStackTrace();
      }
    }
  }

  /**
   * Joins two images into the same sequence.
   *
   * @param mapillaryAbstractImage
   * @param mapillaryAbstractImage2
   */
  public synchronized static void join(
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
   * Separates two images belonging to the same sequence.
   *
   * @param mapillaryAbstractImage
   * @param mapillaryAbstractImage2
   */
  public synchronized static void unjoin(
      MapillaryAbstractImage mapillaryAbstractImage,
      MapillaryAbstractImage mapillaryAbstractImage2) {
    MapillaryAbstractImage firstImage = mapillaryAbstractImage;
    MapillaryAbstractImage secondImage = mapillaryAbstractImage2;

    if (mapillaryAbstractImage.next() != mapillaryAbstractImage2) {
      firstImage = mapillaryAbstractImage2;
      secondImage = mapillaryAbstractImage;
    }

    ArrayList<MapillaryAbstractImage> firstHalf = new ArrayList<>(firstImage
        .getSequence().getImages()
        .subList(0, firstImage.getSequence().getImages().indexOf(secondImage)));
    ArrayList<MapillaryAbstractImage> secondHalf = new ArrayList<>(firstImage
        .getSequence()
        .getImages()
        .subList(firstImage.getSequence().getImages().indexOf(secondImage),
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
   * Zooms to fit all the given {@link MapillaryAbstractImage} objects.
   *
   * @param images
   *          The images your are zooming to.
   * @param select
   *          Whether the added images must be selected or not.
   */
  public static void showPictures(final List<MapillaryAbstractImage> images,
      final boolean select) {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          showPictures(images, select);
        }
      });
    } else {
      double minLat = 90;
      double minLon = 180;
      double maxLat = -90;
      double maxLon = -180;
      for (MapillaryAbstractImage img : images) {
        if (img.getLatLon().lat() < minLat)
          minLat = img.getLatLon().lat();
        if (img.getLatLon().lon() < minLon)
          minLon = img.getLatLon().lon();
        if (img.getLatLon().lat() > maxLat)
          maxLat = img.getLatLon().lat();
        if (img.getLatLon().lon() > maxLon)
          maxLon = img.getLatLon().lon();
      }
      Bounds zoomBounds = new Bounds(new LatLon(minLat, minLon), new LatLon(
          maxLat, maxLon));
      // The zoom rectangle must have a minimum size.
      double latExtent = zoomBounds.getMaxLat() - zoomBounds.getMinLat() >= MIN_ZOOM_SQUARE_SIDE ? zoomBounds
          .getMaxLat() - zoomBounds.getMinLat()
          : MIN_ZOOM_SQUARE_SIDE;
      double lonExtent = zoomBounds.getMaxLon() - zoomBounds.getMinLon() >= MIN_ZOOM_SQUARE_SIDE ? zoomBounds
          .getMaxLon() - zoomBounds.getMinLon()
          : MIN_ZOOM_SQUARE_SIDE;
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
   * Zooms to fit all the {@link MapillaryAbstractImage} objects stored in the
   * database.
   */
  public static void showAllPictures() {
    showPictures(MapillaryLayer.getInstance().getData().getImages(), false);
  }

  /**
   * Returns the current date.
   *
   * @return A {@code String} object containing the current date.
   */
  public static String currentDate() {
    Calendar cal = Calendar.getInstance();
    SimpleDateFormat formatter = new SimpleDateFormat("yyyy:MM:dd hh:mm:ss");
    return formatter.format(cal.getTime());
  }
}
