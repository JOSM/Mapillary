package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Desktop;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;

import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;

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
   * @param img1
   * @param img2
   */
  public synchronized static void join(MapillaryImportedImage img1,
      MapillaryImportedImage img2) {
    MapillaryImportedImage firstImage = img1;
    MapillaryImportedImage secondImage = img2;

    if (img1.next() != null) {
      firstImage = img2;
      secondImage = img1;
    }
    if (firstImage.getSequence() == null) {
      MapillarySequence seq = new MapillarySequence();
      seq.add(firstImage);
      firstImage.setSequence(seq);
    }
    if (secondImage.getSequence() == null) {
      MapillarySequence seq = new MapillarySequence();
      seq.add(secondImage);
      img2.setSequence(seq);
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
   * @param img1
   * @param img2
   */
  public synchronized static void unjoin(MapillaryImportedImage img1,
      MapillaryImportedImage img2) {
    MapillaryImportedImage firstImage = img1;
    MapillaryImportedImage secondImage = img2;

    if (img1.next() != img2) {
      firstImage = img2;
      secondImage = img1;
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
}
