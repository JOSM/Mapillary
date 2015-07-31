package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.apache.commons.imaging.ImageReadException;
import org.apache.commons.imaging.Imaging;
import org.apache.commons.imaging.common.ImageMetadata;
import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.jpeg.JpegImageMetadata;
import org.apache.commons.imaging.formats.tiff.TiffField;
import org.apache.commons.imaging.formats.tiff.constants.ExifTagConstants;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Imports a set of picture files into JOSM. They must be in jpg or png format.
 *
 * @author nokutu
 *
 */
public class MapillaryImportAction extends JosmAction {

  private static final long serialVersionUID = 4995924098228081806L;

  private JFileChooser chooser;

  /**
   * Amount of pictures without the proper EXIF tags.
   */
  private int noTagsPics = 0;

  /**
   * Main constructor.
   */
  public MapillaryImportAction() {
    super(tr("Import pictures"), MapillaryPlugin.getProvider("icon24.png"),
        tr("Import local pictures"), Shortcut.registerShortcut(
            "Import Mapillary", tr("Import pictures into Mapillary layer"),
            KeyEvent.CHAR_UNDEFINED, Shortcut.NONE), false, "mapillaryImport",
        false);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    chooser = new JFileChooser();
    File startDirectory = new File(Main.pref.get("mapillary.start-directory",
        System.getProperty("user.home")));
    chooser.setCurrentDirectory(startDirectory);
    chooser.setDialogTitle(tr("Select pictures"));
    chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
    chooser.setAcceptAllFileFilterUsed(false);
    chooser.addChoosableFileFilter(new FileNameExtensionFilter("images", "jpg",
        "jpeg", "png"));
    chooser.setMultiSelectionEnabled(true);
    if (chooser.showOpenDialog(Main.parent) == JFileChooser.APPROVE_OPTION) {
      for (int i = 0; i < chooser.getSelectedFiles().length; i++) {
        File file = chooser.getSelectedFiles()[i];
        Main.pref.put("mapillary.start-directory", file.getParent());
        MapillaryLayer.getInstance();
        if (file.isDirectory()) {
          if (file.listFiles() == null)
            continue;
          for (int j = 0; j < file.listFiles().length; j++) {
            int k = file.listFiles()[j].getName().lastIndexOf('.');
            String extension = null;
            if (k > 0) {
              extension = file.listFiles()[j].getName().substring(k + 1);
            }
            try {
              if (extension.equals("jpg") || extension.equals("jpeg"))
                MapillaryData.getInstance().add(readJPG(file.listFiles()[j]));

              else if (extension.equals("png"))
                MapillaryData.getInstance().add(readPNG(file.listFiles()[j]));
            } catch (ImageReadException | IOException | NullPointerException e1) {
              Main.error(e1);
            }
          }
        } else {
          if (file.getPath().substring(file.getPath().length() - 4)
              .equals(".jpg")
              || file.getPath().substring(file.getPath().length() - 5)
                  .equals(".jpeg")) {
            try {
              MapillaryData.getInstance().add(readJPG(file));
            } catch (ImageReadException ex) {
              Main.error(ex);
            } catch (IOException ex) {
              Main.error(ex);
            }
          } else if (file.getPath().substring(file.getPath().length() - 4)
              .equals(".png")) {
            MapillaryData.getInstance().add(readPNG(file));
          }
        }
      }
      MapillaryLayer.getInstance().showAllPictures();

    }
  }

  /**
   * Reads a JPG pictures that contains the needed GPS information (position and
   * direction) and creates a new icon in that position.
   *
   * @param file
   * @return The imported image.
   * @throws ImageReadException
   * @throws IOException
   */
  public MapillaryImportedImage readJPG(File file) throws ImageReadException,
      IOException {
    final ImageMetadata metadata = Imaging.getMetadata(file);
    if (metadata instanceof JpegImageMetadata) {
      final JpegImageMetadata jpegMetadata = (JpegImageMetadata) metadata;
      final TiffField lat_ref = jpegMetadata
          .findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF);
      final TiffField lat = jpegMetadata
          .findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LATITUDE);
      final TiffField lon_ref = jpegMetadata
          .findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF);
      final TiffField lon = jpegMetadata
          .findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LONGITUDE);
      final TiffField ca = jpegMetadata
          .findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION);
      final TiffField datetimeOriginal = jpegMetadata
          .findEXIFValueWithExactMatch(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL);
      if (lat_ref == null || lat == null || lon == null || lon_ref == null) {
        return readNoTags(file);
      }
      double latValue = 0;
      double lonValue = 0;
      double caValue = 0;
      if (lat.getValue() instanceof RationalNumber[])
        latValue = degMinSecToDouble((RationalNumber[]) lat.getValue(), lat_ref
            .getValue().toString());
      if (lon.getValue() instanceof RationalNumber[])
        lonValue = degMinSecToDouble((RationalNumber[]) lon.getValue(), lon_ref
            .getValue().toString());
      if (ca != null && ca.getValue() instanceof RationalNumber)
        caValue = ((RationalNumber) ca.getValue()).doubleValue();
      if (datetimeOriginal != null)
        return new MapillaryImportedImage(latValue, lonValue, caValue, file,
            datetimeOriginal.getStringValue());
      else
        return new MapillaryImportedImage(latValue, lonValue, caValue, file);
    }
    throw new IllegalStateException("Invalid format.");
  }

  /**
   * Reads a image file that doesn't contain the needed GPS information. And
   * creates a new icon in the middle of the map.
   *
   * @param file
   * @return The imported image.
   */
  public MapillaryImportedImage readNoTags(File file) {
    return readNoTags(
        file,
        Main.map.mapView.getProjection().eastNorth2latlon(
            Main.map.mapView.getCenter()));
  }

  /**
   * Reads a image file that doesn't contain the needed GPS information. And
   * creates a new icon in the middle of the map.
   *
   * @param file
   * @param pos
   *          A {@link LatLon} object indicating the position in the map where
   *          the image must be set.
   * @return The imported image.
   */
  public MapillaryImportedImage readNoTags(File file, LatLon pos) {
    double HORIZONTAL_DISTANCE = 0.0001;
    double horDev;
    if (noTagsPics % 2 == 0)
      horDev = HORIZONTAL_DISTANCE * noTagsPics / 2;
    else
      horDev = -HORIZONTAL_DISTANCE * ((noTagsPics + 1) / 2);
    noTagsPics++;
    return new MapillaryImportedImage(pos.lat(), pos.lon() + horDev, 0, file);
  }

  /**
   * Reads an image in PNG format.
   *
   * @param file
   * @return The imported image.
   */
  public MapillaryImportedImage readPNG(File file) {
    return readNoTags(file);
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
