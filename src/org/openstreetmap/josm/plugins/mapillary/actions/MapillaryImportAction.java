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
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Imports a set of picture files into JOSM. They must be in jpg or png format.
 *
 * @author nokutu
 *
 */
public class MapillaryImportAction extends JosmAction {

  public JFileChooser chooser;

  /**
   * Amount of pictures without the proper EXIF tags.
   */
  private int noTagsPics = 0;

  public MapillaryImportAction() {
    super(tr("Import pictures"), new ImageProvider("icon24.png"), tr("Import local pictures"), Shortcut
        .registerShortcut("Import Mapillary", tr("Import pictures into Mapillary layer"), KeyEvent.CHAR_UNDEFINED,
            Shortcut.NONE), false, "mapillaryImport", false);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    chooser = new JFileChooser();
    File startDirectory = new File(Main.pref.get("mapillary.start-directory", System.getProperty("user.home")));
    chooser.setCurrentDirectory(startDirectory);
    chooser.setDialogTitle(tr("Select pictures"));
    chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
    chooser.setAcceptAllFileFilterUsed(false);
    chooser.addChoosableFileFilter(new FileNameExtensionFilter("images", "jpg", "jpeg", "png"));
    chooser.setMultiSelectionEnabled(true);
    if (chooser.showOpenDialog(Main.parent) == JFileChooser.APPROVE_OPTION) {
      for (int i = 0; i < chooser.getSelectedFiles().length; i++) {
        File file = chooser.getSelectedFiles()[i];
        Main.pref.put("mapillary.start-directory", file.getParent());
        MapillaryLayer.getInstance();
        if (file.isDirectory()) {
          // TODO import directory
        } else {
          if (file.getPath().substring(file.getPath().length() - 4).equals(".jpg")
              || file.getPath().substring(file.getPath().length() - 5).equals(".jpeg")) {
            try {
              readJPG(file);
            } catch (ImageReadException ex) {
              Main.error(ex);
            } catch (IOException ex) {
              Main.error(ex);
            }
          } else if (file.getPath().substring(file.getPath().length() - 4).equals(".png")) {
            readPNG(file);
          }
        }
      }
    }
  }

  /**
   * Reads a jpg pictures that contains the needed GPS information (position and
   * direction) and creates a new icon in that position.
   *
   * @param file
   * @throws ImageReadException
   * @throws IOException
   */
  public void readJPG(File file) throws ImageReadException, IOException {
    final ImageMetadata metadata = Imaging.getMetadata(file);
    if (metadata instanceof JpegImageMetadata) {
      final JpegImageMetadata jpegMetadata = (JpegImageMetadata) metadata;
      final TiffField lat_ref = jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF);
      final TiffField lat = jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LATITUDE);
      final TiffField lon_ref = jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF);
      final TiffField lon = jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LONGITUDE);
      final TiffField ca = jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION);
      final TiffField datetimeOriginal = jpegMetadata
          .findEXIFValueWithExactMatch(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL);
      if (lat_ref == null || lat == null || lon == null || lon_ref == null) {
        readNoTags(file);
        return;
      }
      double latValue = 0;
      double lonValue = 0;
      double caValue = 0;
      if (lat.getValue() instanceof RationalNumber[])
        latValue = degMinSecToDouble((RationalNumber[]) lat.getValue(), lat_ref.getValue().toString());
      if (lon.getValue() instanceof RationalNumber[])
        lonValue = degMinSecToDouble((RationalNumber[]) lon.getValue(), lon_ref.getValue().toString());
      if (ca != null && ca.getValue() instanceof RationalNumber)
        caValue = ((RationalNumber) ca.getValue()).doubleValue();
      if (datetimeOriginal != null)
        MapillaryData.getInstance()
            .add(new MapillaryImportedImage(latValue, lonValue, caValue, file, datetimeOriginal.getStringValue()));
      else
        MapillaryData.getInstance().add(new MapillaryImportedImage(latValue, lonValue, caValue, file));
    }
  }

  /**
   * Reads a image file that doesn't contain the needed GPS information. And
   * creates a new icon in the middle of the map.
   *
   * @param file
   */
  private void readNoTags(File file) {
    double HORIZONTAL_DISTANCE = 0.0001;
    double horDev;
    if (noTagsPics % 2 == 0)
      horDev = HORIZONTAL_DISTANCE * noTagsPics / 2;
    else
      horDev = -HORIZONTAL_DISTANCE * ((noTagsPics + 1) / 2);
    LatLon pos = Main.map.mapView.getProjection().eastNorth2latlon(Main.map.mapView.getCenter());
    MapillaryData.getInstance().add(new MapillaryImportedImage(pos.lat(), pos.lon() + horDev, 0, file));
    noTagsPics++;
  }

  private void readPNG(File file) {
    readNoTags(file);
  }

  /**
   * Calculates the decimal degree-value from a degree value given in degrees-minutes-seconds-format
   *
   * @param degMinSec an array of length 3, the values in there are (in this order) degrees, minutes and seconds
   * @param ref the latitude or longitude reference determining if the given value is:
   *        <ul>
   *        <li>north ({@link GpsTagConstants#GPS_TAG_GPS_LATITUDE_REF_VALUE_NORTH}) or
   *        south ({@link GpsTagConstants#GPS_TAG_GPS_LATITUDE_REF_VALUE_SOUTH}) of the equator</li>
   *        <li>east ({@link GpsTagConstants#GPS_TAG_GPS_LONGITUDE_REF_VALUE_EAST}) or
   *        west ({@link GpsTagConstants#GPS_TAG_GPS_LONGITUDE_REF_VALUE_WEST}) of the equator</li>
   *        </ul>
   * @return the decimal degree-value for the given input, negative when west of 0-meridian or south of equator,
   *         positive otherwise
   * @throws IllegalArgumentException if {@code degMinSec} doesn't have length 3 or if {@code ref} is not one of the
   *         values mentioned above
   */ // TODO: Maybe move into a separate utility class?
  public static double degMinSecToDouble(RationalNumber[] degMinSec, String ref) {
    if (degMinSec == null || degMinSec.length != 3) { throw new IllegalArgumentException(); }
    switch (ref) {
    case GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_NORTH:
    case GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_SOUTH:
    case GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF_VALUE_EAST:
    case GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF_VALUE_WEST:
      break;
    default:
      throw new IllegalArgumentException();
    }

    double result = degMinSec[0].doubleValue(); // degrees
    result += degMinSec[1].doubleValue() / 60; // minutes
    result += degMinSec[2].doubleValue() / 3600; // seconds

    if (ref == GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF_VALUE_SOUTH
        || ref == GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF_VALUE_WEST) {
      result *= -1;
    }

    return result;
  }
}
