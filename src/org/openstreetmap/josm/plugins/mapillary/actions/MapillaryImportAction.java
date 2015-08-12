package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

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
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandImport;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
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
    this.chooser = new JFileChooser();
    File startDirectory = new File(Main.pref.get("mapillary.start-directory",
        System.getProperty("user.home")));
    this.chooser.setCurrentDirectory(startDirectory);
    this.chooser.setDialogTitle(tr("Select pictures"));
    this.chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
    this.chooser.setAcceptAllFileFilterUsed(false);
    this.chooser.addChoosableFileFilter(new FileNameExtensionFilter("images",
        "jpg", "jpeg", "png"));
    this.chooser.setMultiSelectionEnabled(true);
    if (this.chooser.showOpenDialog(Main.parent) == JFileChooser.APPROVE_OPTION) {
      List<MapillaryAbstractImage> images = new ArrayList<>();
      for (int i = 0; i < this.chooser.getSelectedFiles().length; i++) {
        File file = this.chooser.getSelectedFiles()[i];
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
                images.add(readJPG(file.listFiles()[j]));

              else if (extension.equals("png"))
                images.add(readPNG(file.listFiles()[j]));
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
              images.add(readJPG(file));
            } catch (ImageReadException ex) {
              Main.error(ex);
            } catch (IOException ex) {
              Main.error(ex);
            }
          } else if (file.getPath().substring(file.getPath().length() - 4)
              .equals(".png")) {
            images.add(readPNG(file));
          }
        }
      }
      MapillaryRecord.getInstance().addCommand(new CommandImport(images));
      MapillaryUtils.showAllPictures();
    }
  }

  /**
   * Reads a JPG pictures that contains the needed GPS information (position and
   * direction) and creates a new icon in that position.
   *
   * @param file
   *          The file where the picture is located.
   * @return The imported image.
   * @throws ImageReadException
   *           If the file isn't an image.
   * @throws IOException
   *           If the file doesn't have the valid metadata.
   */
  public MapillaryImportedImage readJPG(File file) throws IOException,
      ImageReadException {
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
        latValue = MapillaryUtils.degMinSecToDouble(
            (RationalNumber[]) lat.getValue(), lat_ref.getValue().toString());
      if (lon.getValue() instanceof RationalNumber[])
        lonValue = MapillaryUtils.degMinSecToDouble(
            (RationalNumber[]) lon.getValue(), lon_ref.getValue().toString());
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
   *          The file where the image is located.
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
   *          The file where the image is located.
   * @param pos
   *          A {@link LatLon} object indicating the position in the map where
   *          the image must be set.
   * @return The imported image.
   */
  public MapillaryImportedImage readNoTags(File file, LatLon pos) {
    double HORIZONTAL_DISTANCE = 0.0001;
    double horDev;
    if (this.noTagsPics % 2 == 0)
      horDev = HORIZONTAL_DISTANCE * this.noTagsPics / 2;
    else
      horDev = -HORIZONTAL_DISTANCE * ((this.noTagsPics + 1) / 2);
    this.noTagsPics++;
    return new MapillaryImportedImage(pos.lat(), pos.lon() + horDev, 0, file);
  }

  /**
   * Reads an image in PNG format.
   *
   * @param file
   *          The file where the image is located.
   * @return The imported image.
   */
  public MapillaryImportedImage readPNG(File file) {
    return readNoTags(file);
  }
}
