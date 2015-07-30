package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;

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
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Imports a set of images and puts them in a single {@link MapillarySequence}.
 *
 * @author nokutu
 *
 */
public class MapillaryImportIntoSequenceAction extends JosmAction {

  private static final long serialVersionUID = -9190217809965894878L;

  private JFileChooser chooser;

  private LinkedList<MapillaryImportedImage> images;

  /**
   * Main constructor.
   */
  public MapillaryImportIntoSequenceAction() {
    super(tr("Import pictures into sequence"), new ImageProvider(MapillaryPlugin.directory + "images/icon24.png"),
        tr("Import local pictures"), Shortcut.registerShortcut(
            "Import Mapillary Sequence",
            tr("Import pictures into Mapillary layer in a sequence"),
            KeyEvent.CHAR_UNDEFINED, Shortcut.NONE), false,
        "mapillaryImportSequence", false);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    images = new LinkedList<>();

    chooser = new JFileChooser();
    File startDirectory = new File(Main.pref.get("mapillary.start-directory",
        System.getProperty("user.home")));
    chooser.setCurrentDirectory(startDirectory);
    chooser.setDialogTitle(tr("Select pictures"));
    chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
    chooser.setAcceptAllFileFilterUsed(false);
    chooser.addChoosableFileFilter(new FileNameExtensionFilter("images", "jpg",
        "jpeg"));
    chooser.setMultiSelectionEnabled(true);

    if (chooser.showOpenDialog(Main.parent) == JFileChooser.APPROVE_OPTION) {
      for (int i = 0; i < chooser.getSelectedFiles().length; i++) {
        File file = chooser.getSelectedFiles()[i];
        if (file == null)
          break;
        Main.pref.put("mapillary.start-directory", file.getParent());
        MapillaryLayer.getInstance();
        if (file.isDirectory()) {
          for (int j = 0; j < file.listFiles().length; j++) {
            int k = file.listFiles()[j].getName().lastIndexOf('.');
            String extension = null;
            if (k > 0) {
              extension = file.listFiles()[j].getName().substring(k + 1);
            }
            try {
              if (extension.equals("jpg") || extension.equals("jpeg"))
                readJPG(file.listFiles()[j]);
            } catch (ImageReadException | NullPointerException | IOException e) {
              Main.error(e);
            }
          }
        } else {
          if (file.getPath().substring(file.getPath().length() - 4)
              .equals(".jpg")
              || file.getPath().substring(file.getPath().length() - 5)
                  .equals(".jpeg")) {
            try {
              readJPG(file);
            } catch (ImageReadException ex) {
              Main.error(ex);
            } catch (IOException ex) {
              Main.error(ex);
            }
          }
        }
      }
      joinImages();
    }

    MapillaryLayer.getInstance().showAllPictures();
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
      if (lat_ref == null || lat == null || lon == null || lon_ref == null
          || datetimeOriginal == null)
        throw new IllegalArgumentException(
            "The picture has not correct EXIF tags");

      double latValue = 0;
      double lonValue = 0;
      double caValue = 0;
      if (lat.getValue() instanceof RationalNumber[])
        latValue = MapillaryImportAction.degMinSecToDouble(
            (RationalNumber[]) lat.getValue(), lat_ref.getValue().toString());
      if (lon.getValue() instanceof RationalNumber[])
        lonValue = MapillaryImportAction.degMinSecToDouble(
            (RationalNumber[]) lon.getValue(), lon_ref.getValue().toString());
      if (ca != null && ca.getValue() instanceof RationalNumber)
        caValue = ((RationalNumber) ca.getValue()).doubleValue();

      MapillaryImportedImage image = new MapillaryImportedImage(latValue,
          lonValue, caValue, file, datetimeOriginal.getStringValue());
      MapillaryData.getInstance().add(image);
      image.getCapturedAt();

      images.add(image);
    }
  }

  /**
   * Joins all the images in a unique {@link MapillarySequence}.
   */
  public void joinImages() {
    Collections.sort(images, new MapillaryEpochComparator());
    MapillarySequence seq = new MapillarySequence();
    for (MapillaryImportedImage img : images) {
      seq.add(img);
      img.setSequence(seq);
    }
  }

  /**
   * Comparator that comperes two {@link MapillaryAbstractImage} objects
   * depending on the time they were taken.
   *
   * @author nokutu
   *
   */
  public class MapillaryEpochComparator implements
      Comparator<MapillaryAbstractImage> {

    @Override
    public int compare(MapillaryAbstractImage arg0, MapillaryAbstractImage arg1) {
      if (arg0.getCapturedAt() < arg1.getCapturedAt())
        return -1;
      if (arg0.getCapturedAt() > arg1.getCapturedAt())
        return 1;
      return 0;
    }
  }
}
