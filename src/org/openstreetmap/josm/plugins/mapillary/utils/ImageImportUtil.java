// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.filechooser.FileFilter;

import org.apache.commons.imaging.ImageReadException;
import org.apache.commons.imaging.Imaging;
import org.apache.commons.imaging.common.ImageMetadata;
import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.jpeg.JpegImageMetadata;
import org.apache.commons.imaging.formats.tiff.TiffField;
import org.apache.commons.imaging.formats.tiff.constants.ExifTagConstants;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;
import org.apache.commons.imaging.formats.tiff.taginfos.TagInfo;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.Logging;

public final class ImageImportUtil {
  public static final FileFilter IMAGE_FILE_FILTER = new ImageFileFilter();

  private ImageImportUtil() {
    // Private constructor to avoid instantiation
  }

  /**
   * Recursive method to retrieve all images inside a directory or in a single file.
   * @param f the file or directory from which all contained images should be read
   * @param defaultLL the coordinates that the images should get, if no coordinates are found in metadata
   * @return all images that were found within the given file or directory
   * @throws IOException if the content of the file could not be read
   */
  public static List<MapillaryImportedImage> readImagesFrom(final File f, final LatLon defaultLL) throws IOException {
    List<MapillaryImportedImage> images = new ArrayList<>();
    if (!f.exists() || !f.canRead()) {
      throw new IOException(f.getAbsolutePath() + " not found or not readable!");
    } else if (f.isDirectory()) {
      File[] files = f.listFiles();
      assert files != null;
      for (File child : files) {
        try {
          images.addAll(readImagesFrom(child, defaultLL));
        } catch (IOException e) {
          // Don't throw an exception here to allow other files that might be readable to be read.
          // Might not be the best solution, but the easiest workaround I could imagine.
          Logging.log(Logging.LEVEL_ERROR, f.getAbsolutePath() + " not found or not readable!", e);
        }
      }
    } else if (IMAGE_FILE_FILTER.accept(f)) {
      try (FileInputStream fis = new FileInputStream(f)) {
        images.add(readImageFrom(fis, f, defaultLL));
      }
    }
    return images;
  }

  /**
   * @param is the input stream to read the metadata from
   * @param f the file that will be set as a field to the returned {@link MapillaryImportedImage}
   * @param defaultLL the coordinates that the image should get, if no coordinates are found in metadata
   * @return the {@link MapillaryImportedImage} with the read metadata and the given file set
   * @throws IOException if an IOException occurs while reading from the input stream
   */
  private static MapillaryImportedImage readImageFrom(
      final InputStream is, final File f, final LatLon defaultLL
  ) throws IOException {
    Object latRef = null;
    Object lonRef = null;
    Object lat = null;
    Object lon = null;
    Object gpsDir = null;
    Object dateTime = null;
    final ImageMetadata meta;
    try {
      meta = Imaging.getMetadata(is, null);
      if (meta instanceof JpegImageMetadata) {
        final JpegImageMetadata jpegMeta = (JpegImageMetadata) meta;
        latRef = getTiffFieldValue(jpegMeta, GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF);
        lonRef = getTiffFieldValue(jpegMeta, GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF);
        lat = getTiffFieldValue(jpegMeta, GpsTagConstants.GPS_TAG_GPS_LATITUDE);
        lon = getTiffFieldValue(jpegMeta, GpsTagConstants.GPS_TAG_GPS_LONGITUDE);
        gpsDir = getTiffFieldValue(jpegMeta, GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION);
        dateTime = getTiffFieldValue(jpegMeta, ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL);
      }
    } catch (ImageReadException e) {
      // Can't read metadata from image, use defaults instead
    }


    final LatLon latLon;
    if (lat instanceof RationalNumber[] && latRef != null && lon instanceof RationalNumber[] && lonRef != null) {
      latLon = new LatLon(
          MapillaryUtils.degMinSecToDouble((RationalNumber[]) lat, latRef.toString()),
          MapillaryUtils.degMinSecToDouble((RationalNumber[]) lon, lonRef.toString())
      );
    } else {
      latLon = defaultLL;
    }
    final double ca;
    if (gpsDir instanceof RationalNumber) {
      ca = ((RationalNumber) gpsDir).doubleValue();
    } else {
      ca = 0;
    }
    if (dateTime == null) {
      return new MapillaryImportedImage(latLon, ca, f);
    }
    return new MapillaryImportedImage(latLon, ca, f, dateTime.toString());
  }

  private static Object getTiffFieldValue(JpegImageMetadata meta, TagInfo tag) {
    TiffField field = meta.findEXIFValueWithExactMatch(tag);
    if (field != null) {
      try {
        return field.getValue();
      } catch (ImageReadException e) {
        // If value couldn't be read, assume it's not set.
      }
    }
    return null;
  }

  private static class ImageFileFilter extends FileFilter {
    private static final byte[] JFIF_MAGIC = new byte[]{-1 /*0xFF*/, -40 /*0xD8*/};
    private static final byte[] PNG_MAGIC = new byte[]{
        -119 /*0x89*/, 80 /*0x50*/, 78 /*0x4E*/, 71 /*0x47*/, 13 /*0x0D*/, 10 /*0x0A*/, 26 /*0x1A*/, 10 /*0x0A*/
    };
    private final byte[] magic = new byte[Math.max(JFIF_MAGIC.length, PNG_MAGIC.length)];

    @Override
    public synchronized boolean accept(File f) {
      if (!f.canRead() || !f.exists()) {
        return false;
      }
      if (f.isDirectory()) {
        return true;
      }
      try (FileInputStream fis = new FileInputStream(f)) {
        int numBytes = fis.read(magic);
        return numBytes >= 0 && (
          Arrays.equals(JFIF_MAGIC, Arrays.copyOf(magic, Math.min(numBytes, JFIF_MAGIC.length)))
          || Arrays.equals(PNG_MAGIC, Arrays.copyOf(magic, Math.min(numBytes, PNG_MAGIC.length)))
        );
      } catch (FileNotFoundException e) {
        return false;
      } catch (IOException e) {
        Logging.log(Logging.LEVEL_WARN, "IO-exception while reading file "+f.getAbsolutePath(), e);
        return false;
      }
    }

    @Override
    public String getDescription() {
      return I18n.tr("Supported image formats (JPG and PNG)");
    }

  }
}
