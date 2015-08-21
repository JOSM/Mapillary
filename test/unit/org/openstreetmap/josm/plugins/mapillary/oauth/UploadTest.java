package org.openstreetmap.josm.plugins.mapillary.oauth;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;

import org.apache.commons.imaging.ImageReadException;
import org.apache.commons.imaging.ImageWriteException;
import org.apache.commons.imaging.Imaging;
import org.apache.commons.imaging.common.ImageMetadata;
import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.jpeg.JpegImageMetadata;
import org.apache.commons.imaging.formats.tiff.constants.ExifTagConstants;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;
import org.junit.Test;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.AbstractTest;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.oauth.UploadUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;

/**
 * Tests the {@link UploadUtils} class.
 *
 * @author nokutu
 * @see UploadUtils
 */
public class UploadTest extends AbstractTest {

  /**
   * Tests the {@link UploadUtils#updateFile(MapillaryImportedImage)} method.
   */
  @Test
  public void updateFileTest() {
    File image = new File("images/icon16.png");
    MapillaryImportedImage img = MapillaryUtils.readNoTags(image,
        new LatLon(0, 0));
    File updatedFile = null;
    try {
      updatedFile = UploadUtils.updateFile(img);
      ImageMetadata metadata = Imaging.getMetadata(updatedFile);
      final JpegImageMetadata jpegMetadata = (JpegImageMetadata) metadata;
      assertTrue(jpegMetadata
          .findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF) != null);
      assertTrue(jpegMetadata
          .findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LATITUDE) != null);
      assertTrue(jpegMetadata
          .findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF) != null);
      assertTrue(jpegMetadata
          .findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LONGITUDE) != null);
      assertTrue(jpegMetadata
          .findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION) != null);
      assertTrue(jpegMetadata
          .findEXIFValueWithExactMatch(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL) != null);
      assertEquals(0, MapillaryUtils.degMinSecToDouble(
          (RationalNumber[]) jpegMetadata.findEXIFValueWithExactMatch(
              GpsTagConstants.GPS_TAG_GPS_LATITUDE).getValue(),
          jpegMetadata
              .findEXIFValueWithExactMatch(
                  GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF).getValue()
              .toString()), 0.01);
      assertEquals(0, MapillaryUtils.degMinSecToDouble(
          (RationalNumber[]) jpegMetadata.findEXIFValueWithExactMatch(
              GpsTagConstants.GPS_TAG_GPS_LONGITUDE).getValue(),
          jpegMetadata
              .findEXIFValueWithExactMatch(
                  GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF).getValue()
              .toString()), 0.01);

    } catch (ImageReadException | ImageWriteException | IOException e) {
      fail();
    } finally {
      updatedFile.delete();
    }
  }
}
