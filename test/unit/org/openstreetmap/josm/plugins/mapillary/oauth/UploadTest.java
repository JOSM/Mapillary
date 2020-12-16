// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageImportUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.testutils.JOSMTestRules;

/**
 * Tests the {@link UploadUtils} class.
 *
 * @author nokutu
 * @see UploadUtils
 */
class UploadTest {
  @RegisterExtension
  public JOSMTestRules rule = new JOSMTestRules().projection();

  /**
   * Tests the {@link UploadUtils#updateFile(MapillaryImportedImage)} method.
   */
  @Test
  void updateFileTest() throws IOException {
    File image = new File(getClass().getResource("/exifTestImages/untagged.png").getFile());
    MapillaryImportedImage img = ImageImportUtil.readImagesFrom(image, new LatLon(0, 0)).get(0);
    File updatedFile = null;
    try {
      updatedFile = UploadUtils.updateFile(img);
      ImageMetadata metadata = Imaging.getMetadata(updatedFile);
      final JpegImageMetadata jpegMetadata = (JpegImageMetadata) metadata;
      assertNotNull(jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF));
      assertNotNull(jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LATITUDE));
      assertNotNull(jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF));
      assertNotNull(jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LONGITUDE));
      assertNotNull(jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION));
      assertNotNull(jpegMetadata.findEXIFValueWithExactMatch(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL));
      assertEquals(0,
        MapillaryUtils.degMinSecToDouble(
          (RationalNumber[]) jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LATITUDE).getValue(),
          jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF).getValue().toString()),
        0.01);
      assertEquals(0,
        MapillaryUtils.degMinSecToDouble(
          (RationalNumber[]) jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LONGITUDE).getValue(),
          jpegMetadata.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF).getValue().toString()),
        0.01);

    } catch (ImageReadException | ImageWriteException | IOException e) {
      fail(e.getLocalizedMessage());
    } finally {
      updatedFile.delete();
    }
  }
}
