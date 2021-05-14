// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.time.Instant;
import java.util.List;
import java.util.TimeZone;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.testutils.JOSMTestRules;

class ImageImportUtilTest {
  @RegisterExtension
  JOSMTestRules rule = new JOSMTestRules().projection();

  @Test
  void testUntaggedImage() throws URISyntaxException, IOException {
    long startTime = System.currentTimeMillis() / 1000 * 1000; // Rounding to last full second
    final File untaggedFile = new File(ImageImportUtil.class.getResource("/exifTestImages/untagged.jpg").toURI());
    LatLon defaultLL = new LatLon(42, -73);
    List<INode> images = ImageImportUtil.readImagesFrom(untaggedFile, defaultLL);
    assertEquals(1, images.size());
    assertEquals(0, MapillaryImageUtils.getAngle(images.get(0)), 1e-9);
    assertEquals(defaultLL, images.get(0).getCoor());
    assertEquals(untaggedFile, MapillaryImageUtils.getFile(images.get(0)));
    long endTime = System.currentTimeMillis() / 1000 * 1000 + 1000; // Rounding to next full second
    Instant capturedAt = MapillaryImageUtils.getDate(images.get(0));
    assertTrue(
      capturedAt.toEpochMilli() / 1000 * 1000 >= startTime && capturedAt.toEpochMilli() / 1000 * 1000 <= endTime);
  }

  @Test
  void testLatLonOnlyImage() throws URISyntaxException, IOException {
    long startTime = System.currentTimeMillis() / 1000 * 1000; // Rounding to last full second
    final File untaggedFile = new File(ImageImportUtil.class.getResource("/exifTestImages/latLonOnly.jpg").toURI());
    LatLon defaultLL = new LatLon(42, -73);
    List<INode> images = ImageImportUtil.readImagesFrom(untaggedFile, defaultLL);
    assertEquals(1, images.size());
    assertEquals(0, MapillaryImageUtils.getAngle(images.get(0)), 1e-9);
    assertEquals(55.6052777777, images.get(0).getCoor().lat(), 1e-9);
    assertEquals(13.0001388888, images.get(0).getCoor().lon(), 1e-9);
    assertEquals(untaggedFile, MapillaryImageUtils.getFile(images.get(0)));
    long endTime = System.currentTimeMillis() / 1000 * 1000 + 1000; // Rounding to next full second
    long capturedAt = MapillaryImageUtils.getDate(images.get(0)).toEpochMilli();
    assertTrue(capturedAt >= startTime && capturedAt <= endTime);
  }

  @Test
  void testGpsDirectionOnlyImage() throws URISyntaxException, IOException {
    long startTime = System.currentTimeMillis() / 1000 * 1000; // Rounding to last full second
    final File untaggedFile = new File(
      ImageImportUtil.class.getResource("/exifTestImages/gpsDirectionOnly.jpg").toURI());
    LatLon defaultLL = new LatLon(42, -73);
    List<INode> images = ImageImportUtil.readImagesFrom(untaggedFile, defaultLL);
    assertEquals(1, images.size());
    assertEquals(42.73, MapillaryImageUtils.getAngle(images.get(0)), 1e-9);
    assertEquals(defaultLL, images.get(0).getCoor());
    assertEquals(untaggedFile, MapillaryImageUtils.getFile(images.get(0)));
    long endTime = System.currentTimeMillis() / 1000 * 1000 + 1000; // Rounding to next full second
    long capturedAt = MapillaryImageUtils.getDate(images.get(0)).toEpochMilli();
    assertTrue(capturedAt >= startTime && capturedAt <= endTime);
  }

  @Test
  void testDateTimeOnlyImage() throws URISyntaxException, IOException {
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    final File untaggedFile = new File(ImageImportUtil.class.getResource("/exifTestImages/dateTimeOnly.jpg").toURI());
    LatLon defaultLL = new LatLon(42, -73);
    List<INode> images = ImageImportUtil.readImagesFrom(untaggedFile, defaultLL);
    assertEquals(1, images.size());
    assertEquals(0, MapillaryImageUtils.getAngle(images.get(0)), 1e-9);
    assertEquals(defaultLL, images.get(0).getCoor());
    assertEquals(untaggedFile, MapillaryImageUtils.getFile(images.get(0)));
    /* http://www.wolframalpha.com/input/?i=convert+2015-12-24T01%3A02%3A03%2B0000+to+unixtime */
    long capturedAt = MapillaryImageUtils.getDate(images.get(0)).toEpochMilli();
    assertEquals(1_450_918_923_000L /* 2015-12-24 01:02:03+0000 */, capturedAt);

    TimeZone.setDefault(TimeZone.getTimeZone("GMT+3:00"));
    images = ImageImportUtil.readImagesFrom(untaggedFile, defaultLL);
    /* http://www.wolframalpha.com/input/?i=convert+2015-12-24T01%3A02%3A03%2B0300+to+unixtime */
    capturedAt = MapillaryImageUtils.getDate(images.get(0)).toEpochMilli();
    assertEquals(1_450_908_123_000L /* 2015-12-24 01:02:03+0300 */, capturedAt);
  }

  @Test
  void testImageDirectory() throws URISyntaxException, IOException {
    final File imageDirectory = new File(ImageImportUtil.class.getResource("/exifTestImages/").toURI());
    List<INode> images = ImageImportUtil.readImagesFrom(imageDirectory, new LatLon(42, -73));
    assertEquals(5, images.size());
  }

  @Test
  void testUtilityClass() {
    TestUtil.testUtilityClass(ImageImportUtil.class);
  }

  @Test
  void testFileFilterAgainstEmptyFile() throws URISyntaxException {
    File f = new File(ImageImportUtil.class.getResource("/zeroByteFile").toURI());
    assertFalse(ImageImportUtil.IMAGE_FILE_FILTER.accept(f));
  }

}
