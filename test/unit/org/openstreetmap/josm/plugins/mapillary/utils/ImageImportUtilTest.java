// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;
import java.util.TimeZone;

import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;

public class ImageImportUtilTest {

  @Test
  public void testUntaggedImage() throws URISyntaxException, IOException {
    long startTime = System.currentTimeMillis() / 1000 * 1000; // Rounding to last full second
    final File untaggedFile = new File(ImageImportUtil.class.getResource("/exifTestImages/untagged.jpg").toURI());
    LatLon defaultLL = new LatLon(42, -73);
    List<MapillaryImportedImage> images = ImageImportUtil.readImagesFrom(untaggedFile, defaultLL);
    assertEquals(1, images.size());
    assertEquals(0, images.get(0).getMovingCa(), 1e-9);
    assertEquals(defaultLL, images.get(0).getMovingLatLon());
    assertEquals(untaggedFile, images.get(0).getFile());
    long endTime = System.currentTimeMillis() / 1000 * 1000 + 1000; // Rounding to next full second
    assertTrue(images.get(0).getCapturedAt() >= startTime && images.get(0).getCapturedAt() <= endTime);
  }

  @Test
  public void testLatLonOnlyImage() throws URISyntaxException, IOException {
    long startTime = System.currentTimeMillis() / 1000 * 1000; // Rounding to last full second
    final File untaggedFile = new File(ImageImportUtil.class.getResource("/exifTestImages/latLonOnly.jpg").toURI());
    LatLon defaultLL = new LatLon(42, -73);
    List<MapillaryImportedImage> images = ImageImportUtil.readImagesFrom(untaggedFile, defaultLL);
    assertEquals(1, images.size());
    assertEquals(0, images.get(0).getMovingCa(), 1e-9);
    assertEquals(55.6052777777, images.get(0).getMovingLatLon().lat(), 1e-9);
    assertEquals(13.0001388888, images.get(0).getMovingLatLon().lon(), 1e-9);
    assertEquals(untaggedFile, images.get(0).getFile());
    long endTime = System.currentTimeMillis() / 1000 * 1000 + 1000; // Rounding to next full second
    assertTrue(images.get(0).getCapturedAt() >= startTime && images.get(0).getCapturedAt() <= endTime);
  }

  @Test
  public void testGpsDirectionOnlyImage() throws URISyntaxException, IOException {
    long startTime = System.currentTimeMillis() / 1000 * 1000; // Rounding to last full second
    final File untaggedFile = new File(ImageImportUtil.class.getResource("/exifTestImages/gpsDirectionOnly.jpg").toURI());
    LatLon defaultLL = new LatLon(42, -73);
    List<MapillaryImportedImage> images = ImageImportUtil.readImagesFrom(untaggedFile, defaultLL);
    assertEquals(1, images.size());
    assertEquals(42.73, images.get(0).getMovingCa(), 1e-9);
    assertEquals(defaultLL, images.get(0).getMovingLatLon());
    assertEquals(untaggedFile, images.get(0).getFile());
    long endTime = System.currentTimeMillis() / 1000 * 1000 + 1000; // Rounding to next full second
    assertTrue(images.get(0).getCapturedAt() >= startTime && images.get(0).getCapturedAt() <= endTime);
  }

  @Test
  public void testDateTimeOnlyImage() throws URISyntaxException, IOException {
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    final File untaggedFile = new File(ImageImportUtil.class.getResource("/exifTestImages/dateTimeOnly.jpg").toURI());
    LatLon defaultLL = new LatLon(42, -73);
    List<MapillaryImportedImage> images = ImageImportUtil.readImagesFrom(untaggedFile, defaultLL);
    assertEquals(1, images.size());
    assertEquals(0, images.get(0).getMovingCa(), 1e-9);
    assertEquals(defaultLL, images.get(0).getMovingLatLon());
    assertEquals(untaggedFile, images.get(0).getFile());
    /* http://www.wolframalpha.com/input/?i=convert+2015-12-24T01%3A02%3A03%2B0000+to+unixtime */
    assertEquals(1_450_918_923_000L /*2015-12-24 01:02:03+0000*/, images.get(0).getCapturedAt());

    TimeZone.setDefault(TimeZone.getTimeZone("GMT+3:00"));
    images = ImageImportUtil.readImagesFrom(untaggedFile, defaultLL);
    /* http://www.wolframalpha.com/input/?i=convert+2015-12-24T01%3A02%3A03%2B0300+to+unixtime */
    assertEquals(1_450_908_123_000L /*2015-12-24 01:02:03+0300*/, images.get(0).getCapturedAt());
  }

  @Test
  public void testImageDirectory() throws URISyntaxException, IOException {
    final File imageDirectory = new File(ImageImportUtil.class.getResource("/exifTestImages/").toURI());
    List<MapillaryImportedImage> images = ImageImportUtil.readImagesFrom(imageDirectory, new LatLon(42,-73));
    assertEquals(5, images.size());
  }

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(ImageImportUtil.class);
  }

  @Test
  public void testFileFilterAgainstEmptyFile() throws URISyntaxException {
    File f = new File(ImageImportUtil.class.getResource("/zeroByteFile").toURI());
    assertFalse(ImageImportUtil.IMAGE_FILE_FILTER.accept(f));
  }

}
