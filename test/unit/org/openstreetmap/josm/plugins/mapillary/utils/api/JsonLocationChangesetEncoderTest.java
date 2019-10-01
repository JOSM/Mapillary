// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

import org.json.JSONException;
import org.junit.Test;
import org.skyscreamer.jsonassert.JSONAssert;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLocationChangeset;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

public class JsonLocationChangesetEncoderTest {

  @Test
  public void testSingleImageChangeset() throws JSONException, URISyntaxException {
    MapillaryImage image = new MapillaryImage("wMAqAFr3xE9072G8Al6WLQ", new LatLon(50.44612, 13.3323), 10.0, false);
    image.move(0.1111, 0.22222);
    image.turn(273.3);
    image.stopMoving();

    String expected = readLines("/api/v3/requests/single_image_changeset.json");
    String actual = JsonLocationChangesetEncoder.encodeImageChanges(image).build().toString();
    JSONAssert.assertEquals(expected, actual, true);
  }

  @Test
  public void testTranslationOnlyChangeset() throws JSONException, URISyntaxException {
    MapillaryImage image = new MapillaryImage("translationOnlyChangesetImageKey", new LatLon(50.44612, 13.3323), 10.0, false);
    image.move(0.1111, 0.22222);
    image.stopMoving();

    String expected = readLines("/api/v3/requests/translation_only_changeset.json");
    String actual = JsonLocationChangesetEncoder.encodeImageChanges(image).build().toString();
    JSONAssert.assertEquals(expected, actual, true);
  }

  @Test
  public void testRotationOnlyChangeset() throws JSONException, URISyntaxException {
    MapillaryImage image = new MapillaryImage("rotationOnlyChangesetImageKey", new LatLon(50.44612, 13.3323), 10.0, false);
    image.turn(-80.3);
    image.stopMoving();

    String expected = readLines("/api/v3/requests/rotation_only_changeset.json");
    String actual = JsonLocationChangesetEncoder.encodeImageChanges(image).build().toString();
    JSONAssert.assertEquals(expected, actual, true);
  }

  private static String readLines(String filePath) throws URISyntaxException {
    StringBuilder contentBuilder = new StringBuilder();
    try (Stream<String> stream = Files.lines( Paths.get(JsonLocationChangesetEncoderTest.class.getResource(filePath).toURI()), StandardCharsets.UTF_8))
    {
      stream.forEach(s -> contentBuilder.append(s).append("\n"));
    }
    catch (IOException e)
    {
      e.printStackTrace();
    }
    return contentBuilder.toString();
  }

  @Test
  public void testMultipleImagesChangeset() throws IllegalArgumentException, URISyntaxException, JSONException {
    MapillaryImage image1 = new MapillaryImage("wMAqAFr3xE9072G8Al6WLQ", new LatLon(50.44612, 13.3323), 10.0, false);
    image1.move(0.1111, 0.22222);
    image1.turn(273.3);
    image1.stopMoving();
    MapillaryImage image2 = new MapillaryImage("7erPn382xDMtmfdh0xtvUw", new LatLon(0, 0), 0, false);
    image2.move(13.3328, 50.44619);
    image2.stopMoving();
    MapillaryImage image3 = new MapillaryImage("invalid image key will be ignored", new LatLon(0, 0), 0, false);
    image3.turn(13.4);
    image3.stopMoving();

    MapillaryLocationChangeset changeset = new MapillaryLocationChangeset();
    changeset.add(image1);
    changeset.add(image2);
    changeset.add(image3);

    String expected = readLines("/api/v3/requests/changeset.json");
    String actual = JsonLocationChangesetEncoder.encodeLocationChangeset(changeset).build().toString();
    JSONAssert.assertEquals(expected, actual, false);
  }

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(JsonLocationChangesetEncoder.class);
  }
}
