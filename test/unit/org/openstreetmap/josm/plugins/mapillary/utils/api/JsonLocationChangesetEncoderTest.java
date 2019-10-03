// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.openstreetmap.josm.plugins.mapillary.utils.JsonUtil.assertJsonEquals;

import org.junit.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLocationChangeset;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

public class JsonLocationChangesetEncoderTest {

  @Test
  public void testSingleImageChangeset() {
    MapillaryImage image = new MapillaryImage("wMAqAFr3xE9072G8Al6WLQ", new LatLon(50.44612, 13.3323), 10.0, false);
    image.move(0.1111, 0.22222);
    image.turn(273.3);
    image.stopMoving();

    assertJsonEquals(JsonLocationChangesetEncoderTest.class, "/api/v3/requests/single_image_changeset.json", JsonLocationChangesetEncoder.encodeImageChanges(image));
  }

  @Test
  public void testTranslationOnlyChangeset() {
    MapillaryImage image = new MapillaryImage("translationOnlyChangesetImageKey", new LatLon(50.44612, 13.3323), 10.0, false);
    image.move(0.1111, 0.22222);
    image.stopMoving();

    assertJsonEquals(JsonLocationChangesetEncoderTest.class, "/api/v3/requests/translation_only_changeset.json", JsonLocationChangesetEncoder.encodeImageChanges(image));
  }

  @Test
  public void testRotationOnlyChangeset() {
    MapillaryImage image = new MapillaryImage("rotationOnlyChangesetImageKey", new LatLon(50.44612, 13.3323), 10.0, false);
    image.turn(-80.3);
    image.stopMoving();

    assertJsonEquals(JsonLocationChangesetEncoderTest.class, "/api/v3/requests/rotation_only_changeset.json", JsonLocationChangesetEncoder.encodeImageChanges(image));
  }

  @Test
  public void testMultipleImagesChangeset() throws IllegalArgumentException {
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

    assertJsonEquals(JsonLocationChangesetEncoderTest.class, "/api/v3/requests/changeset.json", JsonLocationChangesetEncoder.encodeLocationChangeset(changeset));
  }

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(JsonLocationChangesetEncoder.class);
  }
}
