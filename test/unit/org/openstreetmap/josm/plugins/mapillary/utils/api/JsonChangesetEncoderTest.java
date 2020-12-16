// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.openstreetmap.josm.plugins.mapillary.utils.JsonUtil.assertJsonEquals;

import org.junit.jupiter.api.Test;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.gui.changeset.MapillaryChangeset;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

class JsonChangesetEncoderTest {

  @Test
  void testSingleImageChangeset() {
    MapillaryImage image = new MapillaryImage("wMAqAFr3xE9072G8Al6WLQ", new LatLon(50.44612, 13.3323), 10.0, false,
      false);
    image.move(0.1111, 0.22222);
    image.turn(273.3);
    image.stopMoving();

    assertJsonEquals(JsonChangesetEncoderTest.class, "/api/v3/requests/single_image_changeset.json",
      JsonChangesetEncoder.encodeImageLocationChanges(image));
  }

  @Test
  void testTranslationOnlyChangeset() {
    MapillaryImage image = new MapillaryImage("translationOnlyChangesetImageKey", new LatLon(50.44612, 13.3323), 10.0,
      false, false);
    image.move(0.1111, 0.22222);
    image.stopMoving();

    assertJsonEquals(JsonChangesetEncoderTest.class, "/api/v3/requests/translation_only_changeset.json",
      JsonChangesetEncoder.encodeImageLocationChanges(image));
  }

  @Test
  void testRotationOnlyChangeset() {
    MapillaryImage image = new MapillaryImage("rotationOnlyChangesetImageKey", new LatLon(50.44612, 13.3323), 10.0,
      false, false);
    image.turn(-80.3);
    image.stopMoving();

    assertJsonEquals(JsonChangesetEncoderTest.class, "/api/v3/requests/rotation_only_changeset.json",
      JsonChangesetEncoder.encodeImageLocationChanges(image));
  }

  @Test
  void testMultipleImagesChangeset() throws IllegalArgumentException {
    MapillaryImage image1 = new MapillaryImage("wMAqAFr3xE9072G8Al6WLQ", new LatLon(50.44612, 13.3323), 10.0, false,
      false);
    image1.move(0.1111, 0.22222);
    image1.turn(273.3);
    image1.stopMoving();
    MapillaryImage image2 = new MapillaryImage("7erPn382xDMtmfdh0xtvUw", new LatLon(0, 0), 0, false, false);
    image2.move(13.3328, 50.44619);
    image2.stopMoving();
    MapillaryImage image3 = new MapillaryImage("invalid image key will be ignored", new LatLon(0, 0), 0, false, false);
    image3.turn(13.4);
    image3.stopMoving();

    MapillaryChangeset changeset = new MapillaryChangeset();
    changeset.add(image1);
    changeset.add(image2);
    changeset.add(image3);

    assertJsonEquals(JsonChangesetEncoderTest.class, "/api/v3/requests/changeset.json",
      JsonChangesetEncoder.encodeLocationChangeset(changeset));
  }

  @Test
  void testUtilityClass() {
    TestUtil.testUtilityClass(JsonChangesetEncoder.class);
  }
}
