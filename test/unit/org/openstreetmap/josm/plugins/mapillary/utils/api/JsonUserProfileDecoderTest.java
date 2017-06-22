// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import javax.json.Json;

import org.junit.Test;

import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.JsonUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

public class JsonUserProfileDecoderTest {
  private static final Object FAKE_AVATAR;

  static {
    Object fakeAvatar;
    try {
      fakeAvatar = TestUtil.getAccessibleField(JsonUserProfileDecoder.class, "FAKE_AVATAR").get(null);
    } catch (IllegalArgumentException | IllegalAccessException e) {
      fakeAvatar = null;
    }
    FAKE_AVATAR = fakeAvatar;
  }

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(JsonUserProfileDecoder.class);
  }

  private static InputStream getJsonInputStream(final String path) throws IOException, URISyntaxException {
    String fileContent = String.join("\n", Files.readAllLines(
      Paths.get(JsonUserProfileDecoderTest.class.getResource(path).toURI()),
      StandardCharsets.UTF_8
    ));
    fileContent = fileContent.replace(
      "https://d4vkkeqw582u.cloudfront.net/3f9f044b34b498ddfb9afbb6/profile.png",
      JsonUserProfileDecoder.class.getResource("/images/fake-avatar.png").toString()
    );
    fileContent = fileContent.replace(
      "https://example.org",
      JsonUserProfileDecoder.class.getResource("/api/v3/responses/userProfile.json").toString()
    );
    return new ByteArrayInputStream(fileContent.getBytes());
  }

  @Test
  public void testDecodeUserProfile() throws IOException, URISyntaxException, IllegalArgumentException {
    UserProfile profile = JsonUserProfileDecoder.decodeUserProfile(Json.createReader(getJsonInputStream("/api/v3/responses/userProfile.json")).readObject());
    assertEquals("2BJl04nvnfW1y2GNaj7x5w", profile.getKey());
    assertEquals("gyllen", profile.getUsername());
    assertNotNull(profile.getAvatar());
    assertFalse(FAKE_AVATAR == profile.getAvatar());
  }

  @Test
  public void testDecodeUserProfile2() throws IOException, URISyntaxException, IllegalArgumentException {
    UserProfile profile = JsonUserProfileDecoder.decodeUserProfile(Json.createReader(getJsonInputStream("/api/v3/responses/userProfile2.json")).readObject());
    assertEquals("abcdefg1", profile.getKey());
    assertEquals("mapillary_userÄ2!", profile.getUsername());
    assertTrue(FAKE_AVATAR == profile.getAvatar());
  }

  @Test
  public void testDecodeInvalidUserProfile() throws IllegalArgumentException, SecurityException {
    assertNull(JsonUserProfileDecoder.decodeUserProfile(null));
    assertNull(JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject("{}")));
    assertNull(JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject("{\"key\":\"arbitrary_key\"}")));

    UserProfile profile = JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject("{\"key\":\"arbitrary_key\", \"username\":\"arbitrary_username\"}"));
    assertTrue(FAKE_AVATAR == profile.getAvatar());

    profile = JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject("{\"key\":\"arbitrary_key\", \"username\":\"arbitrary_username\", \"avatar\":\"https://127.0.0.1/nonExistingAvatarFile\"}"));
    assertTrue(FAKE_AVATAR == profile.getAvatar());
  }
}
