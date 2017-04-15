// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
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

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(JsonUserProfileDecoder.class);
  }

  private static InputStream getJsonInputStream() throws IOException, URISyntaxException {
    String fileContent = String.join("\n", Files.readAllLines(
      Paths.get(JsonUserProfileDecoderTest.class.getResource("/api/v3/responses/userProfile.json").toURI()),
      StandardCharsets.UTF_8
    ));
    fileContent = fileContent.replace(
      "https://d4vkkeqw582u.cloudfront.net/3f9f044b34b498ddfb9afbb6/profile.png",
      JsonUserProfileDecoder.class.getResource("/images/fake-avatar.png").toString()
    );
    return new ByteArrayInputStream(fileContent.getBytes());
  }

  @Test
  public void testDecodeUserProfile() throws IOException, URISyntaxException {
    UserProfile profile = JsonUserProfileDecoder.decodeUserProfile(Json.createReader(getJsonInputStream()).readObject());
    assertEquals("2BJl04nvnfW1y2GNaj7x5w", profile.getKey());
    assertEquals("gyllen", profile.getUsername());
    assertNotNull(profile.getAvatar());
  }

  @Test
  public void testDecodeInvalidUserProfile() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
    assertNull(JsonUserProfileDecoder.decodeUserProfile(null));
    assertNull(JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject("{}")));
    assertNull(JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject("{\"key\":\"arbitrary_key\"}")));

    UserProfile profile = JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject("{\"key\":\"arbitrary_key\", \"username\":\"arbitrary_username\"}"));
    Field fakeAvatar = JsonUserProfileDecoder.class.getDeclaredField("FAKE_AVATAR");
    fakeAvatar.setAccessible(true);
    assertEquals(fakeAvatar.get(null), profile.getAvatar());

    profile = JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject("{\"key\":\"arbitrary_key\", \"username\":\"arbitrary_username\", \"avatar\":\"null\"}"));
    assertEquals(fakeAvatar.get(null), profile.getAvatar());
  }
}
