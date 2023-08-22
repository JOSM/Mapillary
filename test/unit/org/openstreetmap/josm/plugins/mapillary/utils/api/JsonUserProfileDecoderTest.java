// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import jakarta.json.Json;
import jakarta.json.JsonReader;
import org.junit.jupiter.api.Test;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.JsonUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

@BasicPreferences
class JsonUserProfileDecoderTest {
    private static Object getFakeAvatar() {
        return TestUtil.getPrivateFieldValue(JsonUserProfileDecoder.class, null, "FAKE_AVATAR");
    }

    @Test
    void testUtilityClass() {
        TestUtil.testUtilityClass(JsonUserProfileDecoder.class);
    }

    private static InputStream getJsonInputStream(final String path) throws IOException, URISyntaxException {
        String fileContent = String.join("\n", Files.readAllLines(
            Paths.get(JsonUserProfileDecoderTest.class.getResource(path).toURI()), StandardCharsets.UTF_8));
        fileContent = fileContent.replace("https://d4vkkeqw582u.cloudfront.net/3f9f044b34b498ddfb9afbb6/profile.png",
            JsonUserProfileDecoder.class.getResource("/images/fake-avatar.png").toString());
        fileContent = fileContent.replace("https://example.org",
            JsonUserProfileDecoder.class.getResource("/api/v3/responses/userProfile.json").toString());
        return new ByteArrayInputStream(fileContent.getBytes(StandardCharsets.UTF_8));
    }

    @Test
    void testDecodeUserProfile() throws IOException, URISyntaxException, IllegalArgumentException {
        try (InputStream inputStream = getJsonInputStream("/api/v3/responses/userProfile.json");
            JsonReader reader = Json.createReader(inputStream)) {
            UserProfile profile = JsonUserProfileDecoder.decodeUserProfile(reader.readObject());
            assertEquals("2BJl04nvnfW1y2GNaj7x5w", profile.getKey());
            assertEquals("gyllen", profile.getUsername());
            assertNotNull(profile.getAvatar());
            assertNotSame(getFakeAvatar(), profile.getAvatar());
        }
    }

    @Test
    void testDecodeUserProfile2() throws IOException, URISyntaxException, IllegalArgumentException {
        try (InputStream inputStream = getJsonInputStream("/api/v3/responses/userProfile2.json");
            JsonReader reader = Json.createReader(inputStream)) {
            UserProfile profile = JsonUserProfileDecoder.decodeUserProfile(reader.readObject());
            assertEquals("abcdefg1", profile.getKey());
            assertEquals("mapillary_userÄ2!", profile.getUsername());
            assertSame(getFakeAvatar(), profile.getAvatar());
        }
    }

    @Test
    void testDecodeInvalidUserProfile() throws IllegalArgumentException, SecurityException {
        assertNull(JsonUserProfileDecoder.decodeUserProfile(null));
        assertNull(JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject("{}")));
        assertNull(JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject("{\"key\":\"arbitrary_key\"}")));

        UserProfile profile = JsonUserProfileDecoder.decodeUserProfile(
            JsonUtil.string2jsonObject("{\"key\":\"arbitrary_key\", \"username\":\"arbitrary_username\"}"));
        assertNotNull(profile);
        assertSame(getFakeAvatar(), profile.getAvatar());

        profile = JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject(
            "{\"key\":\"arbitrary_key\", \"username\":\"arbitrary_username\", \"avatar\":\"https://127.0.0.1/nonExistingAvatarFile\"}"));
        assertNotNull(profile);
        assertSame(getFakeAvatar(), profile.getAvatar());
    }
}
