// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.junit.jupiter.api.Test;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.JsonUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

import jakarta.json.Json;
import jakarta.json.JsonReader;

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
        return new ByteArrayInputStream(fileContent.getBytes(StandardCharsets.UTF_8));
    }

    @Test
    void testDecodeUserProfile() throws IOException, URISyntaxException, IllegalArgumentException {
        try (InputStream inputStream = getJsonInputStream("/__files/api/v4/responses/graph/104214208486349.json");
            JsonReader reader = Json.createReader(inputStream)) {
            UserProfile profile = JsonUserProfileDecoder.decodeUserProfile(reader.readObject());
            assertEquals(104_214_208_486_349L, profile.key());
            assertEquals("vorpalblade", profile.username());
            assertSame(getFakeAvatar(), profile.avatar(), "avatar not yet in response");
        }
    }

    @Test
    void testDecodeUserProfile2() throws IOException, URISyntaxException, IllegalArgumentException {
        try (InputStream inputStream = getJsonInputStream("/__files/api/v4/responses/graph/104214208486350.json");
            JsonReader reader = Json.createReader(inputStream)) {
            UserProfile profile = JsonUserProfileDecoder.decodeUserProfile(reader.readObject());
            assertEquals(104_214_208_486_349L, profile.key(),
                "This should be the same as 104214208486349.json except with a different username");
            assertEquals("mapillary_userÄ2!", profile.username());
            assertSame(getFakeAvatar(), profile.avatar(), "avatar not yet in response");
        }
    }

    @Test
    void testDecodeInvalidUserProfile() throws IllegalArgumentException, SecurityException {
        assertNull(JsonUserProfileDecoder.decodeUserProfile(null));
        assertNull(JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject("{}")));
        assertNull(JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject("{\"key\":\"arbitrary_key\"}")));

        UserProfile profile = JsonUserProfileDecoder.decodeUserProfile(
            JsonUtil.string2jsonObject("{\"id\":\"-1\", \"username\":\"arbitrary_username\"}"));
        assertNotNull(profile);
        assertSame(getFakeAvatar(), profile.avatar());

        profile = JsonUserProfileDecoder.decodeUserProfile(JsonUtil.string2jsonObject(
            "{\"id\":\"-1\", \"username\":\"arbitrary_username\", \"avatar\":\"https://127.0.0.1/nonExistingAvatarFile\"}"));
        assertNotNull(profile);
        assertSame(getFakeAvatar(), profile.avatar());
    }
}
