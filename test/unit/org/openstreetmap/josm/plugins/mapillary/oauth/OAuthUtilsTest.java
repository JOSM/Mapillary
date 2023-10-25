// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.GraphicsEnvironment;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URI;
import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import mockit.Invocation;
import mockit.Mock;
import mockit.MockUp;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.platform.commons.support.AnnotationSupport;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.gui.widgets.JMultilineLabel;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMock;
import org.openstreetmap.josm.plugins.mapillary.testutils.annotations.MapillaryURLWireMockErrors;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;
import org.openstreetmap.josm.testutils.annotations.BasicWiremock;
import org.openstreetmap.josm.testutils.annotations.HTTP;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.JosmRuntimeException;

/**
 * Test class for {@link OAuthUtils}
 *
 * @author Taylor Smock
 */
@BasicPreferences
@BasicWiremock
@HTTP
@MapillaryURLWireMock
class OAuthUtilsTest {
    /**
     * This is a simple mocker to check notifications
     */
    private static class NotificationMocker extends MockUp<Notification> {
        boolean shown;
        Notification notification;

        @Mock
        void show(Invocation invocation) {
            this.shown = true;
            this.notification = invocation.getInvokedInstance();
            if (!GraphicsEnvironment.isHeadless()) {
                invocation.proceed();
            }
        }

        /**
         * Reset the mock
         */
        void reset() {
            this.shown = false;
            this.notification = null;
        }
    }

    private static final List<Executable> NOT_LOGGED_IN_CHECKS = Arrays.asList(
        () -> assertFalse(MapillaryProperties.ACCESS_TOKEN.isSet(), "There not be an access token"),
        () -> assertFalse(MapillaryProperties.ACCESS_TOKEN_EXPIRES_AT.isSet(),
            "There should not be any expiration information"),
        () -> assertFalse(MapillaryProperties.ACCESS_TOKEN_REFRESH_IN.isSet(),
            "There should not be any refresh information"),
        () -> assertFalse(MapillaryUser.isTokenValid(), "The token should not be valid"));

    private static final List<Executable> LOGGED_IN_CHECKS = Arrays.asList(
        () -> assertTrue(MapillaryProperties.ACCESS_TOKEN.isSet(), "The access token should be set"),
        () -> assertTrue(MapillaryProperties.ACCESS_TOKEN_REFRESH_IN.isSet(), "There should be a time to refresh in"),
        () -> assertTrue(MapillaryProperties.ACCESS_TOKEN_EXPIRES_AT.isSet(),
            "There should be information on when the token expires"),
        () -> assertTrue(MapillaryUser.isTokenValid(), "The token should be marked as valid"));

    private static final long IMAGE_ID = 135_511_895_288_847L;

    @BeforeEach
    void setUp() {
        // The authorization needs to last more than 10 minutes (otherwise, we try to refresh, which doesn't always test
        // what we actually wanted to test)
        final Duration twentyMinutes = Duration.ofMinutes(20);
        // Most of the tests require that we be authenticated.
        // So we are just going to test the updateAuthorization method every time.
        JsonObjectBuilder jsonObjectBuilder = getUpdateAuthorizationJsonObjectBuilder(twentyMinutes, "test_token",
            "bearer");
        OAuthUtils.updateAuthorization(jsonObjectBuilder.build());
        assertAll("setUp check", Stream.concat(LOGGED_IN_CHECKS.stream(), Stream.of(
            () -> assertTrue(MapillaryUser.isTokenValid()),
            () -> assertEquals("test_token", MapillaryProperties.ACCESS_TOKEN.get()),
            () -> assertTrue(
                Math.abs(Instant.now().getEpochSecond() + twentyMinutes.getSeconds()
                    - MapillaryProperties.ACCESS_TOKEN_EXPIRES_AT.get()) <= 1,
                "Ensure that the expiration time is within a second -- should be equal, but milliseconds would count for this."))));
    }

    @AfterEach
    void tearDown() {
        MapillaryUser.reset();
    }

    /**
     * Ensure that we check for all known bad conditions (this doesn't really test anything, but does help ensure that
     * we actually test for known bad issues)
     */
    @Test
    void testEnsureAllConditionsTested() {
        Set<MapillaryURLWireMockErrors.Type> testedTypes = Stream.of(this.getClass().getDeclaredMethods())
            .map(method -> AnnotationSupport.findAnnotation(method, MapillaryURLWireMockErrors.class))
            .filter(Optional::isPresent).map(Optional::get).map(MapillaryURLWireMockErrors::value)
            .collect(Collectors.toSet());
        assertEquals(EnumSet.allOf(MapillaryURLWireMockErrors.Type.class), testedTypes, "Please test all error types");
    }

    static Stream<Arguments> updateAuthorizationArgs() {
        return Stream.of(Arguments.of(Duration.ofMinutes(1), "test_token", "bearer"),
            Arguments.of(Duration.ofMinutes(30), "test_token_30_min", "bearer"),
            Arguments.of(Duration.ofHours(2), "test_token_2_hour", "bearer"),
            Arguments.of(Duration.ofHours(2).plusSeconds(1), "test_token_2_hour+", "bearer"),
            Arguments.of(Duration.ofDays(1), "test_token_1_day", "bearer"),
            Arguments.of(Duration.ofDays(1).plusSeconds(1), "test_token_1_day+", "bearer"),
            Arguments.of(Duration.ofDays(7), "test_token_1_week", "bearer"),
            Arguments.of(Duration.ofDays(7).plusSeconds(1), "test_token_1_week+", "bearer"),
            Arguments.of(Duration.ofDays(30), "test_token_1_month", "bearer"),
            Arguments.of(Duration.ofDays(30).plusSeconds(1), "test_token_1_month+", "bearer"));
    }

    @ParameterizedTest
    @MethodSource("updateAuthorizationArgs")
    void testUpdateAuthorization(Duration duration, String accessToken, String tokenType) {
        MapillaryUser.reset();
        assertAll("User should be reset", NOT_LOGGED_IN_CHECKS);
        final JsonObjectBuilder jsonObjectBuilder = getUpdateAuthorizationJsonObjectBuilder(duration, accessToken,
            tokenType);
        OAuthUtils.updateAuthorization(jsonObjectBuilder.build());
        assertTrue(MapillaryUser.isTokenValid());
        assertEquals(accessToken, MapillaryProperties.ACCESS_TOKEN.get());
        assertTrue(
            Math.abs(Instant.now().getEpochSecond() + duration.getSeconds()
                - MapillaryProperties.ACCESS_TOKEN_EXPIRES_AT.get()) <= 1,
            "Ensure that the expiration time is within a second -- should be equal, but milliseconds would count for this.");
        /* This code path is copied from the code. If this test section fails, please ensure the code matches */
        final int amount;
        final ChronoUnit expireUnit;
        if (duration.getSeconds() > Duration.of(30, ChronoUnit.DAYS).getSeconds()) {
            amount = 7;
            expireUnit = ChronoUnit.DAYS;
        } else if (duration.getSeconds() > Duration.of(7, ChronoUnit.DAYS).getSeconds()) {
            amount = 1;
            expireUnit = ChronoUnit.DAYS;
        } else if (duration.getSeconds() > Duration.of(1, ChronoUnit.DAYS).getSeconds()) {
            amount = 1;
            expireUnit = ChronoUnit.HALF_DAYS;
        } else if (duration.getSeconds() > Duration.of(2, ChronoUnit.HOURS).getSeconds()) {
            amount = 1;
            expireUnit = ChronoUnit.HOURS;
        } else {
            amount = 10;
            expireUnit = ChronoUnit.MINUTES;
        }
        MapillaryProperties.ACCESS_TOKEN_REFRESH_IN.put(Duration.of(amount, expireUnit).getSeconds());
        /* End copy */
        assertEquals(Duration.of(amount, expireUnit).getSeconds(), MapillaryProperties.ACCESS_TOKEN_REFRESH_IN.get());
        JsonObject jsonObject = getUpdateAuthorizationJsonObjectBuilder(duration, accessToken, "bad_" + tokenType)
            .build();
        JosmRuntimeException josmRuntimeException = assertThrows(JosmRuntimeException.class,
            () -> OAuthUtils.updateAuthorization(jsonObject));
        assertAll(Stream.concat(NOT_LOGGED_IN_CHECKS.stream(),
            Stream.of(() -> assertEquals("Mapillary: Login failed due to unknown token type: bad_" + tokenType,
                josmRuntimeException.getMessage()))));
    }

    @ParameterizedTest
    @MethodSource("updateAuthorizationArgs")
    void testUpdateAuthorizationBadResponse(Duration duration, String accessToken, String tokenType) {
        final NotificationMocker notificationMocker = new NotificationMocker();

        final List<Executable> notificationShownChecks = Arrays.asList(() -> assertTrue(notificationMocker.shown),
            () -> assertTrue(((JMultilineLabel) notificationMocker.notification.getContent()).getText()
                .contains("Mapillary: Could not refresh login, logging out"), "The expected message was not shown"));

        final List<Executable> loggedInChecks = Stream.concat(LOGGED_IN_CHECKS.stream(),
            Stream.of(() -> assertFalse(notificationMocker.shown), () -> assertNull(notificationMocker.notification)))
            .collect(Collectors.toList());
        // Check null duration
        assertAll("null duration init", loggedInChecks);
        OAuthUtils.updateAuthorization(getUpdateAuthorizationJsonObjectBuilder(null, accessToken, tokenType).build());
        GuiHelper.runInEDTAndWait(() -> {
            /* sync threads */});
        assertAll("null duration", NOT_LOGGED_IN_CHECKS);
        assertAll("null duration", notificationShownChecks);

        notificationMocker.reset();
        OAuthUtils
            .updateAuthorization(getUpdateAuthorizationJsonObjectBuilder(duration, accessToken, tokenType).build());

        // Check null access token
        assertAll("null access init", loggedInChecks);
        OAuthUtils.updateAuthorization(getUpdateAuthorizationJsonObjectBuilder(duration, null, tokenType).build());
        GuiHelper.runInEDTAndWait(() -> {
            /* sync threads */});
        assertAll("null access", NOT_LOGGED_IN_CHECKS);
        assertAll("null access", notificationShownChecks);

        notificationMocker.reset();
        OAuthUtils
            .updateAuthorization(getUpdateAuthorizationJsonObjectBuilder(duration, accessToken, tokenType).build());

        // Check null token type
        assertAll("null token init", loggedInChecks);
        OAuthUtils.updateAuthorization(getUpdateAuthorizationJsonObjectBuilder(duration, accessToken, null).build());
        GuiHelper.runInEDTAndWait(() -> {
            /* sync threads */});
        assertAll("null token", NOT_LOGGED_IN_CHECKS);
        assertAll("null token", notificationShownChecks);
    }

    @Test
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.APPLICATION_REQUEST_LIMIT_REACHED)
    void testApplicationRequestLimitReached() {
        final URI url = URI.create(MapillaryConfig.getUrls().getImageInformation(IMAGE_ID));
        assertDoesNotThrow(() -> OAuthUtils.getWithHeader(url), "Responses may be parsed, which may fail.");
        assertAll("Application request limit should not log out the user", LOGGED_IN_CHECKS);
        this.testAuthenticationRefresh(url);
    }

    @Test
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.BAD_REQUEST)
    void testBadRequest() throws IOException {
        final URI url = URI.create(MapillaryConfig.getUrls().getImageInformation(IMAGE_ID));
        verifyCorrectResponseCode(url, HttpURLConnection.HTTP_BAD_REQUEST);
        assertThrows(IOException.class, () -> OAuthUtils.getWithHeader(url),
            "Responses may be parsed, which will likely fail.");
        assertAll("Bad request should not log out the user", LOGGED_IN_CHECKS);
    }

    @Test
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.FORBIDDEN)
    void testForbiddenRequest() throws IOException {
        final URI url = URI.create(MapillaryConfig.getUrls().getImageInformation(IMAGE_ID));
        verifyCorrectResponseCode(url, HttpURLConnection.HTTP_FORBIDDEN);
        assertThrows(IOException.class, () -> OAuthUtils.getWithHeader(url),
            "Responses may be parsed, which will likely fail.");
        assertAll("Forbidden request should not log out the user", LOGGED_IN_CHECKS);
    }

    @Test
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.NOT_FOUND)
    void testNotFoundResponse() throws IOException {
        final URI url = URI.create(MapillaryConfig.getUrls().getImageInformation(IMAGE_ID));
        verifyCorrectResponseCode(url, HttpURLConnection.HTTP_NOT_FOUND);
        assertThrows(IOException.class, () -> OAuthUtils.getWithHeader(url),
            "Responses may be parsed, which will likely fail.");
        assertAll("Not found should not log out the user", LOGGED_IN_CHECKS);
    }

    @Test
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.NO_CONTENT)
    void testNoContentResponse() throws IOException {
        final URI url = URI.create(MapillaryConfig.getUrls().getImageInformation(IMAGE_ID));
        verifyCorrectResponseCode(url, HttpURLConnection.HTTP_NO_CONTENT);
        assertThrows(IOException.class, () -> OAuthUtils.getWithHeader(url),
            "Responses may be parsed, which will likely fail.");
        assertAll("No content should not log out the user", LOGGED_IN_CHECKS);
    }

    @Test
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.SERVER_ERROR)
    void testServerErrorResponse() throws IOException {
        final URI url = URI.create(MapillaryConfig.getUrls().getImageInformation(IMAGE_ID));
        verifyCorrectResponseCode(url, HttpURLConnection.HTTP_INTERNAL_ERROR);
        assertDoesNotThrow(() -> OAuthUtils.getWithHeader(url), "Responses may be parsed, which may fail.");
        assertAll("Server errors should not log out the user", LOGGED_IN_CHECKS);
        this.testAuthenticationRefresh(url);
    }

    @Test
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.UNAUTHORIZED)
    void testUnauthorizedResponse() throws IOException {
        final URI url = URI.create(MapillaryConfig.getUrls().getImageInformation(IMAGE_ID));
        verifyCorrectResponseCode(url, HttpURLConnection.HTTP_UNAUTHORIZED);
        assertThrows(IOException.class, () -> OAuthUtils.getWithHeader(url),
            "Responses may be parsed, which will likely fail.");
        assertAll("A standard unauthorized response should not log out the user", LOGGED_IN_CHECKS);
    }

    @Test
    @MapillaryURLWireMockErrors(MapillaryURLWireMockErrors.Type.UNAUTHORIZED_APPLICATION)
    void testUnauthorizedApplicationResponse() throws IOException {
        final URI url = URI.create(MapillaryConfig.getUrls().getImageInformation(IMAGE_ID));
        verifyCorrectResponseCode(url, HttpURLConnection.HTTP_UNAUTHORIZED);
        assertDoesNotThrow(() -> OAuthUtils.getWithHeader(url), "Responses may be parsed, which may fail.");
        assertAll("An unauthorized application response should log out the user", NOT_LOGGED_IN_CHECKS);
        this.testAuthenticationRefresh(url);
    }

    void testAuthenticationRefresh(final URI url) {
        OAuthUtils.updateAuthorization(
            getUpdateAuthorizationJsonObjectBuilder(Duration.ofSeconds(10), "test", "bearer").build());
        assertDoesNotThrow(() -> OAuthUtils.getWithHeader(url));
    }

    private static void verifyCorrectResponseCode(final URI url, final int responseCode) throws IOException {
        final HttpClient client = HttpClient.create(url.toURL());
        OAuthUtils.addAuthenticationHeader(client);
        try {
            final HttpClient.Response response = client.connect();
            assertEquals(responseCode, response.getResponseCode(), response.fetchContent());
        } finally {
            client.disconnect();
        }
    }

    /**
     * Generate the json for logging in
     *
     * @param duration The duration to use
     * @param accessToken The access token
     * @param tokenType The token type
     * @return The json object builder (if you want to modify stuff)
     */
    @Nonnull
    private static JsonObjectBuilder getUpdateAuthorizationJsonObjectBuilder(@Nullable Duration duration,
        @Nullable String accessToken, @Nullable String tokenType) {
        JsonObjectBuilder jsonObjectBuilder = Json.createObjectBuilder();
        if (accessToken == null) {
            jsonObjectBuilder.addNull("access_token");
        } else {
            jsonObjectBuilder.add("access_token", accessToken);
        }
        if (duration == null) {
            jsonObjectBuilder.addNull("expires_in");
        } else {
            jsonObjectBuilder.add("expires_in", duration.getSeconds());
        }
        if (tokenType == null) {
            jsonObjectBuilder.addNull("token_type");
        } else {
            jsonObjectBuilder.add("token_type", tokenType);
        }
        return jsonObjectBuilder;
    }
}
