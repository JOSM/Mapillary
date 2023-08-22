// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.testutils.annotations;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.imageio.ImageIO;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.common.FileSource;
import com.github.tomakehurst.wiremock.common.TextFile;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import com.github.tomakehurst.wiremock.extension.Parameters;
import com.github.tomakehurst.wiremock.extension.ResponseTransformer;
import com.github.tomakehurst.wiremock.extension.responsetemplating.ResponseTemplateTransformer;
import com.github.tomakehurst.wiremock.http.Request;
import com.github.tomakehurst.wiremock.http.Response;
import com.github.tomakehurst.wiremock.matching.AnythingPattern;
import com.github.tomakehurst.wiremock.matching.StringValuePattern;
import com.github.tomakehurst.wiremock.stubbing.StubMapping;
import com.github.tomakehurst.wiremock.verification.LoggedRequest;
import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonReader;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.platform.commons.support.AnnotationSupport;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.IMapillaryUrls;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryUrls;
import org.openstreetmap.josm.tools.Utils;

/**
 * Mock Mapillary API calls
 *
 * @author Taylor Smock
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@ExtendWith(MapillaryURLWireMock.MapillaryURLMockExtension.class)
public @interface MapillaryURLWireMock {
    /** Use to indicate whether the API calls should be mocked */
    enum Type {
        /** Mock the API calls */
        STANDARD,
        /** Call the real server (may only be used with {@link IntegrationTest}s) */
        INTEGRATION
    }

    Type value() default Type.STANDARD;

    /**
     * Do the test setup and teardowns
     */
    class MapillaryURLMockExtension implements AfterAllCallback, AfterEachCallback, BeforeAllCallback {
        @Override
        public void afterAll(final ExtensionContext context) {
            final ExtensionContext.Namespace namespace = ExtensionContext.Namespace.create(MapillaryURLWireMock.class);
            final WireMockServer server = context.getStore(namespace).get(WireMockServer.class, WireMockServer.class);
            MapillaryConfig.setUrlsProvider(new NullMapillaryUrl());
            server.stop();
        }

        @Override
        public void afterEach(final ExtensionContext context) {
            final ExtensionContext.Namespace namespace = ExtensionContext.Namespace.create(MapillaryURLWireMock.class);
            final WireMockServer server = context.getStore(namespace).get(WireMockServer.class, WireMockServer.class);
            final List<LoggedRequest> unmatched = server.findAllUnmatchedRequests();
            try {
                if (!unmatched.isEmpty()) {
                    fail(unmatched.stream().map(LoggedRequest::getUrl).collect(
                        Collectors.joining(System.lineSeparator(), "Failing URLs:" + System.lineSeparator(), "")));
                }
            } finally {
                // We want to reset it all regardless for future tests
                server.resetAll();
                List<?> stubs = context.getStore(namespace).get(StubMapping.class, List.class);
                stubs.stream().filter(StubMapping.class::isInstance).map(StubMapping.class::cast)
                    .forEach(server::addStubMapping);
            }
        }

        @Override
        public void beforeAll(final ExtensionContext context) throws Exception {
            final ExtensionContext.Namespace namespace = ExtensionContext.Namespace.create(MapillaryURLWireMock.class);
            final Object check = context.getStore(namespace).get(WireMockServer.class);
            assertNull(check, "A wiremock server shouldn't have been started yet");

            final WireMockConfiguration wireMockConfiguration = new WireMockConfiguration().dynamicPort();
            final FillerUrlReplacer fillerUrlReplacer = new FillerUrlReplacer();
            wireMockConfiguration.extensions(new CollectionEndpoint(), new ResponseTemplateTransformer(false),
                fillerUrlReplacer);
            // See JOSM #21121 for why this is necessary
            Path directory = Paths.get("test", "resources");
            if (!Files.isDirectory(directory)) {
                final String mapillary = "Mapillary";
                if (Files.isDirectory(Paths.get(mapillary + "-git"))) {
                    directory = Paths.get(mapillary + "-git", directory.toString());
                } else if (Files.isDirectory(Paths.get(mapillary))) {
                    directory = Paths.get(mapillary, directory.toString());
                } else {
                    try (Stream<Path> paths = Files.list(Paths.get("."))) {
                        fail("Mapillary test/resources directory not found." + System.lineSeparator()
                            + paths.map(Path::toString).collect(Collectors.joining(System.lineSeparator())));
                    }
                }
            }
            wireMockConfiguration.withRootDirectory(directory.toString());
            final WireMockServer server = new WireMockServer(wireMockConfiguration);
            fillerUrlReplacer.server = server;

            context.getStore(namespace).put(WireMockServer.class, server);
            server.start();
            final Map<String, StringValuePattern> potentialParameters = new HashMap<>();
            potentialParameters.put("fields", new AnythingPattern());
            server.stubFor(WireMock.get(WireMock.urlPathMatching("/api/v4/graph/image_ids.*"))
                .withQueryParam("sequence_id", new AnythingPattern())
                .willReturn(WireMock.aResponse()
                    .withBodyFile("api/v4/responses/graph/image_ids/{{request.query.sequence_id}}.json")
                    .withHeader("Content-Type", "application/json").withTransformers("response-template"))
                .atPriority(0));
            server
                .stubFor(WireMock.get(WireMock.urlPathMatching("/api/v4/graph/.*")).withQueryParams(potentialParameters)
                    .willReturn(WireMock.aResponse().withBodyFile("api/v4/responses/graph/{{request.path.[3]}}.json")
                        .withHeader("Content-Type", "application/json").withTransformers("response-template"))
                    .atPriority(100));
            server.stubFor(WireMock.get(WireMock.urlPathMatching("/api/v4/coverageTiles/.*"))
                .withQueryParams(potentialParameters)
                .willReturn(WireMock.aResponse().withBodyFile(
                    "api/v4/responses/coverageTiles/{{request.path.[3]}}/{{request.path.[4]}}/{{request.path.[5]}}/{{request.path.[6]}}/{{request.path.[7]}}.mvt")
                    .withHeader("Content-Type", "application/vnd.mapbox-vector-tile", "application/vnd.google.protobuf",
                        "application/x-protobuf", "application/protobuf")
                    .withTransformers("response-template"))
                .atPriority(0));
            server.stubFor(WireMock.get(WireMock.urlPathMatching("/api/v4/graph/.*?/detections"))
                .withQueryParams(potentialParameters)
                .willReturn(
                    WireMock.aResponse().withBodyFile("api/v4/responses/graph/{{request.path.[3]}}/detections.json")
                        .withHeader("Content-Type", "application/json").withTransformers("response-template"))
                .atPriority(0));

            final Map<String, StringValuePattern> imageIdsParameters = new HashMap<>(potentialParameters);
            imageIdsParameters.put("image_ids", new AnythingPattern());

            // This stub *MUST* be accounted for in the extension (for example, the CollectionEndpoint extension)
            server.stubFor(WireMock.get(WireMock.urlPathMatching("/api/v4/graph/images.*"))
                .withQueryParams(imageIdsParameters).willReturn(WireMock.aResponse()).atPriority(0));
            // This stub is needed, since the creation of new layers often makes a call to the "stub" URL
            server
                .stubFor(WireMock.get("/api/v4/coverageTiles/mly1_computed_public/2/{z}/{x}/{y}?access_token=test_key")
                    .willReturn(WireMock.serverError()));
            server.stubFor(WireMock.get("/paintstyle").willReturn(WireMock.forbidden()));

            // Stubs for images (default to returning a "blank" image at low priority)
            try (ByteArrayOutputStream imageOut = new ByteArrayOutputStream()) {
                final BufferedImage bufferedImage = new BufferedImage(2048, 2048, BufferedImage.TYPE_INT_ARGB);
                final Graphics2D graphics = bufferedImage.createGraphics();
                graphics.setColor(Color.WHITE);
                graphics.drawRect(0, 0, 2048, 2048);
                ImageIO.write(bufferedImage, "png", imageOut);
                server.stubFor(WireMock.get(WireMock.urlPathMatching("/thumb_([0-9]+)_([0-9]+)"))
                    .willReturn(WireMock.aResponse().withBody(imageOut.toByteArray())).atPriority(100));
            } catch (IOException exception) {
                throw new UncheckedIOException(exception);
            }

            // Store the stub mappings for future use.
            context.getStore(namespace).put(StubMapping.class, server.getStubMappings());

            // Only allow real Mapillary API calls in integration tests.
            if (context.getElement().isPresent()
                && AnnotationSupport.findAnnotation(context.getElement().get(), MapillaryURLWireMock.class)
                    .map(MapillaryURLWireMock::value).orElse(Type.STANDARD) == Type.INTEGRATION
                && context.getTags().contains(IntegrationTest.TAG)) {
                MapillaryConfig.setUrlsProvider(new MapillaryUrls());
            } else {
                MapillaryConfig.setUrlsProvider(new WireMockServerMapillaryUrl(server));
            }
        }

        /**
         * Account for collection endpoints
         */
        private static final class CollectionEndpoint extends ResponseTransformer {

            @Override
            public Response transform(Request request, Response response, FileSource files, Parameters parameters) {
                if (request.queryParameter("image_ids").isPresent()) {
                    // Not implemented currently since I don't know if I need to split on `,` or `%2C`
                    final List<String> imageIds = request.queryParameter("image_ids").values().stream()
                        .map(str -> str.split(",", -1)).flatMap(Stream::of).filter(Objects::nonNull)
                        .collect(Collectors.toList());
                    final List<TextFile> imageText = imageIds.stream()
                        .map(image -> files.getTextFileNamed("api/v4/responses/graph/" + image + ".json"))
                        .collect(Collectors.toList());
                    // We need to get the actual bytes prior to returning, so we need to read the files.
                    final String body = imageText.stream().map(TextFile::readContentsAsString)
                        .collect(Collectors.joining(",", "{\"data\":[", "]}"));
                    return Response.Builder.like(response).but().body(body).build();
                }
                return response;
            }

            @Override
            public String getName() {
                return "collectionEndpoint";
            }
        }

        /**
         * Replace filler urls with wiremock URLs
         */
        private static final class FillerUrlReplacer extends ResponseTransformer {
            private static final Pattern FILLER_URL_PATTERN = Pattern.compile("([a-zA-Z0-9_]*?)_filler_url");

            WireMockServer server;

            @Override
            public String getName() {
                return "fillerUrlReplacer";
            }

            @Override
            public Response transform(Request request, Response response, FileSource files, Parameters parameters) {
                if (server == null) {
                    fail("No wiremock server set");
                }
                // If the user is logged in, we <i>technically</i> don't need the access_token in parameters
                if (!request.queryParameter("access_token").isPresent() && !request.containsHeader("Authorization")) {
                    fail("Always pass the access token: " + request.getUrl());
                }
                // Don't try to modify the vector tiles
                if (response.getHeaders().getContentTypeHeader().containsValue("application/vnd.mapbox-vector-tile")) {
                    return response;
                }
                final String origBody = response.getBodyAsString();
                if (Utils.isBlank(origBody)) {
                    return response;
                }
                String newBody = origBody.replaceAll("https?://.*?/", server.baseUrl() + "/");
                // Replace with ids
                try (JsonReader reader = Json
                    .createReader(new ByteArrayInputStream(newBody.getBytes(StandardCharsets.UTF_8)))) {
                    final JsonValue value = reader.readValue();
                    if (value instanceof JsonObject) {
                        // Ensure that we don't modify error codes
                        if (((JsonObject) value).containsKey("error")) {
                            return response;
                        }
                        final JsonValue data = ((JsonObject) value).getOrDefault("data", value);
                        if (data instanceof JsonObject && ((JsonObject) data).containsKey("id")) {
                            newBody = replaceThumbUrls(server, (JsonObject) data).toString();
                        } else if (data instanceof JsonArray) {
                            final JsonArrayBuilder jsonArrayBuilder = Json.createArrayBuilder();
                            for (JsonValue jsonValue : ((JsonArray) data)) {
                                if (jsonValue instanceof JsonObject && ((JsonObject) jsonValue).containsKey("id")) {
                                    jsonArrayBuilder.add(replaceThumbUrls(server, (JsonObject) jsonValue));
                                }
                            }
                            final JsonObjectBuilder dataObject = Json.createObjectBuilder();
                            newBody = dataObject.add("data", jsonArrayBuilder).build().toString();
                        }
                    }
                }

                // Filter based off of parameters
                if (request.queryParameter("fields").isPresent()) {
                    final List<String> fields = request.queryParameter("fields").values().stream()
                        .flatMap(string -> Stream.of(string.split(",", -1))).filter(Objects::nonNull)
                        .filter(string -> !Utils.isStripEmpty(string)).collect(Collectors.toList());
                    try (JsonReader reader = Json
                        .createReader(new ByteArrayInputStream(newBody.getBytes(StandardCharsets.UTF_8)))) {
                        final JsonValue jsonValue = reader.readValue();
                        if (jsonValue instanceof JsonObject) {
                            final JsonObjectBuilder builder = Json.createObjectBuilder();
                            filterFiles(builder, fields, jsonValue);
                            newBody = builder.build().toString();
                        }
                    }
                }

                return Response.Builder.like(response).but().body(newBody).build();
            }

            private static JsonObject replaceThumbUrls(final WireMockServer server, final JsonObject jsonObject) {
                final JsonObjectBuilder builder = Json.createObjectBuilder();
                final String id = jsonObject.getString("id");
                for (Map.Entry<String, JsonValue> entry : jsonObject.entrySet()) {
                    if (entry.getValue().getValueType() == JsonValue.ValueType.STRING
                        && FILLER_URL_PATTERN.matcher(((JsonString) entry.getValue()).getString()).matches()) {
                        builder.add(entry.getKey(),
                            FILLER_URL_PATTERN.matcher(((JsonString) entry.getValue()).getString())
                                .replaceAll(server.baseUrl() + "/$1_" + id));
                    } else {
                        builder.add(entry.getKey(), entry.getValue());
                    }
                }
                return builder.build();
            }

            private static void filterFiles(final JsonObjectBuilder builder, final List<String> fields,
                final JsonValue jsonValue) {
                if (jsonValue instanceof JsonObject) {
                    for (Map.Entry<String, JsonValue> entry : ((JsonObject) jsonValue).entrySet()) {
                        if (entry.getValue() instanceof JsonObject) {
                            final JsonObjectBuilder dataBuilder = Json.createObjectBuilder();
                            for (Map.Entry<String, JsonValue> dataEntry : entry.getValue().asJsonObject().entrySet()) {
                                dataBuilder.add(dataEntry.getKey(), dataEntry.getValue());
                            }
                            final JsonObject jsonObject = dataBuilder.build();
                            if (!jsonObject.isEmpty() && fields.contains(entry.getKey())) {
                                builder.add(entry.getKey(), jsonObject);
                            }
                        } else if (entry.getValue() instanceof JsonArray) {
                            JsonArrayBuilder jsonArrayBuilder = Json.createArrayBuilder();
                            for (JsonValue jsonValue1 : ((JsonArray) entry.getValue())) {
                                JsonObjectBuilder jsonObjectBuilder = Json.createObjectBuilder();
                                filterFiles(jsonObjectBuilder, fields, jsonValue1);
                                final JsonObject jsonObject = jsonObjectBuilder.build();
                                if (!jsonObject.isEmpty()) {
                                    jsonArrayBuilder.add(jsonObject);
                                }
                            }
                            final JsonArray jsonArray = jsonArrayBuilder.build();
                            if (!jsonArray.isEmpty()) {
                                builder.add(entry.getKey(), jsonArray);
                            }
                        } else if (fields.contains(entry.getKey()) || "id".equals(entry.getKey())) {
                            builder.add(entry.getKey(), entry.getValue());
                        }
                    }
                }
            }
        }
    }

    /**
     * A wiremocked implementation of {@link IMapillaryUrls}
     */
    class WireMockServerMapillaryUrl implements IMapillaryUrls {
        final String baseUrl;

        WireMockServerMapillaryUrl(WireMockServer server) {
            this.baseUrl = server.baseUrl();
        }

        public WireMockServerMapillaryUrl(String baseUrl) {
            this.baseUrl = baseUrl;
        }

        @Override
        public String getBaseMetaDataUrl() {
            return this.baseUrl + "/api/v4/graph/";
        }

        @Override
        public String getBaseTileUrl() {
            return this.baseUrl + "/api/v4/coverageTiles/";
        }

        @Override
        public String getPaintStyleUrl() {
            return this.baseUrl + "/paintstyle";
        }

        @Override
        public String getAccessId() {
            return "test-id";
        }

        @Override
        public long getClientId() {
            return 1234;
        }

        @Override
        public String getClientSecret() {
            return "test-secret";
        }

        @Override
        public String getBaseUrl() {
            return this.baseUrl + "/baseUrl";
        }
    }

    /**
     * This exists only to ensure that the appropriate annotations are used throughout tests
     */
    class NullMapillaryUrl implements IMapillaryUrls {
        @Override
        public String getBaseMetaDataUrl() {
            return null;
        }

        @Override
        public String getBaseTileUrl() {
            return null;
        }

        @Override
        public String getPaintStyleUrl() {
            return null;
        }

        @Override
        public String getAccessId() {
            return null;
        }

        @Override
        public long getClientId() {
            throw new UnsupportedOperationException();
        }

        @Override
        public String getClientSecret() {
            return null;
        }

        @Override
        public String getBaseUrl() {
            return null;
        }
    }
}
