package org.openstreetmap.josm.plugins.mapillary.testutils.annotations;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.common.FileSource;
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
import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.platform.commons.support.AnnotationSupport;
import org.openstreetmap.josm.TestUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonReader;
import javax.json.JsonValue;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

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
  /** Use to indicate wether or not the API calls should be mocked */
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
    private static final String defaultBaseMetaDataUrl;
    private static final String defaultBaseTileUrl;
    private static final String defaultAccessKey;
    static {
      String baseMetaDataUrl;
      String baseTileUrl;
      String accessKey;
      try {
        baseMetaDataUrl = (String) TestUtils.getPrivateStaticField(MapillaryURL.APIv4.class, "baseMetaDataUrl");
        baseTileUrl = (String) TestUtils.getPrivateStaticField(MapillaryURL.APIv4.class, "baseTileUrl");
        accessKey = (String) TestUtils.getPrivateStaticField(MapillaryURL.APIv4.class, "ACCESS_ID");
      } catch (ReflectiveOperationException e) {
        Logging.error(e);
        baseMetaDataUrl = null;
        baseTileUrl = null;
        accessKey = null;
      }
      defaultBaseTileUrl = baseTileUrl;
      defaultBaseMetaDataUrl = baseMetaDataUrl;
      defaultAccessKey = accessKey;
    }

    @Override
    public void afterAll(final ExtensionContext context) throws Exception {
      final ExtensionContext.Namespace namespace = ExtensionContext.Namespace.create(MapillaryURLWireMock.class);
      final WireMockServer server = context.getStore(namespace).get(WireMockServer.class, WireMockServer.class);
      server.stop();
      // Ensure things throw if this isn't called
      TestUtils.setPrivateStaticField(MapillaryURL.APIv4.class, "baseMetaDataUrl", null);
      TestUtils.setPrivateStaticField(MapillaryURL.APIv4.class, "baseTileUrl", null);
      TestUtils.setPrivateStaticField(MapillaryURL.APIv4.class, "ACCESS_ID", null);
    }

    @Override
    public void afterEach(final ExtensionContext context) throws Exception {
      final ExtensionContext.Namespace namespace = ExtensionContext.Namespace.create(MapillaryURLWireMock.class);
      final WireMockServer server = context.getStore(namespace).get(WireMockServer.class, WireMockServer.class);
      final List<LoggedRequest> unmatched = server.findAllUnmatchedRequests();
      try {
        if (!unmatched.isEmpty()) {
          fail(unmatched.stream().map(LoggedRequest::getUrl)
            .collect(Collectors.joining(System.lineSeparator(), "Failing URLs:" + System.lineSeparator(), "")));
        }
      } finally {
        // We want to reset it all regardless for future tests
        server.resetAll();
      }
      List<?> stubs = context.getStore(namespace).get(StubMapping.class, List.class);
      stubs.stream().filter(StubMapping.class::isInstance).map(StubMapping.class::cast).forEach(server::addStubMapping);
    }

    @Override
    public void beforeAll(final ExtensionContext context) throws Exception {
      final ExtensionContext.Namespace namespace = ExtensionContext.Namespace.create(MapillaryURLWireMock.class);
      final Object check = context.getStore(namespace).get(WireMockServer.class);
      assertNull(check, "A wiremock server shouldn't have been started yet");

      final WireMockConfiguration wireMockConfiguration = new WireMockConfiguration().dynamicPort();// .usingFilesUnderDirectory("test/resources/api/v4");
      final FillerUrlReplacer fillerUrlReplacer = new FillerUrlReplacer();
      wireMockConfiguration.extensions(new ResponseTemplateTransformer(true), fillerUrlReplacer);
      wireMockConfiguration.withRootDirectory("test" + File.separatorChar + "resources");
      final WireMockServer server = new WireMockServer(wireMockConfiguration);
      fillerUrlReplacer.server = server;

      context.getStore(namespace).put(WireMockServer.class, server);
      server.start();
      final Map<String, StringValuePattern> potentialParameters = new HashMap<>();
      potentialParameters.put("fields", new AnythingPattern());
      server.stubFor(WireMock.get(WireMock.urlPathMatching("/api/v4/graph/image_ids.*"))
        .withQueryParams(potentialParameters).withQueryParam("sequence_id", new AnythingPattern())
        .willReturn(WireMock.aResponse().withBodyFile("api/v4/responses/graph/image_ids/{{request.path.[4]}}.json")
          .withHeader("Content-Type", "application/json")));
      server.stubFor(WireMock.get(WireMock.urlPathMatching("/api/v4/graph/.*")).withQueryParams(potentialParameters)
        .willReturn(WireMock.aResponse().withBodyFile("api/v4/responses/graph/{{request.path.[3]}}.json")
          .withHeader("Content-Type", "application/json")));
      server.stubFor(WireMock.get(WireMock.urlPathMatching("/api/v4/coverageTiles/.*"))
        .withQueryParams(potentialParameters)
        .willReturn(WireMock.aResponse().withBodyFile(
          "api/v4/responses/coverageTiles/{{request.path.[3]}}/{{request.path.[4]}}/{{request.path.[5]}}/{{request.path.[6]}}/{{request.path.[7]}}/{{request.path.[8]}}.mvt")
          .withHeader("Content-Type", "application/vnd.mapbox-vector-tile", "application/vnd.google.protobuf",
            "application/x-protobuf", "application/protobuf")));
      server.stubFor(
        WireMock.get(WireMock.urlPathMatching("/api/v4/graph/.*?/detections")).withQueryParams(potentialParameters)
          .willReturn(WireMock.aResponse().withBodyFile("api/v4/responses/graph/{{request.path.[3]}}/detections.json")
            .withHeader("Content-Type", "application/json")));
      context.getStore(namespace).put(StubMapping.class, server.getStubMappings());
      // Only allow real Mapillary API calls in integration tests.
      if (context.getElement().isPresent()
        && AnnotationSupport.findAnnotation(context.getElement().get(), MapillaryURLWireMock.class)
          .map(MapillaryURLWireMock::value).orElse(Type.STANDARD) == Type.INTEGRATION
        && context.getTags().contains(IntegrationTest.TAG)) {
        TestUtils.setPrivateStaticField(MapillaryURL.APIv4.class, "baseMetaDataUrl", defaultBaseMetaDataUrl);
        TestUtils.setPrivateStaticField(MapillaryURL.APIv4.class, "baseTileUrl", defaultBaseTileUrl);
        TestUtils.setPrivateStaticField(MapillaryURL.APIv4.class, "ACCESS_ID", defaultAccessKey);
      } else {
        TestUtils.setPrivateStaticField(MapillaryURL.APIv4.class, "baseMetaDataUrl",
          server.baseUrl() + "/api/v4/graph/");
        TestUtils.setPrivateStaticField(MapillaryURL.APIv4.class, "baseTileUrl",
          server.baseUrl() + "/api/v4/coverageTiles/");
        // Wiremock pattern matching has issues with the actual key. So replace it with a "test_key".
        TestUtils.setPrivateStaticField(MapillaryURL.APIv4.class, "ACCESS_ID", "test_key");
      }
    }

    private static final class FillerUrlReplacer extends ResponseTransformer {
      private static final Pattern FILLER_URL_PATTERN = Pattern.compile("\"([a-zA-Z0-9_]*?)_filler_url\"");

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
        if (request.contentTypeHeader().containsValue("application/vnd.mapbox-vector-tile")) {
          return response;
        }
        final String origBody = response.getBodyAsString();
        String newBody = origBody.replaceAll("https?:\\/\\/.*?\\/", server.baseUrl() + "/");
        // Replace with ids
        try (
          JsonReader reader = Json.createReader(new ByteArrayInputStream(newBody.getBytes(StandardCharsets.UTF_8)))) {
          final JsonValue value = reader.readValue();
          // Docs indicate that all data should be wrapped in an object in a "data" field
          if (value instanceof JsonObject && ((JsonObject) value).containsKey("data")
            && ((JsonObject) value).get("data") instanceof JsonObject
            && ((JsonObject) value).getJsonObject("data").containsKey("id")) {
            final String id = ((JsonObject) value).getJsonObject("data").getString("id");
            newBody = FILLER_URL_PATTERN.matcher(newBody).replaceAll("\"" + server.baseUrl() + "/$1_" + id + "\"");
          }
        }

        // Filter based off of parameters
        if (request.queryParameter("fields").isPresent()) {
          final List<String> fields = request.queryParameter("fields").values().stream()
            .flatMap(string -> Stream.of(string.split(",", -1))).filter(Objects::nonNull)
            .filter(string -> !Utils.isStripEmpty(string)).collect(Collectors.toList());
          try (
            JsonReader reader = Json.createReader(new ByteArrayInputStream(newBody.getBytes(StandardCharsets.UTF_8)))) {
            final JsonValue jsonValue = reader.readValue();
            if (jsonValue instanceof JsonObject) {
              final JsonObjectBuilder builder = Json.createObjectBuilder();
              for (Map.Entry<String, JsonValue> entry : ((JsonObject) jsonValue).entrySet()) {
                if (entry.getKey().equals("data") && entry.getValue() instanceof JsonObject) {
                  final JsonObjectBuilder dataBuilder = Json.createObjectBuilder();
                  for (Map.Entry<String, JsonValue> dataEntry : entry.getValue().asJsonObject().entrySet()) {
                    if (fields.contains(dataEntry.getKey())) {
                      dataBuilder.add(dataEntry.getKey(), dataEntry.getValue());
                    }
                  }
                  builder.add("data", dataBuilder);
                } else {
                  builder.add(entry.getKey(), entry.getValue());
                }
              }
              newBody = builder.build().toString();
            }
          }
        }

        return Response.Builder.like(response).but().body(newBody).build();
      }
    }
  }
}
