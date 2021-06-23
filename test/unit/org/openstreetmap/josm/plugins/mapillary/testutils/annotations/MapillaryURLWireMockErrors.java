package org.openstreetmap.josm.plugins.mapillary.testutils.annotations;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.ExtensionContext;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This class is specifically for testing errors with the API.
 */
@Documented
@ExtendWith(MapillaryURLWireMockErrors.WireMockExtension.class)
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface MapillaryURLWireMockErrors {
  enum Type {
    APPLICATION_REQUEST_LIMIT_REACHED, BAD_REQUEST, FORBIDDEN, NOT_FOUND, NO_CONTENT, SERVER_ERROR,
  }

  Type value() default Type.APPLICATION_REQUEST_LIMIT_REACHED;

  static class WireMockExtension implements BeforeEachCallback {
    @Override
    public void beforeEach(ExtensionContext context) throws Exception {
      final ExtensionContext.Namespace namespace = ExtensionContext.Namespace.create(MapillaryURLWireMock.class);
      final WireMockServer server = context.getStore(namespace).get(WireMockServer.class, WireMockServer.class);
      final Type type;
      if (context.getElement().isPresent()) {
        final MapillaryURLWireMockErrors annotation = context.getElement().get()
          .getAnnotation(MapillaryURLWireMockErrors.class);
        type = annotation.value();
      } else {
        type = Type.APPLICATION_REQUEST_LIMIT_REACHED;
      }
      server.resetMappings();
      if (type == Type.APPLICATION_REQUEST_LIMIT_REACHED) {
        server.stubFor(WireMock.get(WireMock.anyUrl())
          .willReturn(WireMock.forbidden().withBodyFile("api/v4/responses/graph/application_request_limit_reached.json")
            .withHeader("Content-Type", "application/json")));
      } else if (type == Type.BAD_REQUEST) {
        server.stubFor(WireMock.get(WireMock.anyUrl()).willReturn(WireMock.badRequest()));
      } else if (type == Type.FORBIDDEN) {
        server.stubFor(WireMock.get(WireMock.anyUrl()).willReturn(WireMock.forbidden()));
      } else if (type == Type.NOT_FOUND) {
        server.stubFor(WireMock.get(WireMock.anyUrl()).willReturn(WireMock.notFound()));
      } else if (type == Type.NO_CONTENT) {
        server.stubFor(WireMock.get(WireMock.anyUrl()).willReturn(WireMock.noContent()));
      } else if (type == Type.SERVER_ERROR) {
        server.stubFor(WireMock.get(WireMock.anyUrl())
          .willReturn(WireMock.serverError().withBodyFile("api/v4/responses/graph/internal_server_error.json")
            .withHeader("Content-Type", "application/json")));
      } else {
        fail("Unknown type: Please update the beforeEach method");
      }
    }
  }
}
