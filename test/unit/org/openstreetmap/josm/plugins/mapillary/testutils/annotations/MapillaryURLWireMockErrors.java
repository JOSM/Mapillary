package org.openstreetmap.josm.plugins.mapillary.testutils.annotations;

import static org.junit.jupiter.api.Assertions.fail;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;

/**
 * This class is specifically for testing errors with the API.
 */
@Documented
@ExtendWith(MapillaryURLWireMockErrors.WireMockExtension.class)
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface MapillaryURLWireMockErrors {
    enum Type {
        /** Use for testing rate limiting (403) */
        APPLICATION_REQUEST_LIMIT_REACHED,
        /** Use for testing bad requests (400) */
        BAD_REQUEST,
        /** Use for testing non-specific forbidden urls (403) */
        FORBIDDEN,
        /** Use for testing items that are not found (404) */
        NOT_FOUND,
        /** Generic no-content return information (204) */
        NO_CONTENT,
        /** Use for simulating server errors (500) */
        SERVER_ERROR,
        /** Use for generic unauthorized tests (401) */
        UNAUTHORIZED,
        /**
         * Use for OAuth unauthorized application tests
         * (i.e., user revoked access, or access was revoked when user changed their password)
         * (401)
         */
        UNAUTHORIZED_APPLICATION
    }

    Type value() default Type.APPLICATION_REQUEST_LIMIT_REACHED;

    class WireMockExtension implements BeforeEachCallback {
        @Override
        public void beforeEach(ExtensionContext context) {
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
                server.stubFor(WireMock.any(WireMock.anyUrl())
                    .willReturn(WireMock.forbidden()
                        .withBodyFile("api/v4/responses/graph/application_request_limit_reached.json")
                        .withHeader("Content-Type", "application/json")));
            } else if (type == Type.BAD_REQUEST) {
                server.stubFor(WireMock.any(WireMock.anyUrl()).willReturn(WireMock.badRequest()));
            } else if (type == Type.FORBIDDEN) {
                server.stubFor(WireMock.any(WireMock.anyUrl()).willReturn(WireMock.forbidden()));
            } else if (type == Type.NOT_FOUND) {
                server.stubFor(WireMock.any(WireMock.anyUrl()).willReturn(WireMock.notFound()));
            } else if (type == Type.NO_CONTENT) {
                server.stubFor(WireMock.any(WireMock.anyUrl()).willReturn(WireMock.noContent()));
            } else if (type == Type.UNAUTHORIZED_APPLICATION) {
                server.stubFor(WireMock.any(WireMock.anyUrl())
                    .willReturn(WireMock.unauthorized()
                        .withBody("{\"error\":{\"message\":\"Error validating access token: "
                            + "The user has not authorized application " + MapillaryURL.APIv4.CLIENT_ID + ".\","
                            + "\"type\":\"OAuthException\",\"code\":190,\"fbtrace_id\":\"fbtrace_id_hash\"}}")
                        .withHeader("Content-Type", "application/json")));
            } else if (type == Type.UNAUTHORIZED) {
                server.stubFor(WireMock.any(WireMock.anyUrl()).willReturn(WireMock.unauthorized()));
            } else if (type == Type.SERVER_ERROR) {
                server.stubFor(WireMock.any(WireMock.anyUrl())
                    .willReturn(WireMock.serverError().withBodyFile("api/v4/responses/graph/internal_server_error.json")
                        .withHeader("Content-Type", "application/json")));
            } else {
                fail("Unknown type: Please update the beforeEach method");
            }
        }
    }
}
