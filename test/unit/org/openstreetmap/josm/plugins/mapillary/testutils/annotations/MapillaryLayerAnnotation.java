package org.openstreetmap.josm.plugins.mapillary.testutils.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.tools.Logging;

/**
 * Ensure that the MapillaryLayer is cleaned up
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE, ElementType.METHOD })
@ExtendWith(MapillaryLayerAnnotation.MapillaryLayerExtension.class)
@MapillaryURLWireMock
public @interface MapillaryLayerAnnotation {
    class MapillaryLayerExtension implements AfterEachCallback, BeforeEachCallback {

        @Override
        public void afterEach(ExtensionContext context) throws Exception {
            this.beforeEach(context);
        }

        @Override
        public void beforeEach(ExtensionContext context) {
            try {
                cleanup();
            } catch (IllegalArgumentException illegalArgumentException) {
                // This happens when the layer isn't properly destroyed in a previous test
                Logging.trace(illegalArgumentException);
            }
        }

        void cleanup() {
            if (MapillaryLayer.hasInstance()) {
                final MapillaryLayer layer = MapillaryLayer.getInstance();
                if (MainApplication.getLayerManager().containsLayer(layer)) {
                    MainApplication.getLayerManager().removeLayer(layer);
                }
                layer.destroy();
            }
        }
    }
}
