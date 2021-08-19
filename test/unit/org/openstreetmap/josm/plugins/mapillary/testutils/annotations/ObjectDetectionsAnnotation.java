package org.openstreetmap.josm.plugins.mapillary.testutils.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.openstreetmap.josm.gui.tagging.presets.TaggingPresets;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

/**
 * Annotation for ObjectDetections (ensures they have the appropriate presets)
 *
 * @author Taylor Smock
 */
@Documented
@ExtendWith(ObjectDetectionsAnnotation.ObjectDetectionsExtension.class)
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@BasicPreferences
public @interface ObjectDetectionsAnnotation {
    class ObjectDetectionsExtension implements AfterAllCallback, BeforeAllCallback {

        @Override
        public void afterAll(ExtensionContext context) {
            this.beforeAll(context);
        }

        @Override
        public void beforeAll(ExtensionContext context) {
            // TODO replace with @Presets dependency
            if (TaggingPresets.getTaggingPresets().isEmpty()) {
                TaggingPresets.readFromPreferences();
            }
            ObjectDetections.updatePresets();
        }
    }
}
