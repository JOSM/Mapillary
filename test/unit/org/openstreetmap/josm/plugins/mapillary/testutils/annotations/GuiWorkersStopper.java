// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.testutils.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.concurrent.atomic.AtomicBoolean;

import org.awaitility.Awaitility;
import org.awaitility.Durations;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE, ElementType.METHOD })
@BasicPreferences
@ExtendWith(GuiWorkersStopper.MapillaryDownloader.class)
@MapillaryLayerAnnotation
public @interface GuiWorkersStopper {
    class MapillaryDownloader implements AfterEachCallback {
        @Override
        public void afterEach(ExtensionContext context) {
            AtomicBoolean done = new AtomicBoolean();
            new org.openstreetmap.josm.plugins.mapillary.gui.workers.MapillaryNodesDownloader(nodes -> done.set(true), 1).execute();
            Awaitility.await().atMost(Durations.FIVE_SECONDS).untilTrue(done);
            done.set(false);
            new org.openstreetmap.josm.plugins.mapillary.gui.workers.MapillarySequenceDownloader("", chunks -> done.set(true)).execute();
            Awaitility.await().atMost(Durations.FIVE_SECONDS).untilTrue(done);
        }
    }
}
