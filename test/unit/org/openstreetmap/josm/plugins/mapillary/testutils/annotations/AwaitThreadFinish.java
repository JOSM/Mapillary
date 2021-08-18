package org.openstreetmap.josm.plugins.mapillary.testutils.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.awaitility.Awaitility;
import org.awaitility.Durations;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;

/**
 * An annotation that waits for threads to finish before continuing
 *
 * @author Taylor Smock
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE, ElementType.METHOD })
@ExtendWith(AwaitThreadFinish.AwaitThreadFinishExtension.class)
public @interface AwaitThreadFinish {
    /**
     * An extension that waits for threads to finish, and then continues
     */
    class AwaitThreadFinishExtension implements AfterEachCallback, BeforeEachCallback {
        @Override
        public void afterEach(ExtensionContext context) throws Exception {
            final int threadCount = context.getStore(ExtensionContext.Namespace.create(AwaitThreadFinish.class))
                .get(Thread.class, Integer.class);
            // Wait for everything to finish
            final AtomicBoolean workerDone = new AtomicBoolean();
            MainApplication.worker.submit(() -> workerDone.set(true));
            Awaitility.await().atMost(Durations.FIVE_SECONDS).until(workerDone::get);

            ForkJoinPool.commonPool().awaitQuiescence(5, TimeUnit.SECONDS);
            MapillaryUtils.forkJoinPoolsAwaitQuiescence(5, TimeUnit.SECONDS);

            // Awaitility.await().atMost(5, TimeUnit.SECONDS).until(() -> Thread.activeCount() < threadCount);
        }

        @Override
        public void beforeEach(ExtensionContext context) throws Exception {
            context.getStore(ExtensionContext.Namespace.create(AwaitThreadFinish.class)).put(Thread.class,
                Thread.activeCount());
        }
    }
}
