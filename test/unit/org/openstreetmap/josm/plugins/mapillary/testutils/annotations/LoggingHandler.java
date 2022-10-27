// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.testutils.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.LogRecord;

import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolutionException;
import org.junit.jupiter.api.extension.ParameterResolver;
import org.openstreetmap.josm.tools.Logging;

/**
 *
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.METHOD })
@ExtendWith(LoggingHandler.LoggingHandlerImplementation.class)
public @interface LoggingHandler {
    class LoggingHandlerImplementation implements BeforeEachCallback, AfterEachCallback, ParameterResolver {

        @Override
        public void afterEach(ExtensionContext context) {
            ExtensionContext.Store store = context.getStore(ExtensionContext.Namespace.create(LoggingHandler.class));
            Logging.getLogger().removeHandler(store.get(TestHandler.class, TestHandler.class));
            Handler[] handlers = store.get(Logging.class, Handler[].class);
            for (Handler handler : handlers) {
                Logging.getLogger().addHandler(handler);
            }
        }

        @Override
        public void beforeEach(ExtensionContext context) {
            ExtensionContext.Store store = context.getStore(ExtensionContext.Namespace.create(LoggingHandler.class));
            store.put(Logging.class, Logging.getLogger().getHandlers());
            for (Handler handler : Logging.getLogger().getHandlers()) {
                Logging.getLogger().removeHandler(handler);
            }
            TestHandler testHandler = new TestHandler();
            store.put(TestHandler.class, testHandler);
            Logging.getLogger().addHandler(testHandler);
        }

        @Override
        public boolean supportsParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
            throws ParameterResolutionException {
            return TestHandler.class.isAssignableFrom(parameterContext.getParameter().getType());
        }

        @Override
        public TestHandler resolveParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
            throws ParameterResolutionException {
            return extensionContext.getStore(ExtensionContext.Namespace.create(LoggingHandler.class))
                .get(TestHandler.class, TestHandler.class);
        }
    }

    class TestHandler extends Handler {
        private final List<LogRecord> records = new ArrayList<>();

        /**
         * Get the records saved by this handler
         *
         * @return The saved records
         */
        public List<LogRecord> getRecords() {
            return Collections.unmodifiableList(this.records);
        }

        /**
         * Clear stored records
         */
        public void clearRecords() {
            this.records.clear();
        }

        @Override
        public void publish(LogRecord record) {
            this.records.add(record);
        }

        @Override
        public void flush() {
            // Do nothing
        }

        @Override
        public void close() throws SecurityException {
            // Do nothing
        }
    }
}
