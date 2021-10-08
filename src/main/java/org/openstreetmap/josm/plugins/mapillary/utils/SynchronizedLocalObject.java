package org.openstreetmap.josm.plugins.mapillary.utils;

import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Synchronize on a local object
 *
 * @param <T> the object to synchronize on
 */
public class SynchronizedLocalObject<T> {
    final T object;

    public SynchronizedLocalObject(T object) {
        this.object = object;
    }

    /**
     * Execute a function
     *
     * @param function The function to execute
     * @param <R> The return type
     * @return The return value
     */
    public <R> R execute(Function<T, R> function) {
        synchronized (this.object) {
            return function.apply(this.object);
        }
    }

    /**
     * Execute a consumer
     *
     * @param consumer The consumer to execute
     */
    public void execute(Consumer<T> consumer) {
        synchronized (this.object) {
            consumer.accept(this.object);
        }
    }
}
