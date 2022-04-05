package org.openstreetmap.josm.plugins.mapillary.utils;

import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.WeakHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;
import java.util.function.Supplier;

import javax.annotation.Nonnull;

import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IRelation;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.osm.OsmData;
import org.openstreetmap.josm.data.osm.QuadBucketPrimitiveStore;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorDataStore;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.data.vector.VectorRelation;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.ReflectionUtils;

/**
 * Helper utilities for {@link VectorDataSet}
 *
 * @author Taylor Smock
 */
public class VectorDataSetUtils {
    /**
     * This is a ReentrantReadWriteLock for datasets. This does not guarantee read/write stability, but it alleviates
     * CME issues.
     */
    private static class BadReentrantReadWriteLock extends ReentrantReadWriteLock {
        private static final Map<VectorDataSet, BadReentrantReadWriteLock> lockMap = Collections
            .synchronizedMap(new WeakHashMap<>());
        private final transient WeakReference<VectorDataSet> vectorDataSet;

        BadReentrantReadWriteLock(final VectorDataSet vectorDataSet) {
            this.vectorDataSet = new WeakReference<>(vectorDataSet);
        }

        @Override
        @Nonnull
        public ReadLock readLock() {
            final VectorDataSet vds = this.vectorDataSet.get();
            final Lock readLock = vds != null ? vds.getReadLock() : null;
            return readLock instanceof ReadLock ? (ReadLock) readLock : super.readLock();
        }

        static BadReentrantReadWriteLock get(final VectorDataSet vectorDataSet) {
            return lockMap.computeIfAbsent(vectorDataSet, BadReentrantReadWriteLock::new);
        }
    }

    /**
     * Get the vector data set lock.
     * Note: This method will need to change if JOSM changes the field name.
     *
     * @return The lock to use to avoid CME.
     */
    private static ReentrantReadWriteLock getVectorDataSetLock(final VectorDataSet vectorDataSet) {
        try {
            Field readWriteLock = VectorDataSet.class.getDeclaredField("readWriteLock");
            readWriteLock.setAccessible(true);
            final Object object = readWriteLock.get(vectorDataSet);
            if (object instanceof ReentrantReadWriteLock) {
                return (ReentrantReadWriteLock) object;
            }
        } catch (ReflectiveOperationException | SecurityException e) {
            Logging.error(e);
        }
        return BadReentrantReadWriteLock.get(vectorDataSet);
    }

    /**
     * Try to read something (here to avoid boilerplate)
     *
     * @param dataSet The dataset to read from
     * @param supplier The reading function
     * @param <T> The return type
     * @return The optional return
     */
    public static <T> Optional<T> tryRead(final OsmData<?, ?, ?, ?> dataSet, final Supplier<T> supplier) {
        if (dataSet != null) {
            final Lock lock = dataSet.getReadLock();
            try {
                lock.lockInterruptibly();
                return Optional.ofNullable(supplier.get());
            } catch (InterruptedException e) {
                Logging.error(e);
                Thread.currentThread().interrupt();
            } finally {
                lock.unlock();
            }
        }
        return Optional.empty();
    }

    /**
     * Try to write something (here to avoid boilerplate)
     *
     * @param vectorDataSet The dataset to lock
     * @param runnable The writing function
     */
    public static void tryWrite(final VectorDataSet vectorDataSet, Runnable runnable) {
        final ReentrantReadWriteLock lock = getVectorDataSetLock(vectorDataSet);
        try {
            lock.writeLock().lockInterruptibly();
            runnable.run();
        } catch (InterruptedException exception) {
            Logging.error(exception);
            Thread.currentThread().interrupt();
        } finally {
            if (lock.isWriteLockedByCurrentThread()) {
                lock.writeLock().unlock();
            }
        }
    }

    /**
     * Remove a vector object safely
     *
     * @param object The object to remove
     * @param <O> The type of object to remove
     */
    public synchronized static <O extends VectorPrimitive> void removeObject(final O object) {
        if (object.getDataSet() != null) {
            final VectorDataSet vectorDataSet = object.getDataSet();
            try {
                final Field customDataStoreField = VectorDataSet.class.getDeclaredField("customDataStore");
                ReflectionUtils.setObjectsAccessible(customDataStoreField);
                final VectorDataStore customDataStore = (VectorDataStore) customDataStoreField.get(vectorDataSet);
                tryWrite(vectorDataSet, () -> {
                    if (customDataStore != null && object.getDataSet() != null) {
                        try {
                            object.getDataSet().removePrimitive(object);
                        } catch (JosmRuntimeException josmRuntimeException) {
                            if (!josmRuntimeException.getMessage().startsWith("failed to remove primitive: ")
                                || Logging.isDebugEnabled()) {
                                throw josmRuntimeException;
                            }
                        }
                    }
                });
            } catch (ReflectiveOperationException reflectiveOperationException) {
                Logging.error(reflectiveOperationException);
                throw new JosmRuntimeException(reflectiveOperationException);
            }
        }
    }

    public static synchronized <P extends IPrimitive> void refreshQuadbucketIndex(final P object) {
        Objects.requireNonNull(object);
        Objects.requireNonNull(object.getDataSet());
        Object ds = object.getDataSet();
        try {
            final Field f;
            if (ds instanceof DataSet) {
                f = ds.getClass().getDeclaredField("store");
                ReflectionUtils.setObjectsAccessible(f);
            } else if (ds instanceof VectorDataSet) {
                final Field c = ds.getClass().getDeclaredField("customDataStore");
                ReflectionUtils.setObjectsAccessible(c);
                final VectorDataStore dsStore = (VectorDataStore) c.get(ds);
                f = dsStore.getClass().getSuperclass().getDeclaredField("store");
                ReflectionUtils.setObjectsAccessible(f);
                QuadBucketPrimitiveStore<VectorNode, VectorWay, VectorRelation> qb = (QuadBucketPrimitiveStore<VectorNode, VectorWay, VectorRelation>) f
                    .get(dsStore);
                // Account for cases where the primitive isn't "custom" yet.
                if ((object instanceof VectorNode && !qb.containsNode((VectorNode) object))
                    || (object instanceof VectorWay && !qb.containsWay((VectorWay) object))
                    || (object instanceof VectorRelation) && !qb.containsRelation((VectorRelation) object)) {
                    final VectorDataSet vds = (VectorDataSet) ds;
                    tryWrite((VectorDataSet) ds, () -> {
                        removeObject((VectorPrimitive) object);
                        vds.addPrimitive((VectorPrimitive) object);
                    });
                    return;
                }
                ds = dsStore;
            } else {
                throw new JosmRuntimeException("Unknown dataset type: " + ds.getClass().getName());
            }
            final QuadBucketPrimitiveStore<? extends P, ? extends P, ? extends P> store = (QuadBucketPrimitiveStore<? extends P, ? extends P, ? extends P>) f
                .get(ds);

            if (object instanceof INode) {
                final Method m = QuadBucketPrimitiveStore.class.getDeclaredMethod("reindexNode", INode.class,
                    Consumer.class, Consumer.class, Consumer.class);
                ReflectionUtils.setObjectsAccessible(m);
                m.invoke(store, (INode) object, (Consumer<INode>) node -> {
                    /* Do nothing */}, (Consumer<IWay<?>>) way -> {
                        /* Do nothing */}, (Consumer<IRelation<?>>) rel -> {
                            /* Do nothing */});
            } else if (object instanceof IWay) {
                final Method m = QuadBucketPrimitiveStore.class.getDeclaredMethod("reindexWay", IWay.class,
                    Consumer.class, Consumer.class);
                ReflectionUtils.setObjectsAccessible(m);
                m.invoke(store, (IWay) object, (Consumer<IWay<?>>) way -> {
                    /* Do nothing */}, (Consumer<IRelation<?>>) rel -> {
                        /* Do nothing */});
            } else if (object instanceof IRelation) {
                final Method m = QuadBucketPrimitiveStore.class.getDeclaredMethod("reindexRelation", IRelation.class,
                    Consumer.class);
                ReflectionUtils.setObjectsAccessible(m);
                m.invoke(store, (IRelation) object, (Consumer<IRelation<?>>) rel -> {
                    /* Do nothing */});
            }
        } catch (ReflectiveOperationException e) {
            throw new JosmRuntimeException(e);
        }
    }

    private VectorDataSetUtils() {
        // Hide constructor
    }
}
