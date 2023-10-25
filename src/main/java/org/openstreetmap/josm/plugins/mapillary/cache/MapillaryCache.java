// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.cache;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.stream.Stream;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.HostLimitQueue;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.cache.JCSCachedTileLoaderJob;
import org.openstreetmap.josm.data.imagery.TileJobOptions;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Utils;

/**
 * Stores the downloaded pictures locally.
 *
 * @author nokutu
 */
public class MapillaryCache extends JCSCachedTileLoaderJob<String, BufferedImageCacheEntry> {
    private final URL url;
    private final String key;
    private final Type type;

    /**
     * Types of images.
     *
     * @author nokutu
     */
    public enum Type {
        /** Original image */
        ORIGINAL(MapillaryImageUtils.ImageProperties.THUMB_ORIGINAL_URL, 1),
        /** 2048px image */
        THUMB_2048(MapillaryImageUtils.ImageProperties.THUMB_2048_URL, 1),
        /** 1024px image */
        THUMB_1024(MapillaryImageUtils.ImageProperties.THUMB_1024_URL, Math.max(THREAD_LIMIT.get() / 4, 2)),
        /** 256px image */
        THUMB_256(MapillaryImageUtils.ImageProperties.THUMB_256_URL, Math.max(THREAD_LIMIT.get(), 4));

        private final int width;
        private final String imageUrl;
        private final int maxSize;
        private final ThreadPoolExecutor executor;

        Type(final MapillaryImageUtils.ImageProperties properties, final int downloadThreads) {
            this.imageUrl = properties.name().toLowerCase(Locale.ROOT);
            final Matcher matcher = MapillaryImageUtils.BASE_IMAGE_KEY.matcher(this.imageUrl);
            if (matcher.matches() && "original".equals(matcher.group(1))) {
                this.width = Integer.MIN_VALUE;
                this.maxSize = Integer.MIN_VALUE;
            } else if (matcher.matches()) {
                this.width = Integer.parseInt(matcher.group(1));
                this.maxSize = this.width * this.width * 4;
            } else {
                throw new IllegalArgumentException("Mapillary: " + this.imageUrl + " is not a valid image type");
            }
            this.executor = new ThreadPoolExecutor(downloadThreads, Math.max(downloadThreads, THREAD_LIMIT.get()),
                // keep alive for thread
                5, TimeUnit.MINUTES, new HostLimitQueue(2 * downloadThreads, downloadThreads * 10),
                Utils.newThreadFactory("Mapillary-image-downloader-" + this + "-%d", Thread.NORM_PRIORITY));
        }

        /**
         * Get the anticipated width for the image
         *
         * @param image The image to use for the "original" width
         * @return The width of the image (pixels).
         */
        public int getWidth(@Nullable final INode image) {
            if (this.width == Integer.MIN_VALUE && image != null
                && image.get(MapillaryImageUtils.ImageProperties.WIDTH.toString()) != null) {
                return Integer.parseInt(image.get(MapillaryImageUtils.ImageProperties.WIDTH.toString()));
            }
            return this.width;
        }

        /**
         * Get the anticipated height for the image
         *
         * @param image The image to use for the "original" height
         * @return The height of the image (pixels). If {@link Integer#MIN_VALUE},
         *         the original height of the image should be used.
         */
        public int getHeight(@Nullable final INode image) {
            if (this.width == Integer.MIN_VALUE && image != null
                && image.get(MapillaryImageUtils.ImageProperties.HEIGHT.toString()) != null) {
                return Integer.parseInt(image.get(MapillaryImageUtils.ImageProperties.HEIGHT.toString()));
            }
            return width;
        }

        /**
         * Get the key for this image type
         *
         * @return The key to look for in the image node
         */
        public String getKey() {
            return this.imageUrl;
        }

        /**
         * Get the anticipated maximum size given an image
         *
         * @param image The image to use for original size information
         * @return The best image type
         */
        public int getMaxSize(@Nullable final INode image) {
            if (this == Type.ORIGINAL) {
                return this.getHeight(image) * this.getWidth(image) * 4;
            }
            return this.maxSize;
        }

        /**
         * Get the best type for the memory available (we currently load the whole thing in memory)
         *
         * @param image The image to check
         * @return The best image size for the memory available
         */
        public static Type getTypeForMemory(@Nullable final INode image) {
            // We prefetch images forward/backward, and we should include the memory for the selected image.
            final long otherImages = MapillaryProperties.PRE_FETCH_IMAGE_COUNT.get() * 2 + 1L;
            final long memory = Runtime.getRuntime().freeMemory();
            return Stream.of(Type.values()).filter(type -> otherImages * type.getMaxSize(image) < memory).findFirst()
                .orElse(Type.THUMB_256);
        }

        /**
         * Get the executor for getting images of this type
         *
         * @return The executor to download images with
         */
        public ThreadPoolExecutor getDefaultJobExecutor() {
            return executor;
        }
    }

    /**
     * Cache images. The caching function is run in a separate thread.
     *
     * @param currentImage The image to cache around
     */
    public static void cacheSurroundingImages(INode currentImage) {
        MapillaryUtils.getForkJoinPool(MapillaryCache.class)
            .execute(() -> runnableCacheSurroundingImages(currentImage));
    }

    private static void runnableCacheSurroundingImages(INode currentImage) {
        final ForkJoinPool pool = MapillaryUtils.getForkJoinPool(MapillaryCache.class);
        final Integer prefetch = MapillaryProperties.PRE_FETCH_IMAGE_COUNT.get();
        final int realPrefetch;
        if (prefetch == null) {
            realPrefetch = MapillaryProperties.PRE_FETCH_IMAGE_COUNT.getDefaultValue();
        } else {
            realPrefetch = prefetch > 0 ? prefetch : 0;
        }
        final IWay<?> iWay = MapillaryImageUtils.getSequence(currentImage);
        // We prefetch both ways
        final Type type = Type.THUMB_256;
        if (iWay != null) {
            // Avoid CME -- getImage may remove nodes from the way
            final List<? extends INode> wayNodes = new ArrayList<>(iWay.getNodes());
            final int index = wayNodes.indexOf(currentImage);
            final List<? extends INode> nodes = wayNodes.subList(Math.max(0, index - realPrefetch),
                Math.min(wayNodes.size(), index + realPrefetch));
            nodes.stream().filter(node -> node.getUniqueId() > 0)
                .forEach(image -> pool.execute(() -> CacheUtils.downloadPicture(image, type)));
        }
    }

    /**
     * Main constructor.
     *
     * @param image
     *        The image.
     * @param type The image size
     */
    public MapillaryCache(@Nonnull final INode image, @Nonnull final Type type) {
        super(Caches.FULL_IMAGE_CACHE.getICacheAccess(),
            new TileJobOptions(50_000, 50_000, new HashMap<>(), TimeUnit.HOURS.toSeconds(4)),
            type.getDefaultJobExecutor());
        Objects.requireNonNull(image);
        Objects.requireNonNull(type);
        this.type = type;
        try {
            if (image.hasKey(type.getKey())) {
                this.key = Long.toString(image.getUniqueId()) + '.' + type.width;
                this.url = new URL(image.get(type.getKey()));
            } else {
                // Iterate through the keys, maybe there is another image url?
                String tKey = null;
                URL tUrl = null;
                for (Map.Entry<String, String> entry : image.getKeys().entrySet()) {
                    if (MapillaryImageUtils.BASE_IMAGE_KEY.matcher(entry.getKey()).matches()) {
                        String width = MapillaryImageUtils.BASE_IMAGE_KEY.matcher(entry.getKey()).group(1);
                        tKey = MapillaryImageUtils.getKey(image) + '.' + width;
                        tUrl = new URL(entry.getValue());
                        break;
                    }
                }
                this.key = tKey;
                this.url = tUrl;
            }
        } catch (MalformedURLException e) {
            Logging.error(e);
            throw new JosmRuntimeException(e);
        }
    }

    @Override
    public String getCacheKey() {
        return this.key;
    }

    @Override
    public URL getUrl() {
        return this.url;
    }

    @Override
    protected BufferedImageCacheEntry createCacheEntry(byte[] content) {
        return new BufferedImageCacheEntry(content);
    }

    /**
     * Submit a new task
     *
     * @param listener The listener to notify
     * @param force {@code true} if the load should skip all caches
     * @param removeCurrent {@code true} if any outstanding tasks should be canceled
     * @throws IOException If something happens during fetch ore read
     */
    public void submit(ICachedLoaderListener listener, boolean force, boolean removeCurrent) throws IOException {
        // Clear the queue for larger images
        if (removeCurrent && (this.type == Type.ORIGINAL || this.type == Type.THUMB_2048)) {
            this.cancelOutstandingTasks();
        }
        super.submit(listener, force);
    }

    @Override
    public void submit(ICachedLoaderListener listener, boolean force) throws IOException {
        this.submit(listener, force, true);
    }

    @Override
    protected boolean isObjectLoadable() {
        if (this.cacheData == null) {
            return false;
        }
        byte[] content = this.cacheData.getContent();
        return content != null && content.length > 0;
    }
}
