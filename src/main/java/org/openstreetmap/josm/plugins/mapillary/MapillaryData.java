// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.json.JsonException;
import javax.swing.SwingUtilities;

import org.apache.commons.jcs3.access.CacheAccess;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.Data;
import org.openstreetmap.josm.data.DataSource;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryChangesetDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoPanel;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.DetectionsDownloadRunnable;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.Logging;

/**
 * Database class for all the {@link MapillaryAbstractImage} objects.
 *
 * @author nokutu
 * @see MapillaryAbstractImage
 * @see MapillarySequence
 */
public class MapillaryData implements Data {
  private final Set<MapillaryAbstractImage> images = ConcurrentHashMap.newKeySet();
  /**
   * The image currently selected, this is the one being shown.
   */
  private MapillaryAbstractImage selectedImage;
  /**
   * The image under the cursor.
   */
  private MapillaryAbstractImage highlightedImage;
  /**
   * All the images selected, can be more than one.
   */
  private final Set<MapillaryAbstractImage> multiSelectedImages = ConcurrentHashMap.newKeySet();
  /**
   * Listeners of the class.
   */
  private final List<MapillaryDataListener> listeners = new CopyOnWriteArrayList<>();
  /**
   * The bounds of the areas for which the pictures have been downloaded.
   */
  private final List<DataSource> dataSources;

  /**
   * Keeps track of images where detections have been fully downloaded
   */
  private final Set<MapillaryAbstractImage> fullyDownloadedDetections = ConcurrentHashMap.newKeySet();

  /** Keeps track of highlighted images */
  private final Set<MapillaryAbstractImage> highlightedImages = ConcurrentHashMap.newKeySet();

  /**
   * Creates a new object and adds the initial set of listeners.
   */
  public MapillaryData() {
    this.selectedImage = null;
    this.dataSources = new ArrayList<>();

    // Adds the basic set of listeners.
    Arrays.stream(MapillaryPlugin.getMapillaryDataListeners()).forEach(this::addListener);
    if (MainApplication.getMainFrame() != null) {
      addListener(MapillaryMainDialog.getInstance());
      addListener(ImageInfoPanel.getInstance());
      addListener(MapillaryChangesetDialog.getInstance());
    }
  }

  /**
   * Adds an MapillaryImage to the object, and then repaints mapView.
   *
   * @param image The image to be added.
   */
  public void add(MapillaryAbstractImage image) {
    add(image, true);
  }

  /**
   * Adds a MapillaryImage to the object, but doesn't repaint mapView. This is
   * needed for concurrency.
   *
   * @param image The image to be added.
   * @param update Whether the map must be updated or not.
   * @throws NullPointerException if parameter <code>image</code> is <code>null</code>
   */
  public void add(MapillaryAbstractImage image, boolean update) {
    images.add(image);
    if (update) {
      MapillaryLayer.invalidateInstance();
    }
    fireImagesAdded();
  }

  /**
   * Adds a set of MapillaryImages to the object, and then repaints mapView.
   *
   * @param images The set of images to be added.
   */
  public void addAll(Collection<? extends MapillaryAbstractImage> images) {
    addAll(images, true);
  }

  /**
   * Adds a set of {link MapillaryAbstractImage} objects to this object.
   *
   * @param newImages The set of images to be added.
   * @param update Whether the map must be updated or not.
   */
  public void addAll(Collection<? extends MapillaryAbstractImage> newImages, boolean update) {
    images.addAll(newImages);
    if (update) {
      MapillaryLayer.invalidateInstance();
    }
    fireImagesAdded();
  }

  /**
   * Adds a new listener.
   *
   * @param lis Listener to be added.
   */
  public final void addListener(final MapillaryDataListener lis) {
    listeners.add(lis);
  }

  /**
   * Adds a {@link MapillaryImage} object to the list of selected images, (when
   * ctrl + click)
   *
   * @param image The {@link MapillaryImage} object to be added.
   */
  public void addMultiSelectedImage(final MapillaryAbstractImage image) {
    if (!this.multiSelectedImages.contains(image)) {
      if (this.getSelectedImage() == null) {
        this.setSelectedImage(image);
      } else {
        this.multiSelectedImages.add(image);
      }
    }
    MapillaryLayer.invalidateInstance();
  }

  /**
   * Adds a set of {@code MapillaryAbstractImage} objects to the list of
   * selected images.
   *
   * @param images A {@link Collection} object containing the set of images to be added.
   */
  public void addMultiSelectedImage(Collection<MapillaryAbstractImage> images) {
    images.stream().filter(image -> !this.multiSelectedImages.contains(image)).forEach(image -> {
      if (this.getSelectedImage() == null) {
        this.setSelectedImage(image);
      } else {
        this.multiSelectedImages.add(image);
      }
    });
    MapillaryLayer.invalidateInstance();
  }

  public List<Bounds> getBounds() {
    return getDataSourceBounds();
  }

  public void addDataSource(DataSource source) {
    dataSources.add(source);
  }

  public void removeDataSource(DataSource source) {
    dataSources.remove(source);
  }

  /**
   * Removes an image from the database. From the {@link List} in this object
   * and from its {@link MapillarySequence}.
   *
   * @param image The {@link MapillaryAbstractImage} that is going to be deleted.
   */
  public void remove(MapillaryAbstractImage image) {
    images.remove(image);
    if (getMultiSelectedImages().contains(image)) {
      setSelectedImage(null);
    }
    if (image.getSequence() != null) {
      image.getSequence().remove(image);
    }
    MapillaryLayer.invalidateInstance();
  }

  /**
   * Removes a set of images from the database.
   *
   * @param images A {@link Collection} of {@link MapillaryAbstractImage} objects that are
   *        going to be removed.
   */
  public void remove(Collection<MapillaryAbstractImage> images) {
    images.forEach(this::remove);
  }

  /**
   * Removes a listener.
   *
   * @param lis Listener to be removed.
   */
  public void removeListener(MapillaryDataListener lis) {
    this.listeners.remove(lis);
  }

  /**
   * Highlights the image under the cursor.
   *
   * @param image The image under the cursor.
   */
  public void setHighlightedImage(MapillaryAbstractImage image) {
    this.highlightedImage = image;
  }

  /**
   * Returns the image under the mouse cursor.
   *
   * @return The image under the mouse cursor.
   */
  public MapillaryAbstractImage getHighlightedImage() {
    return this.highlightedImage;
  }

  /**
   * @return The highlighted images (not including the image under the mouse cursor).
   */
  public Collection<MapillaryAbstractImage> getHighlightedImages() {
    return Collections.unmodifiableCollection(highlightedImages);
  }

  /**
   * Set the highlighted images
   *
   * @param images The images to highlight (not including the one hovered over by the mouse)
   */
  public void setHighlightedImages(Collection<MapillaryAbstractImage> images) {
    highlightedImages.clear();
    highlightedImages.addAll(images);
  }

  /**
   * Returns a Set containing all images.
   *
   * @return A Set object containing all images.
   */
  public synchronized Set<MapillaryAbstractImage> getImages() {
    return images;
  }

  /**
   * Get a specific MapillaryImage
   *
   * @param key The key for the MapillaryImage
   * @return The MapillaryImage or {@code null}
   */
  public MapillaryImage getImage(String key) {
    return getImages().parallelStream().filter(MapillaryImage.class::isInstance).map(MapillaryImage.class::cast)
      .filter(m -> m.getKey().equals(key)).findAny().orElse(null);
  }

  /**
   * Returns a Set of all sequences, that the images are part of.
   *
   * @return all sequences that are contained in the Mapillary data
   */
  public Set<MapillarySequence> getSequences() {
    return images.stream().map(MapillaryAbstractImage::getSequence).collect(Collectors.toSet());
  }

  /**
   * Returns the MapillaryImage object that is currently selected.
   *
   * @return The selected MapillaryImage object.
   */
  public MapillaryAbstractImage getSelectedImage() {
    return this.selectedImage;
  }

  private void fireImagesAdded() {
    listeners.stream().filter(Objects::nonNull).forEach(MapillaryDataListener::imagesAdded);
  }

  /**
   * Selects a new image.If the user does ctrl + click, this isn't triggered.
   *
   * @param image The MapillaryImage which is going to be selected
   */
  public void setSelectedImage(MapillaryAbstractImage image) {
    setSelectedImage(image, false);
  }

  /**
   * Selects a new image.If the user does ctrl+click, this isn't triggered. You
   * can choose whether to center the view on the new image or not.
   *
   * @param image The {@link MapillaryImage} which is going to be selected.
   * @param zoom True if the view must be centered on the image; false otherwise.
   */
  public void setSelectedImage(MapillaryAbstractImage image, boolean zoom) {
    MapillaryAbstractImage oldImage = this.selectedImage;
    if (image instanceof MapillaryImage && inCurrentlySelectedDetection((MapillaryImage) image)) {
      getAllDetections(Collections.singleton((MapillaryImage) image));
    }
    this.selectedImage = image;
    this.multiSelectedImages.clear();
    final MapView mv = MapillaryPlugin.getMapView();
    if (image != null) {
      this.multiSelectedImages.add(image);
      if (mv != null && image instanceof MapillaryImage) {
        MapillaryImage mapillaryImage = (MapillaryImage) image;

        // Downloading thumbnails of surrounding pictures.
        downloadSurroundingImages(mapillaryImage);
      }
    }
    if (mv != null && zoom && selectedImage != null) {
      mv.zoomTo(selectedImage.getMovingLatLon());
    }
    fireSelectedImageChanged(oldImage, this.selectedImage);
    MapillaryLayer.invalidateInstance();
  }

  /**
   * Check if the image has a selected point object
   *
   * @param image The image to check
   * @return {@code true} if any point object layer has a selected object with the image key.
   */
  public static boolean inCurrentlySelectedDetection(MapillaryImage image) {
    return MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).parallelStream()
      .map(PointObjectLayer::getDataSet).flatMap(d -> d.getSelected().parallelStream())
      .filter(p -> p.hasTag("detections"))
      .flatMap(p -> PointObjectLayer.parseDetections(p.get("detections")).parallelStream())
      .anyMatch(p -> image.getKey().equals(p.getOrDefault("image_key", null)));
  }

  /**
   * Downloads surrounding images of this mapillary image in background threads
   *
   * @param mapillaryImage the image for which the surrounding images should be downloaded
   */
  private static void downloadSurroundingImages(MapillaryImage mapillaryImage) {
    MainApplication.worker.execute(() -> {
      final int prefetchCount = MapillaryProperties.PRE_FETCH_IMAGE_COUNT.get();
      CacheAccess<String, BufferedImageCacheEntry> imageCache = Caches.ImageCache.getInstance()
        .getCache(MapillaryCache.Type.FULL_IMAGE);

      MapillaryAbstractImage nextImage = mapillaryImage.next();
      MapillaryAbstractImage prevImage = mapillaryImage.previous();

      long freeMemory = Runtime.getRuntime().freeMemory();
      // 3 bytes for RGB (jpg doesn't support the Alpha channel). I'm using 4 bytes instead of 3 for a buffer.
      long estimatedImageSize = Stream.of(MapillaryCache.Type.values()).mapToLong(v -> v.getHeight() * v.getWidth() * 4)
        .sum();

      for (int i = 0; i < prefetchCount; i++) {
        if (freeMemory - estimatedImageSize < 0) {
          break; // It doesn't make sense to try to cache images that won't be kept.
        }
        if (nextImage != null) {
          if ((nextImage instanceof MapillaryImage)
            && (imageCache.get(((MapillaryImage) nextImage).getKey()) == null)) {
            CacheUtils.downloadPicture((MapillaryImage) nextImage);
          }
          nextImage = nextImage.next();
        }
        if (prevImage != null) {
          if ((prevImage instanceof MapillaryImage)
            && (imageCache.get(((MapillaryImage) prevImage).getKey()) == null)) {
            CacheUtils.downloadPicture((MapillaryImage) prevImage);
          }
          prevImage = prevImage.previous();
        }
        imageCache.getCacheControl();
        imageCache.get(mapillaryImage.getKey());
        if (mapillaryImage.next() != null) {
          imageCache.get(mapillaryImage.getKey());
        }
      }
    });
  }

  private void fireSelectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
    listeners.stream().filter(Objects::nonNull).forEach(lis -> lis.selectedImageChanged(oldImage, newImage));
  }

  /**
   * Returns a List containing all {@code MapillaryAbstractImage} objects
   * selected with ctrl + click.
   *
   * @return A List object containing all the images selected.
   */
  public Set<MapillaryAbstractImage> getMultiSelectedImages() {
    return this.multiSelectedImages;
  }

  /**
   * Sets a new {@link Collection} object as the used set of images.
   * Any images that are already present, are removed.
   *
   * @param newImages the new image list (previously set images are completely replaced)
   */
  public void setImages(Collection<MapillaryAbstractImage> newImages) {
    synchronized (this) {
      images.clear();
      images.addAll(newImages);
    }
  }

  @Override
  public Collection<DataSource> getDataSources() {
    return Collections.unmodifiableCollection(dataSources);
  }

  /**
   * Get all the detections for an image
   *
   * @param imagesToGet The images to get detections for
   */
  public void getAllDetections(Collection<MapillaryImage> imagesToGet) {
    List<MapillaryImage> list = imagesToGet.stream().filter(Objects::nonNull)
      .collect(Collectors.toCollection(ArrayList::new));
    int index = list.indexOf(getSelectedImage());
    MapillaryImage current = index >= 0 ? list.get(index) : null;
    if (current != null) {
      list.remove(current);
    }
    if (SwingUtilities.isEventDispatchThread()) {
      MainApplication.worker.execute(() -> {
        getDetections(Collections.singleton(current));
        getDetections(list);
      });
    } else {
      getDetections(Collections.singleton(current));
      getDetections(list);
    }
  }

  private void getDetections(Collection<MapillaryImage> imagesToGetDetections) {
    if (imagesToGetDetections == null || imagesToGetDetections.isEmpty()) {
      return;
    }
    synchronized (fullyDownloadedDetections) {
      Collection<MapillaryImage> imagesToGet = imagesToGetDetections.stream().filter(Objects::nonNull)
        .filter(i -> !fullyDownloadedDetections.contains(i)).collect(Collectors.toList());
      URL nextUrl = MapillaryURL.APIv3
        .retrieveDetections(imagesToGet.stream().map(MapillaryImage::getKey).collect(Collectors.toList()));
      Map<String, List<ImageDetection>> detections = new HashMap<>();
      while (nextUrl != null) {
        HttpClient client = HttpClient.create(nextUrl);
        if (MapillaryUser.getUsername() != null)
          OAuthUtils.addAuthenticationHeader(client);
        try {
          client.connect();
          detections.putAll(DetectionsDownloadRunnable.doRun(client, this));
          nextUrl = MapillaryURL.APIv3.parseNextFromLinkHeaderValue(client.getResponse().getHeaderField("Link"));
        } catch (IOException | JsonException | NumberFormatException e) {
          Logging.error(e);
          nextUrl = null; // Ensure we don't keep looping if there is an error.
        } finally {
          client.disconnect();
        }
      }
      imagesToGet.stream().filter(i -> detections.containsKey(i.getKey())).forEach(i -> {
        i.setAllDetections(detections.get(i.getKey()));
        fullyDownloadedDetections.add(i);
      });
      if (imagesToGet.contains(getSelectedImage())) {
        MapillaryMainDialog.getInstance().imageViewer
          .setAllDetections(((MapillaryImage) getSelectedImage()).getDetections());
      }
    }
  }
}
