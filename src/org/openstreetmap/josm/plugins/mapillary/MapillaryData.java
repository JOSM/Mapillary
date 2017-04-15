// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoPanel;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;

/**
 * Database class for all the {@link MapillaryAbstractImage} objects.
 *
 * @author nokutu
 * @see MapillaryAbstractImage
 * @see MapillarySequence
 */
public class MapillaryData {
  private final Set<MapillaryAbstractImage> images = ConcurrentHashMap.<MapillaryAbstractImage>newKeySet();
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
  private final Set<MapillaryAbstractImage> multiSelectedImages = ConcurrentHashMap.<MapillaryAbstractImage>newKeySet();
  /**
   * Listeners of the class.
   */
  private final List<MapillaryDataListener> listeners = new CopyOnWriteArrayList<>();
  /**
   * The bounds of the areas for which the pictures have been downloaded.
   */
  private final List<Bounds> bounds;

  /**
   * Creates a new object and adds the initial set of listeners.
   */
  protected MapillaryData() {
    this.selectedImage = null;
    this.bounds = new CopyOnWriteArrayList<>();

    // Adds the basic set of listeners.
    addListener(MapillaryPlugin.getWalkAction());
    addListener(MapillaryPlugin.getZoomAction());
    addListener(MapillaryPlugin.getUploadAction());
    if (Main.main != null) {
      addListener(MapillaryMainDialog.getInstance());
      addListener(ImageInfoPanel.getInstance());
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
   * @param image  The image to be added.
   * @param update Whether the map must be updated or not.
   * @throws NullPointerException if parameter <code>image</code> is <code>null</code>
   */
  public void add(MapillaryAbstractImage image, boolean update) {
    synchronized (images) {
      this.images.add(image);
    }
    if (update) {
      dataUpdated();
    }
    fireImagesAdded();
  }

  /**
   * Adds a set of MapillaryImages to the object, and then repaints mapView.
   *
   * @param images The set of images to be added.
   */
  public void addAll(Collection<MapillaryAbstractImage> images) {
    addAll(images, true);
  }

  /**
   * Adds a set of {link MapillaryAbstractImage} objects to this object.
   *
   * @param images The set of images to be added.
   * @param update Whether the map must be updated or not.
   */
  public void addAll(Collection<MapillaryAbstractImage> images, boolean update) {
    synchronized (this.images) {
      this.images.addAll(images);
    }
    if (update) {
      dataUpdated();
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
    if (Main.main != null)
      Main.map.mapView.repaint();
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
    Main.map.mapView.repaint();
  }

  public List<Bounds> getBounds() {
    return bounds;
  }

  /**
   * Removes an image from the database. From the {@link List} in this object
   * and from its {@link MapillarySequence}.
   *
   * @param image The {@link MapillaryAbstractImage} that is going to be deleted.
   */
  public void remove(MapillaryAbstractImage image) {
    synchronized (images) {
      this.images.remove(image);
    }
    if (getMultiSelectedImages().contains(image)) {
      setSelectedImage(null);
    }
    if (image.getSequence() != null) {
      image.getSequence().remove(image);
    }
    dataUpdated();
  }

  /**
   * Removes a set of images from the database.
   *
   * @param images A {@link Collection} of {@link MapillaryAbstractImage} objects that are
   *               going to be removed.
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
   * Repaints mapView object.
   */
  public static void dataUpdated() {
    final MapView mv = MapillaryPlugin.getMapView();
    if (mv != null) {
      mv.repaint();
    }
  }

  /**
   * Returns a Set containing all images.
   *
   * @return A Set object containing all images.
   */
  public Set<MapillaryAbstractImage> getImages() {
    synchronized (images) {
      return this.images;
    }
  }

  /**
   * Returns a Set of all sequences, that the images are part of.
   * @return all sequences that are contained in the Mapillary data
   */
  public Set<MapillarySequence> getSequences() {
    synchronized (images) {
      return images.stream().map(MapillaryAbstractImage::getSequence).collect(Collectors.toSet());
    }
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
   * If the selected MapillaryImage is part of a MapillarySequence then the
   * following visible MapillaryImage is selected. In case there is none, does
   * nothing.
   *
   * @throws IllegalStateException if the selected image is null or the selected image doesn't
   *                               belong to a sequence.
   */
  public void selectNext() {
    selectNext(MapillaryProperties.MOVE_TO_IMG.get());
  }

  /**
   * If the selected MapillaryImage is part of a MapillarySequence then the
   * following visible MapillaryImage is selected. In case there is none, does
   * nothing.
   *
   * @param moveToPicture True if the view must me moved to the next picture.
   * @throws IllegalStateException if the selected image is null or the selected image doesn't
   *                               belong to a sequence.
   */
  public void selectNext(boolean moveToPicture) {
    if (getSelectedImage() == null)
      throw new IllegalStateException();
    if (getSelectedImage().getSequence() == null)
      throw new IllegalStateException();
    MapillaryAbstractImage tempImage = this.selectedImage;
    while (tempImage.next() != null) {
      tempImage = tempImage.next();
      if (tempImage.isVisible()) {
        setSelectedImage(tempImage, moveToPicture);
        break;
      }
    }
  }

  /**
   * If the selected MapillaryImage is part of a MapillarySequence then the
   * previous visible MapillaryImage is selected. In case there is none, does
   * nothing.
   *
   * @throws IllegalStateException if the selected image is null or the selected image doesn't
   *                               belong to a sequence.
   */
  public void selectPrevious() {
    selectPrevious(MapillaryProperties.MOVE_TO_IMG.get());
  }

  /**
   * If the selected MapillaryImage is part of a MapillarySequence then the
   * previous visible MapillaryImage is selected. In case there is none, does
   * nothing. * @throws IllegalStateException if the selected image is null or
   * the selected image doesn't belong to a sequence.
   *
   * @param moveToPicture True if the view must me moved to the previous picture.
   * @throws IllegalStateException if the selected image is null or the selected image doesn't
   *                               belong to a sequence.
   */
  public void selectPrevious(boolean moveToPicture) {
    if (getSelectedImage() == null)
      throw new IllegalStateException();
    if (getSelectedImage().getSequence() == null)
      throw new IllegalStateException();
    MapillaryAbstractImage tempImage = this.selectedImage;
    while (tempImage.previous() != null) {
      tempImage = tempImage.previous();
      if (tempImage.isVisible()) {
        setSelectedImage(tempImage, moveToPicture);
        break;
      }
    }
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
   * @param zoom  True if the view must be centered on the image; false otherwise.
   */
  public void setSelectedImage(MapillaryAbstractImage image, boolean zoom) {
    MapillaryAbstractImage oldImage = this.selectedImage;
    this.selectedImage = image;
    this.multiSelectedImages.clear();
    if (image != null) {
      this.multiSelectedImages.add(image);
    }
    if (image != null && Main.main != null && image instanceof MapillaryImage) {
      MapillaryImage mapillaryImage = (MapillaryImage) image;
      // Downloading thumbnails of surrounding pictures.
      if (mapillaryImage.next() != null) {
        CacheUtils.downloadPicture((MapillaryImage) mapillaryImage.next());
        if (mapillaryImage.next().next() != null)
          CacheUtils.downloadPicture((MapillaryImage) mapillaryImage.next().next());
      }
      if (mapillaryImage.previous() != null) {
        CacheUtils.downloadPicture((MapillaryImage) mapillaryImage.previous());
        if (mapillaryImage.previous().previous() != null)
          CacheUtils.downloadPicture((MapillaryImage) mapillaryImage.previous().previous());
      }
    }
    if (zoom && Main.main != null)
      Main.map.mapView.zoomTo(getSelectedImage().getMovingLatLon());
    if (Main.main != null)
      Main.map.mapView.repaint();
    fireSelectedImageChanged(oldImage, this.selectedImage);
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
   * @param images the new image list (previously set images are completely replaced)
   */
  public void setImages(Collection<MapillaryAbstractImage> images) {
    synchronized (this.images) {
      this.images.clear();
      this.images.addAll(images);
    }
  }

  /**
   * Returns the amount of images contained by this object.
   *
   * @return The amount of images in stored.
   */
  public int size() {
    synchronized (images) {
      return this.images.size();
    }
  }
}
