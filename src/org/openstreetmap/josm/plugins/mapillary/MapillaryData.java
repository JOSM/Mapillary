package org.openstreetmap.josm.plugins.mapillary;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.cache.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Database class for all the MapillaryImage objects.
 *
 * @author nokutu
 * @see MapillaryAbstractImage
 * @see MapillarySequence
 *
 */
public class MapillaryData {

  /** Unique instance of the class */
  public volatile static MapillaryData INSTANCE;
  /** Enable this if you are using in Unit Tests */
  public static boolean TEST_MODE = false;

  private final List<MapillaryAbstractImage> images;
  private MapillaryAbstractImage selectedImage;
  /** The image under the cursor */
  private MapillaryAbstractImage highlightedImage;
  private final List<MapillaryAbstractImage> multiSelectedImages;

  private List<MapillaryDataListener> listeners = new CopyOnWriteArrayList<>();

  /**
   * Main constructor.
   */
  private MapillaryData() {
    images = new CopyOnWriteArrayList<>();
    multiSelectedImages = new ArrayList<>();
    selectedImage = null;

    addListener(MapillaryPlugin.walkAction);
  }

  /**
   * Returns the unique instance of the class.
   *
   * @return The unique instance of the class.
   */
  public static MapillaryData getInstance() {
    if (INSTANCE == null) {
      INSTANCE = new MapillaryData();
    }
    return INSTANCE;
  }

  /**
   * Adds a set of MapillaryImages to the object, and then repaints mapView.
   *
   * @param images
   *        The set of images to be added.
   */
  public synchronized void add(List<MapillaryAbstractImage> images) {
    for (MapillaryAbstractImage image : images) {
      add(image);
    }
  }

  /**
   * Adds an MapillaryImage to the object, and then repaints mapView.
   *
   * @param image
   *        The image to be added.
   */
  public synchronized void add(MapillaryAbstractImage image) {
    if (!images.contains(image)) {
      this.images.add(image);
    }
    dataUpdated();
    fireImagesAdded();
  }

  /**
   * Adds a new listener.
   *
   * @param lis Listener to be added.
   */
  public void addListener(MapillaryDataListener lis) {
    listeners.add(lis);
  }

  /**
   * Removes a listener.
   *
   * @param lis Listener to be removed.
   */
  public void removeListener(MapillaryDataListener lis) {
    listeners.remove(lis);
  }

  /**
   * Adds a set of MapillaryImages to the object, but doesn't repaint mapView.
   * This is needed for concurrency.
   *
   * @param images
   *        The set of images to be added.
   */
  public synchronized void addWithoutUpdate(List<MapillaryAbstractImage> images) {
    for (MapillaryAbstractImage image : images) {
      addWithoutUpdate(image);
    }
  }

  /**
   * Highlights the image under the cursor.
   *
   * @param image
   *        The image under the cursor.
   */
  public void setHighlightedImage(MapillaryAbstractImage image) {
    highlightedImage = image;
  }

  /**
   * Returns the image under the mouse cursor.
   *
   * @return The image under the mouse cursor.
   */
  public MapillaryAbstractImage getHighlighted() {
    return highlightedImage;
  }

  /**
   * Adds a MapillaryImage to the object, but doesn't repaint mapView. This is
   * needed for concurrency.
   *
   * @param image
   *        The image to be added.
   */
  public synchronized void addWithoutUpdate(MapillaryAbstractImage image) {
    if (!images.contains(image)) {
      this.images.add(image);
    }
    fireImagesAdded();
  }

  /**
   * Repaints mapView object.
   */
  public synchronized void dataUpdated() {
    if (!TEST_MODE)
      Main.map.mapView.repaint();
  }

  /**
   * Returns a List containing all images.
   *
   * @return A List object containing all images.
   */
  public List<MapillaryAbstractImage> getImages() {
    return images;
  }

  /**
   * Returns the MapillaryImage object that is currently selected.
   *
   * @return The selected MapillaryImage object.
   */
  public MapillaryAbstractImage getSelectedImage() {
    return selectedImage;
  }

  private void fireImagesAdded() {
    if (listeners.isEmpty())
      return;
    for (MapillaryDataListener lis : listeners)
      lis.imagesAdded();
  }

  /**
   * If the selected MapillaryImage is part of a MapillarySequence then the
   * following visible MapillaryImage is selected. In case there is none, does
   * nothing.
   */
  public void selectNext() {
    if (getSelectedImage() == null)
      return;
    if (getSelectedImage().getSequence() == null)
      return;
    MapillaryAbstractImage tempImage = selectedImage;
    while (tempImage.next() != null) {
      tempImage = tempImage.next();
      if (tempImage.isVisible()) {
        setSelectedImage(tempImage, Main.pref.getBoolean("mapillary.move-to-picture", true));
        break;
      }
    }
  }

  /**
   * If the selected MapillaryImage is part of a MapillarySequence then the
   * previous visible MapillaryImage is selected. In case there is none, does
   * nothing.
   */
  public void selectPrevious() {
    if (getSelectedImage() == null)
      return;
    if (getSelectedImage().getSequence() == null)
      throw new IllegalStateException();
    MapillaryAbstractImage tempImage = selectedImage;
    while (tempImage.previous() != null) {
      tempImage = tempImage.previous();
      if (tempImage.isVisible()) {
        setSelectedImage(tempImage, Main.pref.getBoolean("mapillary.move-to-picture", true));
        break;
      }
    }
  }

  /**
   * Selects a new image.If the user does ctrl + click, this isn't triggered.
   *
   * @param image
   *        The MapillaryImage which is going to be selected
   */
  public void setSelectedImage(MapillaryAbstractImage image) {
    setSelectedImage(image, false);
  }

  /**
   * Selects a new image.If the user does ctrl+click, this isn't triggered.
   * You can choose whether to center the view on the new image or not.
   *
   * @param image
   *        The {@link MapillaryImage} which is going to be selected.
   * @param zoom
   *        True if the view must be centered on the image; false otherwise.
   */
  public void setSelectedImage(MapillaryAbstractImage image, boolean zoom) {
    if (image != null) {
      System.out.println("----------------------------");
      for (MapillaryAbstractImage img : getImages())
        System.out.println(img.getSequence());
    }
    MapillaryAbstractImage oldImage = selectedImage;
    selectedImage = image;
    multiSelectedImages.clear();
    multiSelectedImages.add(image);
    if (image != null) {
      if (image instanceof MapillaryImage) {
        MapillaryImage mapillaryImage = (MapillaryImage) image;
        // Downloading thumbnails of surrounding pictures.
        if (mapillaryImage.next() != null) {
          Utils.downloadPicture(mapillaryImage.next());
          if (mapillaryImage.next().next() != null)
            Utils.downloadPicture(mapillaryImage.next().next());
        }
        if (mapillaryImage.previous() != null) {
          Utils.downloadPicture(mapillaryImage.previous());
          if (mapillaryImage.previous().previous() != null)
            Utils.downloadPicture(mapillaryImage.previous().previous());
        }
      }
    }
    if (zoom)
      Main.map.mapView.zoomTo(MapillaryData.getInstance().getSelectedImage().getLatLon());
    if (Main.map != null)
      Main.map.mapView.repaint();
    fireSelectedImageChanged(oldImage, selectedImage);
  }

  private void fireSelectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
    if (listeners.isEmpty())
      return;
    for (MapillaryDataListener lis : listeners)
      lis.selectedImageChanged(oldImage, newImage);
  }

  /**
   * Adds a MapillaryImage object to the list of selected images, (when ctrl +
   * click)
   *
   * @param image
   *        The MapillaryImage object to be added.
   */
  public void addMultiSelectedImage(MapillaryAbstractImage image) {
    if (!this.multiSelectedImages.contains(image)) {
      if (this.getSelectedImage() != null)
        this.multiSelectedImages.add(image);
      else
        this.setSelectedImage(image);
    }
    Main.map.mapView.repaint();
  }

  /**
   * Adds a set of {@code MapillaryAbstractImage} objects to the list of
   * selected images.
   *
   * @param images
   *        A List object containing the set of images to be added.
   */
  public void addMultiSelectedImage(List<MapillaryAbstractImage> images) {
    for (MapillaryAbstractImage image : images)
      if (!this.multiSelectedImages.contains(image)) {
        if (this.getSelectedImage() != null)
          this.multiSelectedImages.add(image);
        else
          this.setSelectedImage(image);
      }
    Main.map.mapView.repaint();
  }

  /**
   * Returns a List containing all {@code MapillaryAbstractImage} objects
   * selected with ctrl + click
   *
   * @return A List object containing all the images selected.
   */
  public List<MapillaryAbstractImage> getMultiSelectedImages() {
    return multiSelectedImages;
  }

  /**
   * Returns the amount of images contained by this object.
   *
   * @return The amount of images in stored.
   */
  public int size() {
    return images.size();
  }
}