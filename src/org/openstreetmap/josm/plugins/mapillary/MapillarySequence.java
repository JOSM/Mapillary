package org.openstreetmap.josm.plugins.mapillary;

import java.util.ArrayList;
import java.util.List;

/**
 * Class that stores a sequence of MapillaryImage objects.
 * 
 * @author nokutu
 * @see MapillaryImage
 *
 */
public class MapillarySequence {
  private final List<MapillaryAbstractImage> images;
  private String key;
  private long created_at;
  
  public MapillarySequence() {
    this.images = new ArrayList<>();
  }

  public MapillarySequence(String key, long created_at) {
    this.images = new ArrayList<>();
    this.key = key;
    this.created_at = created_at;
  }

  /**
   * Returns all MapillaryImages objects contained by this object.
   * 
   * @return
   */
  public List<MapillaryAbstractImage> getImages() {
    return this.images;
  }

  public long getCreatedAt() {
    return created_at;
  }

  /**
   * Adds a new MapillaryImage object to this object.
   * 
   * @param image
   */
  public synchronized void add(MapillaryAbstractImage image) {
    this.images.add(image);
  }

  public String getKey() {
    return this.key;
  }

  /**
   * Adds a set of MapillaryImage objects to this object.
   * 
   * @param images
   */
  public synchronized void add(List<MapillaryAbstractImage> images) {
    for (MapillaryAbstractImage image : images)
      add(image);
  }

  /**
   * Removes a MapillaryImage object from this object.
   * 
   * @param image
   */
  public void remove(MapillaryAbstractImage image) {
    this.images.remove(image);
  }

  /**
   * Returns the next MapillaryImage in the sequence.
   * 
   * @param image
   * @return
   */
  public MapillaryAbstractImage next(MapillaryAbstractImage image) {
    if (!images.contains(image))
      throw new IllegalArgumentException();
    int i = images.indexOf(image);
    if (i == images.size() - 1)
      return null;
    else
      return images.get(i + 1);
  }

  /**
   * Returns the previous {@link MapillaryAbstractImage} in the sequence.
   * 
   * @param image
   * @return
   */
  public MapillaryAbstractImage previous(MapillaryAbstractImage image) {
    if (!images.contains(image))
      throw new IllegalArgumentException();
    int i = images.indexOf(image);
    if (i == 0)
      return null;
    else
      return images.get(i - 1);
  }

  /**
   * Returns the difference of index between two {@link MapillaryAbstractImage}
   * objects belonging to the same {@link MapillarySequence}.
   * 
   * @param image1
   * @param image2
   * @return
   */
  public int getDistance(MapillaryAbstractImage image1, MapillaryAbstractImage image2) {
    if (!this.images.contains(image1) || !this.images.contains(image2))
      throw new IllegalArgumentException();
    return Math.abs(this.images.indexOf(image1) - this.images.indexOf(image2));
  }
}
