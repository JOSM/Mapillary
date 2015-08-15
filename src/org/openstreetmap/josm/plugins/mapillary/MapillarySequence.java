package org.openstreetmap.josm.plugins.mapillary;

import java.util.ArrayList;
import java.util.List;

/**
 * Class that stores a sequence of {@link MapillaryAbstractImage} objects.
 *
 * @author nokutu
 * @see MapillaryAbstractImage
 *
 */
public class MapillarySequence {
  private final List<MapillaryAbstractImage> images;
  private String key;
  private long created_at;

  /**
   * Creates a sequence without key or timestamp. Used for
   * {@link MapillaryImportedImage} sequences.
   */
  public MapillarySequence() {
    this.images = new ArrayList<>();
  }

  /**
   * Creates a sequence object with the given parameters.
   *
   * @param key
   *          The unique identifier of the sequence.
   * @param created_at
   *          The date the sequence was created.
   */
  public MapillarySequence(String key, long created_at) {
    this.images = new ArrayList<>();
    this.key = key;
    this.created_at = created_at;
  }

  /**
   * Returns all {@link MapillaryAbstractImage} objects contained by this
   * object.
   *
   * @return A List object containing all the {@link MapillaryAbstractImage}
   *         objects that are part of the sequence.
   */
  public List<MapillaryAbstractImage> getImages() {
    return this.images;
  }

  /**
   * Returns the Epoch time when the sequence was captured.
   *
   * @return A long containing the Epoch time when the sequence was captured.
   *
   */
  public long getCreatedAt() {
    return this.created_at;
  }

  /**
   * Adds a new {@link MapillaryAbstractImage} object to the database.
   *
   * @param image
   *          The {@link MapillaryAbstractImage} object to be added
   */
  public synchronized void add(MapillaryAbstractImage image) {
    this.images.add(image);
  }

  /**
   * Returns the unique identifier of the sequence.
   *
   * @return A String containing the unique identifier of the sequence. null
   *         means that the sequence has been created locally for imported
   *         images.
   */
  public String getKey() {
    return this.key;
  }

  /**
   * Adds a set of {@link MapillaryAbstractImage} objects to the database.
   *
   * @param images
   *          The set of {@link MapillaryAbstractImage} objects to be added.
   */
  public synchronized void add(List<MapillaryAbstractImage> images) {
    for (MapillaryAbstractImage image : images)
      add(image);
  }

  /**
   * Removes a {@link MapillaryAbstractImage} object from the database.
   *
   * @param image
   *          The {@link MapillaryAbstractImage} object to be removed.
   */
  public void remove(MapillaryAbstractImage image) {
    this.images.remove(image);
  }

  /**
   * Returns the next {@link MapillaryAbstractImage} in the sequence of a given
   * {@link MapillaryAbstractImage} object.
   *
   * @param image
   *          The {@link MapillaryAbstractImage} object whose next image is
   *          going to be returned.
   * @return The next {@link MapillaryAbstractImage} object in the sequence.
   * @throws IllegalArgumentException
   *           if the given {@link MapillaryAbstractImage} object doesn't belong
   *           the this sequence.
   */
  public MapillaryAbstractImage next(MapillaryAbstractImage image) {
    if (!this.images.contains(image))
      throw new IllegalArgumentException();
    int i = this.images.indexOf(image);
    if (i == this.images.size() - 1)
      return null;
    return this.images.get(i + 1);
  }

  /**
   * Returns the previous {@link MapillaryAbstractImage} in the sequence of a given
   * {@link MapillaryAbstractImage} object.
   *
   * @param image
   *          The {@link MapillaryAbstractImage} object whose previous image is
   *          going to be returned.
   * @return The previous {@link MapillaryAbstractImage} object in the sequence.
   * @throws IllegalArgumentException
   *           if the given {@link MapillaryAbstractImage} object doesn't belong
   *           the this sequence.
   */
  public MapillaryAbstractImage previous(MapillaryAbstractImage image) {
    if (!this.images.contains(image))
      throw new IllegalArgumentException();
    int i = this.images.indexOf(image);
    if (i == 0)
      return null;
    return this.images.get(i - 1);
  }
}
