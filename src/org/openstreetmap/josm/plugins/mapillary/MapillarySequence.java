// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.openstreetmap.josm.plugins.mapillary.utils.ValidationUtil;

/**
 * Class that stores a sequence of {@link MapillaryAbstractImage} objects.
 *
 * @author nokutu
 * @see MapillaryAbstractImage
 */
public class MapillarySequence {
  /**
   * The images in the sequence.
   */
  private final List<MapillaryAbstractImage> images;
  /**
   * Unique identifier. Used only for {@link MapillaryImage} sequences.
   */
  private String key;
  /**
   * Epoch time when the sequence was created
   */
  private long createdAt;

  /**
   * Creates a sequence without key or timestamp. Used for
   * {@link MapillaryImportedImage} sequences.
   */
  public MapillarySequence() {
    this.images = new CopyOnWriteArrayList<>();
  }

  /**
   * Creates a sequence object with the given parameters.
   *
   * @param key       The unique identifier of the sequence.
   * @param createdAt The date the sequence was created.
   * @throws IllegalArgumentException if the key is invalid
   *           according to {@link ValidationUtil#validateSequenceKey(String)}
   */
  public MapillarySequence(String key, long createdAt) {
    ValidationUtil.throwExceptionForInvalidSeqKey(key, true);

    this.images = new CopyOnWriteArrayList<>();
    this.key = key;
    this.createdAt = createdAt;
  }

  /**
   * Adds a new {@link MapillaryAbstractImage} object to the database.
   *
   * @param image The {@link MapillaryAbstractImage} object to be added
   */
  public synchronized void add(MapillaryAbstractImage image) {
    this.images.add(image);
  }

  /**
   * Adds a set of {@link MapillaryAbstractImage} objects to the database.
   *
   * @param images The set of {@link MapillaryAbstractImage} objects to be added.
   */
  public synchronized void add(List<MapillaryAbstractImage> images) {
    for (MapillaryAbstractImage image : images)
      add(image);
  }

  /**
   * Returns the Epoch time when the sequence was captured.
   *
   * @return A long containing the Epoch time when the sequence was captured.
   */
  public long getCreatedAt() {
    return this.createdAt;
  }

  /**
   * Returns all {@link MapillaryAbstractImage} objects contained by this
   * object.
   *
   * @return A {@link List} object containing all the
   * {@link MapillaryAbstractImage} objects that are part of the
   * sequence.
   */
  public List<MapillaryAbstractImage> getImages() {
    return this.images;
  }

  /**
   * Returns the unique identifier of the sequence.
   *
   * @return A {@code String} containing the unique identifier of the sequence.
   * null means that the sequence has been created locally for imported
   * images.
   */
  public String getKey() {
    return this.key;
  }

  /**
   * Returns the next {@link MapillaryAbstractImage} in the sequence of a given
   * {@link MapillaryAbstractImage} object.
   *
   * @param image The {@link MapillaryAbstractImage} object whose next image is
   *              going to be returned.
   * @return The next {@link MapillaryAbstractImage} object in the sequence.
   * @throws IllegalArgumentException if the given {@link MapillaryAbstractImage} object doesn't belong
   *                                  the this sequence.
   */
  public MapillaryAbstractImage next(MapillaryAbstractImage image) {
    int i = this.images.indexOf(image);
    if (i == -1) {
      throw new IllegalArgumentException();
    }
    if (i == this.images.size() - 1) {
      return null;
    }
    return this.images.get(i + 1);
  }

  /**
   * Returns the previous {@link MapillaryAbstractImage} in the sequence of a
   * given {@link MapillaryAbstractImage} object.
   *
   * @param image The {@link MapillaryAbstractImage} object whose previous image is
   *              going to be returned.
   * @return The previous {@link MapillaryAbstractImage} object in the sequence.
   * @throws IllegalArgumentException if the given {@link MapillaryAbstractImage} object doesn't belong
   *                                  the this sequence.
   */
  public MapillaryAbstractImage previous(MapillaryAbstractImage image) {
    int i = this.images.indexOf(image);
    if (i == -1) {
      throw new IllegalArgumentException();
    }
    if (i == 0) {
      return null;
    }
    return this.images.get(i - 1);
  }

  /**
   * Removes a {@link MapillaryAbstractImage} object from the database.
   *
   * @param image The {@link MapillaryAbstractImage} object to be removed.
   */
  public void remove(MapillaryAbstractImage image) {
    this.images.remove(image);
  }
}
