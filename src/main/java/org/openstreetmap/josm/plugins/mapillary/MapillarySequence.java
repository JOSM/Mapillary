// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.json.Json;

import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonUserProfileDecoder;
import org.openstreetmap.josm.tools.Logging;

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
  private final String key;
  private UserProfile user;
  /**
   * Epoch time when the sequence was created
   */
  private final long capturedAt;

  /**
   * Creates a sequence without key or timestamp. Used for
   * {@link MapillaryImportedImage} sequences.
   */
  public MapillarySequence() {
    this.images = new CopyOnWriteArrayList<>();
    this.capturedAt = -1L;
    this.key = null;
  }

  /**
   * Creates a sequence object with the given parameters.
   *
   * @param key The unique identifier of the sequence.
   * @param userKey The user key
   * @param capturedAt The date the sequence was created.
   */
  public MapillarySequence(final String key, final String userKey, final long capturedAt) {
    this.images = new CopyOnWriteArrayList<>();
    this.key = key;
    this.capturedAt = capturedAt;
    setUser(userKey);
  }

  /**
   * Adds a new {@link MapillaryAbstractImage} object to the database.
   *
   * @param image The {@link MapillaryAbstractImage} object to be added
   */
  public synchronized void add(MapillaryAbstractImage image) {
    this.images.add(image);
    image.setSequence(this);
  }

  /**
   * Adds a set of {@link MapillaryAbstractImage} objects to the database.
   *
   * @param images The set of {@link MapillaryAbstractImage} objects to be added.
   */
  public synchronized void add(Collection<? extends MapillaryAbstractImage> images) {
    this.images.addAll(images);
    images.forEach(img -> img.setSequence(this));
  }

  /**
   * Returns the Epoch time when the sequence was captured.
   *
   * Negative values mean, no value is set.
   *
   * @return A long containing the Epoch time when the sequence was captured.
   */
  public long getCapturedAt() {
    return this.capturedAt;
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

  public UserProfile getUser() {
    return user;
  }

  /**
   * Returns the next {@link MapillaryAbstractImage} in the sequence of a given
   * {@link MapillaryAbstractImage} object.
   *
   * @param image The {@link MapillaryAbstractImage} object whose next image is
   * going to be returned.
   *
   * @return The next {@link MapillaryAbstractImage} object in the sequence.
   *
   * @throws IllegalArgumentException if the given {@link MapillaryAbstractImage} object doesn't belong
   * the this sequence.
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
   * going to be returned.
   *
   * @return The previous {@link MapillaryAbstractImage} object in the sequence.
   *
   * @throws IllegalArgumentException if the given {@link MapillaryAbstractImage} object doesn't belong
   * the this sequence.
   */
  public MapillaryAbstractImage previous(MapillaryAbstractImage image) {
    int i = this.images.indexOf(image);
    if (i < 0) {
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

  private void setUser(String userKey) {
    new Thread(() -> {
      UserProfile cachedProfile = Caches.UserProfileCache.getInstance().get(userKey);
      if (cachedProfile == null) {
        try {
          Caches.UserProfileCache.getInstance().put(
            userKey,
            JsonUserProfileDecoder.decodeUserProfile(
              Json.createReader(MapillaryURL.APIv3.getUser(userKey).openStream()).readObject()
            )
          );
        } catch (IOException var4) {
          Logging.log(Logging.LEVEL_WARN, "Error when downloading user profile for user key '" + userKey + "'!", var4);
        }
      }

      this.user = Caches.UserProfileCache.getInstance().get(userKey);
    }, "userProfileDownload_" + userKey).start();
 }
}
