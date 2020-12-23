// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.image;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;

import javax.json.Json;
import javax.json.JsonReader;

import org.openstreetmap.josm.data.osm.TagMap;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
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
public class MapillarySequence implements Keyed, MapillaryTagged, Serializable {

  private final TagMap tags = new TagMap();
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
  private OrganizationRecord organization;

  /**
   * Creates a sequence without key or timestamp. Used for {@link MapillaryImportedImage} sequences.
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
   * @param organizationKey The organization key
   * @param capturedAt The date the sequence was created.
   */
  public MapillarySequence(final String key, final String userKey, String organizationKey, final long capturedAt) {
    this.images = new CopyOnWriteArrayList<>();
    this.key = key;
    this.capturedAt = capturedAt;
    setUser(userKey);
    setOrganization(organizationKey);
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
   * Negative values mean, no value is set.
   *
   * @return A long containing the Epoch time when the sequence was captured.
   */
  public long getCapturedAt() {
    return this.capturedAt;
  }

  /**
   * Returns all {@link MapillaryAbstractImage} objects contained by this object.
   *
   * @return A {@link List} object containing all the {@link MapillaryAbstractImage} objects that are part of the
   *         sequence.
   */
  public List<MapillaryAbstractImage> getImages() {
    return this.images.stream().filter(m -> !m.isDeleted()).collect(Collectors.toList());
  }

  /**
   * @param imageList
   */
  public void setImages(List<MapillaryAbstractImage> imageList) {
    for (MapillaryAbstractImage image : new ArrayList<>(this.images)) {
      if (!imageList.contains(image)) {
        this.images.remove(image);
      }
    }
    for (MapillaryAbstractImage image : imageList) {
      if (!this.images.contains(image)) {
        this.add(image);
      }
    }
    // Clear the list, and then readd for order.
    this.images.clear();
    this.images.addAll(imageList);
  }

  /**
   * Returns the unique identifier of the sequence.
   *
   * @return A {@code String} containing the unique identifier of the sequence. null means that the sequence has been
   *         created locally for imported images.
   */
  @Override
  public String getKey() {
    return this.key;
  }

  public UserProfile getUser() {
    return user;
  }

  /**
   * Returns the next {@link MapillaryAbstractImage} in the sequence of a given {@link MapillaryAbstractImage} object.
   *
   * @param image The {@link MapillaryAbstractImage} object whose next image is going to be returned.
   * @return The next {@link MapillaryAbstractImage} object in the sequence.
   * @throws IllegalArgumentException if the given {@link MapillaryAbstractImage} object doesn't belong the this
   *         sequence.
   */
  public MapillaryAbstractImage next(MapillaryAbstractImage image) {
    int i = this.getImages().indexOf(image);
    if (i == -1) {
      throw new IllegalArgumentException();
    }
    if (i == this.getImages().size() - 1) {
      return null;
    }
    return this.getImages().get(i + 1);
  }

  /**
   * Returns the previous {@link MapillaryAbstractImage} in the sequence of a given {@link MapillaryAbstractImage}
   * object.
   *
   * @param image The {@link MapillaryAbstractImage} object whose previous image is going to be returned.
   * @return The previous {@link MapillaryAbstractImage} object in the sequence.
   * @throws IllegalArgumentException if the given {@link MapillaryAbstractImage} object doesn't belong the this
   *         sequence.
   */
  public MapillaryAbstractImage previous(MapillaryAbstractImage image) {
    int i = this.getImages().indexOf(image);
    if (i < 0) {
      throw new IllegalArgumentException();
    }
    if (i == 0) {
      return null;
    }
    return this.getImages().get(i - 1);
  }

  private void setUser(String userKey) {
    if (userKey != null) {
      MainApplication.worker.submit(() -> {
        UserProfile cachedProfile = Caches.UserProfileCache.getInstance().get(userKey);
        if (cachedProfile == null) {
          try (InputStream inputStream = MapillaryURL.APIv3.getUser(userKey).openStream();
            JsonReader reader = Json.createReader(inputStream)) {
            Caches.UserProfileCache.getInstance().put(userKey,
              JsonUserProfileDecoder.decodeUserProfile(reader.readObject()));
          } catch (IOException var4) {
            Logging.log(Logging.LEVEL_WARN, "Error when downloading user profile for user key '" + userKey + "'!",
              var4);
          }
        }

        this.user = Caches.UserProfileCache.getInstance().get(userKey);
      }, "userProfileDownload_" + userKey);
    } else {
      this.user = null;
    }
  }

  /**
   * @param organizationKey
   */
  private void setOrganization(String organizationKey) {
    organization = OrganizationRecord.getOrganization(organizationKey);
  }

  /**
   * @return The organization for this sequence
   */
  public OrganizationRecord getOrganization() {
    return organization;
  }

  @Override
  public Map<String, String> getTagMap() {
    return this.tags;
  }
}
