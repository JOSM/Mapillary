// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.Serializable;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;

import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.ListenerList;
import org.openstreetmap.josm.tools.Logging;

/**
 * Record organization information
 *
 * @author Taylor Smock
 */
public final class OrganizationRecord implements Serializable {
  private static final ListenerList<OrganizationRecordListener> LISTENERS = ListenerList.create();
  private String description;
  private final String key;
  private String name;
  private String niceName;
  private ImageIcon avatar;

  private static final Map<String, OrganizationRecord> CACHE = new ConcurrentHashMap<>();

  public static final OrganizationRecord NULL_RECORD = new OrganizationRecord("", "", "", "");

  static {
    CACHE.put("", NULL_RECORD);
  }

  private OrganizationRecord(String key, String slug, String name, String description) {
    // TODO actually get the avatar icon
    this.avatar = createAvatarIcon(null, key);
    this.description = description;
    this.key = key;
    this.name = slug;
    this.niceName = name;
  }

  // OAuthUtils.addAuthenticationHeader returns the resource passed into it.
  @SuppressWarnings("resource")
  private static ImageIcon createAvatarIcon(String avatar, String organizationKey) {
    if (avatar != null && !avatar.isEmpty()) {
      return ImageProvider.get(avatar, ImageProvider.ImageSizes.DEFAULT);
    } else if (organizationKey != null && !organizationKey.isEmpty()) {
      final String url = MapillaryURL.APIv3.retrieveOrganizationAvatar(organizationKey);
      final BufferedImage avatarImage = Caches.META_IMAGES.get(url, () -> {
        try {
          HttpClient client = HttpClient.create(new URL(url));
          OAuthUtils.addAuthenticationHeader(client);
          HttpClient.Response response = client.connect();
          if (response.getResponseCode() >= 200 && response.getResponseCode() < 400) {
            return ImageIO.read(response.getContent());
          }
        } catch (IOException e) {
          Logging.error(e);
        }
        return null;
      });
      return avatarImage != null ? new ImageIcon(avatarImage) : ImageProvider.createBlankIcon(ImageSizes.DEFAULT);
    }
    return ImageProvider.getEmpty(ImageSizes.DEFAULT);
  }

  public static OrganizationRecord getOrganization(String key, String slug, String name, String description) {
    boolean newRecord = !CACHE.containsKey(key);
    OrganizationRecord record = CACHE.computeIfAbsent(key, k -> new OrganizationRecord(k, slug, name, description));
    // TODO remove when getNewOrganization is done, and make vars final again
    record.avatar = createAvatarIcon(null, key);
    record.description = description;
    record.name = slug;
    record.niceName = name;
    if (newRecord) {
      LISTENERS.fireEvent(l -> l.organizationAdded(record));
    }
    return record;
  }

  public static OrganizationRecord getOrganization(String key) {
    return key == null ? NULL_RECORD : CACHE.computeIfAbsent(key, OrganizationRecord::getNewOrganization);
  }

  // OAuthUtils.addAuthenticationHeader returns the resource passed into it.
  private static OrganizationRecord getNewOrganization(String key) {
    // TODO check for API in v4 (preferably one that doesn't need user auth)
    OrganizationRecord gr = new OrganizationRecord(key, "", "", "");
    // Ensure that we aren't blocking the main EDT thread
    MainApplication.worker.execute(() -> LISTENERS.fireEvent(l -> l.organizationAdded(gr)));
    return gr;
  }

  /**
   * Read organizations from a tile and add them to the list
   *
   * @param tile The tile to read
   */
  public static void addFromTile(MVTTile tile) {
    tile.getData().getAllPrimitives().stream().filter(INode.class::isInstance).map(INode.class::cast)
      .forEach(MapillaryImageUtils::getOrganization);
  }

  /**
   * Get the avatar for the organization
   *
   * @return The avatar for the organization
   */
  public ImageIcon getAvatar() {
    return avatar != null ? avatar : ImageProvider.createBlankIcon(ImageSizes.DEFAULT);
  }

  /**
   * Get the description for the organization
   *
   * @return The organization description
   */
  public String getDescription() {
    return description;
  }

  /**
   * Get the unique key for the organization
   *
   * @return The organization key
   */
  public String getKey() {
    return key;
  }

  /**
   * Get the machine-readable name for an organization
   *
   * @return The name of the organization
   * @see OrganizationRecord#getNiceName
   */
  public String getName() {
    return name;
  }

  /**
   * Get the human-readable name for an organization
   *
   * @return The nice-looking name of the organization
   */
  public String getNiceName() {
    return niceName;
  }

  public static void addOrganizationListener(OrganizationRecordListener listener) {
    LISTENERS.addListener(listener);
  }

  public static void removeOrganizationListener(OrganizationRecordListener listener) {
    LISTENERS.removeListener(listener);
  }

  public interface OrganizationRecordListener extends Serializable {
    void organizationAdded(OrganizationRecord organization);
  }

  /**
   * Get all current organizations
   *
   * @return Get all the organizations
   */
  public static Collection<OrganizationRecord> getOrganizations() {
    return Collections.unmodifiableCollection(CACHE.values());
  }
}
