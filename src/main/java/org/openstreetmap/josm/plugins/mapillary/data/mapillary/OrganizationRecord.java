// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.json.Json;
import javax.json.JsonReader;
import javax.swing.ImageIcon;

import org.openstreetmap.josm.io.CachedFile;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.ListenerList;
import org.openstreetmap.josm.tools.Logging;

/**
 * Record organization information
 *
 * @author Taylor Smock
 */
public final class OrganizationRecord {
  private static final ListenerList<OrganizationRecordListener> LISTENERS = ListenerList.create();
  private String description;
  private final String key;
  private String name;
  private String niceName;
  private boolean privateRepository;
  private boolean publicRepository;
  private ImageIcon avatar;

  private static final Map<String, OrganizationRecord> CACHE = new ConcurrentHashMap<>();

  public static final OrganizationRecord NULL_RECORD = new OrganizationRecord("", "", "", "", "", false, false);

  private OrganizationRecord(
    String avatar, String description, String key, String name, String niceName, boolean privateRepository,
    boolean publicRepository
  ) {
    this.avatar = createAvatarIcon(avatar, key);
    this.description = description;
    this.key = key;
    this.name = name;
    this.niceName = niceName;
    this.privateRepository = privateRepository;
    this.publicRepository = publicRepository;
  }

  private static ImageIcon createAvatarIcon(String avatar, String organizationKey) {
    if (avatar != null && !avatar.isEmpty()) {
      return ImageProvider.get(avatar, ImageProvider.ImageSizes.DEFAULT);
    } else if (organizationKey != null && !organizationKey.isEmpty()) {
      try (CachedFile possibleAvatar = new CachedFile(
        MapillaryURL.APIv3.retrieveOrganizationAvatar(organizationKey).toExternalForm()
      )) {
        OAuthUtils.addAuthenticationHeader(possibleAvatar);
        return ImageProvider.get(possibleAvatar.getFile().getAbsolutePath(), ImageProvider.ImageSizes.DEFAULT);
      } catch (IOException e) {
        Logging.error(e);
      }
    }
    return ImageProvider.getEmpty(ImageSizes.DEFAULT);
  }

  public static OrganizationRecord getOrganization(
    String avatar, String description, String key, String name, String niceName, boolean privateRepository,
    boolean publicRepository
  ) {
    boolean newRecord = !CACHE.containsKey(key);
    OrganizationRecord record = CACHE.computeIfAbsent(
      key, k -> new OrganizationRecord(avatar, description, key, name, niceName, privateRepository, publicRepository)
    );
    // TODO remove when getNewOrganization is done, and make vars final again
    record.avatar = createAvatarIcon(avatar, key);
    record.description = description;
    record.name = name;
    record.niceName = niceName;
    record.privateRepository = privateRepository;
    record.publicRepository = publicRepository;
    if (newRecord) {
      LISTENERS.fireEvent(l -> l.organizationAdded(record));
    }
    return record;
  }

  public static OrganizationRecord getOrganization(String key) {
    return key == null ? NULL_RECORD : CACHE.computeIfAbsent(key, OrganizationRecord::getNewOrganization);
  }

  private static OrganizationRecord getNewOrganization(String key) {
    // TODO Fix
    CachedFile file = new CachedFile(MapillaryURL.APIv3.retrieveOrganization(key).toString());
    Logging.error(file.getName());
    OAuthUtils.addAuthenticationHeader(file);
    try (BufferedReader br = file.getContentReader(); JsonReader reader = Json.createReader(br)) {
      String line = br.readLine();
      Logging.error(line);
      while (line != null) {
        line = br.readLine();
        Logging.error(line);
      }
    } catch (IOException e) {
      Logging.error(e);
    } finally {
      file.close();
    }
    OrganizationRecord gr = new OrganizationRecord("", "", key, "", "", false, false);
    LISTENERS.fireEvent(l -> l.organizationAdded(gr));
    return gr;
  }

  /**
   * @return The avatar for the organization
   */
  public ImageIcon getAvatar() {
    return avatar;
  }

  /**
   * @return The organization description
   */
  public String getDescription() {
    return description;
  }

  /**
   * @return The organization key
   */
  public String getKey() {
    return key;
  }

  /**
   * @return The name of the organization
   * @see OrganizationRecord#getNiceName
   */
  public String getName() {
    return name;
  }

  /**
   * @return The nice-looking name of the organization
   */
  public String getNiceName() {
    return niceName;
  }

  /**
   * @return {@code true} if the organization has a private repository
   */
  public boolean hasPrivateRepository() {
    return privateRepository;
  }

  /**
   * @return {@code true} if the organization has a public repository
   */
  public boolean hasPublicRepository() {
    return publicRepository;
  }

  public static void addOrganizationListener(OrganizationRecordListener listener) {
    LISTENERS.addListener(listener);
  }

  public static void removeOrganizationListener(OrganizationRecordListener listener) {
    LISTENERS.removeListener(listener);
  }

  public interface OrganizationRecordListener {
    void organizationAdded(OrganizationRecord organization);
  }

  /**
   * @return Get all the organizations
   */
  public static Collection<OrganizationRecord> getOrganizations() {
    return Collections.unmodifiableCollection(CACHE.values());
  }
}
