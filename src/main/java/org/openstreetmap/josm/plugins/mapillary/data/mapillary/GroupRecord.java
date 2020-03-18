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
 * Record group information
 *
 * @author Taylor Smock
 */
public final class GroupRecord {
  private static final ListenerList<GroupRecordListener> listeners = ListenerList.create();
  private String description;
  private String key;
  private String name;
  private String niceName;
  private boolean privateRepository;
  private boolean publicRepository;
  private ImageIcon avatar;

  private static final Map<String, GroupRecord> CACHE = new ConcurrentHashMap<>();

  public static final GroupRecord NULL_RECORD = new GroupRecord("", "", "", "", "", false, false);

  private GroupRecord(
    String avatar, String description, String key, String name, String niceName, boolean privateRepository,
    boolean publicRepository
  ) {
    this.avatar = createAvatarIcon(avatar);
    this.description = description;
    this.key = key;
    this.name = name;
    this.niceName = niceName;
    this.privateRepository = privateRepository;
    this.publicRepository = publicRepository;
  }

  private static ImageIcon createAvatarIcon(String avatar) {
    if (avatar != null && !avatar.isEmpty()) {
      return new ImageProvider(avatar).get();
    }
    return ImageProvider.getEmpty(ImageSizes.DEFAULT);
  }

  public static GroupRecord getGroup(
    String avatar, String description, String key, String name, String niceName, boolean privateRepository,
    boolean publicRepository
  ) {
    boolean newRecord = !CACHE.containsKey(key);
    GroupRecord record = CACHE.computeIfAbsent(
      key, k -> new GroupRecord(avatar, description, key, name, niceName, privateRepository, publicRepository)
    );
    // TODO remove when getNewGroup is done, and make vars final again
    record.avatar = createAvatarIcon(avatar);
    record.description = description;
    record.name = name;
    record.niceName = niceName;
    record.privateRepository = privateRepository;
    record.publicRepository = publicRepository;
    if (newRecord) {
      listeners.fireEvent(l -> l.groupAdded(record));
    }
    return record;
  }

  public static GroupRecord getGroup(String key) {
    return key == null ? NULL_RECORD : CACHE.computeIfAbsent(key, GroupRecord::getNewGroup);
  }

  private static GroupRecord getNewGroup(String key) {
    // TODO Fix
    CachedFile file = new CachedFile(MapillaryURL.APIv3.retrieveGroup(key).toString());
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
    GroupRecord gr = new GroupRecord("", "", key, "", "", false, false);
    listeners.fireEvent(l -> l.groupAdded(gr));
    return gr;
  }

  /**
   * @return The avatar for the group
   */
  public ImageIcon getAvatar() {
    return avatar;
  }

  /**
   * @return The group description
   */
  public String getDescription() {
    return description;
  }

  /**
   * @return The group key
   */
  public String getKey() {
    return key;
  }

  /**
   * @return The name of the group
   * @see GroupRecord#getNiceName
   */
  public String getName() {
    return name;
  }

  /**
   * @return The nice-looking name of the group
   */
  public String getNiceName() {
    return niceName;
  }

  /**
   * @return {@code true} if the group has a private repository
   */
  public boolean hasPrivateRepository() {
    return privateRepository;
  }

  /**
   * @return {@code true} if the group has a public repository
   */
  public boolean hasPublicRepository() {
    return publicRepository;
  }
  
  public static void addGroupListener(GroupRecordListener listener) {
    listeners.addListener(listener);
  }
  
  public static void removeGroupListener(GroupRecordListener listener) {
    listeners.removeListener(listener);
  }
  
  public static interface GroupRecordListener {
    public void groupAdded(GroupRecord group);
  }

  /**
   * @return Get all the groups
   */
  public static Collection<GroupRecord> getGroups() {
    return Collections.unmodifiableCollection(CACHE.values());
  }
}
