// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.Serializable;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import org.openstreetmap.josm.data.imagery.vectortile.mapbox.MVTTile;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.cache.Caches;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.ListenerList;
import org.openstreetmap.josm.tools.Logging;

/**
 * Record organization information
 *
 * @param id the unique key for the organization
 * @param name the machine-readable name for an organization
 * @param niceName the human-readable name for an organization
 * @param description the description for the organization
 * @param avatar the avatar for the organization
 * @author Taylor Smock
 */
public record OrganizationRecord(long id, String name, String niceName, String description, ImageIcon avatar)
    implements Serializable {

    private static final Pattern NUMBER_PATTERN = Pattern.compile("\\d+");
    private static final ListenerList<OrganizationRecordListener> LISTENERS = ListenerList.create();

    private static final Map<Long, OrganizationRecord> CACHE = new ConcurrentHashMap<>(1);

    public static final OrganizationRecord NULL_RECORD = new OrganizationRecord(0L, "", "", "", createAvatarIcon(""));

    static {
        CACHE.put(0L, NULL_RECORD);
    }

    /**
     * Get or create an avatar icon
     *
     * @param avatar The url for the avatar
     * @return The avatar to show
     */
    @Nonnull
    private static ImageIcon createAvatarIcon(@Nullable String avatar) {
        if (avatar != null && !avatar.isEmpty()) {
            final BufferedImage avatarImage = Caches.META_IMAGES.get(avatar, OrganizationRecord::fetchAvatarIcon);
            return avatarImage != null ? new ImageIcon(avatarImage) : ImageProvider.createBlankIcon(ImageSizes.DEFAULT);
        }
        return ImageProvider.getEmpty(ImageSizes.DEFAULT);
    }

    /**
     * Do the actual avatar icon fetch
     *
     * @param url The icon to get
     * @return The image
     */
    private static BufferedImage fetchAvatarIcon(String url) {
        try {
            final var client = HttpClient.create(URI.create(url).toURL());
            OAuthUtils.addAuthenticationHeader(client);
            final var response = client.connect();
            if (response.getResponseCode() >= 200 && response.getResponseCode() < 400) {
                return ImageIO.read(response.getContent());
            }
        } catch (IOException e) {
            Logging.error(e);
        }
        return null;
    }

    /**
     * Get an organization
     *
     * @param id The organization id
     * @return The organization
     */
    public static OrganizationRecord getOrganization(long id) {
        return CACHE.computeIfAbsent(id, OrganizationRecord::getNewOrganization);
    }

    /**
     * Get an organization
     *
     * @param id The organization id
     * @return The organization
     */
    public static OrganizationRecord getOrganization(String id) {
        if (id.matches("^\\d+$")) {
            return getOrganization(Long.parseLong(id));
        } else { // Assume json
            try (var reader = Json.createReader(new ByteArrayInputStream(id.getBytes(StandardCharsets.UTF_8)))) {
                return getOrganization(Long.parseLong(reader.readObject().getString("id")));
            }
        }
    }

    /**
     * Get a new organization
     *
     * @param id The organization id
     * @return The new organization record
     */
    private static OrganizationRecord getNewOrganization(long id) {
        // TODO check for API in v4 (preferably one that doesn't need user auth)
        final String url = MapillaryConfig.getUrls().getOrganizationInformation(id);
        try {
            final var data = OAuthUtils.getWithHeader(URI.create(url));
            final var organizationRecord = decodeNewOrganization(data);
            // Ensure that we aren't blocking the main EDT thread
            MainApplication.worker.execute(() -> LISTENERS.fireEvent(l -> l.organizationAdded(organizationRecord)));
            return organizationRecord;
        } catch (IOException exception) {
            Logging.error(exception);
        }
        return null;
    }

    /**
     * Decode a new organization
     *
     * @param organization The object to decode
     * @return A new organization record
     */
    private static OrganizationRecord decodeNewOrganization(JsonObject organization) {
        final var description = organization.getString("description", "");
        final var slug = organization.getString("slug", "");
        final var name = organization.getString("name", "");
        final var idString = organization.getString("id", "");
        final var avatarUrl = organization.getString("profile_photo_url", null);
        long id = 0;
        if (NUMBER_PATTERN.matcher(idString).matches()) {
            id = Long.parseLong(idString);
        }
        return new OrganizationRecord(id, slug, name, description, createAvatarIcon(avatarUrl));
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
     * Add listener for organizations
     *
     * @param listener The listener to notify when a new organization is added
     */
    public static void addOrganizationListener(OrganizationRecordListener listener) {
        LISTENERS.addListener(listener);
    }

    /**
     * Remove a listener for organizations
     *
     * @param listener The listener to remove from the notification list
     */
    public static void removeOrganizationListener(OrganizationRecordListener listener) {
        LISTENERS.removeListener(listener);
    }

    /**
     * The interface for listening for new organizations
     */
    public interface OrganizationRecordListener extends Serializable {
        /**
         * Called when a new organization is added
         *
         * @param organization The new organization
         */
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
