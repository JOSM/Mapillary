// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.Optional;

import javax.swing.JOptionPane;

import jakarta.annotation.Nullable;
import jakarta.json.Json;
import jakarta.json.JsonException;
import jakarta.json.JsonObject;
import jakarta.json.JsonReader;
import jakarta.json.JsonValue;
import org.openstreetmap.josm.data.Version;
import org.openstreetmap.josm.data.preferences.IntegerProperty;
import org.openstreetmap.josm.data.preferences.StringProperty;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.io.CachedFile;
import org.openstreetmap.josm.plugins.PluginHandler;
import org.openstreetmap.josm.plugins.PluginInformation;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.Logging;

/**
 * Check if Mapillary or JOSM are old versions
 */
public final class OldVersionDialog {
    private static final IntegerProperty SHOWN_OLD = new IntegerProperty("mapillary.shown_old", 0);
    private static final StringProperty NEW_VERSION = new StringProperty("mapillary.new_version", null);

    /**
     * Show a message to the user that there is a new version of JOSM <i>and</i> a new version of the Mapillary plugin.
     */
    public static void showOldVersion() {
        int currentVersion = getTestedVersion();
        final PluginInformation plugin = PluginHandler.getPlugins().stream()
            .filter(pluginInformation -> "mapillary".equalsIgnoreCase(pluginInformation.getName())).findAny()
            .orElse(null);
        final String version = latestMapillaryVersion();
        final Boolean newMapillaryVersionAvailable = Objects.nonNull(version) && plugin != null
            ? !Objects.equals(version, plugin.version)
            : null;
        if (Boolean.TRUE.equals(newMapillaryVersionAvailable)
            && ((Version.getInstance().getVersion() < currentVersion && SHOWN_OLD.get() < 5)
                || !version.equals(NEW_VERSION.get()))) {
            // If there is a new version reset the count.
            if (!version.equals(NEW_VERSION.get())) {
                SHOWN_OLD.remove();
            }
            SHOWN_OLD.put(SHOWN_OLD.get() + 1);
            NEW_VERSION.put(version);
            GuiHelper.runInEDT(() -> {
                Notification notification = new Notification();
                notification.setIcon(JOptionPane.WARNING_MESSAGE);
                notification
                    .setContent(tr("There is a newer version of JOSM and/or the Mapillary plugin. Please update."));
                notification.show();
            });
        } else if (currentVersion > 0 && Boolean.FALSE.equals(newMapillaryVersionAvailable)) {
            SHOWN_OLD.remove();
        }
    }

    /**
     * Check if this is the latest Mapillary version
     *
     * @return {@code true} if this is the latest Mapillary plugin version
     */
    @Nullable
    static String latestMapillaryVersion() {
        try (CachedFile latestMapillary = new CachedFile("https://api.github.com/repos/JOSM/Mapillary/releases/latest");
            JsonReader reader = Json.createReader(latestMapillary.getContentReader())) {
            latestMapillary.setMaxAge(60L * 15); // 15 minutes
            final JsonValue value = reader.read();
            final JsonObject object = value.asJsonObject();
            return Optional.ofNullable(object.getString("tag_name", null)).map(s -> s.replaceFirst("^v", ""))
                .orElse(null);
        } catch (IOException | JsonException | IllegalStateException e) {
            Logging.error(e);
        }
        return null;
    }

    /**
     * Get the current JOSM tested version
     *
     * @return The current JOSM tested version, or -1 if we couldn't get the current version
     */
    @SuppressWarnings("DuplicatedCode")
    static int getTestedVersion() {
        try (CachedFile testedVersion = new CachedFile(Config.getUrls().getJOSMWebsite() + "/tested")) {
            testedVersion.setMaxAge(60L * 15); // 15 Minutes
            String testedString = new String(testedVersion.getByteContent(), StandardCharsets.ISO_8859_1);
            return Integer.parseInt(testedString.trim());
        } catch (NumberFormatException | IOException e) {
            Logging.log(Logging.LEVEL_WARN, "Unable to detect current tested version of JOSM:", e);
            return -1;
        }
    }

    private OldVersionDialog() {
        // Prevent instantiation
    }
}
