// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Supplier;
import java.util.stream.Stream;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.stream.JsonParser;
import javax.swing.ImageIcon;

import org.openstreetmap.josm.data.osm.Tag;
import org.openstreetmap.josm.data.osm.TagMap;
import org.openstreetmap.josm.gui.mappaint.MapPaintStyles;
import org.openstreetmap.josm.gui.mappaint.mapcss.MapCSSStyleSource;
import org.openstreetmap.josm.gui.tagging.presets.TaggingPreset;
import org.openstreetmap.josm.gui.tagging.presets.TaggingPresetType;
import org.openstreetmap.josm.gui.tagging.presets.TaggingPresets;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Pair;
import org.openstreetmap.josm.tools.ResourceProvider;
import org.openstreetmap.josm.tools.bugreport.BugReport;

/**
 * All current object detections. TODO some kind of warning when a detection does not exist here?
 */
public final class ObjectDetections {
    public static final ImageIcon NO_ICON = ImageProvider.createBlankIcon(ImageProvider.ImageSizes.MAP);
    public static final ObjectDetections UNKNOWN = new ObjectDetections("unknown", null, null, null, null,
        DataType.PRODUCTION, false);

    private static final List<ObjectDetections> VALUES = loadObjectDetections(1649, "complementary", "construction",
        "information", "marking", "object", "regulatory", "warning");
    public static final List<ObjectDetections> IGNORE_DETECTIONS = Collections
        .unmodifiableList(loadObjectDetections(21, "ignorable"));

    private static ObjectDetections[] cachedValuesArray;
    static {
        VALUES.addAll(IGNORE_DETECTIONS);
    }

    @Nonnull
    private final String key;
    @Nonnull
    private final DetectionType[] detectionTypes;
    @Nonnull
    private final Map<String, String> osmKey;
    @Nonnull
    private final Collection<TaggingPresetType> taggingPresetType;
    @Nullable
    private final SerializableSupplier<AdditionalInstructions> additionalCommands;
    @Nonnull
    private final DataType dataType;
    @Nullable
    private final ImageProvider icon;
    // Not final just in case a preset change listener needs to be implemented
    @Nonnull
    private TaggingPreset[] presets = new TaggingPreset[0];

    /**
     * Create an ObjectDetection
     *
     * @param key The key for the detection
     * @param tagMap The OSM key(s) for the object
     * @param taggingPresetType The valid types for the preset
     * @param additionalCommands Additional commands to run
     * @param detectionTypes The types that the detection <i>may</i> appear as
     * @param dataType The type of the data (AKA, how ready the detection is)
     * @param hasImage {@code true} if there is an image
     */
    private ObjectDetections(@Nonnull String key, @Nullable TagMap tagMap,
        @Nullable TaggingPresetType[] taggingPresetType,
        @Nullable SerializableSupplier<AdditionalInstructions> additionalCommands,
        @Nullable DetectionType[] detectionTypes, @Nonnull DataType dataType, boolean hasImage) {
        this.key = key;
        this.detectionTypes = detectionTypes != null ? detectionTypes : Constants.NO_DETECTION_TYPE;
        this.osmKey = tagMap != null ? tagMap : Collections.emptyMap();
        this.taggingPresetType = taggingPresetType != null ? Arrays.asList(taggingPresetType) : Collections.emptyList();
        this.additionalCommands = additionalCommands;
        this.dataType = dataType;
        ImageProvider foundIcon = null;
        final boolean trace = Logging.isTraceEnabled();

        if (hasImage || trace) {
            if (!hasImage) {
                Logging.trace("Checking if an icon for {0} exists", this.key);
            }
            for (DetectionType type : this.getDetectionTypes()) {
                if (type.getImageLocationString() == null)
                    continue;
                MapCSSStyleSource source = PointObjectLayer.getMapCSSStyle();
                MapPaintStyles.IconReference iconReference = new MapPaintStyles.IconReference(
                    type.getImageLocationString() + '/' + this.getKey(), source);
                foundIcon = MapPaintStyles.getIconProvider(iconReference, false);
                if (foundIcon != null) {
                    foundIcon.setOptional(!hasImage);
                }
            }
        }
        if (trace && foundIcon != null) {
            foundIcon.getAsync(imageIcon -> {
                if (hasImage && imageIcon == null) {
                    BugReport.intercept(new IllegalStateException("No icon found for " + key)).warn();
                } else if (!hasImage && imageIcon != null) {
                    BugReport.intercept(new IllegalStateException("Icon found for " + key)).warn();
                }
            });
        }
        if (!hasImage) {
            this.icon = null;
        } else {
            this.icon = foundIcon;
        }
        this.updateMappingPresets();
    }

    /**
     * Convert a string detection into an enum
     *
     * @param detection The detection to get an enum for
     * @return The enum
     */
    public static ObjectDetections getDetection(String detection) {
        for (ObjectDetections value : values()) {
            if (value.getKey().equals(detection)) {
                return value;
            }
        }
        return null;
    }

    /**
     * Get fallback detections for a detection
     *
     * @param detection The detection to find a fallback for
     * @return A (somewhat) specific fallback detection <i>or</i> {@link #UNKNOWN}, with the boolean indicating if it
     *         was rejected.
     */
    public static Pair<Boolean, ObjectDetections> findFallbackDetection(String detection) {
        ObjectDetections objectDetection = getDetection(detection);
        if (objectDetection != null) {
            return new Pair<>(false, objectDetection);
        }
        if (detection.endsWith("--reject")) {
            Pair<Boolean, ObjectDetections> pair = findFallbackDetection(
                detection.substring(0, detection.length() - "--reject".length()));
            pair.a = true;
            return pair;
        }
        // Don't log void-- detections as unknown (we don't care about them)
        if (!detection.startsWith("void--")) {
            Logging.error("Unknown detection \"" + detection + "\"");
        }
        String toFind = detection.replaceAll("--g\\d+$", "");
        return new Pair<>(false, Stream.of(ObjectDetections.values()).filter(d -> d.getKey().contains(toFind))
            .findFirst().orElse(ObjectDetections.UNKNOWN));
    }

    /**
     * Get additional instructions
     *
     * @return The additional instructions
     */
    public AdditionalInstructions getAdditionalInstructions() {
        if (this.additionalCommands != null) {
            return this.additionalCommands.get();
        }
        return null;
    }

    /**
     * Get the actual key used by Mapillary
     *
     * @return The key used for object detections
     */
    @Nonnull
    public String getKey() {
        return this.key;
    }

    /**
     * Check if this detection has OSM keys
     *
     * @return {@code true} if there is an OSM equivalent in presets
     */
    public boolean hasOsmKeys() {
        return !this.osmKey.isEmpty();
    }

    /**
     * Get the tag map
     *
     * @return The OSM tag map
     */
    public Map<String, String> getOsmKeys() {
        return Collections.unmodifiableMap(this.osmKey);
    }

    /**
     * Get the valid detection types for the object
     *
     * @return The detection types that the object <i>may</i> appear in
     */
    public Collection<DetectionType> getDetectionTypes() {
        if (this.detectionTypes.length == 0) {
            return Collections.emptyList();
        }
        return Collections.unmodifiableCollection(Arrays.asList(this.detectionTypes));
    }

    /**
     * Update presets for a set key/value
     */
    private void updateMappingPresets() {
        if (!this.osmKey.isEmpty()) {
            this.presets = TaggingPresets.getMatchingPresets(this.taggingPresetType, this.osmKey, false)
                .toArray(this.presets);
        } else {
            this.presets = new TaggingPreset[0];
        }
    }

    /**
     * Get tagging presets for a specific Mapillary detection key
     *
     * @param mapillaryKey The Mapillary detection key to get presets for
     * @return A list of OSM presets for the Mapillary key
     */
    public static TaggingPreset[] getTaggingPresetsFor(String mapillaryKey) {
        return Stream.of(ObjectDetections.values()).filter(detection -> !detection.osmKey.isEmpty())
            .filter(mapping -> mapping.key.equals(mapillaryKey)).flatMap(mapping -> Stream.of(mapping.presets))
            .toArray(TaggingPreset[]::new);
    }

    /**
     * Get the tagging presets for this ObjectDetect
     *
     * @return A list of tagging presets
     */
    public Collection<TaggingPreset> getTaggingPresets() {
        return Collections.unmodifiableCollection(Arrays.asList(this.presets));
    }

    /**
     * Update presets for each key
     */
    public static void updatePresets() {
        Stream.of(values()).forEach(ObjectDetections::updateMappingPresets);
    }

    /**
     * Get the base key
     *
     * @return The base key without subcategories (specifically, without "--gNUMBER")
     */
    public String getBaseKey() {
        return getKey().replaceAll("--g\\d*$", "");
    }

    /**
     * Check if the object is addable
     *
     * @return {@code true} if the object should be addable
     */
    public boolean shouldBeAddable() {
        return this.dataType.shouldBeVisible();
    }

    /**
     * Convert a Mapillary object detection value to an enum
     *
     * @param value The value to convert
     * @return The enum
     */
    public static ObjectDetections valueOfMapillaryValue(String value) {
        return VALUES.stream().filter(det -> det.key.equals(value)).findAny().orElse(UNKNOWN);
    }

    /**
     * Get the values for object detections
     *
     * @return The loaded detections (don't modify this array)
     */
    public static ObjectDetections[] values() {
        if (cachedValuesArray == null || cachedValuesArray.length != VALUES.size()) {
            cachedValuesArray = VALUES.toArray(new ObjectDetections[0]);
        }
        return cachedValuesArray;
    }

    /**
     * Load object detections from a file
     *
     * @param expectedSize The expected size of the list to avoid allocations
     * @param fileNames The base file name to load ({@code "detections/" + file + ".json"})
     * @return The loaded detections
     */
    private static List<ObjectDetections> loadObjectDetections(int expectedSize, String... fileNames) {
        ArrayList<ObjectDetections> detections = new ArrayList<>();
        for (String file : fileNames) {
            try (InputStream inputStream = ResourceProvider.getResourceAsStream("detections/" + file + ".json");
                JsonParser parser = Json.createParser(inputStream)) {
                while (parser.hasNext()) {
                    if (parser.next() == JsonParser.Event.START_OBJECT) {
                        JsonObject object = parser.getObject();
                        TaggingPresetType[] types = null;
                        if (object.containsKey("tagging_preset_type")) {
                            JsonArray t = object.getJsonArray("tagging_preset_type");
                            types = t.stream().filter(JsonString.class::isInstance).map(JsonString.class::cast)
                                .map(JsonString::getString).map(String::toLowerCase).map(TaggingPresetType::fromString)
                                .toArray(TaggingPresetType[]::new);
                        }
                        SerializableSupplier<AdditionalInstructions> additionalInstructionsSupplier = null;
                        if (object.containsKey("additional_commands")) {
                            List<String> commands = object.getJsonArray("additional_commands")
                                .getValuesAs(s -> ((JsonString) s).getString());
                            if (commands.size() != 1) {
                                throw new IllegalStateException("Json for additional_commands cannot be larger than 1: "
                                    + String.join(";", commands));
                            }
                            for (String command : commands) {
                                if ("SnapToRoad".equals(command)) {
                                    additionalInstructionsSupplier = AdditionalInstructions.SnapToRoad::new;
                                } else if ("AddSecondaryTagsToRoad".equals(command)) {
                                    additionalInstructionsSupplier = AdditionalInstructions.AddSecondaryTagsToRoad::new;
                                } else {
                                    throw new IllegalStateException(
                                        "Unknown json value for additional_commands: " + command);
                                }
                            }
                        }
                        DetectionType[] detectionTypes = null;
                        if (object.containsKey("detection_type")) {
                            JsonArray t = object.getJsonArray("detection_type");
                            detectionTypes = t.stream().filter(JsonString.class::isInstance).map(JsonString.class::cast)
                                .map(JsonString::getString).map(String::toUpperCase).map(DetectionType::valueOf)
                                .toArray(DetectionType[]::new);
                        }
                        DataType dataType = DataType
                            .valueOf(object.getString("data_type", "PRODUCTION").toUpperCase(Locale.ROOT));
                        boolean hasImage = object.getBoolean("has_image", true);
                        TagMap tagMap = null;
                        if (object.containsKey("osm_key")) {
                            tagMap = new TagMap();
                            for (JsonString entry : object.getJsonArray("osm_key").getValuesAs(JsonString.class)) {
                                Tag tag = Tag.ofString(entry.getString());
                                tagMap.put(tag.getKey(), tag.getValue());
                            }
                        }
                        ObjectDetections detection = new ObjectDetections(object.getString("type"), tagMap, types,
                            additionalInstructionsSupplier, detectionTypes, dataType, hasImage);
                        detections.add(detection);
                    }
                }
            } catch (IOException e) {
                throw new JosmRuntimeException(e);
            }
        }
        detections.trimToSize();
        if (expectedSize != detections.size()) {
            throw new IllegalStateException("Set the appropriate size for " + String.join(";", fileNames));
        }
        return detections;
    }

    /**
     * Get the icon for the detection
     *
     * @return The icon
     */
    @Nonnull
    public ImageIcon getIcon() {
        if (this.icon == null) {
            return NO_ICON;
        }
        ImageIcon rImage = this.icon.get();
        if (rImage == null) {
            return NO_ICON;
        }
        return rImage;
    }

    /**
     * Get the image provider for this
     *
     * @return The image provider
     */
    @Nullable
    public ImageProvider getImageProvider() {
        return this.icon;
    }

    @Override
    public String toString() {
        return this.key;
    }

    /**
     * Useful constants
     */
    private static class Constants {
        private static final DetectionType[] NO_DETECTION_TYPE = new DetectionType[0];
    }

    @FunctionalInterface
    private interface SerializableSupplier<T> extends Serializable, Supplier<T> {
        // Empty
    }
}
