// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils.api;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.function.Function;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.bugreport.BugReport;
import org.openstreetmap.josm.tools.bugreport.ReportedException;

/**
 * Decode json objects
 */
public final class JsonDecoder {
    private static final double[] EMPTY_DOUBLE = new double[0];

    private JsonDecoder() {
        // Private constructor to avoid instantiation
    }

    /**
     * Parses a given {@link JsonObject} as a data entity into a {@link Collection}
     * of the desired Java objects. The method, which converts the data features into Java objects
     * is given as a parameter to this method.
     *
     * @param <T> feature type
     * @param json the {@link JsonObject} to be parsed
     * @param featureDecoder feature decoder which transforms JSON objects to Java objects
     * @return a {@link Collection} which is parsed from the given {@link JsonValue}, which contains the data object.
     *         The return value will not be <code>null</code>.
     */
    @Nonnull
    public static <T> Collection<T> decodeData(@Nonnull final JsonObject json,
        @Nonnull final Function<JsonValue, Collection<T>> featureDecoder) {
        Objects.requireNonNull(json, "JSON cannot be null");
        Objects.requireNonNull(featureDecoder, "The features must be decoded into something.");
        // Check if there was an error -- this could be due to API key limits, or some other issue.
        if (json.containsKey("error")) {
            Logging.error("Mapillary API error: {0}", json);
            return Collections.emptyList();
        }
        // This is deliberate -- we want to use the "data" value, if present
        final JsonValue data = json.getOrDefault("data", json);
        if (data.getValueType() != JsonValue.ValueType.ARRAY && data.getValueType() != JsonValue.ValueType.OBJECT) {
            throw new IllegalArgumentException("Mapillary v4 json data objects must either be an object or an array.");
        }
        try {
            return Collections.unmodifiableCollection(featureDecoder.apply(data));
        } catch (Exception e) {
            Logging.error(e);
            GuiHelper.runInEDT(() -> {
                final ReportedException bugReport = BugReport.intercept(e);
                bugReport.put("json", json);
                bugReport.warn();
            });
        }
        return Collections.emptyList();
    }

    /**
     * Decodes a {@link JsonArray} of exactly size 2 to a {@link LatLon} instance.
     * The first value in the {@link JsonArray} is treated as longitude, the second one as latitude.
     *
     * @param json the {@link JsonArray} containing the two numbers
     * @return the decoded {@link LatLon} instance, or <code>null</code> if the parameter is
     *         not a {@link JsonArray} of exactly size 2 containing two {@link JsonNumber}s.
     */
    @Nullable
    static LatLon decodeLatLon(final JsonArray json) {
        final double[] result = decodeDoublePair(json);
        if (!Arrays.equals(EMPTY_DOUBLE, result)) {
            return new LatLon(result[1], result[0]);
        }
        return null;
    }

    /**
     * Decodes a pair of double values, which are stored in a {@link JsonArray} of exactly size 2.
     *
     * @param json the {@link JsonArray} containing the two values
     * @return a double array which contains the two values in the same order, or an empty array
     *         if the parameter was not a {@link JsonArray} of exactly size 2 containing two {@link JsonNumber}s
     */
    @Nonnull
    static double[] decodeDoublePair(final JsonArray json) {
        if (json != null && json.size() == 2 && json.get(0) instanceof JsonNumber
            && json.get(1) instanceof JsonNumber) {
            return new double[] { json.getJsonNumber(0).doubleValue(), json.getJsonNumber(1).doubleValue() };
        }
        return EMPTY_DOUBLE;
    }
}
