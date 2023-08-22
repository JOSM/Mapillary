// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary.smartedit;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import jakarta.json.Json;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.stream.JsonParser;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.tools.JosmRuntimeException;

/**
 * A temporary class to ignore objects
 *
 * @deprecated This is temporary (whenever Mapillary adds a reporting function, this class will be removed)
 */
@Deprecated
public final class IgnoredObjects {
    private static long[] ignoredObjs;

    private IgnoredObjects() {
    }

    /**
     * Ignore an object
     *
     * @param object The object to ignore
     */
    public static void addIgnoredObject(JsonObject object) {
        // First add it to the list
        long id = object.getJsonNumber("object_id").longValue();
        if (ignoredObjs == null) {
            ignoredObjs = new long[] { id };
        } else {
            int insertion = Arrays.binarySearch(ignoredObjs, id);
            // OK. Time to insert.
            if (insertion < 0) {
                // -(insertion point) - 1 == insertion -> insertion + 1 == -(insertion point)
                ignoredObjs = ArrayUtils.insert(-(insertion + 1), ignoredObjs, id);
            }
        }
        // Now add it to the list
        MainApplication.worker.execute(() -> writeObject(object));
    }

    /**
     * Check if an id is ignored
     *
     * @param id The id to check
     * @return {@code true} if the user has previously ignored the object
     */
    public static boolean isIgnoredObject(long id) {
        if (ignoredObjs == null) {
            MainApplication.worker.execute(IgnoredObjects::load);
            return false;
        }
        return Arrays.binarySearch(ignoredObjs, id) >= 0;
    }

    private static File getFile() {
        return new File(MapillaryConfig.getPluginDirs().getUserDataDirectory(true), "ignored_detections.json");

    }

    private static void load() {
        File data = getFile();
        if (data.exists() && data.isFile() && data.canRead()) {
            Set<Long> ignored = new HashSet<>();
            try (Reader reader = new WrappedBufferedReader(Files.newBufferedReader(data.toPath()));
                JsonParser parser = Json.createParser(reader)) {
                while (parser.hasNext()) {
                    if (parser.next() == JsonParser.Event.START_OBJECT) {
                        ignored.addAll(parser.getObjectStream().filter(entry -> "object_id".equals(entry.getKey()))
                            .map(Map.Entry::getValue).filter(JsonNumber.class::isInstance).map(JsonNumber.class::cast)
                            .map(JsonNumber::longValue).collect(Collectors.toList()));
                    }
                }
            } catch (IOException e) {
                throw new JosmRuntimeException(e);
            }
            ignoredObjs = ignored.stream().mapToLong(i -> i).toArray();
            Arrays.sort(ignoredObjs);
        } else {
            ignoredObjs = new long[0];
        }
    }

    private static void writeObject(JsonObject jsonObject) {
        try {
            FileUtils.writeStringToFile(getFile(), (char) 0x1e + jsonObject.toString() + System.lineSeparator(),
                StandardCharsets.UTF_8, true);
        } catch (IOException e) {
            throw new JosmRuntimeException(e);
        }
    }

    /**
     * Helps avoid creating new json parsers
     */
    private static class WrappedBufferedReader extends Reader {
        private final BufferedReader reader;
        private boolean startedReading;

        public WrappedBufferedReader(BufferedReader newBufferedReader) {
            super(newBufferedReader);
            this.reader = newBufferedReader;
            if (!this.reader.markSupported()) {
                throw new IllegalArgumentException("BufferedReader must support marks");
            }
        }

        @Override
        public int read(char[] cbuf, int off, int len) throws IOException {
            int readChar = this.reader.read(cbuf, off, len);
            for (int i = 0; i < readChar; i++) {
                if (cbuf[i] == (char) 0x1e) {
                    if (!startedReading) {
                        startedReading = true;
                        cbuf[i] = '[';
                    } else {
                        cbuf[i] = ',';
                    }
                }
            }
            if (readChar < len && readChar >= 0) {
                cbuf[readChar] = ']';
                readChar++;
            }
            return readChar;
        }

        @Override
        public void close() throws IOException {
            this.reader.close();
        }
    }
}
