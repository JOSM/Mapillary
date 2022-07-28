// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.remotecontrol;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.openstreetmap.josm.actions.AutoScaleAction;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.io.remotecontrol.PermissionPrefWithDefault;
import org.openstreetmap.josm.io.remotecontrol.handler.RequestHandler;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.workers.MapillaryNodesDownloader;
import org.openstreetmap.josm.plugins.mapillary.gui.workers.MapillarySequenceDownloader;

/**
 * Remote Control handler for Mapillary
 */
public class MapillaryRemoteControl extends RequestHandler.RawURLParseRequestHandler {
    private static final String[] EMPTY_STRING_ARRAY = new String[0];
    private static final PermissionPrefWithDefault PERMISSION_PREF_WITH_DEFAULT = new PermissionPrefWithDefault(
        "mapillary.remote_control", true, tr("Mapillary"));

    private static final String IMAGE_STRING = "photo";
    private static final String SEQUENCE_STRING = "sequence";
    private static final String MAPILLARY_PREFIX = "Mapillary/";

    private String[] images;
    private String[] sequences;

    @Override
    public String[] getMandatoryParams() {
        return EMPTY_STRING_ARRAY;
    }

    @Override
    public String[] getOptionalParams() {
        return new String[] { IMAGE_STRING, SEQUENCE_STRING };
    }

    @Override
    public String getPermissionMessage() {
        final String br = "<br />";
        StringBuilder sb = new StringBuilder().append(tr("Remote Control has been asked to load:"));
        if (images != null && Stream.of(images).anyMatch(image -> !image.trim().isEmpty())) {
            sb.append(br).append(tr("Image: ")).append(String.join(", ", images));
        }
        if (sequences != null && Stream.of(sequences).anyMatch(sequence -> !sequence.trim().isEmpty())) {
            sb.append(br).append(tr("Sequence: ")).append(String.join(", ", sequences));
        }
        return sb.toString();
    }

    @Override
    public PermissionPrefWithDefault getPermissionPref() {
        return PERMISSION_PREF_WITH_DEFAULT;
    }

    @Override
    public String getUsage() {
        return tr("downloads street level images (currently supports {0})", "Mapillary");
    }

    @Override
    public String[] getUsageExamples() {
        return new String[] { "/photo", "/photo?photo=Mapillary/135511895288847",
            "photo?sequence=Mapillary/7nfcwfvjdtphz7yj6zat6a" };
    }

    @Override
    protected void handleRequest() throws RequestHandlerBadRequestException {
        final long[] mapillaryImages = Stream.of(images).filter(image -> image.startsWith(MAPILLARY_PREFIX))
            .map(image -> image.substring(MAPILLARY_PREFIX.length())).mapToLong(Long::parseLong).toArray();
        final List<String> mapillarySequences = Stream.of(sequences)
            .filter(sequence -> sequence.startsWith(MAPILLARY_PREFIX))
            .map(sequence -> sequence.substring(MAPILLARY_PREFIX.length()))
            .collect(Collectors.toCollection(ArrayList::new));
        if (mapillaryImages.length == 0 && mapillarySequences.isEmpty()) {
            throw new RequestHandlerBadRequestException(tr("No known image provider used"));
        }
        // This will create a mapillary layer if one does not already exist
        downloadImages(mapillaryImages);
        mapillarySequences.removeIf(string -> string.trim().isEmpty());
        downloadSequences(mapillarySequences);
    }

    /**
     * Download images
     *
     * @param mapillaryImages The images to download
     */
    private static void downloadImages(long[] mapillaryImages) {
        if (mapillaryImages.length > 0) {
            MapillaryNodesDownloader downloader = new MapillaryNodesDownloader(downloadedImages -> {
                if (downloadedImages.size() == 1) {
                    GuiHelper
                        .runInEDTAndWait(() -> MapillaryLayer.getInstance().setCurrentImage(downloadedImages.get(0)));
                    GuiHelper.runInEDT(() -> AutoScaleAction.zoomTo(downloadedImages));
                }
            }, mapillaryImages);
            downloader.execute();
        }
    }

    /**
     * Download sequences
     *
     * @param mapillarySequences The sequences to download
     */
    private static void downloadSequences(Collection<String> mapillarySequences) {
        if (!mapillarySequences.isEmpty()) {
            mapillarySequences.stream().map(seq -> new MapillarySequenceDownloader(seq, s -> {
                if (!s.isEmpty()) {
                    GuiHelper.runInEDTAndWait(() -> MapillaryLayer.getInstance().setCurrentImage(s.getNode(0)));
                }
            })).forEach(MapillarySequenceDownloader::execute);
        }
    }

    @Override
    protected void validateRequest() throws RequestHandlerBadRequestException {
        if (args != null && !args.isEmpty()) {
            images = args.getOrDefault(IMAGE_STRING, "").split(";", 0);
            sequences = args.getOrDefault(SEQUENCE_STRING, "").split(";", 0);
            if (args.containsKey(IMAGE_STRING) && !args.get(IMAGE_STRING).startsWith(MAPILLARY_PREFIX)) {
                throw new RequestHandlerBadRequestException(
                    tr("Only Mapillary images are supported at this time, use photo=Mapillary/<image_id>"));
            }
            if (args.containsKey(SEQUENCE_STRING) && !args.get(SEQUENCE_STRING).startsWith(MAPILLARY_PREFIX)) {
                throw new RequestHandlerBadRequestException(
                    tr("Only Mapillary sequences are supported at this time, use sequence=Mapillary/<sequence_id>"));
            }
        } else {
            throw new RequestHandlerBadRequestException(tr("At least one of the optional arguments must be used ({0})",
                String.join(", ", getOptionalParams())));
        }
    }
}
