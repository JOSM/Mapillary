// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.remotecontrol;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.openstreetmap.josm.actions.AutoScaleAction;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.io.remotecontrol.PermissionPrefWithDefault;
import org.openstreetmap.josm.io.remotecontrol.handler.RequestHandler;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;

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
        Collection<MapillaryNode> nodes = new HashSet<>();
        if (mapillaryImages.length > 0) {
            Map<String, Collection<MapillaryNode>> images = GuiHelper
                .runInEDTAndWaitAndReturn(() -> MapillaryDownloader.downloadImages(mapillaryImages));
            mapillarySequences.addAll(images.keySet());
            images.values().forEach(nodes::addAll);
            List<MapillaryNode> addedImages = images.entrySet().stream().flatMap(entry -> entry.getValue().stream())
                .collect(Collectors.toList());
            if (addedImages.size() == 1) {
                GuiHelper
                    .runInEDTAndWait(() -> MapillaryLayer.getInstance().setCurrentImage(addedImages.iterator().next()));
                // TODO zoom to selected image?
            }
        }
        mapillarySequences.removeIf(string -> string.trim().isEmpty());
        if (!mapillarySequences.isEmpty()) {
            List<MapillaryNode> tNodes = Optional
                .ofNullable(GuiHelper.runInEDTAndWaitAndReturn(
                    () -> MapillaryDownloader.downloadSequences(mapillarySequences.toArray(new String[0])).stream()
                        .flatMap(seq -> seq.getNodes().stream()).collect(Collectors.toList())))
                .orElseGet(Collections::emptyList);
            if (nodes.isEmpty()) {
                nodes.addAll(tNodes);
                if (!tNodes.isEmpty()) {
                    GuiHelper.runInEDTAndWait(() -> MapillaryLayer.getInstance().setCurrentImage(tNodes.get(0)));
                }
            }
        }
        nodes.removeIf(Objects::isNull);
        if (!nodes.isEmpty()) {
            GuiHelper.runInEDT(() -> AutoScaleAction.zoomTo(nodes));
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
