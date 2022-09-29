// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import java.time.Duration;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.BinaryOperator;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import org.openstreetmap.josm.data.coor.EastNorth;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.tools.Pair;

/**
 * Navigate to different images
 */
public class ImageNavigation {
    /**
     * The direction of images relative to the initial image
     */
    public enum Direction {
        FORWARD, BACKWARD, LEFT, RIGHT, TURN_LEFT, TURN_RIGHT, U_TURN, THREE_SIXTY, UNKNOWN
    }

    private final Map<Direction, VectorNode> originalSort;
    private final Map<Direction, VectorNode> computedSort;
    private final MapillaryNode node;

    private final VectorDataSet dataset;

    /**
     * Create a new {@link ImageNavigation} object
     *
     * @param dataset The dataset to use
     * @param node The node to use
     */
    public ImageNavigation(final VectorDataSet dataset, final MapillaryNode node) {
        Objects.requireNonNull(dataset);
        Objects.requireNonNull(node);
        this.dataset = dataset;
        this.node = node;
        final BBox bbox = new BBox(node);
        bbox.addPrimitive(node, 0.001);

        Set<VectorNode> surroundingNodes = new HashSet<>(dataset.searchNodes(bbox));
        surroundingNodes.removeIf(n -> n.getUniqueId() == node.getUniqueId());
        final String sequenceKey = MapillaryImageUtils.getSequenceKey(this.node);
        surroundingNodes.removeIf(n -> Objects.equals(MapillaryImageUtils.getSequenceKey(n), sequenceKey));
        // Sort the surrounding nodes into 90 degree buckets
        final double angle = Optional.ofNullable(node.get(MapillaryImageUtils.ImageProperties.COMPASS_ANGLE.toString()))
            .map(Double::parseDouble).orElse(Double.NaN);
        final double computed = Optional
            .ofNullable(node.get(MapillaryImageUtils.ImageProperties.COMPUTED_COMPASS_ANGLE.toString()))
            .map(Double::parseDouble).orElse(Double.NaN);
        final boolean isPano = MapillaryImageUtils.IS_PANORAMIC.test(node);
        // Needed to avoid an NPE
        final VectorNode defaultNode = new VectorNode("");
        defaultNode.setCoor(new LatLon(90, LatLon.normalizeLon(node.lon() + 180)));
        final Collector<Pair<Direction, VectorNode>, ?, VectorNode> reducer = Collectors.reducing(defaultNode, p -> p.b,
            BinaryOperator.minBy(Comparator.comparingDouble(n -> getPriority(this.node, n))));
        final EastNorth nodeEn = node.getEastNorth();
        final double travelAngle;
        MapillarySequence seq = this.node.getSequence();
        if (Objects.equals(this.node, seq.firstNode()) && seq.getNodes().size() > 1) {
            travelAngle = Math.toDegrees(this.node.bearing(seq.getNode(1)));
        } else if (Objects.equals(this.node, seq.lastNode()) && seq.getNodes().size() > 1) {
            travelAngle = Math.toDegrees(seq.getNode(seq.getNodesCount() - 1).bearing(this.node));
        } else {
            int idx = seq.getNodes().indexOf(this.node);
            if (seq.getNodes().size() <= 3) {
                travelAngle = MapillaryImageUtils.getAngle(this.node);
            } else {
                travelAngle = Math.toDegrees(seq.getNode(idx - 1).bearing(seq.getNode(idx + 1)));
            }
        }
        this.originalSort = surroundingNodes.stream().map(n -> sort(isPano, false, angle, travelAngle, n, nodeEn))
            .filter(Objects::nonNull)
            .collect(Collectors.groupingBy(p -> p.a, () -> new EnumMap<>(Direction.class), reducer));
        this.computedSort = surroundingNodes.stream().map(n -> sort(isPano, true, computed, travelAngle, n, nodeEn))
            .filter(Objects::nonNull)
            .collect(Collectors.groupingBy(p -> p.a, () -> new EnumMap<>(Direction.class), reducer));
        clearDefault(this.originalSort, defaultNode);
        clearDefault(this.computedSort, defaultNode);
    }

    /**
     * Prefer images that are nearby and close in time
     *
     * @param current The current node
     * @param other The node to get a priority for
     * @return The priority of the other node. The lower, the better.
     */
    private static double getPriority(INode current, INode other) {
        double monthMillis = Duration.ofDays(30).toMillis();
        long timeDelta = Math.abs(other.getInstant().toEpochMilli() - current.getInstant().toEpochMilli());
        double distance = current.greatCircleDistance(other);
        return distance + timeDelta / monthMillis;
    }

    private static void clearDefault(Map<Direction, VectorNode> map, VectorNode defaultNode) {
        List<Direction> keys = map.entrySet().stream().filter(e -> Objects.equals(defaultNode, e.getValue()))
            .map(Map.Entry::getKey).collect(Collectors.toList());
        keys.forEach(map::remove);
    }

    /**
     * Get the appropriate image for a specified direction
     *
     * @param direction The direction to look at
     * @return The node, if any
     */
    public Optional<INode> getImage(Direction direction) {
        MapillarySequence sequence = this.node.getSequence();
        // Prefer next/previous in the current sequence
        if (sequence != null) {
            if (direction == Direction.FORWARD && !Objects.equals(this.node, sequence.lastNode())) {
                return Optional.ofNullable(
                    MapillarySequenceUtils.getNextOrPrevious(this.node, MapillarySequenceUtils.NextOrPrevious.NEXT));
            }
            if (direction == Direction.BACKWARD && !Objects.equals(this.node, sequence.firstNode())) {
                return Optional.ofNullable(MapillarySequenceUtils.getNextOrPrevious(this.node,
                    MapillarySequenceUtils.NextOrPrevious.PREVIOUS));
            }
        }
        if (useComputed()) {
            return Optional.ofNullable(this.computedSort.getOrDefault(direction, null));
        }
        return Optional.ofNullable(this.originalSort.getOrDefault(direction, null));
    }

    /**
     * Get the dataset used to compute the nearby nodes
     *
     * @return The dataset used
     */
    public VectorDataSet getDataset() {
        return this.dataset;
    }

    private static boolean useComputed() {
        return Boolean.TRUE.equals(MapillaryProperties.USE_COMPUTED_LOCATIONS.get());
    }

    /**
     * A helper method to sort images into buckets
     *
     * @param isPano if the original image is a panoramic image
     * @param computed {@code true} to use computed angles
     * @param angle The angle to compare against
     * @param travelAngle The angle of travel
     * @param image The image we are sorting
     * @param nodeEn The location of the image we are sorting for
     * @return A pair of direction, node
     */
    private static Pair<Direction, VectorNode> sort(boolean isPano, boolean computed, double angle, double travelAngle,
        VectorNode image, EastNorth nodeEn) {
        if (!isPano && MapillaryImageUtils.IS_PANORAMIC.test(image)) {
            return new Pair<>(Direction.THREE_SIXTY, image);
        }
        String prop = computed ? MapillaryImageUtils.ImageProperties.COMPUTED_COMPASS_ANGLE.toString()
            : MapillaryImageUtils.ImageProperties.COMPASS_ANGLE.toString();
        final double imageAngle = Optional.ofNullable(image.get(prop)).map(Double::parseDouble).orElseGet(
            () -> Optional.ofNullable(image.get(MapillaryImageUtils.ImageProperties.COMPASS_ANGLE.toString()))
                .map(Double::parseDouble).orElse(Double.NaN));
        if (Double.isNaN(imageAngle)) {
            return null;
        }
        final double difference = normalizeAngle(imageAngle - angle);
        if (difference < 45 || difference >= 315) {
            // This gets us the heading from north to the other image.
            double heading = Math.toDegrees(nodeEn.heading(image.getEastNorth()));
            // But we need to compare that with the travel path.
            heading = normalizeAngle(heading - angle);
            if (heading < 45 || heading >= 315) {
                return new Pair<>(Direction.FORWARD, image);
            } else if (heading < 135) {
                return new Pair<>(Direction.RIGHT, image);
            } else if (heading < 225) {
                return new Pair<>(Direction.BACKWARD, image);
            } else if (heading < 315) {
                return new Pair<>(Direction.LEFT, image);
            }
        } else if (difference < 135) {
            return new Pair<>(Direction.TURN_RIGHT, image);
        } else if (difference < 225) {
            return new Pair<>(Direction.U_TURN, image);
        } else if (difference < 315) {
            return new Pair<>(Direction.TURN_LEFT, image);
        }
        return new Pair<>(Direction.UNKNOWN, image);
    }

    /**
     * Normalize an angle (degrees)
     *
     * @param a The angle to normalize
     * @return The normalized angle
     */
    private static double normalizeAngle(double a) {
        return a - 360 * Math.floor(a / 360);
    }
}
