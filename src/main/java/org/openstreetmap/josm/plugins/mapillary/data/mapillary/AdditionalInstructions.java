// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.annotation.Nonnull;

import org.openstreetmap.josm.command.ChangeNodesCommand;
import org.openstreetmap.josm.command.ChangePropertyCommand;
import org.openstreetmap.josm.command.Command;
import org.openstreetmap.josm.command.MoveCommand;
import org.openstreetmap.josm.command.SequenceCommand;
import org.openstreetmap.josm.data.coor.EastNorth;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.IWaySegment;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.TagMap;
import org.openstreetmap.josm.data.osm.Way;
import org.openstreetmap.josm.data.projection.ProjectionRegistry;
import org.openstreetmap.josm.gui.tagging.presets.TaggingPreset;
import org.openstreetmap.josm.gui.tagging.presets.TaggingPresetItem;
import org.openstreetmap.josm.gui.tagging.presets.TaggingPresets;
import org.openstreetmap.josm.gui.tagging.presets.items.KeyedItem;
import org.openstreetmap.josm.spi.preferences.Config;
import org.openstreetmap.josm.tools.Geometry;

/**
 * Additional instructions for ObjectDetections
 */
public interface AdditionalInstructions {
    /**
     * Create an additional command to be run on the primitive
     *
     * @param primitive The primitive to make the command for
     * @return An additional command to run on the primitive, or {@code null} if nothing could be run.
     */
    Command apply(final OsmPrimitive primitive);

    /**
     * Put the detection on the nearest road
     */
    class SnapToRoad implements AdditionalInstructions {

        @Override
        public Command apply(final OsmPrimitive primitive) {
            if (!(primitive instanceof Node) || primitive.getDataSet() == null) {
                return null;
            }

            Optional<Way> nearest = getClosestHighway((Node) primitive);
            if (!nearest.isPresent()) {
                return null;
            }
            final IWaySegment<?, ?> nearestSegment = Geometry.getClosestWaySegment(nearest.get(), primitive);
            final EastNorth nearestPointSegment = Geometry.closestPointToSegment(
                nearestSegment.getFirstNode().getEastNorth(), nearestSegment.getSecondNode().getEastNorth(),
                ((Node) primitive).getEastNorth());
            final Command moveCommand = new MoveCommand((Node) primitive,
                ProjectionRegistry.getProjection().eastNorth2latlon(nearestPointSegment));
            final List<Node> nodes = nearest.get().getNodes();
            nodes.add(nearestSegment.getUpperIndex(), (Node) primitive);
            final Command addToWayCommand = new ChangeNodesCommand(nearest.get(), nodes);
            return SequenceCommand.wrapIfNeeded(tr("Snap to highway"), moveCommand, addToWayCommand);
        }

        static Optional<Way> getClosestHighway(Node primitive) {
            final BBox searchBBox = primitive.getBBox();
            final double distance = 100 * ProjectionRegistry.getProjection().getMetersPerUnit();
            searchBBox.add(primitive);
            searchBBox.add(Geometry.getLatLonFrom(primitive, Math.PI / 4, distance));
            searchBBox.add(Geometry.getLatLonFrom(primitive, 5 * Math.PI / 4, distance));
            List<Way> highways = primitive.getDataSet().searchWays(searchBBox).stream()
                .filter(way -> way.hasKey("highway")).collect(Collectors.toList());
            highways.removeIf(way -> way.hasTag("highway", "footway", "path"));
            if (highways.isEmpty())
                return Optional.empty();
            return Optional.of(Geometry.getClosestPrimitive(primitive, highways));
        }
    }

    /**
     * Add the secondary tag to a road (this strongly depends upon the ordering of the {@code osm_key} json value)
     * Example:
     * {@code "osm_key": ["traffic_sign=maxspeed", "maxspeed=40 mph"]}
     * The secondary key is {@code maxspeed=40 mph}, which is what will be added to the road.
     * This only applies to nodes.
     */
    class AddSecondaryTagsToRoad implements AdditionalInstructions {
        private static final Pattern PATTERN_FLOAT = Pattern.compile("^\\d*\\.?\\d*$");

        @Override
        public Command apply(OsmPrimitive primitive) {
            if (!(primitive instanceof Node) || primitive.getDataSet() == null) {
                return null;
            }
            Optional<Way> closestHighway = SnapToRoad.getClosestHighway((Node) primitive);
            Collection<TaggingPreset> presets = TaggingPresets.getMatchingPresets(primitive);
            final String dirString = primitive.get("direction");
            if (closestHighway.isPresent() && !presets.isEmpty() && dirString != null
                && PATTERN_FLOAT.matcher(dirString).matches()
                && correctDirection(closestHighway.get(), primitive, dirString)) {
                TagMap tags = new TagMap();

                for (TaggingPreset p : presets) {
                    for (TaggingPresetItem item : p.data) {
                        if (item instanceof KeyedItem) {
                            KeyedItem keyedItem = (KeyedItem) item;
                            if (!keyedItem.isKeyRequired() && primitive.hasKey(keyedItem.key)
                                && !closestHighway.get().hasKey(keyedItem.key) && !"direction".equals(keyedItem.key)) {
                                tags.put(keyedItem.key, primitive.get(keyedItem.key));
                            }
                        }
                    }

                }
                return new ChangePropertyCommand(Collections.singleton(closestHighway.get()), tags);
            }
            return null;
        }

        /**
         * Ensure that the sign likely belongs to the highway
         *
         * @param closestHighway The closest highway
         * @param primitive The primitive with the tags
         * @param dirString The direction from the primitive with the tags
         * @return {@code true} if it is possible that the sign belongs with the highway
         */
        static boolean correctDirection(@Nonnull Way closestHighway, @Nonnull OsmPrimitive primitive,
            @Nonnull String dirString) {
            final IWaySegment<?, ?> nearestSegment = Geometry.getClosestWaySegment(closestHighway, primitive);
            double heading = nearestSegment.getFirstNode().getEastNorth()
                .heading(nearestSegment.getSecondNode().getEastNorth());
            double direction = Math.toRadians(Double.parseDouble(dirString));
            final double difference;
            switch (closestHighway.isOneway()) {
            case 1: // Standard oneway, example: 270 - 104 - 180 = -14 degrees
                difference = Math.abs(heading - direction - Math.PI);
                break;
            case 0: // Not a oneway, see oneways for examples
                difference = Math.min(Math.abs(heading - direction), Math.abs(heading - direction - Math.PI));
                break;
            case -1: // Reversed oneway, example: 90 - 104 = -14 degrees
                difference = Math.abs(heading - direction);
                break;
            default:
                throw new IllegalStateException("Oneway is an unknown value: " + closestHighway.isOneway());
            }
            return Math.abs(Math.toDegrees(difference)) < Config.getPref()
                .getDouble("mapillary.smart-add.max-angle-add-secondary-tags-to-road", 30);
        }
    }
}
