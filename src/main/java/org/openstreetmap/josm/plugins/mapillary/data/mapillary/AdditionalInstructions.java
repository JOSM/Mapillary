// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.openstreetmap.josm.command.ChangeNodesCommand;
import org.openstreetmap.josm.command.Command;
import org.openstreetmap.josm.command.MoveCommand;
import org.openstreetmap.josm.command.SequenceCommand;
import org.openstreetmap.josm.data.coor.EastNorth;
import org.openstreetmap.josm.data.osm.BBox;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.Way;
import org.openstreetmap.josm.data.osm.WaySegment;
import org.openstreetmap.josm.data.projection.ProjectionRegistry;
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

  class SnapToRoad implements AdditionalInstructions {

    @Override
    public Command apply(final OsmPrimitive primitive) {
      if (primitive.getDataSet() == null || !(primitive instanceof Node)) {
        return null;
      }
      final BBox searchBBox = primitive.getBBox();
      searchBBox.addPrimitive(primitive, 0.001); // ~111m at equator
      Collection<Way> highways = primitive.getDataSet().searchWays(searchBBox).stream()
        .filter(way -> way.hasKey("highway")).collect(Collectors.toList());
      if (highways.isEmpty())
        return null;
      final Way nearest = Geometry.getClosestPrimitive(primitive, highways);
      final WaySegment nearestSegment = Geometry.getClosestWaySegment(nearest, primitive);
      final EastNorth nearestPointSegment = Geometry.closestPointToSegment(nearestSegment.getFirstNode().getEastNorth(),
        nearestSegment.getSecondNode().getEastNorth(), ((Node) primitive).getEastNorth());
      final Command moveCommand = new MoveCommand((Node) primitive,
        ProjectionRegistry.getProjection().eastNorth2latlon(nearestPointSegment));
      final List<Node> nodes = nearest.getNodes();
      nodes.add(nearestSegment.getUpperIndex(), (Node) primitive);
      final Command addToWayCommand = new ChangeNodesCommand(nearest, nodes);
      return SequenceCommand.wrapIfNeeded(tr("Snap to highway"), moveCommand, addToWayCommand);
    }

  }
}
