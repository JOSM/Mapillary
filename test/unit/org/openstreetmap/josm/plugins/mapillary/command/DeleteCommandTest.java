package org.openstreetmap.josm.plugins.mapillary.command;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

@BasicPreferences
class DeleteCommandTest {
  @Test
  void testDeleteCommand() {
    final DataSet dataSet = new DataSet();
    final OsmPrimitive primitive = new Node(LatLon.ZERO);
    dataSet.addPrimitive(primitive);
    DeleteCommand<?, ?, ?, ?, ?> deleteCommand = new DeleteCommand<>(primitive.getDataSet(), primitive);
    assertFalse(primitive.isDeleted());
    deleteCommand.executeCommand();
    assertTrue(primitive.isDeleted());
    deleteCommand.undoCommand();
    assertFalse(primitive.isDeleted());
    assertSame(dataSet, deleteCommand.getAffectedDataSet());
  }

  @Test
  void testDescriptionText() {
    final DataSet dataSet = new DataSet();
    final OsmPrimitive primitive = new Node(LatLon.ZERO);
    dataSet.addPrimitive(primitive);
    DeleteCommand<?, ?, ?, ?, ?> deleteCommand = new DeleteCommand<>(primitive.getDataSet(), primitive);
    assertEquals("Delete Mapillary object", deleteCommand.getDescriptionText());
  }

  @Test
  void testParticipatingPrimitives() {
    final DataSet dataSet = new DataSet();
    final OsmPrimitive primitive = new Node(LatLon.ZERO);
    dataSet.addPrimitive(primitive);
    DeleteCommand<?, ?, ?, ?, ?> deleteCommand = new DeleteCommand<>(primitive.getDataSet(), primitive);

    assertEquals(deleteCommand.getParticipatingPrimitives(), deleteCommand.getParticipatingIPrimitives());
    assertEquals(1, deleteCommand.getParticipatingPrimitives().size());
    assertSame(primitive, deleteCommand.getParticipatingPrimitives().iterator().next());
  }

  @Test
  void testParticipatingIPrimitives() {
    final VectorDataSet vectorDataSet = new VectorDataSet();
    final VectorNode vectorNode = new VectorNode("test");
    vectorNode.setCoor(LatLon.ZERO);
    vectorDataSet.addPrimitive(vectorNode);
    DeleteCommand<?, ?, ?, ?, ?> deleteCommand = new DeleteCommand<>(vectorNode.getDataSet(), vectorNode);

    assertNotEquals(deleteCommand.getParticipatingPrimitives(), deleteCommand.getParticipatingIPrimitives());
    assertTrue(deleteCommand.getParticipatingPrimitives().isEmpty());
    assertEquals(1, deleteCommand.getParticipatingIPrimitives().size());
    assertSame(vectorNode, deleteCommand.getParticipatingIPrimitives().iterator().next());
  }

  @Test
  void testLockedDataSet() {
    final DataSet dataSet = new DataSet();
    final OsmPrimitive primitive = new Node(LatLon.ZERO);
    dataSet.addPrimitive(primitive);
    dataSet.lock();
    DeleteCommand<?, ?, ?, ?, ?> deleteCommand = new DeleteCommand<>(primitive.getDataSet(), primitive);
    assertTrue(dataSet.isLocked());
    assertFalse(primitive.isDeleted());
    assertTrue(dataSet.isLocked());
    deleteCommand.executeCommand();
    assertTrue(primitive.isDeleted());
    assertTrue(dataSet.isLocked());
    deleteCommand.undoCommand();
    assertFalse(primitive.isDeleted());
    assertTrue(dataSet.isLocked());
    assertSame(dataSet, deleteCommand.getAffectedDataSet());
  }
}
