package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.openstreetmap.josm.command.Command;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.Way;
import org.openstreetmap.josm.testutils.JOSMTestRules;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

/**
 * Test class for {@link AdditionalInstructions}
 *
 * @author Taylor Smock
 */
@BasicPreferences
class AdditionalInstructionsTest {
  @RegisterExtension
  static JOSMTestRules josmTestRules = new JOSMTestRules().projection();
  private DataSet dataSet;

  @BeforeEach
  void setUp() {
    this.dataSet = new DataSet();
  }

  /**
   * Test class for {@link AdditionalInstructions.SnapToRoad}
   */
  @Nested
  class SnapToRoadTest {
    AdditionalInstructions.SnapToRoad snapToRoad;

    @BeforeEach
    void setUp() {
      this.snapToRoad = new AdditionalInstructions.SnapToRoad();
    }

    @Test
    void testApplyNull() {
      assertNull(this.snapToRoad.apply(null));
      final Node node = new Node(LatLon.ZERO);
      assertNull(this.snapToRoad.apply(node));
      AdditionalInstructionsTest.this.dataSet.addPrimitive(node);
      assertNull(this.snapToRoad.apply(node));
    }

    @JOSMTestRules.OverrideAssumeRevision("18108\n")
    @Test
    void testApplyHighway() {
      this.applyHighwayCommon();
    }

    @JOSMTestRules.OverrideAssumeRevision("18109\n")
    @Test
    void testApplyHighway18109() {
      this.applyHighwayCommon();
    }

    private void applyHighwayCommon() {
      final Node node = new Node(LatLon.ZERO);
      AdditionalInstructionsTest.this.dataSet.addPrimitive(node);
      final Way way = new Way();
      way.addNode(new Node(new LatLon(0, 0.001)));
      way.addNode(new Node(new LatLon(0, -0.001)));
      way.getNodes().forEach(AdditionalInstructionsTest.this.dataSet::addPrimitive);
      AdditionalInstructionsTest.this.dataSet.addPrimitive(way);
      assertNull(this.snapToRoad.apply(node));

      way.put("highway", "residential");
      assertNotNull(this.snapToRoad.apply(node));
      Command command = this.snapToRoad.apply(node);
      assertEquals(2, way.getNodesCount());
      command.executeCommand();
      assertEquals(3, way.getNodesCount());
      assertTrue(way.getNodes().contains(node));
      assertEquals(1, way.getNodes().indexOf(node));
      command.undoCommand();
      assertEquals(2, way.getNodesCount());
      assertFalse(way.getNodes().contains(node));
    }
  }
}
