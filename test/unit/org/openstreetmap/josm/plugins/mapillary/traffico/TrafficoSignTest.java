package org.openstreetmap.josm.plugins.mapillary.traffico;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

import org.junit.Test;

public class TrafficoSignTest {
  @Test
  public void test() {
    assertEquals(null, TrafficoSign.getSign("de", ""));
    TrafficoSign trafficSignals = TrafficoSign.getSign("de", "traffic-signals-ahead");
    assertNotEquals(null, trafficSignals);
    assertEquals(5, trafficSignals.getNumElements());
    assertEquals(null, TrafficoSign.getSign("us", ""));
    assertEquals(null, TrafficoSign.getSign("xyz", ""));
  }
}
