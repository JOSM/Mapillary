package org.openstreetmap.josm.plugins.mapillary.traffico;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

import org.junit.Test;
import org.openstreetmap.josm.plugins.mapillary.utils.TestUtil;

public class TrafficoSignTest {

  @Test
  public void test() {
    assertArrayEquals(null, TrafficoSign.getSign("de", ""));
    TrafficoSignElement[] trafficSignals = TrafficoSign.getSign("de", "traffic-signals-ahead");
    assertNotEquals(null, trafficSignals);
    assertEquals(5, trafficSignals.length);
    assertArrayEquals(null, TrafficoSign.getSign("us", ""));
    assertArrayEquals(null, TrafficoSign.getSign("xyz", ""));
  }

  @Test
  public void testUtilityClass() {
    TestUtil.testUtilityClass(TrafficoSign.class);
  }

}
