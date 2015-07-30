package org.openstreetmap.josm.plugins.mapillary;

import org.junit.BeforeClass;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.util.TestUtil;

public abstract class AbstractTest {

  private static boolean started = false;

  @BeforeClass
  public static void setUpBeforeClass() throws Exception {
    if (!started) {
      new Thread() {
        @Override
        public synchronized void run() {
          MainApplication
              .main(new String[] { "--download=http://www.openstreetmap.org/#map=18/40.42013/-3.68923" });
        }
      }.start();
      started = true;
      while (Main.map == null || Main.map.mapView == null) {
        synchronized (Thread.currentThread()) {
          Thread.currentThread().wait(1000);
        }
      }
    }
  }
}
