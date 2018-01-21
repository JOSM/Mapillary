// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import org.junit.Test;

/**
 * Tests the {@link OAuthPortListener} class.
 *
 * @author nokutu
 * @see OAuthPortListener
 *
 */
public class OAuthPortListenerTest {

  /**
   * Test that the threads responds when the browser makes the request.
   */
  @Test
  public void responseTest() throws IOException, InterruptedException {
    OAuthPortListener t = new OAuthPortListener(null);
    t.start();
    Thread.sleep(500);
    URL url = new URL("http://localhost:"+OAuthPortListener.PORT+"?access_token=access_token");
    HttpURLConnection con = (HttpURLConnection) url.openConnection();
    BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream(), StandardCharsets.UTF_8));
    in.readLine();
    assertEquals(OAuthPortListener.RESPONSE, in.readLine());
  }
}
