package org.openstreetmap.josm.plugins.mapillary.gui;

import java.awt.Desktop;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import javax.swing.JLabel;
import javax.swing.JPanel;

import org.openstreetmap.josm.plugins.mapillary.oauth.PortListener;

/**
 * JPanel used to get the OAuth tokens from Mapillary.
 *
 * @author nokutu
 *
 */
public class MapillaryOAuthUI extends JPanel {

  PortListener portListener;
  JLabel text;

  /**
   * Main constructor.
   */
  public MapillaryOAuthUI() {
    text = new JLabel("Authorize in browser");
    this.add(text);
    portListener = new PortListener(text);
    portListener.start();

    String url = "http://www.mapillary.io/connect?redirect_uri=http:%2F%2Flocalhost:8763%2F&client_id=MkJKbDA0bnZuZlcxeTJHTmFqN3g1dzplZTlkZjQyYjYyZTczOTdi&response_type=code&scope=user:email";
    Desktop desktop = Desktop.getDesktop();
    try {
      desktop.browse(new URI(url));
    } catch (IOException | URISyntaxException ex) {
      ex.printStackTrace();
    } catch (UnsupportedOperationException ex) {
      Runtime runtime = Runtime.getRuntime();
      try {
        runtime.exec("xdg-open " + url);
      } catch (IOException exc) {
        exc.printStackTrace();
      }
    }
  }
}
