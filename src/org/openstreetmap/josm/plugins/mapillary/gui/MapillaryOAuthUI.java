package org.openstreetmap.josm.plugins.mapillary.gui;

import java.util.Scanner;

import javax.swing.JLabel;
import javax.swing.JPanel;

import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryOAuthApi;
import org.scribe.builder.ServiceBuilder;
import org.scribe.model.Token;
import org.scribe.model.Verifier;
import org.scribe.oauth.OAuthService;

/**
 * JPanel used to get the OAuth tokens from Mapillary.
 *
 * @author nokutu
 *
 */
public class MapillaryOAuthUI extends JPanel {

  private static final Token EMPTY_TOKEN = null;

  public MapillaryOAuthUI() {
    Scanner in = new Scanner(System.in);
    OAuthService service = new ServiceBuilder()
        .provider(MapillaryOAuthApi.class)
        .apiKey("NzNRM2otQkR2SHJzaXJmNmdQWVQ0dzo1YTA2NmNlODhlNWMwOTBm")
        .apiSecret("Secret").build();
    String authorizationUrl = service.getAuthorizationUrl(EMPTY_TOKEN);
    this.add(new JLabel("Login"));
    System.out.println("Fetching the Authorization URL...");
    System.out.println("Got the Authorization URL!");
    System.out.println("Now go and authorize Scribe here:");
    System.out.println(authorizationUrl);
    System.out.println("And paste the authorization code here");
    System.out.print(">>");
    Verifier verifier = new Verifier(in.nextLine());
    in.close();
    System.out.println();
    in.close();
  }

}
