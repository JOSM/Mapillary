package org.openstreetmap.josm.plugins.mapillary.oauth;

import org.scribe.builder.api.DefaultApi20;
import org.scribe.model.OAuthConfig;
import org.scribe.model.Verb;

/**
 * API class of Mapillary for the scrable library.
 * 
 * @author nokutu
 *
 */
public class MapillaryOAuthApi extends DefaultApi20 {

  @Override
  public String getAccessTokenEndpoint() {
    return "https://a.mapillary.com/v2/oauth/token";
  }

  @Override
  public String getAuthorizationUrl(OAuthConfig config) {
    return "http://www.mapillary.io/connect?client_id=MkJKbDA0bnZuZlcxeTJHTmFqN3g1dzplZTlkZjQyYjYyZTczOTdi&redirect_uri=https:%2F%2Fjosm.openstreetmap.de%2F&response_type=token&scope=upload";
  }

  @Override
  public Verb getAccessTokenVerb() {
    return Verb.POST;
  }
}
