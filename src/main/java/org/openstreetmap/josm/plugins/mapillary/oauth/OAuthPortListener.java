// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.Logging;

/**
 * Listens to the OAuth port (8763) in order to get the access token and sends
 * back a simple reply.
 *
 * @author nokutu
 *
 */
public class OAuthPortListener extends Thread {
  public static final int PORT = 8763;

  protected static final String RESPONSE = String.format(
      "<!DOCTYPE html><html><head><meta charset=\"utf8\"><title>%s</title></head><body>%s</body></html>",
      I18n.tr("Mapillary login"),
      I18n.tr("Login successful, return to JOSM.")
  );
  private final MapillaryLoginListener callback;

  public OAuthPortListener(MapillaryLoginListener loginCallback) {
    this.callback = loginCallback;
  }

  @Override
  public void run() {
    try (
        ServerSocket serverSocket = new ServerSocket(PORT);
        Socket clientSocket = serverSocket.accept();
        PrintWriter out = new PrintWriter(new OutputStreamWriter(clientSocket.getOutputStream(), "UTF-8"), true);
        Scanner in = new Scanner(new InputStreamReader(clientSocket.getInputStream(), "UTF-8"))
    ) {
      String s;
      String accessToken = null;
      while (in.hasNextLine()) {
        s = in.nextLine();
        Matcher tokenMatcher = Pattern.compile("^.*&access_token=([^&]+)&.*$").matcher('&'+s+'&');
        if (tokenMatcher.matches()) {
          accessToken = tokenMatcher.group(1);
          break;
        } else if (s.contains("keep-alive")) {
          break;
        }
      }

      writeContent(out);
      out.flush();

      MapillaryUser.reset();

      Logging.info("Successful login with Mapillary, the access token is: {0}", accessToken);
      // Saves the access token in preferences.
      MapillaryUser.setTokenValid(true);

      MapillaryProperties.ACCESS_TOKEN.put(accessToken);
      String username = MapillaryUser.getUsername();
      Logging.info("The username is: {0}", username);
      if (callback != null) {
        callback.onLogin(username);
      }
    } catch (BindException e) {
      Logging.warn(e);
    } catch (IOException e) {
      Logging.error(e);
    }
  }

  private static void writeContent(PrintWriter out) {
    out.println("HTTP/1.1 200 OK");
    out.println("Content-Length: " + RESPONSE.length());
    out.println("Content-Type: text/html" + "\r\n\r\n");
    out.println(RESPONSE);
  }
}
