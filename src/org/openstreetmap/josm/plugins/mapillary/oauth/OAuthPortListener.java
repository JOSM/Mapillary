package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Scanner;

import org.openstreetmap.josm.Main;

/**
 * Listens to the OAuth port in order to get the access token.
 *
 * @author nokutu
 *
 */
public class OAuthPortListener extends Thread {

  protected static String RESPONSE = "<html><head><title>Mapillary login</title></head><body>Login successful, return to JOSM.</body></html>";

  @Override
  public void run() {
    try {
      ServerSocket serverSocket = new ServerSocket(8763);
      Socket clientSocket = serverSocket.accept();
      PrintWriter out = new PrintWriter(new OutputStreamWriter(
          clientSocket.getOutputStream(), "UTF-8"), true);
      Scanner in = new Scanner(new InputStreamReader(
          clientSocket.getInputStream(), "UTF-8"));
      String s;
      String accessToken = null;
      while (in.hasNextLine()) {
        s = in.nextLine();
        if (s.contains("access_token=")) {
          String[] ss = s.split("&");
          for (int i = 0; i < ss.length; i++) {
            if (ss[i].contains("access_token=")) {
              accessToken = ss[i].substring(
                  ss[i].indexOf("access_token=") + 13, ss[i].length());
              break;
            }
          }
          break;
        } else if (s.contains("keep-alive"))
          break;
      }

      writeContent(out);

      out.close();
      in.close();
      serverSocket.close();

      MapillaryUser.reset();

      Main.info("Successful login with Mapillary, the access token is: "
          + accessToken);
      // Saves the access token in preferences.
      if (Main.main != null) {
        Main.pref.put("mapillary.access-token", accessToken);
        Main.info("The username is: " + MapillaryUser.getUsername());
      }

    } catch (BindException e) {
      return;
    } catch (IOException e) {
      Main.error(e);
    }
  }

  private static void writeContent(PrintWriter out) {
    out.println("HTTP/1.1 200 OK");
    out.println("Content-Length: " + RESPONSE.length());
    out.println("Content-Type: text/html" + "\r\n\r\n");
    out.println(RESPONSE);
  }
}
