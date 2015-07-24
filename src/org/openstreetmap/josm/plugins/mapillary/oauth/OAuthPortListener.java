package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.IOException;
import java.io.InputStreamReader;
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

  @Override
  public void run() {
    try {
      ServerSocket serverSocket = new ServerSocket(8763);
      Socket clientSocket = serverSocket.accept();
      PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
      Scanner in = new Scanner(new InputStreamReader(
          clientSocket.getInputStream()));
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
        }
      }

      writeContent(out);

      Main.info("Successful login with Mapillary, the access token is: "
          + accessToken);
      Main.pref.put("mapillary.access-token", accessToken);

      out.close();
      in.close();
      serverSocket.close();
    } catch (BindException e) {
      return;
    } catch (IOException e) {
      Main.error(e);
    }
  }

  private void writeContent(PrintWriter out) {
    String response = "<html><head><title>Mapillary login</title></head><body>Login successful, return to JOSM.</body></html>";
    out.println("HTTP/1.1 200 OK");
    out.println("Content-Length: " + response.length());
    out.println("Content-Type: text/html" + "\r\n\r\n");
    out.println(response);
  }
}
