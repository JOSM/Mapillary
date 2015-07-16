package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Scanner;

import javax.swing.JLabel;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.Main;

/**
 * @author nokutu
 *
 */
public class PortListener extends Thread {

  JLabel text;

  /**
   * Main constructor.
   *
   * @param text
   */
  public PortListener(JLabel text) {
    this.text = text;
  }

  @Override
  public void run() {
    try {
      ServerSocket serverSocket = new ServerSocket(8763);
      Socket clientSocket = serverSocket.accept();
      PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
      Scanner in = new Scanner(new InputStreamReader(clientSocket.getInputStream()));
      String s;
      String code = null;
      while (in.hasNextLine()) {
        s = in.nextLine();
        if (s.contains("?code=")) {
          code = s.substring(s.indexOf("=") + 1, s.indexOf("HTTP") - 1);
          break;
        }
      }

      writeContent(out);

      System.out.println("The code is: " + code);

      out.close();
      in.close();
      serverSocket.close();
    } catch (IOException e) {
      Main.error(e);
    }
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          text.setText("Authorization successful");
        }
      });
    } else
      text.setText("Authorization successful");
  }

  private void writeContent(PrintWriter out) {
    String response = "";
    response += "<html><body>Authorization successful</body></html>";
    out.println("HTTP/1.1 200 OK");
    out.println("Content-Length: " + response.length());
    out.println("Content-Type: text/html" + "\r\n\r\n");
    out.println(response);
  }
}
