// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonReader;
import jakarta.json.stream.JsonParsingException;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.Logging;

/**
 * Listens to the OAuth port (8763) in order to get the access token and sends
 * back a simple reply.
 *
 * @author nokutu
 */
public class OAuthPortListener extends Thread {
    public static final int PORT = 8763;

    protected static final String RESPONSE = String.format(
        "<!DOCTYPE html><html><head><meta charset=\"utf8\"><title>%s</title></head><body>%s</body></html>",
        I18n.tr("Mapillary login"), I18n.tr("Login successful, return to JOSM."));
    private final MapillaryLoginListener callback;

    /**
     * Create a new OAuth callback listener
     *
     * @param loginCallback The callback to call once we have authenticated
     */
    public OAuthPortListener(MapillaryLoginListener loginCallback) {
        this.callback = loginCallback;
    }

    @Override
    public void run() {
        try (ServerSocket serverSocket = new ServerSocket(PORT);
            Socket clientSocket = serverSocket.accept();
            PrintWriter out = new PrintWriter(
                new OutputStreamWriter(clientSocket.getOutputStream(), StandardCharsets.UTF_8), true);
            Scanner in = new Scanner(new InputStreamReader(clientSocket.getInputStream(), StandardCharsets.UTF_8))) {
            String s;
            String authorizationCode = null;
            final Pattern codePattern = Pattern.compile("^.*[&?]code=([^& ]+).*$");
            final Pattern errorPattern = Pattern.compile("^.*[&?]error=([^& ]+).*$");
            while (in.hasNextLine()) {
                s = in.nextLine();
                final Matcher tokenMatcher = codePattern.matcher(s);
                if (tokenMatcher.matches()) {
                    authorizationCode = tokenMatcher.group(1);
                    break;
                } else if (s.contains("keep-alive")) {
                    break;
                }
                final Matcher errorMatcher = errorPattern.matcher(s);
                if (errorMatcher.matches()) {
                    writeContent(out, errorMatcher.group(1));
                    out.flush();
                    MapillaryUser.reset();
                    Logging.info("Unsuccessful login with Mapillary, reason: {0}", errorMatcher.group(1));
                    return;
                }
            }
            writeContent(out, RESPONSE);
            out.flush();

            MapillaryUser.reset();

            final HttpClient client = HttpClient.create(URI.create("https://graph.mapillary.com/token").toURL(),
                "POST");
            client.setHeader("Authorization", "OAuth " + MapillaryConfig.getUrls().getClientSecret());
            client.setRequestBody(("grant_type=authorization_code&client_id=" + MapillaryConfig.getUrls().getClientId()
                + "&code=" + authorizationCode).getBytes(StandardCharsets.UTF_8));
            client.setHeader("Content-Type", "application/x-www-form-urlencoded");

            final HttpClient.Response response = client.connect();
            try (JsonReader jsonReader = Json.createReader(response.getContentReader())) {
                final JsonObject jsonObject = jsonReader.readObject();
                if (!OAuthUtils.updateAuthorization(jsonObject)) {
                    Logging.info("Mapillary: Failed to login: {0}", jsonObject);
                    return;
                }
            } catch (JsonParsingException jsonParsingException) {
                Logging.error(response.fetchContent());
                throw jsonParsingException;
            }

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

    /**
     * Write content out to web page
     *
     * @param out The writer
     * @param response The response to write
     */
    private static void writeContent(final PrintWriter out, final String response) {
        out.println("HTTP/1.1 200 OK");
        out.println("Content-Length: " + response.length());
        out.println("Content-Type: text/html" + "\r\n\r\n");
        out.println(response);
    }
}
