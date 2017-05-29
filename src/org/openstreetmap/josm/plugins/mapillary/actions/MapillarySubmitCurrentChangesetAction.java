// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.IOException;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObjectBuilder;
import javax.swing.JOptionPane;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLocationChangeset;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryChangesetDialog;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Imports a set of picture files into JOSM. They must be in jpg or png format.
 */
public class MapillarySubmitCurrentChangesetAction extends JosmAction {

  private static final long serialVersionUID = 4995924098228082806L;
  private final MapillaryChangesetDialog changesetDialog;

  /**
   * Main constructor.
   */
  public MapillarySubmitCurrentChangesetAction(MapillaryChangesetDialog changesetDialog) {
    super(
      tr("Submit changeset"),
      new ImageProvider("dialogs", "mapillary-upload").setSize(ImageSizes.DEFAULT),
      tr("Submit the current changeset"),
      Shortcut.registerShortcut(
        "Submit changeset to Mapillary", tr("Submit the current changeset to Mapillary"),
        KeyEvent.CHAR_UNDEFINED, Shortcut.NONE
      ),
      false,
      "mapillarySubmitChangeset",
      false
    );
    this.changesetDialog = changesetDialog;
    setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent event) {
    new Thread(() -> {
      changesetDialog.setUploadPending(true);
      String token = MapillaryProperties.ACCESS_TOKEN.get();
      if (token != null && !token.trim().isEmpty()) {
        PluginState.setSubmittingChangeset(true);
        MapillaryUtils.updateHelpText();
        HttpClientBuilder builder = HttpClientBuilder.create();
        HttpPost httpPost = new HttpPost(APIv3.submitChangeset().toString());
        httpPost.addHeader("content-type", "application/json");
        httpPost.addHeader("Authorization", "Bearer " + token);
        MapillaryLocationChangeset locationChangeset = MapillaryLayer.getInstance().getLocationChangeset();
        String json = buildLocationChangesetJson(locationChangeset).build().toString();
        Main.info("Sending JSON to " + APIv3.submitChangeset() + "\n  " + json);
        try (CloseableHttpClient httpClient = builder.build()) {
          httpPost.setEntity(new StringEntity(json));
          CloseableHttpResponse response = httpClient.execute(httpPost);
          Main.debug("HTTP request finished with response code " + response.getStatusLine().getStatusCode());
          if (response.getStatusLine().getStatusCode() == 200) {
            String key = Json.createReader(response.getEntity().getContent()).readObject().getString("key");
            Main.debug("Received key " + key);
            synchronized (MapillaryUtils.class) {
              Main.map.statusLine.setHelpText(String.format("%s images submitted, Changeset key: %s", locationChangeset.size(), key));
            }
            locationChangeset.cleanChangeset(); // TODO: Remove only uploaded changes. If the user made changes while uploading the changeset, these changes would also be removed, although they weren't uploaded. Alternatively: Disallow editing while uploading.
          } else {
            Notification n = new Notification(
              tr("Changeset upload failed with {0} error ''{1} {2}''!",
                response.getStatusLine().getProtocolVersion(),
                response.getStatusLine().getStatusCode(),
                response.getStatusLine().getReasonPhrase()
              )
            );
            n.setIcon(JOptionPane.ERROR_MESSAGE);
            n.setDuration(Notification.TIME_LONG);
            n.show();
            Main.error("Failed response " + EntityUtils.toString(response.getEntity()));
          }
        } catch (IOException e) {
          Main.error(e, "Exception while trying to submit a changeset to mapillary.com");
          Notification n = new Notification(
            tr("An exception occured while trying to submit a changeset. If this happens repeatedly, consider reporting a bug via the Help menu. If this message appears for the first time, simply try it again. This might have been an issue with the internet connection.")
          );
          n.setDuration(Notification.TIME_LONG);
          n.setIcon(JOptionPane.ERROR_MESSAGE);
          n.show();
        } finally {
          PluginState.setSubmittingChangeset(false);
        }
      } else {
        PluginState.notLoggedInToMapillaryDialog();
      }
      changesetDialog.setUploadPending(false);
    }, "Mapillary changeset upload").start();
  }

  private static JsonObjectBuilder buildImgChangeJson(MapillaryImage img) {
    return Json.createObjectBuilder()
      .add("image_key", img.getKey())
      .add("to", Json.createObjectBuilder()
        .add("geometry", Json.createObjectBuilder()
          .add("type", "Point")
          .add("coordinates", Json.createArrayBuilder()
            .add(img.getTempLatLon().getX())
            .add(img.getTempLatLon().getY())
          )
        )
        .add("properties", Json.createObjectBuilder()
          .add("ca", img.getTempCa())
        )
        .add("type", "Feature")
      );
  }

  private static JsonObjectBuilder buildLocationChangesetJson(MapillaryLocationChangeset changeset) {
    JsonArrayBuilder imgChanges = Json.createArrayBuilder();
    for (MapillaryImage img : changeset) {
      imgChanges.add(buildImgChangeJson(img));
    }
    return Json.createObjectBuilder()
      .add("type", "location")
      .add("changes", imgChanges.build())
      .add("request_comment", "JOSM-created");
  }
}
