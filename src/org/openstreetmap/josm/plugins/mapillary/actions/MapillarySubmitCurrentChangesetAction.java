// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.IOException;

import javax.json.Json;
import javax.swing.JOptionPane;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLocationChangeset;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryChangesetDialog;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL.APIv3;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonLocationChangesetEncoder;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Imports a set of picture files into JOSM. They must be in jpg or png format.
 */
public class MapillarySubmitCurrentChangesetAction extends JosmAction {

  private static final long serialVersionUID = 4995924098228082806L;
  private final MapillaryChangesetDialog changesetDialog;

  /**
   * Main constructor.
   * @param changesetDialog Mapillary changeset dialog
   */
  public MapillarySubmitCurrentChangesetAction(MapillaryChangesetDialog changesetDialog) {
    super(
      I18n.tr("Submit changeset"),
      new ImageProvider("dialogs", "mapillary-upload").setSize(ImageSizes.DEFAULT),
      I18n.tr("Submit the current changeset"),
      // CHECKSTYLE.OFF: LineLength
      Shortcut.registerShortcut("Submit changeset to Mapillary", I18n.tr("Submit the current changeset to Mapillary"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
      // CHECKSTYLE.ON: LineLength
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
        String json = JsonLocationChangesetEncoder.encodeLocationChangeset(locationChangeset).build().toString();
        Logging.info("Sending JSON to " + APIv3.submitChangeset() + "\n  " + json);
        try (CloseableHttpClient httpClient = builder.build()) {
          httpPost.setEntity(new StringEntity(json));
          CloseableHttpResponse response = httpClient.execute(httpPost);
          Logging.debug("HTTP request finished with response code " + response.getStatusLine().getStatusCode());
          if (response.getStatusLine().getStatusCode() == 201) {
            final String key = Json.createReader(response.getEntity().getContent()).readObject().getString("key");
            final String state = Json.createReader(response.getEntity().getContent()).readObject().getString("state");
            I18n.marktr("rejected");
            I18n.marktr("pending");
            I18n.marktr("approved");
            final String message = I18n.trn(
              "{0} image submitted, Changeset key: {1}, State: {2}",
              "{0} images submitted, Changeset key: {1}, State: {2}",
              locationChangeset.size(), key, state
            );
            Logging.debug(message);
            new Notification(message)
              .setDuration(Notification.TIME_LONG)
              .setIcon("rejected".equals(state) ? JOptionPane.ERROR_MESSAGE : JOptionPane.INFORMATION_MESSAGE)
              .show();
            locationChangeset.cleanChangeset(); // TODO: Remove only uploaded changes. If the user made changes while uploading the changeset, these changes would also be removed, although they weren't uploaded. Alternatively: Disallow editing while uploading.
          } else {
            new Notification(
              I18n.tr("Changeset upload failed with {0} error ''{1} {2}''!",
                response.getStatusLine().getProtocolVersion(),
                response.getStatusLine().getStatusCode(),
                response.getStatusLine().getReasonPhrase()
              )
            ).setIcon(JOptionPane.ERROR_MESSAGE)
              .setDuration(Notification.TIME_LONG)
              .show();
            Logging.error("Failed response " + EntityUtils.toString(response.getEntity()));
          }
        } catch (IOException e) {
          Logging.log(Logging.LEVEL_ERROR, "Exception while trying to submit a changeset to mapillary.com", e);
          new Notification(
            I18n.tr("An exception occured while trying to submit a changeset. If this happens repeatedly, consider reporting a bug via the Help menu. If this message appears for the first time, simply try it again. This might have been an issue with the internet connection.")
          ).setDuration(Notification.TIME_LONG)
            .setIcon(JOptionPane.ERROR_MESSAGE)
            .show();
        } finally {
          PluginState.setSubmittingChangeset(false);
        }
      } else {
        PluginState.notLoggedInToMapillaryDialog();
      }
      changesetDialog.setUploadPending(false);
    }, "Mapillary changeset upload").start();
  }
}
