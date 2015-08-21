package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.openstreetmap.josm.tools.I18n.tr;

import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.gui.FinishedUploadDialog;

/**
 * @author nokutu
 *
 */
public class PluginState {

  private static int runningDownloads = 0;
  /** Images that have to be uploaded. */
  protected static int imagesToUpload = 0;
  /** Images that have been uploaded. */
  public static int imagesUploaded = 0;

  /**
   * Called when a download is started.
   */
  public static void startDownload() {
    runningDownloads++;
  }

  /**
   * Called when a download is finished.
   */
  public static void finishDownload() {
    if (runningDownloads == 0)
      throw new IllegalStateException(
          "The amount of running downlaods is less or equals to 0");
    runningDownloads--;
  }

  /**
   * Checks if there is any running download.
   *
   * @return true if the plugin is downloading; false otherwise.
   */
  public static boolean isDownloading() {
    return runningDownloads > 0;
  }

  /**
   * Checks if there is any running upload.
   *
   * @return true if the plugin is uploading; false otherwise.
   */
  public static boolean isUploading() {
    return imagesToUpload > imagesUploaded;
  }

  /**
   * Sets the amount of images that are going to be uploaded.
   *
   * @param amount
   *          The amount of images that are going to be uploaded.
   */
  public static void imagesToUpload(int amount) {
    if (imagesToUpload <= imagesUploaded) {
      imagesToUpload = 0;
      imagesUploaded = 0;
    }
    imagesToUpload += amount;
  }

  /**
   * Called when an image is uploaded.
   */
  public static void imageUploaded() {
    imagesUploaded++;
    if (imagesToUpload == imagesUploaded) {
      if (Main.main != null)
        finishedUploadDialog();
    }
  }

  private static void finishedUploadDialog() {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          finishedUploadDialog();
        }
      });
    } else {
      JOptionPane pane = new JOptionPane();
      pane.setMessage(new FinishedUploadDialog());
      JDialog dlg = pane.createDialog(Main.parent, tr("Finished upload"));
      dlg.setVisible(true);
    }
  }

  /**
   * Returns the text to be written in the status bar.
   *
   * @return The {@code String} that is going to be written in the status bar.
   */
  public static String getUploadString() {
    return tr("Uploading: {0}", "(" + imagesUploaded + "/" + imagesToUpload
        + ")");
  }
}
