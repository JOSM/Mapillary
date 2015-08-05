package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.openstreetmap.josm.tools.I18n.tr;

/**
 * @author nokutu
 *
 */
public class PluginState {

  private static int runningDownloads = 0;
  private static int runningUploads = 0;
  private static int imagesToUpload = 0;
  private static int imagesUploaded = 0;

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
   * Called when an upload is starting.
   */
  public static void startUpload() {
    runningUploads++;
  }

  /**
   * Called when an upload is finished.
   */
  public static void finishUpload() {
    runningUploads--;
    if (imagesUploaded >= imagesToUpload) {
      imagesUploaded = 0;
      imagesToUpload = 0;
    }
  }

  /**
   * Checks if there is any running upload.
   *
   * @return true if the plugin is uploading; false otherwise.
   */
  public static boolean isUploading() {
    return runningUploads > 0;
  }

  /**
   * Sets the amount of images that are going to be uploaded.
   *
   * @param amount
   *          The amount of images that are going to be uploaded.
   */
  public static void imagesToUpload(int amount) {
    imagesToUpload += amount;
  }

  /**
   * Called when an image is uploaded.
   */
  public static void imageUploaded() {
    imagesUploaded++;
  }

  /**
   * Returns the string to be written in the status bar.
   *
   * @return The String that is going to be written in the status bar.
   */
  public static String getUploadString() {
    return tr("Downloading: {0}", "(" + imagesUploaded + "/" + imagesToUpload
        + ")");
  }
}
