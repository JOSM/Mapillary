package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * Tests {@link PluginState} class.
 *
 * @author nokutu
 * @see PluginState
 */
public class PluginStateTest {

  /**
   * Test the methods related to the download.
   */
  @Test
  public void downloadTest() {
    assertEquals(false, PluginState.isDownloading());
    PluginState.startDownload();
    assertEquals(true, PluginState.isDownloading());
    PluginState.startDownload();
    assertEquals(true, PluginState.isDownloading());
    PluginState.finishDownload();
    assertEquals(true, PluginState.isDownloading());
    PluginState.finishDownload();
    assertEquals(false, PluginState.isDownloading());
  }

  /**
   * Tests the methods related to the upload.
   */
  @Test
  public void uploadTest() {
    assertEquals(false, PluginState.isUploading());
    PluginState.addImagesToUpload(2);
    assertEquals(2, PluginState.getImagesToUpload());
    assertEquals(0, PluginState.getImagesUploaded());
    assertEquals(true, PluginState.isUploading());
    PluginState.imageUploaded();
    assertEquals(1, PluginState.getImagesUploaded());
    assertEquals(true, PluginState.isUploading());
    PluginState.imageUploaded();
    assertEquals(false, PluginState.isUploading());
    assertEquals(2, PluginState.getImagesToUpload());
    assertEquals(2, PluginState.getImagesUploaded());
  }
}
