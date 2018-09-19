// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

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
    assertFalse(PluginState.isDownloading());
    PluginState.startDownload();
    assertTrue(PluginState.isDownloading());
    PluginState.startDownload();
    assertTrue(PluginState.isDownloading());
    PluginState.finishDownload();
    assertTrue(PluginState.isDownloading());
    PluginState.finishDownload();
    assertFalse(PluginState.isDownloading());
  }

  /**
   * Tests the methods related to the upload.
   */
  @Test
  public void uploadTest() {
    assertFalse(PluginState.isUploading());
    PluginState.addImagesToUpload(2);
    assertEquals(2, PluginState.getImagesToUpload());
    assertEquals(0, PluginState.getImagesUploaded());
    assertTrue(PluginState.isUploading());
    PluginState.imageUploaded();
    assertEquals(1, PluginState.getImagesUploaded());
    assertTrue(PluginState.isUploading());
    PluginState.imageUploaded();
    assertFalse(PluginState.isUploading());
    assertEquals(2, PluginState.getImagesToUpload());
    assertEquals(2, PluginState.getImagesUploaded());
  }
}
