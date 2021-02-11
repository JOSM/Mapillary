// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import org.apache.commons.imaging.ImageReadException;
import org.apache.commons.imaging.ImageWriteException;
import org.apache.commons.imaging.Imaging;
import org.apache.commons.imaging.common.ImageMetadata;
import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.jpeg.JpegImageMetadata;
import org.apache.commons.imaging.formats.jpeg.exif.ExifRewriter;
import org.apache.commons.imaging.formats.tiff.TiffImageMetadata;
import org.apache.commons.imaging.formats.tiff.constants.ExifTagConstants;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;
import org.apache.commons.imaging.formats.tiff.constants.TiffTagConstants;
import org.apache.commons.imaging.formats.tiff.write.TiffOutputDirectory;
import org.apache.commons.imaging.formats.tiff.write.TiffOutputSet;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;

import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandDelete;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.Logging;

/**
 * Upload utilities.
 *
 * @author nokutu
 */
public final class UploadUtils {
  /**
   * Count to name temporal files.
   */
  private static int c;

  private UploadUtils() {
    // Private constructor to avoid instantiation.
  }

  private static final class SequenceUploadRunnable implements Runnable {
    private final Set<MapillaryAbstractImage> images;
    private final boolean delete;
    private final ThreadPoolExecutor ex;

    public SequenceUploadRunnable(Set<MapillaryAbstractImage> images, boolean delete) {
      this.images = images;
      this.ex = new ThreadPoolExecutor(1, Runtime.getRuntime().availableProcessors() * 2, 25, TimeUnit.SECONDS,
        new ArrayBlockingQueue<>(images.size()));
      this.delete = delete;
    }

    @Override
    public void run() {
      final Set<MapillaryImportedImage> uploadImages = this.images.stream()
        .map(it -> it instanceof MapillaryImportedImage ? (MapillaryImportedImage) it : null)
        .collect(Collectors.toSet());
      if (uploadImages.stream().anyMatch(Objects::isNull)) {
        new Notification(I18n.tr(
          "You are trying to upload a sequence to mapillary.com that you previously downloaded from there. That is not possible."))
            .setIcon(MapillaryPlugin.LOGO.get()).show();
      } else {
        PluginState.addImagesToUpload(uploadImages.size());
        MapillaryUtils.updateHelpText();
        uploadImages.forEach(img -> ex.execute(() -> upload(img)));
        try {
          ex.shutdown();
          ex.awaitTermination(5L * uploadImages.size(), TimeUnit.MINUTES);
          uploadDoneFile();
          if (this.delete) {
            GuiHelper.runInEDT(() -> MapillaryRecord.getInstance().addCommand(new CommandDelete(uploadImages)));
          }
        } catch (final InterruptedException e) {
          Logging.error(e);
          Thread.currentThread().interrupt();
        }
      }
    }
  }

  /**
   * Returns a file containing the picture and an updated version of the EXIF
   * tags.
   *
   * @param image The image to be uploaded
   * @return A File object containing the picture and an updated version of the
   *         EXIF tags.
   * @throws ImageReadException if there are errors reading the image from the file.
   * @throws IOException if there are errors getting the metadata from the file or writing
   *         the output.
   * @throws ImageWriteException if there are errors writing the image in the file.
   */
  static File updateFile(MapillaryImportedImage image) throws ImageReadException, IOException, ImageWriteException {
    TiffOutputSet outputSet = new TiffOutputSet();
    TiffOutputDirectory exifDirectory;
    TiffOutputDirectory gpsDirectory;
    TiffOutputDirectory rootDirectory;

    // If the image is imported, loads the rest of the EXIF data.
    final ImageMetadata metadata = Imaging.getMetadata(image.getFile());
    if (metadata instanceof JpegImageMetadata) {
      final TiffImageMetadata exif = ((JpegImageMetadata) metadata).getExif();
      if (exif != null) {
        outputSet = exif.getOutputSet();
      }
    }

    gpsDirectory = outputSet.getOrCreateGPSDirectory();
    exifDirectory = outputSet.getOrCreateExifDirectory();
    rootDirectory = outputSet.getOrCreateRootDirectory();

    gpsDirectory.removeField(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF);
    gpsDirectory.add(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF,
      GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF_VALUE_TRUE_NORTH);

    gpsDirectory.removeField(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION);
    gpsDirectory.add(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION, RationalNumber.valueOf(image.getMovingCa()));

    exifDirectory.removeField(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL);
    exifDirectory.add(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL, image.getDate("yyyy/MM/dd HH:mm:ss"));

    // Removes the ImageDescription tag, that causes problems in the upload.
    rootDirectory.removeField(TiffTagConstants.TIFF_TAG_IMAGE_DESCRIPTION);

    outputSet.setGPSInDegrees(image.getMovingLatLon().lon(), image.getMovingLatLon().lat());
    File tempFile = File.createTempFile("imagetoupload_" + c, ".tmp");
    c++;
    try (OutputStream os = new BufferedOutputStream(new FileOutputStream(tempFile))) {
      // Transforms the image into a byte array.
      ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
      ImageIO.write(image.getImage(), "jpg", outputStream);
      byte[] imageBytes = outputStream.toByteArray();
      new ExifRewriter().updateExifMetadataLossless(imageBytes, os, outputSet);
    }
    return tempFile;
  }

  /**
   * Uploads the DONE file to S3, signalling that all images of this seqeunce are uplaoded.
   */
  public static void uploadDoneFile() {
    JsonObject secretMap = MapillaryUser.getSecrets();
    if (secretMap == null) {
      throw new IllegalStateException("Can't obtain secrets from user");
    }
    URL url;
    String urlString = String.format(MapillaryURL.APIv3.uploadFinish(),
      ((JsonString) secretMap.get("key")).getString());
    try {
      url = new URL(urlString);
    } catch (MalformedURLException e1) {
      Logging.error("Could not finish upload: {0}", urlString);
      return;
    }
    HttpClient client = HttpClient.create(url, "PUT");
    // This avoids several NPE's in JOSM code
    client.setRequestBody(new byte[] {});
    try {
      OAuthUtils.addAuthenticationHeader(client);
      client.connect();
    } catch (IOException e) {
      Logging.error(e);
    } finally {
      client.disconnect();
      MapillaryUser.clearSecrets();
    }
  }

  /**
   * Uploads the given MapillaryImportedImage object.
   *
   * @param image The image to be uploaded
   * @throws IllegalStateException If {@link MapillaryUser#getSecrets()} returns null
   */
  public static void upload(MapillaryImportedImage image) {
    JsonObject secretMap = MapillaryUser.getSecrets();
    if (secretMap == null) {
      throw new IllegalStateException("Can't obtain secrets from user");
    }

    String key = String.format(Locale.UK, "%s%f_%f_%f_%d.jpg", ((JsonString) secretMap.get("key_prefix")).getString(),
      image.getMovingLatLon().lat(), image.getMovingLatLon().lon(), image.getMovingCa(), image.getCapturedAt());
    try {
      uploadFile(key, updateFile(image), secretMap);
    } catch (ImageReadException | ImageWriteException | IOException e) {
      Logging.error(e);
    }
  }

  private static Map<String, String> getUploadFormParts(JsonObject secretMap) {
    return secretMap.getJsonObject("fields").entrySet().stream()
      .filter(entry -> entry.getValue().getValueType() == JsonValue.ValueType.STRING)
      .collect(Collectors.toMap(Map.Entry::getKey, entry -> ((JsonString) entry.getValue()).getString()));
  }

  /**
   * @param file File that is going to be uploaded
   * @param secretMap Information attached to the upload
   * @throws IllegalArgumentException if the hash doesn't contain all the needed keys.
   * @throws IOException if an HTTP connection cannot be opened
   */
  private static void uploadFile(String key, File file, JsonObject secretMap) throws IOException {
    boolean uploaded = false;
    HttpPost httpPost = new HttpPost(secretMap.getString("url"));
    HttpClientBuilder builder = HttpClientBuilder.create();
    try (CloseableHttpClient httpClient = builder.build()) {
      MultipartEntityBuilder entityBuilder = MultipartEntityBuilder.create();
      getUploadFormParts(secretMap)
        .forEach((key1, value) -> entityBuilder.addTextBody(key1, value, ContentType.MULTIPART_FORM_DATA));
      entityBuilder.addTextBody("key", key, ContentType.MULTIPART_FORM_DATA);
      entityBuilder.addPart("file", new FileBody(file));
      HttpEntity entity = entityBuilder.build();
      httpPost.setEntity(entity);
      try (CloseableHttpResponse response = httpClient.execute(httpPost)) {
        if (response.getStatusLine().toString().contains("204")) {
          GuiHelper.runInEDT(PluginState::imageUploaded);
          Logging.info("{0} (Mapillary)", PluginState.getUploadString());
          uploaded = true;
        } else {
          Logging.info("Upload error");
        }
      }
    }
    if (uploaded) {
      try {
        Files.delete(file.toPath());
      } catch (IOException | SecurityException e) {
        Logging.log(Logging.LEVEL_ERROR, "MapillaryPlugin: File could not be deleted during upload", e);
      }
      MapillaryUtils.updateHelpText();
    }
  }

  /**
   * Uploads the given {@link MapillarySequence}.
   *
   * @param sequence The sequence to upload. It must contain only
   *        {@link MapillaryImportedImage} objects.
   * @param delete Whether the images must be deleted after upload or not.
   */
  public static void uploadSequence(MapillarySequence sequence, boolean delete) {
    MainApplication.worker
      .execute(new SequenceUploadRunnable(new ConcurrentSkipListSet<>(sequence.getImages()), delete));
  }
}
