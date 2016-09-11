// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.imageio.ImageIO;

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
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandDelete;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.PluginState;

/**
 * Upload utilities.
 *
 * @author nokutu
 */
public final class UploadUtils {
  /**
   * Required keys for POST
   */
  private static final String[] keys = {"key", "AWSAccessKeyId", "acl", "policy", "signature", "Content-Type"};

  /**
   * Mapillary upload URL
   */
  private static final String UPLOAD_URL = "https://s3-eu-west-1.amazonaws.com/mapillary.uploads.manual.images";

  /**
   * Count to name temporal files.
   */
  private static int c;

  private UploadUtils() {
    // Private constructor to avoid instantiation.
  }

  private static final class SequenceUploadThread extends Thread {
    private final Set<MapillaryAbstractImage> images;
    private final UUID uuid;
    private final boolean delete;
    private final ThreadPoolExecutor ex;

    private SequenceUploadThread(Set<MapillaryAbstractImage> images,
                                 boolean delete) {
      this.images = images;
      this.uuid = UUID.randomUUID();
      this.ex = new ThreadPoolExecutor(8, 8, 25, TimeUnit.SECONDS, new ArrayBlockingQueue<>(15));
      this.delete = delete;
    }

    @Override
    public void run() {
      PluginState.addImagesToUpload(this.images.size());
      MapillaryUtils.updateHelpText();
      for (MapillaryAbstractImage img : this.images) {
        if (!(img instanceof MapillaryImportedImage))
          throw new IllegalArgumentException("The sequence contains downloaded images.");
        this.ex.execute(new SingleUploadThread((MapillaryImportedImage) img, this.uuid));
        while (this.ex.getQueue().remainingCapacity() == 0) {
          try {
            Thread.sleep(100);
          } catch (InterruptedException e) {
            Main.error(e);
          }
        }
      }
      this.ex.shutdown();
      try {
        this.ex.awaitTermination(15, TimeUnit.SECONDS);
      } catch (InterruptedException e) {
        Main.error(e);
      }
      if (this.delete)
        MapillaryRecord.getInstance()
                .addCommand(new CommandDelete(images));
    }
  }

  private static final class SingleUploadThread extends Thread {

    private final MapillaryImportedImage image;
    private final UUID uuid;

    private SingleUploadThread(MapillaryImportedImage image, UUID uuid) {
      this.image = image;
      this.uuid = uuid;
    }

    @Override
    public void run() {
      upload(this.image, this.uuid);
    }
  }

  /**
   * Returns a file containing the picture and an updated version of the EXIF
   * tags.
   *
   * @param image The image to be uploaded
   * @return A File object containing the picture and an updated version of the
   * EXIF tags.
   * @throws ImageReadException  if there are errors reading the image from the file.
   * @throws IOException         if there are errors getting the metadata from the file or writing
   *                             the output.
   * @throws ImageWriteException if there are errors writing the image in the file.
   */
  private static File updateFile(MapillaryImportedImage image)
          throws ImageReadException, IOException, ImageWriteException {
    TiffOutputSet outputSet = null;
    TiffOutputDirectory exifDirectory;
    TiffOutputDirectory gpsDirectory;
    TiffOutputDirectory rootDirectory;

    // If the image is imported, loads the rest of the EXIF data.
    ImageMetadata metadata = Imaging.getMetadata(image.getFile());
    JpegImageMetadata jpegMetadata = (JpegImageMetadata) metadata;

    if (null != jpegMetadata) {
      final TiffImageMetadata exif = jpegMetadata.getExif();
      if (null != exif) {
        outputSet = exif.getOutputSet();
      }
    }
    if (null == outputSet) {
      outputSet = new TiffOutputSet();
    }
    gpsDirectory = outputSet.getOrCreateGPSDirectory();
    exifDirectory = outputSet.getOrCreateExifDirectory();
    rootDirectory = outputSet.getOrCreateRootDirectory();

    gpsDirectory.removeField(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF);
    gpsDirectory.add(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF,
            GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF_VALUE_TRUE_NORTH);

    gpsDirectory.removeField(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION);
    gpsDirectory.add(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION,
            RationalNumber.valueOf(image.getMovingCa()));

    exifDirectory.removeField(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL);
    exifDirectory.add(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL,
            image.getDate("yyyy/MM/dd HH:mm:ss"));

    // Removes the ImageDescription tag, that causes problems in the upload.
    rootDirectory.removeField(TiffTagConstants.TIFF_TAG_IMAGE_DESCRIPTION);

    outputSet.setGPSInDegrees(image.getMovingLatLon().lon(), image.getMovingLatLon().lat());
    File tempFile = File.createTempFile("imagetoupload_" + c, ".tmp");
    c++;
    OutputStream os = new BufferedOutputStream(new FileOutputStream(tempFile));

    // Transforms the image into a byte array.
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    ImageIO.write(image.getImage(), "jpg", outputStream);
    byte[] imageBytes = outputStream.toByteArray();
    new ExifRewriter().updateExifMetadataLossless(imageBytes, os, outputSet);
    os.close();
    return tempFile;
  }

  /**
   * Uploads the given MapillaryImportedImage object.
   *
   * @param image The image to be uploaded
   * @throws IllegalStateException If {@link MapillaryUser#getSecrets()} returns null
   */
  public static void upload(MapillaryImportedImage image) {
    upload(image, UUID.randomUUID());
  }

  /**
   * @param image The image to be uploaded
   * @param uuid  The UUID used to create the sequence.
   */
  public static void upload(MapillaryImportedImage image, UUID uuid) {
    Map<String, String> secretMap = MapillaryUser.getSecrets();
    if (secretMap == null) {
      throw new IllegalStateException("Can't obtain secrents from user");
    }

    String key = MapillaryUser.getUsername() +
      '/' + uuid +
      '/' + image.getMovingLatLon().lat() + // TODO: Make sure, that the double values are not appended as something like "10e-4", "Infinity" or "NaN" (all possible values of Double.toString(double))
      '_' + image.getMovingLatLon().lon() +
      '_' + image.getMovingCa() +
      '_' + image.getCapturedAt() +
      ".jpg";

    String policy;
    String signature;
    policy = secretMap.get("images_policy");
    signature = secretMap.get("images_hash");

    Map<String, String> hash = new HashMap<>();
    hash.put("key", key);
    hash.put("AWSAccessKeyId", "AKIAI2X3BJAT2W75HILA");
    hash.put("acl", "private");
    hash.put("policy", policy);
    hash.put("signature", signature);
    hash.put("Content-Type", "image/jpeg");
    try {
      uploadFile(updateFile(image), hash);
    } catch (ImageReadException | ImageWriteException | IOException e) {
      Main.error(e);
    }
  }

  /**
   * @param file File that is going to be uploaded
   * @param hash Information attached to the upload
   * @throws IllegalArgumentException if the hash doesn't contain all the needed keys.
   */
  private static void uploadFile(File file, Map<String, String> hash) throws IOException {
    HttpClientBuilder builder = HttpClientBuilder.create();
    HttpPost httpPost = new HttpPost(UPLOAD_URL);

    try (CloseableHttpClient httpClient = builder.build()) {
      MultipartEntityBuilder entityBuilder = MultipartEntityBuilder.create();
      for (String key : keys) {
        if (hash.get(key) == null)
          throw new IllegalArgumentException();
        entityBuilder.addPart(key, new StringBody(hash.get(key),
                ContentType.TEXT_PLAIN));
      }
      entityBuilder.addPart("file", new FileBody(file));
      HttpEntity entity = entityBuilder.build();
      httpPost.setEntity(entity);
      try (CloseableHttpResponse response = httpClient.execute(httpPost)) {
        if (response.getStatusLine().toString().contains("204")) {
          PluginState.imageUploaded();
          Main.info(PluginState.getUploadString() + " (Mapillary)");
        } else {
          Main.info("Upload error");
        }
      }
    }
    if (!file.delete()) {
      Main.error("MapillaryPlugin: File could not be deleted during upload");
    }
    MapillaryUtils.updateHelpText();
  }

  /**
   * Uploads the given {@link MapillarySequence}.
   *
   * @param sequence The sequence to upload. It must contain only
   *                 {@link MapillaryImportedImage} objects.
   * @param delete   Whether the images must be deleted after upload or not.
   */
  public static void uploadSequence(MapillarySequence sequence, boolean delete) {
    Main.worker.submit(new SequenceUploadThread(new ConcurrentSkipListSet<>(sequence.getImages()), delete));
  }
}
