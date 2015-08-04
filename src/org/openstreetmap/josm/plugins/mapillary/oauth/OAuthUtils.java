package org.openstreetmap.josm.plugins.mapillary.oauth;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.UUID;

import javax.imageio.ImageIO;
import javax.json.Json;
import javax.json.JsonObject;

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
import org.apache.commons.imaging.formats.tiff.write.TiffOutputDirectory;
import org.apache.commons.imaging.formats.tiff.write.TiffOutputSet;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.HttpClientBuilder;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;

/**
 * A set of utilities related to OAuth.
 *
 * @author nokutu
 *
 */
public class OAuthUtils {

  private static final String[] keys = { "key", "AWSAccessKeyId", "acl",
      "policy", "signature", "Content-Type" };
  private static final String UPLOAD_URL = "https://s3-eu-west-1.amazonaws.com/mapillary.uploads.manual.images";

  // Count to name temporal files.
  private static int c = 0;

  /**
   * Returns a JsonObject containing the result of making a GET request with the
   * authorization header.
   *
   * @param url
   *          The {@link URL} where the request must be made.
   * @return A JsonObject containing the result of the GET request.
   * @throws IOException
   *           Errors relating to the connection.
   */
  public static JsonObject getWithHeader(URL url) throws IOException {
    HttpURLConnection con = (HttpURLConnection) url.openConnection();
    con.setRequestMethod("GET");
    con.setRequestProperty("Authorization",
        "Bearer " + Main.pref.get("mapillary.access-token"));

    BufferedReader in = new BufferedReader(new InputStreamReader(
        con.getInputStream()));
    return Json.createReader(in).readObject();
  }

  /**
   * Uploads the given MapillaryImportedImage object.
   *
   * @param image
   *
   * @throws NoSuchAlgorithmException
   * @throws UnsupportedEncodingException
   * @throws InvalidKeyException
   *
   */
  public static void upload(MapillaryImportedImage image)
      throws InvalidKeyException, UnsupportedEncodingException,
      NoSuchAlgorithmException {
    try {
      upload(image, UUID.randomUUID());
    } catch (IOException e) {
      Main.error(e);
    }
  }

  /**
   * @param image
   * @param uuid
   *          The UUID used to create the sequence.
   * @param username
   *          The username who is going to upload the picture.
   * @throws NoSuchAlgorithmException
   * @throws InvalidKeyException
   * @throws IOException
   */
  public static void upload(MapillaryImportedImage image, UUID uuid)
      throws NoSuchAlgorithmException, InvalidKeyException, IOException {
    String key = MapillaryUser.getUsername() + "/" + uuid.toString() + "/"
        + image.getLatLon().lat() + "_" + image.getLatLon().lon() + "_"
        + image.getCa() + "_" + image.datetimeOriginal + ".jpg";

    String policy = null;
    String signature = null;
    policy = MapillaryUser.getSecrets().get("images_policy");
    signature = MapillaryUser.getSecrets().get("images_hash");

    HashMap<String, String> hash = new HashMap<>();
    hash.put("key", key);
    hash.put("AWSAccessKeyId", "AKIAI2X3BJAT2W75HILA");
    hash.put("acl", "private");
    hash.put("policy", policy);
    hash.put("signature", signature);
    hash.put("Content-Type", "image/jpeg");

    try {
      uploadFile(updateFile(image), hash);
    } catch (ImageReadException | ImageWriteException e) {
      Main.error(e);
    }

  }

  /**
   * @param file
   * @param hash
   * @throws IOException
   */
  public static void uploadFile(File file, HashMap<String, String> hash)
      throws IOException {
    HttpClientBuilder builder = HttpClientBuilder.create();
    HttpClient httpClient = builder.build();
    HttpPost httpPost = new HttpPost(UPLOAD_URL);

    MultipartEntityBuilder entityBuilder = MultipartEntityBuilder.create();
    for (String key : keys) {
      entityBuilder.addPart(key, new StringBody(hash.get(key),
          ContentType.TEXT_PLAIN));
      System.out.println(key + " => " + hash.get(key));
    }
    entityBuilder.addPart("file", new FileBody(file));

    HttpEntity entity = entityBuilder.build();
    httpPost.setEntity(entity);

    System.out.println(httpPost);
    HttpResponse response = httpClient.execute(httpPost);
    System.out.println(response.toString());
  }

  /**
   * Uploads the given {@link MapillarySequence}.
   *
   * @param sequence
   *          The sequence to upload. It must contain only
   *          {@link MapillaryImportedImage} objects.
   * @throws NoSuchAlgorithmException
   * @throws UnsupportedEncodingException
   * @throws InvalidKeyException
   */
  public static void uploadSequence(MapillarySequence sequence)
      throws InvalidKeyException, UnsupportedEncodingException,
      NoSuchAlgorithmException {
    UUID uuid = UUID.randomUUID();

    for (MapillaryAbstractImage img : sequence.getImages()) {
      if (!(img instanceof MapillaryImportedImage))
        throw new IllegalArgumentException(
            "The sequence contains downloaded images.");
      try {
        upload((MapillaryImportedImage) img, uuid);
      } catch (IOException e) {
        Main.error(e);
      }
    }
  }

  private static File updateFile(MapillaryImportedImage image)
      throws ImageReadException, IOException, ImageWriteException {
    TiffOutputSet outputSet = null;
    TiffOutputDirectory exifDirectory = null;
    // If the image is imported, loads the rest of the EXIF data.
    ImageMetadata metadata = Imaging.getMetadata(image.getFile());
    final JpegImageMetadata jpegMetadata = (JpegImageMetadata) metadata;
    if (null != jpegMetadata) {
      final TiffImageMetadata exif = jpegMetadata.getExif();
      if (null != exif) {
        outputSet = exif.getOutputSet();
      }
    }
    if (null == outputSet) {
      outputSet = new TiffOutputSet();
    }
    exifDirectory = outputSet.getOrCreateExifDirectory();

    exifDirectory.removeField(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF);
    exifDirectory.add(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF,
        GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF_VALUE_TRUE_NORTH);

    exifDirectory.removeField(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION);
    exifDirectory.add(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION,
        RationalNumber.valueOf(image.getCa()));

    exifDirectory.removeField(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL);
    exifDirectory.add(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL,
        ((MapillaryImportedImage) image).getDate("yyyy/MM/dd hh:mm:ss"));

    outputSet.setGPSInDegrees(image.getLatLon().lon(), image.getLatLon().lat());
    File tempFile = new File(c + ".tmp");
    OutputStream os = new BufferedOutputStream(new FileOutputStream(tempFile));

    // Transforms the image into a byte array.
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    ImageIO.write(image.getImage(), "jpg", outputStream);
    byte[] imageBytes = outputStream.toByteArray();

    new ExifRewriter().updateExifMetadataLossless(imageBytes, os, outputSet);

    return tempFile;
  }
}
