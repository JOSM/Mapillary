package org.openstreetmap.josm.plugins.mapillary;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import javax.imageio.ImageIO;

/**
 * A MapillaryImoprtedImage object represents a picture imported locally.
 *
 * @author nokutu
 *
 */
public class MapillaryImportedImage extends MapillaryAbstractImage {

  /** The picture file. */
  protected File file;
  /** The date when the picture was taken. */
  public final long datetimeOriginal;

  /**
   * Creates a new MapillaryImportedImage object using as date the current date,
   * because it is missing in the file.
   *
   * @param lat
   *          Latitude where the picture was taken.
   * @param lon
   *          Longitude where the picture was taken.
   * @param ca
   *          Direction of the picture (0 means north).
   * @param file
   *          The file containing the picture.
   */
  public MapillaryImportedImage(double lat, double lon, double ca, File file) {
    this(lat, lon, ca, file, currentDate());
  }

  /**
   * Main constructor of the class.
   *
   * @param lat
   *          Latitude where the picture was taken.
   * @param lon
   *          Longitude where the picture was taken.
   * @param ca
   *          Direction of the picture (0 means north),
   * @param file
   *          The file containing the picture.
   * @param datetimeOriginal
   *          The date the picture was taken.
   */
  public MapillaryImportedImage(double lat, double lon, double ca, File file,
      String datetimeOriginal) {
    super(lat, lon, ca);
    this.file = file;
    this.datetimeOriginal = getEpoch(datetimeOriginal, "yyyy:MM:dd hh:mm:ss");
  }

  /**
   * Returns the pictures of the file.
   *
   * @return A BufferedImage object containing the picture, or null if the
   *         {@link File} given in the constructor was null.
   * @throws IOException
   *           If the file parameter of the object isn't an image.
   */
  public BufferedImage getImage() throws IOException {
    return ImageIO.read(this.file);
  }

  /**
   * Returns the File object where the picture is located.
   *
   * @return The File object where the picture is located.
   */
  public File getFile() {
    return this.file;
  }

  @Override
  public boolean equals(Object object) {
    if (object instanceof MapillaryImportedImage)
      return this.file.equals(((MapillaryImportedImage) object).file);
    return false;
  }

  @Override
  public int hashCode() {
    return this.file.hashCode();
  }

  private static String currentDate() {
    Calendar cal = Calendar.getInstance();
    SimpleDateFormat formatter = new SimpleDateFormat("yyyy:MM:dd hh:mm:ss");
    return formatter.format(cal.getTime());
  }
}
