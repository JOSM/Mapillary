// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.ParseException;
import java.util.Calendar;

import javax.imageio.ImageIO;

import org.openstreetmap.josm.data.coor.CachedLatLon;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.gpx.GpxImageEntry;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.layer.geoimage.GeoImageLayer;
import org.openstreetmap.josm.gui.layer.geoimage.ImageEntry;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageMetaDataUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.Logging;

/**
 * A MapillaryImoprtedImage object represents a picture imported locally.
 *
 * @author nokutu
 *
 */
public class MapillaryImportedImage extends MapillaryAbstractImage {

  /**
   * Creates a new MapillaryImportedImage object using as date the current date.
   * Using when the EXIF tags doesn't contain that info.
   *
   * @param latLon  The latitude and longitude where the picture was taken.
   * @param ca      Direction of the picture (0 means north).
   * @param file    The file containing the picture.
   * @param pano    The property to indicate whether image is panorama or not.
   */
  public MapillaryImportedImage(final LatLon latLon, final double ca, final File file, final boolean pano) {
    this(latLon, ca, file, pano, Calendar.getInstance().getTimeInMillis());
  }

  /**
   * Main constructor of the class.
   *
   * @param latLon           Latitude and Longitude where the picture was taken.
   * @param ca               Direction of the picture (0 means north),
   * @param file             The file containing the picture.
   * @param datetimeOriginal The date the picture was taken.
   * @param pano             The property to indicate whether image is panorama or not.
   */
  public MapillaryImportedImage(final LatLon latLon, final double ca, final File file, final boolean pano, final String datetimeOriginal) {
    this(latLon, ca, file, pano, parseTimestampElseCurrentTime(datetimeOriginal));
  }

  /**
   * Constructs a new image from an image entry of a {@link GeoImageLayer}.
   * @param geoImage the {@link ImageEntry}, from which the corresponding fields are taken
   * @return new image
   */
  public static MapillaryImportedImage createInstance(final ImageEntry geoImage) {
    if (geoImage == null) {
      return null;
    }
    if (geoImage.getFile() == null) {
      throw new IllegalArgumentException("Can't create an imported image from an ImageEntry without associated file.");
    }
    final CachedLatLon cachedCoord = geoImage.getPos();
    LatLon coord = cachedCoord == null ? null : cachedCoord.getRoundedToOsmPrecision();
    if (coord == null) {
      final MapView mv = MapillaryPlugin.getMapView();
      coord = mv == null ? new LatLon(0, 0) : mv.getProjection().eastNorth2latlon(mv.getCenter());
    }
    final double ca = geoImage.getExifImgDir() == null ? 0 : geoImage.getExifImgDir();
    final long time = geoImage.hasGpsTime()
      ? geoImage.getGpsTime().getTime()
      : geoImage.hasExifTime() ? geoImage.getExifTime().getTime() : System.currentTimeMillis();
    boolean pano = false;
    try (FileInputStream fis = new FileInputStream(geoImage.getFile())) {
      pano = ImageMetaDataUtil.isPanorama(fis);
    } catch(IOException ex) {
      Logging.trace(ex);
    }
    return new MapillaryImportedImage(coord, ca, geoImage.getFile(), pano, time);
  }

  private static long parseTimestampElseCurrentTime(final String timestamp) {
    try {
      return MapillaryUtils.getEpoch(timestamp, "yyyy:MM:dd HH:mm:ss");
    } catch (ParseException e) {
      try {
        return MapillaryUtils.getEpoch(timestamp, "yyyy/MM/dd HH:mm:ss");
      } catch (ParseException e1) {
        return MapillaryUtils.currentTime();
      }
    }
  }

  public MapillaryImportedImage(final LatLon latLon, final double ca, final File file, final boolean pano, final long capturedAt) {
    super(latLon, ca, pano);
    super.setFile(file);
    this.capturedAt = capturedAt;
  }

  /**
   * Returns the pictures of the file.
   *
   * @return A {@link BufferedImage} object containing the picture, or null if
   *         the {@link File} given in the constructor was null.
   * @throws IOException
   *           If the file parameter of the object isn't an image.
   */
  public BufferedImage getImage() throws IOException {
    if (getFile() != null)
      return ImageIO.read(getFile());
    return null;
  }

  @Override
  public int compareTo(GpxImageEntry image) {
    if (image instanceof MapillaryImportedImage)
      return getFile().compareTo(((MapillaryImportedImage) image).getFile());
    return hashCode() - image.hashCode();
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((getFile() == null) ? 0 : getFile().hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!(obj instanceof MapillaryImportedImage)) {
      return false;
    }
    MapillaryImportedImage other = (MapillaryImportedImage) obj;
    if (getFile() == null) {
      if (other.getFile() != null) {
        return false;
      }
    } else if (!getFile().equals(other.getFile())) {
      return false;
    }
    return true;
  }

  @Override
  public Color paintHighlightedColour() {
    return MapillaryColorScheme.SEQ_IMPORTED_HIGHLIGHTED;
  }

  @Override
  public Color paintHighlightedAngleColour() {
    return MapillaryColorScheme.SEQ_IMPORTED_HIGHLIGHTED_CA;
  }

  @Override
  public Color paintSelectedColour() {
    return MapillaryColorScheme.SEQ_IMPORTED_SELECTED;
  }

  @Override
  public Color paintSelectedAngleColour() {
    return MapillaryColorScheme.SEQ_IMPORTED_SELECTED_CA;
  }

  @Override
  public Color paintUnselectedColour() {
    return MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED;
  }

  @Override
  public Color paintUnselectedAngleColour() {
    return MapillaryColorScheme.SEQ_IMPORTED_UNSELECTED_CA;
  }

}
