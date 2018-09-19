// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.export;

import java.awt.image.BufferedImage;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.concurrent.ArrayBlockingQueue;

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
import org.apache.commons.imaging.formats.tiff.write.TiffOutputDirectory;
import org.apache.commons.imaging.formats.tiff.write.TiffOutputSet;

import org.openstreetmap.josm.gui.progress.ProgressMonitor;
import org.openstreetmap.josm.gui.progress.swing.PleaseWaitProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.tools.Logging;

/**
 * Writes the images from the queue in the file system.
 *
 * @author nokutu
 * @see MapillaryExportManager
 */
public class MapillaryExportWriterThread extends Thread {

  private final String path;
  private final ArrayBlockingQueue<BufferedImage> queue;
  private final ArrayBlockingQueue<MapillaryAbstractImage> queueImages;
  private final int amount;
  private final ProgressMonitor monitor;

  /**
   * Main constructor.
   *
   * @param path
   *          Path to write the pictures.
   * @param queue
   *          Queue of {@link MapillaryAbstractImage} objects.
   * @param queueImages
   *          Queue of {@link BufferedImage} objects.
   * @param amount
   *          Amount of images that are going to be exported.
   * @param monitor
   *          Progress monitor.
   */
  public MapillaryExportWriterThread(String path,
      ArrayBlockingQueue<BufferedImage> queue,
      ArrayBlockingQueue<MapillaryAbstractImage> queueImages, int amount,
      ProgressMonitor monitor) {
    this.path = path;
    this.queue = queue;
    this.queueImages = queueImages;
    this.amount = amount;
    this.monitor = monitor;
  }

  @Override
  public void run() {
    this.monitor.setCustomText("Downloaded 0/" + this.amount);
    BufferedImage img;
    MapillaryAbstractImage mimg;
    String finalPath = "";
    for (int i = 0; i < this.amount; i++) {
      try {
        img = this.queue.take();
        mimg = this.queueImages.take();
        if (this.path == null && mimg instanceof MapillaryImportedImage) {
          String path = ((MapillaryImportedImage) mimg).getFile().getPath();
          finalPath = path.substring(0, path.lastIndexOf('.'));
        } else if (mimg instanceof MapillaryImage) {
          finalPath = this.path + '/' + ((MapillaryImage) mimg).getKey();
        } else if (mimg instanceof MapillaryImportedImage) {
          finalPath = this.path + '/' + ((MapillaryImportedImage) mimg).getFile().getName();
        }

        // Transforms the image into a byte array.
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ImageIO.write(img, "jpg", outputStream);
        byte[] imageBytes = outputStream.toByteArray();

        // Write EXIF tags
        TiffOutputSet outputSet = null;
        TiffOutputDirectory exifDirectory;
        TiffOutputDirectory gpsDirectory;
        // If the image is imported, loads the rest of the EXIF data.
        if (mimg instanceof MapillaryImportedImage) {
          final ImageMetadata metadata = Imaging
              .getMetadata(((MapillaryImportedImage) mimg).getFile());
          final JpegImageMetadata jpegMetadata = (JpegImageMetadata) metadata;
          if (null != jpegMetadata) {
            final TiffImageMetadata exif = jpegMetadata.getExif();
            if (null != exif) {
              outputSet = exif.getOutputSet();
            }
          }
        }
        if (null == outputSet) {
          outputSet = new TiffOutputSet();
        }
        exifDirectory = outputSet.getOrCreateExifDirectory();
        gpsDirectory = outputSet.getOrCreateGPSDirectory();

        gpsDirectory.removeField(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF);
        gpsDirectory.add(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF,
            GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF_VALUE_TRUE_NORTH);

        gpsDirectory.removeField(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION);
        gpsDirectory.add(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION,
            RationalNumber.valueOf(mimg.getMovingCa()));

        exifDirectory.removeField(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL);
        if (mimg instanceof MapillaryImportedImage) {
          exifDirectory.add(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL,
              mimg.getDate("yyyy/MM/dd HH:mm:ss"));
        } else if (mimg instanceof MapillaryImage) {
          exifDirectory.add(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL,
              mimg.getDate("yyyy/MM/dd HH/mm/ss"));
        }
        outputSet.setGPSInDegrees(mimg.getMovingLatLon().lon(), mimg.getMovingLatLon().lat());
        OutputStream os = new BufferedOutputStream(new FileOutputStream(finalPath + ".jpg"));
        new ExifRewriter().updateExifMetadataLossless(imageBytes, os, outputSet);

        os.close();
      } catch (InterruptedException e) {
        Logging.info("Mapillary export cancelled");
        return;
      } catch (IOException | ImageReadException | ImageWriteException e) {
        Logging.error(e);
      }

      // Increases the progress bar.
      this.monitor.worked(PleaseWaitProgressMonitor.PROGRESS_BAR_MAX / this.amount);
      this.monitor.setCustomText("Downloaded " + (i + 1) + "/" + this.amount);
    }
  }
}
