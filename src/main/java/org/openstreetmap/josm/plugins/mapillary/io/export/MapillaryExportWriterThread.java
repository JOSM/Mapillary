// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.export;

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
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.gui.progress.ProgressMonitor;
import org.openstreetmap.josm.gui.progress.swing.PleaseWaitProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.tools.Logging;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.ArrayBlockingQueue;

/**
 * Writes the images from the queue in the file system.
 *
 * @author nokutu
 * @see MapillaryExportManager
 */
public class MapillaryExportWriterThread extends Thread {

    private final String path;
    private final ArrayBlockingQueue<BufferedImage> queue;
    private final ArrayBlockingQueue<INode> queueImages;
    private final int amount;
    private final ProgressMonitor monitor;

    /**
     * Main constructor.
     *
     * @param path
     *        Path to write the pictures.
     * @param queue
     *        Queue of {@link INode} objects.
     * @param queueImages
     *        Queue of {@link BufferedImage} objects.
     * @param amount
     *        Amount of images that are going to be exported.
     * @param monitor
     *        Progress monitor.
     */
    public MapillaryExportWriterThread(String path, ArrayBlockingQueue<BufferedImage> queue,
        ArrayBlockingQueue<INode> queueImages, int amount, ProgressMonitor monitor) {
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
        INode mimg;
        String finalPath;
        for (int i = 0; i < this.amount; i++) {
            try {
                img = this.queue.take();
                mimg = this.queueImages.take();
                if (this.path == null && mimg.hasKey(MapillaryImageUtils.IMPORTED_KEY)) {
                    String runPath = new File(mimg.get(MapillaryImageUtils.IMPORTED_KEY)).getPath();
                    finalPath = runPath.substring(0, runPath.lastIndexOf('.'));
                } else if (MapillaryImageUtils.getKey(mimg) != 0 && mimg.getUniqueId() > 0) {
                    finalPath = Paths
                        .get(this.path, MapillaryImageUtils.getSequenceKey(mimg), Long.toString(mimg.getUniqueId()))
                        .toString();
                } else if (mimg.hasKey(MapillaryImageUtils.IMPORTED_KEY)) {
                    finalPath = Paths.get(this.path, mimg.get(MapillaryImageUtils.IMPORTED_KEY)).toString();
                } else {
                    // Increases the progress bar.
                    this.monitor.worked(PleaseWaitProgressMonitor.PROGRESS_BAR_MAX / this.amount);
                    this.monitor.setCustomText("Downloaded " + (i + 1) + "/" + this.amount);
                    continue;
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
                if (mimg.hasKey(MapillaryImageUtils.IMPORTED_KEY)) {
                    final ImageMetadata metadata = Imaging
                        .getMetadata(new File(mimg.get(MapillaryImageUtils.IMPORTED_KEY)));
                    if (metadata instanceof JpegImageMetadata) {
                        final JpegImageMetadata jpegMetadata = (JpegImageMetadata) metadata;
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
                    RationalNumber.valueOf(MapillaryImageUtils.getAngle(mimg)));

                exifDirectory.removeField(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL);
                // TODO this might need some fixes
                // "yyyy/MM/dd HH:mm:ss" for imported
                // "yyyy/MM/dd HH/mm/ss" for Mapillary
                // exif specifies YYYY:MM:DD HH:MM:SS format
                final String dateTime = ZonedDateTime.ofInstant(MapillaryImageUtils.getDate(mimg), ZoneOffset.UTC)
                    .format(DateTimeFormatter.ofPattern("yyyy:MM:dd HH:mm:ss"));
                exifDirectory.add(ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL, dateTime);

                outputSet.setGPSInDegrees(mimg.getCoor().lon(), mimg.getCoor().lat());
                File file = new File(finalPath + ".jpg");
                File parentFile = file.getParentFile();
                if (!parentFile.exists() && !parentFile.isDirectory()) {
                    parentFile.mkdirs();
                    parentFile.setLastModified(MapillaryImageUtils.getDate(mimg).toEpochMilli());
                }
                try (OutputStream os = new BufferedOutputStream(new FileOutputStream(file))) {
                    new ExifRewriter().updateExifMetadataLossless(imageBytes, os, outputSet);
                }
                if (!file.setLastModified(MapillaryImageUtils.getDate(mimg).toEpochMilli())) {
                    Logging.info("Unable to set last modified time for {0} to {1}", file,
                        MapillaryImageUtils.getDate(mimg));
                }
            } catch (InterruptedException e) {
                Logging.info("Mapillary export cancelled");
                Thread.currentThread().interrupt();
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
