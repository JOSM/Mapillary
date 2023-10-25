// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.export;

import java.awt.image.BufferedImage;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.ArrayBlockingQueue;

import javax.imageio.ImageIO;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.apache.commons.imaging.ImageReadException;
import org.apache.commons.imaging.ImageWriteException;
import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.jpeg.exif.ExifRewriter;
import org.apache.commons.imaging.formats.jpeg.xmp.JpegXmpRewriter;
import org.apache.commons.imaging.formats.tiff.constants.ExifTagConstants;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;
import org.apache.commons.imaging.formats.tiff.write.TiffOutputDirectory;
import org.apache.commons.imaging.formats.tiff.write.TiffOutputSet;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.gui.progress.ProgressMonitor;
import org.openstreetmap.josm.gui.progress.swing.PleaseWaitProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
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
    private final ArrayBlockingQueue<INode> queueImages;
    private final ProgressMonitor monitor;
    private int amount;
    private int written;

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
        Path file;
        for (int i = 0; i < this.amount; i++) {
            while (this.queue.peek() == null) {
                if (this.amount == this.written) {
                    return;
                }
                synchronized (this.queue) {
                    try {
                        this.queue.wait(100);
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                        return;
                    }
                }
            }
            try {
                img = this.queue.take();
                mimg = this.queueImages.take();
                if (MapillaryImageUtils.getKey(mimg) != 0 && mimg.getUniqueId() > 0) {
                    file = Paths.get(this.path, MapillaryImageUtils.getSequenceKey(mimg), mimg.getUniqueId() + ".jpg");
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

                final String xml = getXmpXml(img, mimg);

                outputSet.setGPSInDegrees(mimg.lon(), mimg.lat());
                Path parentFile = file.getParent();
                if (!Files.exists(parentFile) && !Files.isDirectory(parentFile)) {
                    Files.createDirectories(parentFile);
                    Files.setLastModifiedTime(parentFile,
                        FileTime.fromMillis(MapillaryImageUtils.getDate(mimg).toEpochMilli()));
                }
                try (OutputStream os = new BufferedOutputStream(Files.newOutputStream(file))) {
                    new ExifRewriter().updateExifMetadataLossless(imageBytes, os, outputSet);
                }
                if (xml != null) {
                    Path tFile = Files.createTempFile("mapillary-", "-" + file.getFileName());
                    try (InputStream is = Files.newInputStream(file);
                        OutputStream os = new BufferedOutputStream(Files.newOutputStream(tFile))) {
                        new JpegXmpRewriter().updateXmpXml(is, os, xml);
                    }
                    Files.deleteIfExists(file);
                    Files.move(tFile, file);
                }
                Files.setLastModifiedTime(file, FileTime.fromMillis(MapillaryImageUtils.getDate(mimg).toEpochMilli()));
                this.written++;
            } catch (InterruptedException e) {
                if (this.written != this.amount) {
                    Logging.info("Mapillary export cancelled");
                    Logging.trace(e);
                }
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

    /**
     * Get the XMP information for this image
     *
     * @param bufferedImage The image data
     * @param imageNode The originating image node with the appropriate information
     * @return The XMP data to write to the image
     */
    @Nullable
    private static String getXmpXml(@Nonnull BufferedImage bufferedImage, @Nonnull INode imageNode) {
        if (MapillaryImageUtils.IS_PANORAMIC.test(imageNode)) {
            // Assume equirectangular for now
            return "<x:xmpmeta xmlns:x='adobe:ns:meta/'>"
                + "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">"
                + "<rdf:Description rdf:about=\"\" xmlns:GPano=\"http://ns.google.com/photos/1.0/panorama/\">"
                + "<GPano:ProjectionType>equirectangular</GPano:ProjectionType>" + "<GPano:CroppedAreaImageWidthPixels>"
                + bufferedImage.getWidth() + "</GPano:CroppedAreaImageWidthPixels>" + "<GPano:FullPanoWidthPixels>"
                + bufferedImage.getWidth() + "</GPano:FullPanoWidthPixels>" + "<GPano:CroppedAreaImageHeightPixels>"
                + bufferedImage.getHeight() + "</GPano:CroppedAreaImageHeightPixels>" + "<GPano:FullPanoHeightPixels>"
                + bufferedImage.getHeight() + "</GPano:FullPanoHeightPixels>"
                // We don't actually do any cropping, so these are 0.
                + "<GPano:CroppedAreaLeftPixels>0</GPano:CroppedAreaLeftPixels>"
                + "<GPano:CroppedAreaTopPixels>0</GPano:CroppedAreaTopPixels>"
                + "</rdf:Description></rdf:RDF></x:xmpmeta>";
        }
        return null;
    }

    /**
     * Called when the size is decreased
     */
    public void decrementSize() {
        this.amount = this.amount - 1;
        if (this.amount == this.written) {
            synchronized (this.queue) {
                this.queue.notifyAll();
            }
        }
    }
}
