package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.awt.image.BufferedImage;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.concurrent.ArrayBlockingQueue;

import javax.imageio.ImageIO;

import org.apache.commons.imaging.ImageReadException;
import org.apache.commons.imaging.ImageWriteException;
import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.jpeg.exif.ExifRewriter;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;
import org.apache.commons.imaging.formats.tiff.write.TiffOutputDirectory;
import org.apache.commons.imaging.formats.tiff.write.TiffOutputSet;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.progress.PleaseWaitProgressMonitor;
import org.openstreetmap.josm.gui.progress.ProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;

/**
 * Writes the images from the queue in the HD.
 * 
 * @author nokutu
 * @see MapillaryExportManager
 */
public class MapillaryExportWriterThread implements Runnable {

	private final String path;
	private final ArrayBlockingQueue<BufferedImage> queue;
	private final ArrayBlockingQueue<MapillaryImage> queueImages;
	private final int amount;
	private final ProgressMonitor monitor;

	public MapillaryExportWriterThread(String path,
			ArrayBlockingQueue<BufferedImage> queue,
			ArrayBlockingQueue<MapillaryImage> queueImages, int amount,
			ProgressMonitor monitor) {
		this.path = path;
		this.queue = queue;
		this.queueImages = queueImages;
		this.amount = amount;
		this.monitor = monitor;
	}

	@Override
	public void run() {
		monitor.setCustomText("Downloaded 0/" + amount);
		File tempFile = null;
		BufferedImage img;
		MapillaryImage mimg = null;
		String finalPath = "";
		for (int i = 0; i < amount; i++) {
			try {
				img = queue.take();
				mimg = queueImages.take();
				finalPath = path + "/" + mimg.getKey();
				// Creates a temporal file that is going to be deleted after
				// writing the EXIF tags.
				tempFile = new File(finalPath + ".tmp");
				ImageIO.write(img, "jpg", tempFile);

				// Write EXIF tags
				TiffOutputSet outputSet = new TiffOutputSet();
				TiffOutputDirectory exifDirectory = outputSet
						.getOrCreateGPSDirectory();
				exifDirectory
						.add(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF,
								GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION_REF_VALUE_TRUE_NORTH);
				exifDirectory.add(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION,
						RationalNumber.valueOf(mimg.getCa()));
				outputSet.setGPSInDegrees(mimg.getLatLon().lon(), mimg
						.getLatLon().lat());
				OutputStream os = new BufferedOutputStream(
						new FileOutputStream(finalPath + ".jpg"));
				new ExifRewriter().updateExifMetadataLossless(tempFile, os,
						outputSet);
				tempFile.delete();
				os.close();
			} catch (InterruptedException e) {
				Main.error(e);
			} catch (IOException e) {
				Main.error(e);
			} catch (ImageWriteException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ImageReadException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			// Increases the progress bar.
			monitor.worked(PleaseWaitProgressMonitor.PROGRESS_BAR_MAX / amount);
			monitor.setCustomText("Downloaded " + (i + 1) + "/" + amount);
		}
	}

}
