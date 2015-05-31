package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.ArrayBlockingQueue;

import javax.imageio.ImageIO;

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

	private String path;
	private ArrayBlockingQueue<BufferedImage> queue;
	private ArrayBlockingQueue<MapillaryImage> queueImages;
	private int amount;
	private ProgressMonitor monitor;

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
		File outputfile = null;
		BufferedImage img;
		MapillaryImage mimg = null;
		String finalPath = "";
		for (int i = 0; i < amount; i++) {
			try {
				img = queue.take();
				mimg = queueImages.take();
				finalPath = path + "/" + mimg.getKey() + ".jpeg";
				outputfile = new File(finalPath);
				ImageIO.write(img, "jpeg", outputfile);
			} catch (InterruptedException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			monitor.worked(PleaseWaitProgressMonitor.PROGRESS_BAR_MAX / amount);
			monitor.setCustomText("Downloaded " + (i + 1) + "/" + amount);
		}
	}

}
