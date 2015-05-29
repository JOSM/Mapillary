package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.ArrayBlockingQueue;

import javax.imageio.ImageIO;

import org.openstreetmap.josm.gui.progress.PleaseWaitProgressMonitor;
import org.openstreetmap.josm.gui.progress.ProgressMonitor;


public class MapillaryExportWriterThread implements Runnable {

	private String path;
	private ArrayBlockingQueue<BufferedImage> queue;
	private int amount;
	private ProgressMonitor monitor;
	
	public MapillaryExportWriterThread(String path,
			ArrayBlockingQueue<BufferedImage> queue, int amount, ProgressMonitor monitor) {
		this.path = path;
		this.queue = queue;
		this.amount = amount;
		this.monitor = monitor;
	}

	@Override
	public void run() {
		monitor.setCustomText("Downloaded 0/" + amount);
		File outputfile;
		BufferedImage img;
		for (int i = 0; i < amount; i++) {
			try {
				img = queue.take();
				outputfile = new File(path + "/" + i + ".png");
				ImageIO.write(img, "png", outputfile);
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
