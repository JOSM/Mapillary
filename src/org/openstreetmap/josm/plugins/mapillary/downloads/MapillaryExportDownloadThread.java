package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.concurrent.ArrayBlockingQueue;

import javax.imageio.ImageIO;

import org.openstreetmap.josm.gui.progress.ProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;

public class MapillaryExportDownloadThread implements Runnable {

	String url;
	ArrayBlockingQueue<BufferedImage> queue;
	ProgressMonitor monitor;

	public MapillaryExportDownloadThread(MapillaryImage image, ArrayBlockingQueue<BufferedImage> queue) {
		url = "https://d1cuyjsrcm0gby.cloudfront.net/" + image.getKey()
				+ "/thumb-2048.jpg";
		this.queue = queue;
	}

	@Override
	public void run() {
		try {
			queue.put(ImageIO.read(new URL(url)));
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO -generated catch block
			e.printStackTrace();
		}
	}

}
