package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.openstreetmap.josm.gui.PleaseWaitRunnable;
import org.openstreetmap.josm.gui.progress.PleaseWaitProgressMonitor;
import org.openstreetmap.josm.io.OsmTransferException;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.xml.sax.SAXException;

public class MapillaryExportManager extends PleaseWaitRunnable {

	ArrayBlockingQueue<BufferedImage> queue;
	List<MapillaryImage> images;
	String path;

	public MapillaryExportManager(String title, List<MapillaryImage> images, String path) {
		super(title, new PleaseWaitProgressMonitor("Exporting Mapillary Images"), true);
		queue = new ArrayBlockingQueue<>(10);
		this.images = images;
		this.path = path;
	}

	@Override
	protected void cancel() {
		// TODO Auto-generated method stub

	}

	@Override
	protected void realRun() throws SAXException, IOException,
			OsmTransferException {
		Thread writer = new Thread(new MapillaryExportWriterThread(path, queue, images.size(), this.getProgressMonitor()));
		writer.start();
		ThreadPoolExecutor ex = new ThreadPoolExecutor(20, 35, 25,
				TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(10));
		for (MapillaryImage image : images) {
			ex.execute(new MapillaryExportDownloadThread(image, queue));
			try {
				while (ex.getQueue().remainingCapacity() == 0)
					Thread.sleep(100);
			} catch (Exception e) {
				System.out.println(e);
			}
		}
		try {
			writer.join();
		}
		catch(Exception e){
		}

	}

	@Override
	protected void finish() {
		// TODO Auto-generated method stub

	}

}
