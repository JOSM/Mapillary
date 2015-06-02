package org.openstreetmap.josm.plugins.mapillary.downloads;

import static org.openstreetmap.josm.tools.I18n.tr;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.PleaseWaitRunnable;
import org.openstreetmap.josm.gui.progress.PleaseWaitProgressMonitor;
import org.openstreetmap.josm.io.OsmTransferException;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.xml.sax.SAXException;

/**
 * Export main thread. Exportation works by creating a
 * {@link MapillaryWriterThread} and several
 * {@link MapillaryExportDownloadThread}. The second ones download every single
 * image that is going to be exported and stores them in an
 * {@link ArrayBlockingQueue}. Then it is picked by the first one and written on
 * the selected folder. Each image will be named by its key.
 * 
 * @author nokutu
 *
 */
public class MapillaryExportManager extends PleaseWaitRunnable {

	ArrayBlockingQueue<BufferedImage> queue;
	ArrayBlockingQueue<MapillaryImage> queueImages;

	List<MapillaryImage> images;
	String path;

	public MapillaryExportManager(List<MapillaryImage> images, String path) {
		super(tr("Downloading") + "...", new PleaseWaitProgressMonitor(
				"Exporting Mapillary Images"), true);
		queue = new ArrayBlockingQueue<>(10);
		queueImages = new ArrayBlockingQueue<>(10);

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
		// Starts a writer thread in order to write the pictures on the disk.
		Thread writer = new Thread(new MapillaryExportWriterThread(path, queue,
				queueImages, images.size(), this.getProgressMonitor()));
		writer.start();
		ThreadPoolExecutor ex = new ThreadPoolExecutor(20, 35, 25,
				TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(10));
		for (MapillaryImage image : images) {
			try {
				ex.execute(new MapillaryExportDownloadThread(image, queue,
						queueImages));
			} catch (Exception e) {
				Main.error(e);
			}
			try {
				// If the queue is full, waits for it to have more space
				// available before executing anything else.
				while (ex.getQueue().remainingCapacity() == 0)
					Thread.sleep(100);
			} catch (Exception e) {
				Main.error(e);
			}
		}
		try {
			writer.join();
		} catch (Exception e) {
			Main.error(e);
		}

	}

	@Override
	protected void finish() {
		// TODO Auto-generated method stub
	}
}
