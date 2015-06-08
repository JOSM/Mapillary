package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ArrayBlockingQueue;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.PleaseWaitRunnable;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;

/**
 * This Class is needed to create an indeterminate amount of downloads, because
 * the Mapillary API has a param page which is needed when the amount of
 * requested images is quite big.
 * 
 * @author nokutu
 * 
 * @see MapillaryDownloader
 */
public class MapillarySquareDownloadManagerThread extends PleaseWaitRunnable {

	@SuppressWarnings("unused")
	private final String urlImages;
	private final String urlSequences;
	private final Bounds bounds;

	public MapillarySquareDownloadManagerThread(String urlImages, String urlSequences, Bounds bounds) {
		super("Mapillary plugin");
		this.urlImages = urlImages;
		this.urlSequences = urlSequences;
		this.bounds = bounds;
	}

	public void realRun() {
		this.getProgressMonitor().indeterminateSubTask("Downloading images");
		downloadSequences();
	}

	public void downloadSequences() {
		ThreadPoolExecutor ex = new ThreadPoolExecutor(3, 5, 25,
				TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(5));
		int page = 0;
		while (!ex.isShutdown()) {
			ex.execute(new MapillarySequenceDownloadThread(ex,
					urlSequences + "&page=" + page + "&limit=1", bounds));
			try {
				while (ex.getQueue().remainingCapacity() == 0)
					Thread.sleep(100);
			} catch (Exception e) {
				Main.error(e);
			}
			page++;
		}
		try {
			while (!ex.awaitTermination(15, TimeUnit.SECONDS)) {
			}
		} catch (Exception e) {
			Main.error(e);
		}
		MapillaryData.getInstance().dataUpdated();
	}

	@Override
	protected void cancel() {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void finish() {
		// TODO Auto-generated method stub
		
	}
}
