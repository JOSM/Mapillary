package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.util.concurrent.ThreadPoolExecutor;
import static org.openstreetmap.josm.tools.I18n.tr;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ArrayBlockingQueue;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;

/**
 * This Class is needed to create an indeterminate amount of downloads, because
 * the Mapillary API has a param page which is needed when the amount of
 * requested images is quite big.
 * 
 * @author nokutu
 *
 */
public class MapillarySquareDownloadManagerThread implements Runnable {

	@SuppressWarnings("unused")
	private String urlImages;
	private String urlSequences;
	private MapillaryData data;
	private Bounds bounds;

	public MapillarySquareDownloadManagerThread(MapillaryData data,
			String urlImages, String urlSequences, Bounds bounds) {
		this.data = data;
		this.urlImages = urlImages;
		this.urlSequences = urlSequences;
		this.bounds = bounds;
	}

	public void run() {
		/*
		 * ThreadPoolExecutor ex = new ThreadPoolExecutor(30, 35, 25,
		 * TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(20));
		 * ArrayList<JSONArray> ret = new ArrayList<JSONArray>(); int page = 0;
		 * while (!ex.isShutdown()) { ex.execute(new
		 * MapillarySquareDownloadThread(ex, this.data, urlImages + "&page=" +
		 * page + "&limit=25")); try { if (ex.getQueue().peek() != null)
		 * Thread.sleep(1000); } catch(Exception e){ System.out.println(e); }
		 * page++; }
		 */
		System.out.println(tr("INFORMATION: GET") + urlSequences);
		fullfillSequences();
		return;
	}

	public void fullfillSequences() {
		ThreadPoolExecutor ex = new ThreadPoolExecutor(20, 35, 25,
				TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(5));
		;
		int page = 0;
		while (!ex.isShutdown()) {
			ex.execute(new MapillarySequenceDownloadThread(ex, data,
					urlSequences + "&page=" + page + "&limit=1", bounds));
			try {
				while (ex.getQueue().remainingCapacity() == 0)
					Thread.sleep(100);
			} catch (Exception e) {
				System.out.println(e);
			}
			page++;
		}
		try {
			while (!ex.awaitTermination(15, TimeUnit.SECONDS)) {
			}
		} catch (Exception e) {
			System.out.println(e);
		}
		data.dataUpdated();
	}
}
