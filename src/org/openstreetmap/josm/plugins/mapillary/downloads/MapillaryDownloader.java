package org.openstreetmap.josm.plugins.mapillary.downloads;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.LatLon;

import java.util.concurrent.ConcurrentHashMap;

/**
 * Class that concentrates all the ways of downloading of the plugin.
 * 
 * @author nokutu
 *
 */
public class MapillaryDownloader {

	public final static String BASE_URL = "https://a.mapillary.com/v2/";
	public final static String CLIENT_ID = "NzNRM2otQkR2SHJzaXJmNmdQWVQ0dzo1YTA2NmNlODhlNWMwOTBm";

	private String[] parameters = { "lat", "lon", "distance", "limit",
			"min_lat", "min_lon", "max_lat", "max_lon" };

	public MapillaryDownloader() {
	}

	/**
	 * Gets all the images in a square. It downloads all the images of all the
	 * sequences that pass through the given rectangle.
	 * 
	 * @param minLatLon
	 *            The minimum latitude and longitude of the rectangle.
	 * @param maxLatLon
	 *            The maximum latitude and longitude of the rectangle
	 */
	public void getImages(LatLon minLatLon, LatLon maxLatLon) {
		String url1 = BASE_URL;
		String url2 = BASE_URL;
		url1 += "search/im/";
		url2 += "search/s/";
		ConcurrentHashMap<String, Double> hash = new ConcurrentHashMap<>();
		hash.put("min_lat", minLatLon.lat());
		hash.put("min_lon", minLatLon.lon());
		hash.put("max_lat", maxLatLon.lat());
		hash.put("max_lon", maxLatLon.lon());
		url1 += buildParameters(hash);
		url2 += buildParameters(hash);
		try {
			Main.worker.submit(new MapillarySquareDownloadManagerThread(url1, url2, new Bounds(minLatLon, maxLatLon)));
		} catch (Exception e) {
			Main.error(e);
		}
	}

	private String buildParameters(ConcurrentHashMap<String, Double> hash) {
		String ret = "?client_id=" + CLIENT_ID;
		for (int i = 0; i < parameters.length; i++)
			if (hash.get(parameters[i]) != null)
				ret += "&" + parameters[i] + "=" + hash.get(parameters[i]);
		return ret;
	}
}
