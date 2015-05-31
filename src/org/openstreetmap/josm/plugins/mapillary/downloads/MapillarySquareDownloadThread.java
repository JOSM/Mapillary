package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.io.BufferedReader;
import java.net.URL;
import java.io.InputStreamReader;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.Json;

import java.util.ArrayList;
import java.util.concurrent.ExecutorService;

import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;

/**
 * This thread downloads one of the images in a given area.
 * 
 * @author nokutu
 * @see MapillarySqueareDownloadManagerThread
 */
public class MapillarySquareDownloadThread implements Runnable {
	String url;
	MapillaryData data;
	ExecutorService ex;

	public MapillarySquareDownloadThread(ExecutorService ex,
			MapillaryData data, String url) {
		this.ex = ex;
		this.data = data;
		this.url = url;
	}

	public void run() {
		try {
			BufferedReader br;
			br = new BufferedReader(new InputStreamReader(
					new URL(url).openStream()));
			/*
			 * String jsonLine = ""; while (br.ready()) { jsonLine +=
			 * br.readLine(); }
			 */
			JsonObject jsonobj = Json.createReader(br).readObject();
			if (!jsonobj.getBoolean("more")) {
				ex.shutdownNow();
			}
			JsonArray jsonarr = jsonobj.getJsonArray("ims");
			ArrayList<MapillaryImage> images = new ArrayList<>();
			JsonObject image;
			for (int i = 0; i < jsonarr.size(); i++) {
				try {
					image = jsonarr.getJsonObject(i);
					images.add(new MapillaryImage(image.getString("key"), image
							.getJsonNumber("lat").doubleValue(), image
							.getJsonNumber("lon").doubleValue(), image
							.getJsonNumber("ca").doubleValue()));
				} catch (Exception e) {
					System.out.println(e);
				}
			}
			data.add(images);
			return;
		} catch (Exception e) {
			return;
		}
	}
}
