package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.URL;
import java.io.InputStreamReader;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.Json;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;

import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;

/**
 * This Class downloads all the pictures in a given sequence and creates the
 * needed MapillaryImage and MapillarySequence objects
 * 
 * @author nokutu
 *
 */
public class MapillarySequenceDownloadThread implements Runnable {

	private MapillaryData data;
	private String url;
	private ExecutorService ex;
	private Bounds bounds;

	public MapillarySequenceDownloadThread(ExecutorService ex,
			MapillaryData data, String url, Bounds bounds) {
		this.data = data;
		this.url = url;
		this.ex = ex;
		this.bounds = bounds;
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
			JsonObject jsonall = Json.createReader(br).readObject();

			if (!jsonall.getBoolean("more") && !ex.isShutdown()) {
				ex.shutdownNow();
			}
			JsonArray jsonseq = jsonall.getJsonArray("ss");
			for (int i = 0; i < jsonseq.size(); i++) {
				JsonObject jsonobj = jsonseq.getJsonObject(i);
				JsonArray cas = jsonobj.getJsonArray("cas");
				JsonArray coords = jsonobj.getJsonArray("coords");
				JsonArray keys = jsonobj.getJsonArray("keys");
				ArrayList<MapillaryImage> images = new ArrayList<>();
				for (int j = 0; j < cas.size(); j++) {
					try {
						images.add(new MapillaryImage(keys.getString(j),
								coords.getJsonArray(j).getJsonNumber(1)
										.doubleValue(), coords.getJsonArray(j)
										.getJsonNumber(0).doubleValue(), cas
										.getJsonNumber(j).doubleValue()));
					} catch (Exception e) {
						// Mapillary service bug here
						// System.out.println(cas.length());
						// System.out.println(coords.length());
						// System.out.println(keys.length());
						System.out.println(e);
					}
				}
				MapillarySequence sequence = new MapillarySequence();
				int first = -1;
				int last = -1;
				int pos = 0;

				// Here it gets only those images which are in the downloaded
				// area.
				for (MapillaryImage img : images) {
					if (first == -1 && bounds.contains(img.getLatLon()))
						first = pos;
					else if (first != -1 && last == -1
							&& !bounds.contains(img.getLatLon()))
						last = pos;
					else if (last != -1 && bounds.contains(img.getLatLon()))
						last = -1;
					pos++;
				}
				if (last == -1) {
					last = pos;
				}
				if (first == -1)
					continue;
				List<MapillaryImage> finalImages = images.subList(first, last);
				for (MapillaryImage img : finalImages) {
					img.setSequence(sequence);
				}
				data.addWithoutUpdate(finalImages);
				sequence.add(finalImages);
			}
		} catch (IOException e) {
			return;
		}
		return;
	}
}
