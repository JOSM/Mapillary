package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.io.BufferedReader;
import java.net.URL;
import java.io.InputStreamReader;

import org.json.JSONObject;
import org.json.JSONArray;

import java.util.ArrayList;
import java.util.concurrent.ExecutorService;

import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;

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
			String jsonLine = "";
			while (br.ready()) {
				jsonLine += br.readLine();
			}
			JSONObject jsonobj = new JSONObject(jsonLine);
			if (!jsonobj.getBoolean("more")) {
				ex.shutdownNow();
			}
			JSONArray jsonarr = jsonobj.getJSONArray("ims");
			ArrayList<MapillaryImage> images = new ArrayList<>();
			JSONObject image;
			for (int i = 0; i < jsonarr.length(); i++) {
				try {
					image = jsonarr.getJSONObject(i);
					images.add(new MapillaryImage(image.getString("key"), image
							.getDouble("lat"), image.getDouble("lon"), image
							.getDouble("ca")));
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
