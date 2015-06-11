package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.concurrent.ExecutorService;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;

public class MapillarySignalDownloaderThread implements Runnable {

	private final String url;
	private final ExecutorService ex;

	public MapillarySignalDownloaderThread(ExecutorService ex, String url) {
		this.ex = ex;
		this.url = url;
	}

	@Override
	public void run() {
		BufferedReader br;
		try {
			System.out.println(url);
			br = new BufferedReader(new InputStreamReader(
					new URL(url).openStream()));
			JsonObject jsonobj = Json.createReader(br).readObject();
			if (!jsonobj.getBoolean("more")) {
				ex.shutdown();
			}
			JsonArray jsonarr = jsonobj.getJsonArray("ims");
			for (int i = 0; i < jsonarr.size(); i++) {
				JsonArray rects = jsonarr.getJsonObject(i)
						.getJsonArray("rects");
				if (rects == null)
					return;
				String key = jsonarr.getJsonObject(i).getString("key");
				for (int j = 0; j < rects.size(); j++) {
					JsonObject data = rects.getJsonObject(j);
					for (MapillaryAbstractImage image : MapillaryData
							.getInstance().getImages()) {
						if (((MapillaryImage) image).getKey().equals(key)) {
							if (image instanceof MapillaryImage) {
								if (((MapillaryImage) image).getKey().equals(
										key)) {
									((MapillaryImage) image).addSignal(data
											.getString("type"));
								}
							}
						}
					}
				}
			}
		} catch (MalformedURLException e) {
			Main.error(e);
		} catch (IOException e) {
			Main.error(e);
		}
	}
}
