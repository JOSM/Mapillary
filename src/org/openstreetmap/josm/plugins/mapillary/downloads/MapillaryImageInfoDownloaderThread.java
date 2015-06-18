package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.io.InputStreamReader;

import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.Json;

import java.util.concurrent.ExecutorService;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;

/**
 * This thread downloads one of the images in a given area.
 * 
 * @author nokutu
 * @see MapillarySqueareDownloadManagerThread
 */
public class MapillaryImageInfoDownloaderThread implements Runnable {
    private final String url;
    private final ExecutorService ex;

    public MapillaryImageInfoDownloaderThread(ExecutorService ex, String url) {
        this.ex = ex;
        this.url = url;
    }

    public void run() {
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(
                    new URL(url).openStream()));
            JsonObject jsonobj = Json.createReader(br).readObject();
            if (!jsonobj.getBoolean("more"))
                ex.shutdown();
            JsonArray jsonarr = jsonobj.getJsonArray("ims");
            JsonObject data;
            for (int i = 0; i < jsonarr.size(); i++) {
                data = jsonarr.getJsonObject(i);
                String key = data.getString("key");
                for (MapillaryAbstractImage image : MapillaryData.getInstance()
                        .getImages()) {
                    if (image instanceof MapillaryImage) {
                        if (((MapillaryImage) image).getKey().equals(key)) {
                            ((MapillaryImage) image).setUser(data
                                    .getString("user"));
                            ((MapillaryImage) image).setCapturedAt(data
                                    .getJsonNumber("captured_at").longValue());
                            ((MapillaryImage) image).setLocation(data
                                    .getString("location"));
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
