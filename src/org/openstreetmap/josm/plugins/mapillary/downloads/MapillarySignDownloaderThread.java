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
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;

public class MapillarySignDownloaderThread implements Runnable {

    private final String url;
    private final ExecutorService ex;
    private final MapillaryLayer layer;

    public MapillarySignDownloaderThread(ExecutorService ex, String url,
            MapillaryLayer layer) {
        this.ex = ex;
        this.url = url;
        this.layer = layer;
    }

    @Override
    public void run() {
        BufferedReader br;
        try {
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
                JsonArray rectversions = jsonarr.getJsonObject(i).getJsonArray(
                        "rectversions");
                String key = jsonarr.getJsonObject(i).getString("key");
                if (rectversions != null) {
                    for (int j = 0; j < rectversions.size(); j++) {
                        rects = rectversions.getJsonObject(j).getJsonArray(
                                "rects");
                        for (int k = 0; k < rects.size(); k++) {
                            JsonObject data = rects.getJsonObject(k);
                            for (MapillaryAbstractImage image : layer.data
                                    .getImages())
                                if (image instanceof MapillaryImage
                                        && ((MapillaryImage) image).getKey()
                                                .equals(key))
                                    ((MapillaryImage) image).addSign(data
                                            .getString("type"));
                        }
                    }
                }

                // Just one sign on the picture
                else if (rects != null) {
                    for (int j = 0; j < rects.size(); j++) {
                        JsonObject data = rects.getJsonObject(j);
                        for (MapillaryAbstractImage image : layer.data
                                .getImages())
                            if (image instanceof MapillaryImage
                                    && ((MapillaryImage) image).getKey()
                                            .equals(key))
                                ((MapillaryImage) image).addSign(data
                                        .getString("type"));
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
