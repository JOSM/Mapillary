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

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;

/**
 * This thread downloads all the pictures in a given sequence and creates the
 * needed MapillaryImage and MapillarySequence objects. It just stores the ones
 * in the given area.
 * 
 * @author nokutu
 * @see MapillarySquareDownloadManagarThread
 */
public class MapillarySequenceDownloadThread implements Runnable {

    private String url;
    private ExecutorService ex;
    private List<Bounds> bounds;

    public MapillarySequenceDownloadThread(ExecutorService ex, String url) {
        this.url = url;
        this.ex = ex;
        this.bounds = MapillaryLayer.getInstance().bounds;
    }

    public void run() {
        try {
            BufferedReader br;
            br = new BufferedReader(new InputStreamReader(
                    new URL(url).openStream()));
            JsonObject jsonall = Json.createReader(br).readObject();

            if (!jsonall.getBoolean("more") && !ex.isShutdown())
                ex.shutdown();
            JsonArray jsonseq = jsonall.getJsonArray("ss");
            // At the moment there is a bug with some sequences at Mapillay API,
            // so if they are wrong he use this variable to skip them.
            boolean isSequenceWrong = false;
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
                    } catch (IndexOutOfBoundsException e) {
                        Main.warn("Mapillary bug at " + url);
                        isSequenceWrong = true;
                    }
                }
                if (isSequenceWrong)
                    break;
                MapillarySequence sequence = new MapillarySequence(
                        jsonobj.getString("key"), jsonobj.getJsonNumber(
                                "captured_at").longValue());

               

                List<MapillaryImage> finalImages = new ArrayList<>(images);
                // Here it gets only those images which are in the downloaded
                // area.
                for (MapillaryAbstractImage img : images) {
                    if (!isInside(img))
                    	finalImages.remove(img);
                }
                
                for (MapillaryImage img : finalImages) {
                    MapillaryData.getInstance().getImages().remove(img);
                    img.setSequence(sequence);
                }
                MapillaryData.getInstance().addWithoutUpdate(
                        new ArrayList<MapillaryAbstractImage>(finalImages));
                sequence.add(finalImages);
            }
        } catch (IOException e) {
            Main.error("Error reading the url " + url
                    + " might be a Mapillary problem.");
        }
    }

    private boolean isInside(MapillaryAbstractImage image) {
        for (int i = 0; i < bounds.size(); i++) {
            if (bounds.get(i).contains(image.getLatLon()))
                return true;
        }
        return false;
    }
}
