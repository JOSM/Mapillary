package org.openstreetmap.josm.plugins.mapillary.downloads;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.concurrent.ArrayBlockingQueue;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;
import javax.imageio.metadata.IIOMetadata;

import org.apache.commons.jcs.access.CacheAccess;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.gui.progress.ProgressMonitor;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;

//import com.sun.imageio.plugins.png.PNGMetadata;

public class MapillaryExportDownloadThread implements Runnable,
		ICachedLoaderListener {

	String url;
	ArrayBlockingQueue<BufferedImage> queue;
	ProgressMonitor monitor;
	MapillaryImage image;

	public MapillaryExportDownloadThread(MapillaryImage image,
			ArrayBlockingQueue<BufferedImage> queue) {
		url = "https://d1cuyjsrcm0gby.cloudfront.net/" + image.getKey()
				+ "/thumb-2048.jpg";
		this.queue = queue;
		this.image = image;
	}

	@Override
	public void run() {
	    ImageWriter writer = ImageIO.getImageWritersByFormatName("png").next();
	    //IIOMetadata metadata = writer.getDefaultImageMetadata(typeSpecifier, writeParam);
		try {
			CacheAccess<String, BufferedImageCacheEntry> prev = JCSCacheManager
					.getCache("mapillary");
			new MapillaryCache(image.getKey(), MapillaryCache.Type.FULL_IMAGE,
					prev, 200000, 200000, new HashMap<String, String>())
					.submit(this, false);
			queue.put(ImageIO.read(new URL(url)));
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO -generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void loadingFinished(CacheEntry data,
			CacheEntryAttributes attributes, LoadResult result) {
		try {
			queue.put(ImageIO.read(new ByteArrayInputStream(data.getContent())));
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

}
