package org.openstreetmap.josm.plugins.mapillary;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

public class MapillaryImportedImage extends MapillaryAbstractImage {

    /**
     * The picture file.
     */
    protected File file;
    public final long datetimeOriginal;

    public MapillaryImportedImage(double lat, double lon, double ca, File file,
            String datetimeOriginal) {
        super(lat, lon, ca);
        this.file = file;
        System.out.println(datetimeOriginal);
        this.datetimeOriginal = getEpoch(datetimeOriginal, "yyyy/MM/dd hh:mm:ss");
    }

    /**
     * Returns the pictures of the file.
     * 
     * @return
     * @throws IOException
     */
    public BufferedImage getImage() throws IOException {
        return ImageIO.read(file);
    }

    public File getFile() {
        return file;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof MapillaryImportedImage)
            return this.file.equals(((MapillaryImportedImage) object).file);
        return false;
    }

    @Override
    public int hashCode() {
        return this.file.hashCode();
    }
}
