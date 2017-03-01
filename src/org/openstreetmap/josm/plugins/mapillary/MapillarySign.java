// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.traffico.TrafficoSign;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;

import javax.imageio.ImageIO;

/**
 * Each {@link MapillarySign} represents a traffic sign detected by the Mapillary's system.
 *
 * @author nokutu
 */
public class MapillarySign {


    private static Map<String, BufferedImage> signMap = new HashMap<>();

    private final String fullName;
    private final String uuid;


    /**
     * @param fullName the name of the sign
     * @param uuid     the id of the detection
     * @throws NullPointerException if the name does not match the regex <code>.*--.*--.*</code> or if it is
     *                              <code>null</code>.
     */
    public MapillarySign(String fullName, String uuid) {
        this.fullName = fullName;
        this.uuid = uuid;
        if (!signMap.containsKey(fullName)) {
            InputStream is = null;
            BufferedImage image = null;
            try {
                URL url = new URL(MapillaryURL.trafficSignIconUrl(fullName));
                image = ImageIO.read(url);
                signMap.put(fullName, image);
            } catch (IOException exp) {
                exp.printStackTrace();
            } finally {
                try {
                    is.close();
                } catch (Exception e) {
                }
            }
        }
    }

    public static MapillarySign getSign(String name) {
        return new MapillarySign(name, "dummy");
    }

    public String getFullName() {
        return fullName;
    }

    @Override
    public String toString() {
        return fullName;
    }

    @Override
    public int hashCode() {
        return 31 + ((fullName == null) ? 0 : fullName.hashCode());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (fullName == null) {
            return ((MapillarySign) o).fullName == null;
        }
        return fullName.equals(((MapillarySign) o).fullName);
    }

    public String getKey() {
        return uuid;
    }

    public BufferedImage getIcon() {
        return signMap.get(fullName);
    }
}
