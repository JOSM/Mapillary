// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;

import org.openstreetmap.josm.tools.Logging;

public final class ImageUtil {
    private ImageUtil() {
        // Private constructor to avoid instantiation
    }

    /**
     * Scales an {@link ImageIcon} to the desired size
     *
     * @param icon the icon, which should be resized
     * @param size the desired length of the longest edge of the icon
     * @return the resized {@link ImageIcon}. It is the same object that you put in,
     *         only the contained {@link Image} is exchanged.
     */
    public static ImageIcon scaleImageIcon(final ImageIcon icon, int size) {
        Logging.debug("Scale icon {0} → {1}", icon.getIconWidth(), size);
        return new ImageIcon(icon.getImage().getScaledInstance(
            icon.getIconWidth() >= icon.getIconHeight() ? size
                : Math.max(1, Math.round(icon.getIconWidth() / (float) icon.getIconHeight() * size)),
            icon.getIconHeight() >= icon.getIconWidth() ? size
                : Math.max(1, Math.round(icon.getIconHeight() / (float) icon.getIconWidth() * size)),
            Image.SCALE_SMOOTH));
    }

    public static BufferedImage scale(BufferedImage src, int width, int height) {
        BufferedImage dest = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Graphics2D g = dest.createGraphics();
        AffineTransform at = AffineTransform.getScaleInstance((double) width / src.getWidth(),
            (double) height / src.getHeight());
        g.drawRenderedImage(src, at);
        return dest;
    }
}
