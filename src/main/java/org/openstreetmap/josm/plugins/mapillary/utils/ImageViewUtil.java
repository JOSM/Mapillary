// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;

public final class ImageViewUtil {
    private ImageViewUtil() {

    }

    public static void checkVisibleRectPos(Image image, Rectangle visibleRect) {
        if (visibleRect.x < 0) {
            visibleRect.x = 0;
        }
        if (visibleRect.y < 0) {
            visibleRect.y = 0;
        }
        if (visibleRect.x + visibleRect.width > image.getWidth(null)) {
            visibleRect.x = image.getWidth(null) - visibleRect.width;
        }
        if (visibleRect.y + visibleRect.height > image.getHeight(null)) {
            visibleRect.y = image.getHeight(null) - visibleRect.height;
        }
    }

    /**
     * calculateDrawImageRectangle
     *
     * @param imgRect the part of the image that should be drawn (in image coordinates)
     * @param compRect the part of the component where the image should be drawn (in component coordinates)
     * @return the part of compRect with the same width/height ratio as the image
     */
    public static Rectangle calculateDrawImageRectangle(Rectangle imgRect, Rectangle compRect) {
        int x = 0;
        int y = 0;
        int w = compRect.width;
        int h = compRect.height;
        int wFact = w * imgRect.height;
        int hFact = h * imgRect.width;
        if (wFact != hFact) {
            if (wFact > hFact) {
                w = hFact / imgRect.height;
                x = (compRect.width - w) / 2;
            } else {
                h = wFact / imgRect.width;
                y = (compRect.height - h) / 2;
            }
        }
        return new Rectangle(x + compRect.x, y + compRect.y, w, h);
    }

    public static void checkVisibleRectSize(Image image, Rectangle visibleRect) {
        if (visibleRect.width > image.getWidth(null)) {
            visibleRect.width = image.getWidth(null);
        }
        if (visibleRect.height > image.getHeight(null)) {
            visibleRect.height = image.getHeight(null);
        }
    }

    /**
     * Check if the given point is within the given rectangle.
     * If it is out of bounds then it change it is brought back to rectangle's edges.
     *
     * @param p The point to check (will be modified)
     * @param rect The rectangle boundaries
     */
    public static void checkPointInRect(Point p, Rectangle rect) {
        if (p.x < rect.x) {
            p.x = rect.x;
        }
        if (p.x >= rect.x + rect.width) {
            p.x = rect.x + rect.width - 1;
        }
        if (p.y < rect.y) {
            p.y = rect.y;
        }
        if (p.y >= rect.y + rect.height) {
            p.y = rect.y + rect.height - 1;
        }
    }
}
