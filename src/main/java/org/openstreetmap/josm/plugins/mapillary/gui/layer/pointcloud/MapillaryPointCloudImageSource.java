// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer.pointcloud;

import static org.openstreetmap.gui.jmapviewer.OsmMercator.MERCATOR_256;

import java.awt.Point;

import org.openstreetmap.gui.jmapviewer.Coordinate;
import org.openstreetmap.gui.jmapviewer.OsmMercator;
import org.openstreetmap.gui.jmapviewer.Projected;
import org.openstreetmap.gui.jmapviewer.Tile;
import org.openstreetmap.gui.jmapviewer.TileRange;
import org.openstreetmap.gui.jmapviewer.TileXY;
import org.openstreetmap.gui.jmapviewer.interfaces.ICoordinate;
import org.openstreetmap.gui.jmapviewer.interfaces.IProjected;
import org.openstreetmap.gui.jmapviewer.tilesources.AbstractTMSTileSource;
import org.openstreetmap.josm.data.coor.ILatLon;
import org.openstreetmap.josm.data.imagery.ImageryInfo;

/**
 * The source for creating tiles for Mapillary Point Clouds
 */
public class MapillaryPointCloudImageSource extends AbstractTMSTileSource {

    private final ILatLon origin;

    public MapillaryPointCloudImageSource(ImageryInfo info, ILatLon origin) {
        super(info);
        this.origin = origin;
    }

    @Override
    public double getDistance(double lat1, double lon1, double lat2, double lon2) {
        return osmMercator().getDistance(lat1, lon1, lat2, lon2);
    }

    @Override
    public Point latLonToXY(double lat, double lon, int zoom) {
        return new Point((int) Math.round(osmMercator().lonToX(lon, zoom)),
            (int) Math.round(osmMercator().latToY(lat, zoom)));
    }

    @Override
    public ICoordinate xyToLatLon(int x, int y, int zoom) {
        return new Coordinate(osmMercator().yToLat(y, zoom), osmMercator().xToLon(x, zoom));
    }

    @Override
    public TileXY latLonToTileXY(double lat, double lon, int zoom) {
        return new TileXY(osmMercator().lonToX(lon, zoom) / getTileSize(),
            osmMercator().latToY(lat, zoom) / getTileSize());
    }

    @Override
    public ICoordinate tileXYToLatLon(int x, int y, int zoom) {
        return new Coordinate(osmMercator().yToLat((long) y * getTileSize(), zoom),
            osmMercator().xToLon((long) x * getTileSize(), zoom));
    }

    @Override
    public IProjected tileXYtoProjected(int x, int y, int zoom) {
        final var mercatorWidth = 2 * Math.PI * OsmMercator.EARTH_RADIUS;
        final var f = mercatorWidth * getTileSize() / osmMercator().getMaxPixels(zoom);
        return new Projected(f * x - mercatorWidth / 2, -(f * y - mercatorWidth / 2));
    }

    @Override
    public TileXY projectedToTileXY(IProjected p, int zoom) {
        final var mercatorWidth = 2 * Math.PI * OsmMercator.EARTH_RADIUS;
        final var f = mercatorWidth * getTileSize() / osmMercator().getMaxPixels(zoom);
        return new TileXY((p.getEast() + mercatorWidth / 2) / f, (-p.getNorth() + mercatorWidth / 2) / f);
    }

    @Override
    public boolean isInside(Tile inner, Tile outer) {
        final int dz = inner.getZoom() - outer.getZoom();
        if (dz < 0)
            return false;
        return outer.getXtile() == inner.getXtile() >> dz && outer.getYtile() == inner.getYtile() >> dz;
    }

    @Override
    public TileRange getCoveringTileRange(Tile tile, int newZoom) {
        if (newZoom <= tile.getZoom()) {
            final int dz = tile.getZoom() - newZoom;
            final var xy = new TileXY(tile.getXtile() >> dz, tile.getYtile() >> dz);
            return new TileRange(xy, xy, newZoom);
        } else {
            final int dz = newZoom - tile.getZoom();
            final var t1 = new TileXY(tile.getXtile() << dz, tile.getYtile() << dz);
            final var t2 = new TileXY(t1.getX() + (1 << dz) - 1, t1.getY() + (1 << dz) - 1);
            return new TileRange(t1, t2, newZoom);
        }
    }

    @Override
    public String getServerCRS() {
        return "EPSG:3857";
    }

    private static OsmMercator osmMercator() {
        return MERCATOR_256;
    }
}
