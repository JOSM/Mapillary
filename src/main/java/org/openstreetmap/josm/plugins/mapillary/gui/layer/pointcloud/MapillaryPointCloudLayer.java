// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.layer.pointcloud;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.LongStream;
import java.util.zip.InflaterInputStream;

import javax.swing.Action;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.data.coor.ILatLon;
import org.openstreetmap.josm.data.imagery.ImageryInfo;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.layer.ImageryLayer;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.pointcloud.PointCloudReconstruction;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.io.PointCloudParser;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.IMapillaryUrls;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.tools.HttpClient;
import org.openstreetmap.josm.tools.Logging;

/**
 * Show Mapillary Point Clouds as aerial imagery
 */
public class MapillaryPointCloudLayer extends ImageryLayer {
    private final String id;
    private final String url;
    private final ILatLon origin;
    private List<PointCloudReconstruction> reconstructions;

    /**
     * Create a new point cloud layer
     *
     * @param id The id of the point cloud
     * @param url The URL of the point cloud
     * @param origin A point to use for determining the origin of the layer
     */
    public MapillaryPointCloudLayer(String id, String url, ILatLon origin) {
        super(new ImageryInfo("Mapillary Point Cloud: " + id, url));
        this.id = id;
        this.url = url;
        this.origin = origin;
    }

    @Override
    protected Action getAdjustAction() {
        return null;
    }

    @Override
    protected List<OffsetMenuEntry> getOffsetMenuEntries() {
        return List.of();
    }

    @Override
    public String getToolTipText() {
        return "Point Cloud: " + this.id;
    }

    @Override
    public void visitBoundingBox(BoundingXYVisitor v) {

    }

    @Override
    public Action[] getMenuEntries() {
        return new Action[0];
    }

    @Override
    public void paint(Graphics2D g, MapView mv, Bounds bbox) {
        if (reconstructions == null) {
            reconstructions = Collections.emptyList();
            MainApplication.worker.execute(() -> this.getData(bbox));
        }
        // "gps_position": [
        // -731.7142190674425,
        // -373.7361228710761,
        // 1399.7760358098894
        // ],
        final var currentOrigin = mv.getPoint(this.origin);
        final var xyzToPixel = 1.5 / mv.getScale();
        for (var reconstruction : reconstructions) {
            // Perform world translation
            final Point2D reference;
            if (reconstruction.referenceLatLonAlt() != null) {
                reference = mv.getPoint(reconstruction.referenceLatLonAlt());
                g.setTransform(AffineTransform.getTranslateInstance(reference.getX(), reference.getY()));
                g.setColor(Color.RED);
                g.drawRect(-2, -2, 4, 4);
            } else {
                // Find the potential image(s)
                final var potentialImages = reconstruction.shots().values().stream()
                    .filter(s -> s.captureTime().equals(((MapillaryNode) this.origin).getInstant())).toList();
                // For now, just handle the "simple" case where we have a single possible image
                if (potentialImages.size() == 1) {
                    final var xyz = potentialImages.get(0).gpsPosition();
                    g.setTransform(
                        AffineTransform.getTranslateInstance(currentOrigin.x - xyz.z(), currentOrigin.y - xyz.x()));
                    reference = new Point2D.Double(currentOrigin.x - xyz.z(), currentOrigin.y - xyz.x());
                } else {
                    reference = null;
                }
            }
            final Rectangle2D bounds = new Rectangle2D.Double();
            for (var point : reconstruction.points().values().stream()
                .sorted(Comparator.comparingDouble(d -> d.coordinates().z())).toList()) {
                final var point2d = new Point2D.Double(xyzToPixel * point.coordinates().x(),
                    -xyzToPixel * point.coordinates().y());
                g.setColor(new Color(point.color().r(), point.color().g(), point.color().b()));
                g.drawRect((int) point2d.x, (int) point2d.y, 1, 1);
                bounds.add(point2d);
            }
            if (reference != null && Logging.isTraceEnabled() && false) {
                g.setColor(Color.RED);
                g.draw(bounds);
                g.drawLine((int) bounds.getMinX(), (int) bounds.getMinY(), 0, 0);
            }
        }
        g.setTransform(AffineTransform.getTranslateInstance(0, 0));
    }

    private static JsonObject cacheSfmKey(INode... images) {
        final var imageIds = Arrays.stream(images).mapToLong(INode::getUniqueId).toArray();
        final var sfmCluster = MapillaryImageUtils.ImageProperties.SFM_CLUSTER;
        final JsonObject json;
        final URI tUrl;
        if (imageIds.length == 1) {
            tUrl = URI.create(MapillaryConfig.getUrls().getImageInformation(images[0].getId(), sfmCluster));
        } else {
            Map<String, String> queryFields = new HashMap<>(2);
            queryFields.put(IMapillaryUrls.FIELDS, sfmCluster.toString());
            queryFields.put("image_ids",
                LongStream.of(imageIds).mapToObj(Long::toString).collect(Collectors.joining(",")));
            tUrl = URI.create(
                MapillaryConfig.getUrls().getBaseMetaDataUrl() + "images" + IMapillaryUrls.queryString(queryFields));
        }
        try {
            json = OAuthUtils.getWithHeader(tUrl);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

        // Stick the data in the objects so we don't have to fetch it again later.
        for (JsonObject obj : json.getJsonArray("data").getValuesAs(JsonObject.class)) {
            if (obj.containsKey(sfmCluster.toString())) {
                final var id = Long.parseLong(obj.getString(MapillaryImageUtils.ImageProperties.ID.toString()));
                Arrays.stream(images).filter(i -> i.getUniqueId() == id).findFirst().ifPresent(
                    image -> image.put(sfmCluster.toString(), obj.getJsonObject(sfmCluster.toString()).toString()));
            }
        }
        return json;
    }

    private void getData(Bounds bbox) {
        if (true) {
            /* The collection of reconstructions to show when we finish */
            final var tReconstructions = new ArrayList<PointCloudReconstruction>();
            /* Reconstructions we have already fetched */
            final var currentReconstructions = new HashMap<String, List<PointCloudReconstruction>>();
            /* Images that we have already fetched */
            final var fetched = new HashSet<Long>();
            final var searchNodes = MapillaryLayer.getInstance().getData().searchNodes(bbox.toBBox());
            cacheSfmKey(searchNodes.stream()
                .filter(node -> !node.hasKey(MapillaryImageUtils.ImageProperties.SFM_CLUSTER.toString()))
                .toArray(INode[]::new));
            for (var selected : searchNodes) {
                if (fetched.contains(selected.getUniqueId()) || selected.getUniqueId() <= 0) {
                    continue;
                }
                final var sfmJson = getSfmCluster(selected);
                if (sfmJson == null || !sfmJson.containsKey("url") || !sfmJson.containsKey("id")) {
                    continue; // No clue what is causing this on the backend. Skip for now.
                }
                fetched.add(selected.getUniqueId());

                final var tUrl = sfmJson.getString("url");
                final var id = sfmJson.getString("id");
                if (currentReconstructions.containsKey(id)) {
                    continue;
                }
                final var cReconstructions = parseReconstruction(tUrl);
                tReconstructions.addAll(cReconstructions);
                this.reconstructions = List.copyOf(tReconstructions);
                currentReconstructions.put(id, cReconstructions);
            }
            tReconstructions.trimToSize();
            this.reconstructions = Collections.unmodifiableList(tReconstructions);
        } else {
            this.reconstructions = parseReconstruction(this.url);
        }
    }

    private static JsonObject getSfmCluster(INode selected) {
        final var sfmCluster = MapillaryImageUtils.ImageProperties.SFM_CLUSTER;
        if (!selected.hasKey(sfmCluster.toString())) {
            return cacheSfmKey(selected);
        }
        if (selected.hasKey(sfmCluster.toString())) {
            try (var reader = Json.createReader(
                new ByteArrayInputStream(selected.get(sfmCluster.toString()).getBytes(StandardCharsets.UTF_8)))) {
                return reader.readObject();
            }
        }
        return Json.createObjectBuilder().build();
    }

    private static List<PointCloudReconstruction> parseReconstruction(String url) {
        try {
            final var client = HttpClient.create(URI.create(url).toURL());
            final var response = client.connect();
            try (var iis = new InflaterInputStream(response.getContent())) {
                return PointCloudParser.parse(iis);
            } finally {
                client.disconnect();
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
