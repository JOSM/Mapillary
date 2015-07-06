package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;
import static org.openstreetmap.josm.tools.I18n.marktr;

import org.apache.commons.jcs.access.CacheAccess;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryDownloadViewAction;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.downloads.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryFilterDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.MapView.EditLayerChangeListener;
import org.openstreetmap.josm.gui.MapView.LayerChangeListener;
import org.openstreetmap.josm.gui.layer.AbstractModifiableLayer;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
import org.openstreetmap.josm.data.osm.visitor.paint.PaintColors;
import org.openstreetmap.josm.gui.dialogs.LayerListDialog;
import org.openstreetmap.josm.gui.dialogs.LayerListPopup;
import org.openstreetmap.josm.data.osm.event.PrimitivesAddedEvent;
import org.openstreetmap.josm.data.osm.event.PrimitivesRemovedEvent;
import org.openstreetmap.josm.data.osm.event.TagsChangedEvent;
import org.openstreetmap.josm.data.osm.event.NodeMovedEvent;
import org.openstreetmap.josm.data.osm.event.WayNodesChangedEvent;
import org.openstreetmap.josm.data.osm.event.RelationMembersChangedEvent;
import org.openstreetmap.josm.data.osm.event.AbstractDatasetChangedEvent;
import org.openstreetmap.josm.data.osm.event.DataChangedEvent;
import org.openstreetmap.josm.data.osm.event.DataSetListener;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.event.MouseAdapter;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.swing.ImageIcon;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JOptionPane;

import java.util.List;
import java.util.ArrayList;

public class MapillaryLayer extends AbstractModifiableLayer implements
        DataSetListener, EditLayerChangeListener, LayerChangeListener {

    public final static int SEQUENCE_MAX_JUMP_DISTANCE = Main.pref.getInteger(
            "mapillary.sequence-max-jump-distance", 100);

    private boolean TEMP_MANUAL = false;

    public static MapillaryLayer INSTANCE;
    public static CacheAccess<String, BufferedImageCacheEntry> CACHE;
    public static MapillaryImage BLUE;
    public static MapillaryImage RED;

    public final MapillaryData data = MapillaryData.getInstance();

    public ArrayList<Bounds> bounds;

    private MouseAdapter mouseAdapter;

    private int highlightPointRadius = Main.pref.getInteger(
            "mappaint.highlight.radius", 7);
    private int highlightStep = Main.pref.getInteger("mappaint.highlight.step",
            4);

    private volatile TexturePaint hatched;

    private MapillaryLayer() {
        super(tr("Mapillary Images"));
        bounds = new ArrayList<>();
        init();
    }

    /**
     * Initializes the Layer.
     */
    private void init() {
        mouseAdapter = new MapillaryMouseAdapter();
        try {
            CACHE = JCSCacheManager.getCache("Mapillary");
        } catch (IOException e) {
            Main.error(e);
        }
        if (Main.map != null && Main.map.mapView != null) {
            Main.map.mapView.addMouseListener(mouseAdapter);
            Main.map.mapView.addMouseMotionListener(mouseAdapter);
            Main.map.mapView.addLayer(this);
            MapView.addEditLayerChangeListener(this, false);
            MapView.addLayerChangeListener(this);
            if (Main.map.mapView.getEditLayer() != null)
                Main.map.mapView.getEditLayer().data.addDataSetListener(this);
        }
        if (MapillaryPlugin.EXPORT_MENU != null) { // Does not execute when in headless mode
            MapillaryPlugin.setMenuEnabled(MapillaryPlugin.EXPORT_MENU, true);
            if (!MapillaryMainDialog.getInstance().isShowing())
                MapillaryMainDialog.getInstance().getButton().doClick();
        }

        createHatchTexture();
        data.dataUpdated();
    }

    public synchronized static MapillaryLayer getInstance() {
        if (MapillaryLayer.INSTANCE == null)
            MapillaryLayer.INSTANCE = new MapillaryLayer();
        return MapillaryLayer.INSTANCE;
    }

    /**
     * Downloads all images of the area covered by the OSM data. This is only
     * just for automatic download.
     */
    public void download() {
        checkAreaTooBig();
        if (Main.pref.getBoolean("mapillary.download-manually") || TEMP_MANUAL)
            return;
        for (Bounds bounds : Main.map.mapView.getEditLayer().data
                .getDataSourceBounds()) {
            if (!this.bounds.contains(bounds)) {
                this.bounds.add(bounds);
                new MapillaryDownloader().getImages(bounds.getMin(),
                        bounds.getMax());
            }
        }
    }

    /**
     * Checks if the area of the OSM data is too big. This means that probably
     * lots of Mapillary images are going to be downloaded, slowing down the
     * program too much. To solve this the automatic is stopped, an alert is
     * shown and you will have to download areas manually.
     */
    private void checkAreaTooBig() {
        double area = 0;
        for (Bounds bounds : Main.map.mapView.getEditLayer().data
                .getDataSourceBounds()) {
            area += bounds.getArea();
        }
        if (area > MapillaryDownloadViewAction.MAX_AREA) {
            TEMP_MANUAL = true;
            MapillaryPlugin.setMenuEnabled(MapillaryPlugin.DOWNLOAD_VIEW_MENU,
                    true);
            JOptionPane
                    .showMessageDialog(
                            Main.parent,
                            tr("The downloaded OSM area is too big. Download mode has been changed to manual until the layer is restarted."));
        }
    }

    /**
     * Returns the MapillaryData object, which acts as the database of the
     * Layer.
     *
     * @return
     */
    public MapillaryData getMapillaryData() {
        return data;
    }

    /**
     * Method invoked when the layer is destroyed.
     */
    @Override
    public void destroy() {
        MapillaryMainDialog.getInstance().setImage(null);
        MapillaryMainDialog.getInstance().updateImage();
        data.getImages().clear();
        MapillaryLayer.INSTANCE = null;
        MapillaryData.INSTANCE = null;
        MapillaryPlugin.setMenuEnabled(MapillaryPlugin.EXPORT_MENU, false);
        MapillaryPlugin.setMenuEnabled(MapillaryPlugin.ZOOM_MENU, false);
        Main.map.mapView.removeMouseListener(mouseAdapter);
        Main.map.mapView.removeMouseMotionListener(mouseAdapter);
        MapView.removeEditLayerChangeListener(this);
        if (Main.map.mapView.getEditLayer() != null)
            Main.map.mapView.getEditLayer().data.removeDataSetListener(this);
        super.destroy();
    }

    /**
     * Returns true any of the images from the database has been modified.
     */
    @Override
    public boolean isModified() {
        for (MapillaryAbstractImage image : data.getImages())
            if (image.isModified())
                return true;
        return false;
    }

    @Override
    public void setVisible(boolean visible) {
        super.setVisible(visible);
        for (MapillaryAbstractImage img : data.getImages())
            img.setVisible(visible);
        MapillaryFilterDialog.getInstance().refresh();
    }

    /**
     * Replies background color for downloaded areas.
     *
     * @return background color for downloaded areas. Black by default
     */
    private Color getBackgroundColor() {
        return Main.pref.getColor(marktr("background"), Color.BLACK);
    }

    /**
     * Replies background color for non-downloaded areas.
     *
     * @return background color for non-downloaded areas. Yellow by default
     */
    private Color getOutsideColor() {
        return Main.pref.getColor(marktr("outside downloaded area"),
                Color.YELLOW);
    }

    /**
     * Initialize the hatch pattern used to paint the non-downloaded area
     */
    private void createHatchTexture() {
        BufferedImage bi = new BufferedImage(15, 15,
                BufferedImage.TYPE_INT_ARGB);
        Graphics2D big = bi.createGraphics();
        big.setColor(getBackgroundColor());
        Composite comp = AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
                0.3f);
        big.setComposite(comp);
        big.fillRect(0, 0, 15, 15);
        big.setColor(getOutsideColor());
        big.drawLine(0, 15, 15, 0);
        Rectangle r = new Rectangle(0, 0, 15, 15);
        hatched = new TexturePaint(bi, r);
    }

    /**
     * Paints the database in the map.
     */
    @Override
    public synchronized void paint(Graphics2D g, MapView mv, Bounds box) {
        if (Main.map.mapView.getActiveLayer() == this) {
            Rectangle b = mv.getBounds();
            // on some platforms viewport bounds seem to be offset from the
            // left,
            // over-grow it just to be sure
            b.grow(100, 100);
            Area a = new Area(b);
            // now successively subtract downloaded areas
            for (Bounds bounds : this.bounds) {
                Point p1 = mv.getPoint(bounds.getMin());
                Point p2 = mv.getPoint(bounds.getMax());
                Rectangle r = new Rectangle(Math.min(p1.x, p2.x), Math.min(
                        p1.y, p2.y), Math.abs(p2.x - p1.x), Math.abs(p2.y
                        - p1.y));
                a.subtract(new Area(r));
            }
            // paint remainder
            g.setPaint(hatched);
            g.fill(a);
        }

        // Draw colored lines
        MapillaryLayer.BLUE = null;
        MapillaryLayer.RED = null;
        MapillaryMainDialog.getInstance().blueButton.setEnabled(false);
        MapillaryMainDialog.getInstance().redButton.setEnabled(false);

        // Sets blue and red lines and enables/disables the buttons
        if (data.getSelectedImage() != null) {
            MapillaryImage[] closestImages = getClosestImagesFromDifferentSequences();
            Point selected = mv.getPoint(data.getSelectedImage().getLatLon());
            if (closestImages[0] != null) {
                MapillaryLayer.BLUE = closestImages[0];
                g.setColor(Color.BLUE);
                g.drawLine(mv.getPoint(closestImages[0].getLatLon()).x,
                        mv.getPoint(closestImages[0].getLatLon()).y,
                        selected.x, selected.y);
                MapillaryMainDialog.getInstance().blueButton.setEnabled(true);
            }
            if (closestImages[1] != null) {
                MapillaryLayer.RED = closestImages[1];
                g.setColor(Color.RED);
                g.drawLine(mv.getPoint(closestImages[1].getLatLon()).x,
                        mv.getPoint(closestImages[1].getLatLon()).y,
                        selected.x, selected.y);
                MapillaryMainDialog.getInstance().redButton.setEnabled(true);
            }
        }
        g.setColor(Color.WHITE);
        for (MapillaryAbstractImage imageAbs : data.getImages()) {
            if (!imageAbs.isVisible())
                continue;
            Point p = mv.getPoint(imageAbs.getLatLon());
            if (imageAbs instanceof MapillaryImage) {
                MapillaryImage image = (MapillaryImage) imageAbs;
                Point nextp = null;
                // Draw sequence line
                if (image.getSequence() != null) {
                    MapillaryImage tempImage = image.next();
                    while (tempImage != null) {
                        if (tempImage.isVisible()) {
                            nextp = mv.getPoint(tempImage.getLatLon());
                            break;
                        }
                        tempImage = tempImage.next();
                    }
                    if (nextp != null)
                        g.drawLine(p.x, p.y, nextp.x, nextp.y);
                }

                ImageIcon icon;
                if (!data.getMultiSelectedImages().contains(image))
                    icon = MapillaryPlugin.MAP_ICON;
                else
                    icon = MapillaryPlugin.MAP_ICON_SELECTED;
                draw(g, image, icon, p);
                if (!image.getSigns().isEmpty()) {
                    g.drawImage(MapillaryPlugin.MAP_SIGN.getImage(),
                            p.x + icon.getIconWidth() / 2,
                            p.y - icon.getIconHeight() / 2, Main.map.mapView);
                }
            } else if (imageAbs instanceof MapillaryImportedImage) {
                MapillaryImportedImage image = (MapillaryImportedImage) imageAbs;
                ImageIcon icon;
                if (!data.getMultiSelectedImages().contains(image))
                    icon = MapillaryPlugin.MAP_ICON_IMPORTED;
                else
                    icon = MapillaryPlugin.MAP_ICON_SELECTED;
                draw(g, image, icon, p);
            }
        }
    }

    /**
     * Draws the highlight of the icon.
     *
     * @param g
     * @param p
     * @param size
     */
    private void drawPointHighlight(Graphics2D g, Point p, int size) {
        Color oldColor = g.getColor();
        Color highlightColor = PaintColors.HIGHLIGHT.get();
        Color highlightColorTransparent = new Color(highlightColor.getRed(),
                highlightColor.getGreen(), highlightColor.getBlue(), 100);
        g.setColor(highlightColorTransparent);
        int s = size + highlightPointRadius;
        while (s >= size) {
            int r = (int) Math.floor(s / 2d);
            g.fillRoundRect(p.x - r, p.y - r, s, s, r, r);
            s -= highlightStep;
        }
        g.setColor(oldColor);
    }

    /**
     * Draws the given icon of an image. Also checks if the mouse is over the
     * image.
     *
     * @param g
     * @param image
     * @param icon
     * @param p
     */
    private void draw(Graphics2D g, MapillaryAbstractImage image,
            ImageIcon icon, Point p) {
        Image imagetemp = icon.getImage();
        BufferedImage bi = (BufferedImage) imagetemp;
        int width = icon.getIconWidth();
        int height = icon.getIconHeight();

        // Rotate the image
        double rotationRequired = Math.toRadians(image.getCa());
        double locationX = width / 2;
        double locationY = height / 2;
        AffineTransform tx = AffineTransform.getRotateInstance(
                rotationRequired, locationX, locationY);
        AffineTransformOp op = new AffineTransformOp(tx,
                AffineTransformOp.TYPE_BILINEAR);

        g.drawImage(op.filter(bi, null), p.x - (width / 2), p.y - (height / 2),
                Main.map.mapView);
        if (data.getHoveredImage() == image) {
            drawPointHighlight(g, p, 16);
        }
    }

    @Override
    public Icon getIcon() {
        return MapillaryPlugin.ICON16;
    }

    @Override
    public boolean isMergable(Layer other) {
        return false;
    }

    @Override
    public void mergeFrom(Layer from) {
        throw new UnsupportedOperationException(
                "This layer does not support merging yet");
    }

    @Override
    public Action[] getMenuEntries() {
        List<Action> actions = new ArrayList<>();
        actions.add(LayerListDialog.getInstance().createShowHideLayerAction());
        actions.add(LayerListDialog.getInstance().createDeleteLayerAction());
        actions.add(new LayerListPopup.InfoAction(this));
        return actions.toArray(new Action[actions.size()]);
    }

    /**
     * Returns the 2 closest images belonging to a different sequence.
     *
     * @return
     */
    private MapillaryImage[] getClosestImagesFromDifferentSequences() {
        if (!(data.getSelectedImage() instanceof MapillaryImage))
            return new MapillaryImage[2];
        MapillaryImage selected = (MapillaryImage) data.getSelectedImage();
        MapillaryImage[] ret = new MapillaryImage[2];
        double[] distances = { SEQUENCE_MAX_JUMP_DISTANCE,
                SEQUENCE_MAX_JUMP_DISTANCE };
        LatLon selectedCoords = data.getSelectedImage().getLatLon();
        for (MapillaryAbstractImage imagePrev : data.getImages()) {
            if (!(imagePrev instanceof MapillaryImage))
                continue;
            if (!imagePrev.isVisible())
                continue;
            MapillaryImage image = (MapillaryImage) imagePrev;
            if (image.getLatLon().greatCircleDistance(selectedCoords) < SEQUENCE_MAX_JUMP_DISTANCE
                    && selected.getSequence() != image.getSequence()) {
                if ((ret[0] == null && ret[1] == null)
                        || (image.getLatLon().greatCircleDistance(
                                selectedCoords) < distances[0] && (ret[1] == null || image
                                .getSequence() != ret[1].getSequence()))) {
                    ret[0] = image;
                    distances[0] = image.getLatLon().greatCircleDistance(
                            selectedCoords);
                } else if ((ret[1] == null || image.getLatLon()
                        .greatCircleDistance(selectedCoords) < distances[1])
                        && image.getSequence() != ret[0].getSequence()) {
                    ret[1] = image;
                    distances[1] = image.getLatLon().greatCircleDistance(
                            selectedCoords);
                }
            }
        }
        // Predownloads the thumbnails
        if (ret[0] != null)
            new MapillaryCache(ret[0].getKey(), MapillaryCache.Type.THUMBNAIL)
                    .submit(data, false);
        if (ret[1] != null)
            new MapillaryCache(ret[1].getKey(), MapillaryCache.Type.THUMBNAIL)
                    .submit(data, false);
        return ret;
    }

    @Override
    public Object getInfoComponent() {
        StringBuilder sb = new StringBuilder();
        sb.append(tr("Mapillary layer"));
        sb.append("\n");
        sb.append(tr("Total images:"));
        sb.append(" ");
        sb.append(data.size());
        sb.append("\n");
        return sb.toString();
    }

    @Override
    public String getToolTipText() {
        return data.size() + " " + tr("images");
    }

    // EditDataLayerChanged
    @Override
    public void editLayerChanged(OsmDataLayer oldLayer, OsmDataLayer newLayer) {
        if (oldLayer == null && newLayer != null) {
            newLayer.data.addDataSetListener(this);

        } else if (oldLayer != null && newLayer == null) {
            oldLayer.data.removeDataSetListener(this);
        }
    }

    /**
     * When more data is downloaded, a delayed update is thrown, in order to
     * wait for the data bounds to be set.
     *
     * @param event
     */
    @Override
    public void dataChanged(DataChangedEvent event) {
        Main.worker.submit(new delayedDownload());
    }

    private class delayedDownload extends Thread {

        @Override
        public void run() {
            try {
                sleep(1000);
            } catch (InterruptedException e) {
                Main.error(e);
            }
            MapillaryLayer.getInstance().download();
        }
    }

    @Override
    public void primitivesAdded(PrimitivesAddedEvent event) {
    }

    @Override
    public void primitivesRemoved(PrimitivesRemovedEvent event) {
    }

    @Override
    public void tagsChanged(TagsChangedEvent event) {
    }

    @Override
    public void nodeMoved(NodeMovedEvent event) {
    }

    @Override
    public void wayNodesChanged(WayNodesChangedEvent event) {
    }

    @Override
    public void relationMembersChanged(RelationMembersChangedEvent event) {
    }

    @Override
    public void otherDatasetChange(AbstractDatasetChangedEvent event) {
    }

    @Override
    public void visitBoundingBox(BoundingXYVisitor v) {
    }

    @Override
    public void activeLayerChange(Layer oldLayer, Layer newLayer) {
        if (newLayer == this) {
            if (data.size() > 0)
                Main.map.statusLine.setHelpText(tr("Total images: {0}",
                        data.size()));
            else
                Main.map.statusLine.setHelpText(tr("No images found"));
        }
    }

    @Override
    public void layerAdded(Layer newLayer) {
    }

    @Override
    public void layerRemoved(Layer oldLayer) {
    }
}
