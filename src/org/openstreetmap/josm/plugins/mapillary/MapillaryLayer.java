package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import org.apache.commons.jcs.access.CacheAccess;
import org.openstreetmap.josm.plugins.mapillary.downloads.MapillaryDownloader;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.data.Bounds;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.MapView.EditLayerChangeListener;
import org.openstreetmap.josm.gui.layer.AbstractModifiableLayer;
import org.openstreetmap.josm.gui.layer.OsmDataLayer;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.JCSCacheManager;
import org.openstreetmap.josm.data.osm.visitor.BoundingXYVisitor;
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

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.IOException;

import javax.swing.ImageIcon;
import javax.swing.Action;
import javax.swing.Icon;

import java.util.List;
import java.util.ArrayList;

public class MapillaryLayer extends AbstractModifiableLayer implements
		MouseListener, DataSetListener, EditLayerChangeListener {

	private final MapillaryData mapillaryData;
	private List<Bounds> bounds;
	private MapillaryToggleDialog tgd;
	public static Boolean INSTANCED = false;
	public static CacheAccess<String, BufferedImageCacheEntry> CACHE;

	public MapillaryLayer() {
		super(tr("Mapillary Images"));
		mapillaryData = MapillaryData.getInstance();
		bounds = new ArrayList<>();
		init();
	}

	private void init() {
		INSTANCED = true;
		try {
			CACHE = JCSCacheManager.getCache("Mapillary");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (Main.map != null && Main.map.mapView != null) {
			Main.map.mapView.addMouseListener(this);
			Main.map.mapView.addLayer(this);
			MapView.addEditLayerChangeListener(this, false);
			Main.map.mapView.getEditLayer().data.addDataSetListener(this);
			if (tgd == null) {
				if (MapillaryToggleDialog.INSTANCE == null) {
					tgd = MapillaryToggleDialog.getInstance();
					Main.map.addToggleDialog(tgd, false);
				} else
					tgd = MapillaryToggleDialog.getInstance();
			}
		}
		MapillaryPlugin.setMenuEnabled(MapillaryPlugin.EXPORT_MENU, true);
		download();
	}

	protected void download() {
		for (Bounds bounds : Main.map.mapView.getEditLayer().data
				.getDataSourceBounds()) {
			if (!this.bounds.contains(bounds)) {
				this.bounds.add(bounds);
				new MapillaryDownloader(mapillaryData).getImages(
						bounds.getMin(), bounds.getMax());
			}
		}
	}

	public MapillaryData getMapillaryData() {
		return mapillaryData;
	}

	@Override
	public void destroy() {
		MapillaryToggleDialog.getInstance().mapillaryImageDisplay.setImage(null);
		INSTANCED = false;
		MapillaryPlugin.setMenuEnabled(MapillaryPlugin.EXPORT_MENU, false);
		MapillaryData.deleteInstance();
		Main.map.mapView.removeMouseListener(this);
		MapView.removeEditLayerChangeListener(this);
		Main.map.mapView.getEditLayer().data.removeDataSetListener(this);
		super.destroy();
	}

	@Override
	public boolean isModified() {
		for (MapillaryImage image : mapillaryData.getImages()) {
			if (image.isModified()) {
				return true;
			}
		}
		return false;
	}

	@Override
	public void paint(Graphics2D g, MapView mv, Bounds box) {
		synchronized (this) {
			for (MapillaryImage image : mapillaryData.getImages()) {
				Point p = mv.getPoint(image.getLatLon());
				Point nextp;
				if (image.getSequence() != null
						&& image.getSequence().next(image) != null) {
					nextp = mv.getPoint(image.getSequence().next(image)
							.getLatLon());
					g.drawLine(p.x, p.y, nextp.x, nextp.y);
				}
				ImageIcon icon;
				if (!mapillaryData.getMultiSelectedImages().contains(image))
					icon = MapillaryPlugin.ICON16;
				else
					icon = MapillaryPlugin.ICON16SELECTED;
				int width = icon.getIconWidth();
				int height = icon.getIconHeight();
				g.drawImage(icon.getImage(), p.x - (width / 2), p.y
						- (height / 2), Main.map.mapView);
			}
		}
	}

	@Override
	public Icon getIcon() {
		return MapillaryPlugin.ICON16;
	}

	@Override
	public boolean isMergable(Layer other) {
		return other instanceof MapillaryLayer;
	}

	@Override
	public void mergeFrom(Layer from) {
		throw new UnsupportedOperationException(
				"Notes layer does not support merging yet");
	}

	@Override
	public Action[] getMenuEntries() {
		List<Action> actions = new ArrayList<>();
		actions.add(LayerListDialog.getInstance().createShowHideLayerAction());
		actions.add(LayerListDialog.getInstance().createDeleteLayerAction());
		actions.add(new LayerListPopup.InfoAction(this));
		return actions.toArray(new Action[actions.size()]);
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		if (e.getButton() != MouseEvent.BUTTON1) {
			return;
		}
		if (Main.map.mapView.getActiveLayer() != this) {
			return;
		}
		Point clickPoint = e.getPoint();
		double snapDistance = 10;
		double minDistance = Double.MAX_VALUE;
		MapillaryImage closest = null;
		for (MapillaryImage image : mapillaryData.getImages()) {
			Point imagePoint = Main.map.mapView.getPoint(image.getLatLon());
			// move the note point to the center of the icon where users are
			// most likely to click when selecting
			imagePoint.setLocation(imagePoint.getX(), imagePoint.getY());
			double dist = clickPoint.distanceSq(imagePoint);
			if (minDistance > dist
					&& clickPoint.distance(imagePoint) < snapDistance) {
				minDistance = dist;
				closest = image;
			}
		}
		if (e.getModifiers() == (MouseEvent.BUTTON1_MASK | MouseEvent.CTRL_MASK))
			mapillaryData.addMultiSelectedImage(closest);
		else
			mapillaryData.setSelectedImage(closest);
	}

	@Override
	public Object getInfoComponent() {
		StringBuilder sb = new StringBuilder();
		sb.append(tr("Mapillary layer"));
		sb.append("\n");
		sb.append(tr("Total images:"));
		sb.append(" ");
		sb.append(this.size());
		sb.append("\n");
		return sb.toString();
	}

	@Override
	public String getToolTipText() {
		return this.size() + " " + tr("images");
	}

	private int size() {
		return mapillaryData.getImages().size();
	}

	// MouseListener
	@Override
	public void visitBoundingBox(BoundingXYVisitor v) {
	}

	@Override
	public void mousePressed(MouseEvent e) {
	}

	@Override
	public void mouseReleased(MouseEvent e) {
	}

	@Override
	public void mouseEntered(MouseEvent e) {
	}

	@Override
	public void mouseExited(MouseEvent e) {
	}

	// EditDataLayerChanged
	@Override
	public void editLayerChanged(OsmDataLayer oldLayer, OsmDataLayer newLayer) {
	}

	// DataSetListener

	@Override
	public void dataChanged(DataChangedEvent event) {
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
}
