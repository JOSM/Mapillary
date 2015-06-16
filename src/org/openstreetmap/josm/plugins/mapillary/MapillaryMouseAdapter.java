package org.openstreetmap.josm.plugins.mapillary;

import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.commands.CommandMoveImage;
import org.openstreetmap.josm.plugins.mapillary.commands.CommandTurnImage;
import org.openstreetmap.josm.plugins.mapillary.commands.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryToggleDialog;

/**
 * Handles the input event related with the layer. Mainly clicks.
 * 
 * @author nokutu
 *
 */
public class MapillaryMouseAdapter extends MouseAdapter {
	private Point start;
	private int lastButton;
	private MapillaryAbstractImage closest;
	private MapillaryAbstractImage lastClicked;
	private MapillaryData mapillaryData;
	private MapillaryRecord record;

	public MapillaryMouseAdapter() {
		mapillaryData = MapillaryData.getInstance();
		record = MapillaryRecord.getInstance();
	}

	@Override
	public void mousePressed(MouseEvent e) {
		lastButton = e.getButton();
		if (e.getButton() != MouseEvent.BUTTON1)
			return;
		if (Main.map.mapView.getActiveLayer() != MapillaryLayer.getInstance())
			return;
		MapillaryAbstractImage closestTemp = getClosest(e.getPoint());
		if (closestTemp instanceof MapillaryImage || closestTemp == null) {
			MapillaryImage closest = (MapillaryImage) closestTemp;
			// Doube click
			if (e.getClickCount() == 2
					&& mapillaryData.getSelectedImage() != null
					&& closest != null) {
				for (MapillaryAbstractImage img : closest.getSequence()
						.getImages()) {
					mapillaryData.addMultiSelectedImage(img);
				}
			}
			this.start = e.getPoint();
			this.lastClicked = this.closest;
			this.closest = closest;
			if (mapillaryData.getMultiSelectedImages().contains(closest))
				return;
			// ctrl+click
			if (e.getModifiers() == (MouseEvent.BUTTON1_MASK | MouseEvent.CTRL_MASK)
					&& closest != null)
				mapillaryData.addMultiSelectedImage(closest);
			// shift + click
			else if (e.getModifiers() == (MouseEvent.BUTTON1_MASK | MouseEvent.SHIFT_MASK)
					&& this.closest instanceof MapillaryImage
					&& this.lastClicked instanceof MapillaryImage) {
				if (this.closest != null
						&& this.lastClicked != null
						&& ((MapillaryImage) this.closest).getSequence() == ((MapillaryImage) this.lastClicked)
								.getSequence()) {
					int i = ((MapillaryImage) this.closest).getSequence()
							.getImages().indexOf(this.closest);
					int j = ((MapillaryImage) this.lastClicked).getSequence()
							.getImages().indexOf(this.lastClicked);
					if (i < j)
						mapillaryData
								.addMultiSelectedImage(new ArrayList<MapillaryAbstractImage>(
										((MapillaryImage) this.closest)
												.getSequence().getImages()
												.subList(i, j + 1)));
					else
						mapillaryData
								.addMultiSelectedImage(new ArrayList<MapillaryAbstractImage>(
										((MapillaryImage) this.closest)
												.getSequence().getImages()
												.subList(j, i + 1)));
				}
				// click
			} else
				mapillaryData.setSelectedImage(closest);
			// If you select an imported image
		} else if (closestTemp instanceof MapillaryImportedImage) {
			MapillaryImportedImage closest = (MapillaryImportedImage) closestTemp;
			this.start = e.getPoint();
			this.lastClicked = this.closest;
			this.closest = closest;
			if (mapillaryData.getMultiSelectedImages().contains(closest))
				return;
			if (e.getModifiers() == (MouseEvent.BUTTON1_MASK | MouseEvent.CTRL_MASK)
					&& closest != null)
				mapillaryData.addMultiSelectedImage(closest);
			else
				mapillaryData.setSelectedImage(closest);
		}
	}

	private MapillaryAbstractImage getClosest(Point clickPoint) {
		double snapDistance = 10;
		double minDistance = Double.MAX_VALUE;
		MapillaryAbstractImage closest = null;
		for (MapillaryAbstractImage image : mapillaryData.getImages()) {
			Point imagePoint = Main.map.mapView.getPoint(image.getLatLon());
			imagePoint.setLocation(imagePoint.getX(), imagePoint.getY());
			double dist = clickPoint.distanceSq(imagePoint);
			if (minDistance > dist
					&& clickPoint.distance(imagePoint) < snapDistance) {
				minDistance = dist;
				closest = image;
			}
		}
		return closest;
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		if (Main.map.mapView.getActiveLayer() != MapillaryLayer.getInstance())
			return;
		if (MapillaryData.getInstance().getSelectedImage() != null) {
			if (lastButton == MouseEvent.BUTTON1 && !e.isShiftDown()) {
				LatLon to = Main.map.mapView.getLatLon(e.getX(), e.getY());
				LatLon from = Main.map.mapView.getLatLon(start.getX(),
						start.getY());
				for (MapillaryAbstractImage img : MapillaryData.getInstance()
						.getMultiSelectedImages()) {
					img.move(to.getX() - from.getX(), to.getY() - from.getY());
				}
				Main.map.repaint();
			} else if (lastButton == MouseEvent.BUTTON1 && e.isShiftDown()) {
				this.closest.turn(Math.toDegrees(Math.atan2(
						(e.getX() - start.x), -(e.getY() - start.y)))
						- closest.getTempCa());
				for (MapillaryAbstractImage img : MapillaryData.getInstance()
						.getMultiSelectedImages()) {
					img.turn(Math.toDegrees(Math.atan2((e.getX() - start.x),
							-(e.getY() - start.y))) - closest.getTempCa());
				}
				Main.map.repaint();
			}
		}
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		if (mapillaryData.getSelectedImage() == null)
			return;
		if (mapillaryData.getSelectedImage().getTempCa() != mapillaryData
				.getSelectedImage().getCa()) {
			double from = mapillaryData.getSelectedImage().getTempCa();
			double to = mapillaryData.getSelectedImage().getCa();
			record.addCommand(new CommandTurnImage(mapillaryData
					.getMultiSelectedImages(), to - from));
		} else if (mapillaryData.getSelectedImage().getTempLatLon() != mapillaryData
				.getSelectedImage().getLatLon()) {
			LatLon from = mapillaryData.getSelectedImage().getTempLatLon();
			LatLon to = mapillaryData.getSelectedImage().getLatLon();
			record.addCommand(new CommandMoveImage(mapillaryData
					.getMultiSelectedImages(), to.getX() - from.getX(), to
					.getY() - from.getY()));
		}
		for (MapillaryAbstractImage img : mapillaryData
				.getMultiSelectedImages()) {
			if (img != null)
				img.stopMoving();
		}
	}

	/**
	 * Checks if the mouse is over pictures.
	 */
	@Override
	public void mouseMoved(MouseEvent e) {
		MapillaryAbstractImage closestTemp = getClosest(e.getPoint());
		// TODO check if it is possible to do this while the OSM data layer is
		// selected.
		if (Main.map.mapView.getActiveLayer() instanceof MapillaryLayer
				&& MapillaryData.getInstance().getHoveredImage() != closestTemp
				&& closestTemp != null) {
			MapillaryData.getInstance().setHoveredImage(closestTemp);
			MapillaryToggleDialog.getInstance().setImage(closestTemp);
			MapillaryData.getInstance().dataUpdated();
			MapillaryToggleDialog.getInstance().updateImage();
		} else if (MapillaryData.getInstance().getHoveredImage() != closestTemp
				&& closestTemp == null) {
			MapillaryData.getInstance().setHoveredImage(null);
			MapillaryToggleDialog.getInstance().setImage(
					MapillaryData.getInstance().getSelectedImage());
			MapillaryData.getInstance().dataUpdated();
			MapillaryToggleDialog.getInstance().updateImage();
		}
	}
}
