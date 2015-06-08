package org.openstreetmap.josm.plugins.mapillary.commands;

import java.util.ArrayList;
import java.util.List;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;

/**
 * Command created when an image's position is changed.
 * 
 * @author nokutu
 *
 */
public class CommandMoveImage extends MapillaryCommand {
	private List<MapillaryAbstractImage> images;
	private double x;
	private double y;

	public CommandMoveImage(List<MapillaryAbstractImage> images, double x, double y) {
		this.images = new ArrayList<>(images);
		this.x = x;
		this.y = y;
	}

	@Override
	public void undo() {
		for (MapillaryAbstractImage image : images) {
			image.move(-x, -y);
			image.stopMoving();
		}
		Main.map.repaint();
	}

	@Override
	public void redo() {
		for (MapillaryAbstractImage image : images) {
			image.move(x, y);
			image.stopMoving();
		}
		Main.map.repaint();
	}
}
