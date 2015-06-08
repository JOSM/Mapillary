package org.openstreetmap.josm.plugins.mapillary.commands;

import java.util.ArrayList;
import java.util.List;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;

/**
 * Command created when an image's position is changed.
 * 
 * @author nokutu
 *
 */
public class CommandMoveImage extends MapillaryCommand {
	private List<MapillaryImage> images;
	private double x;
	private double y;

	public CommandMoveImage(List<MapillaryImage> images, double x, double y) {
		this.images = new ArrayList<>(images);
		this.x = x;
		this.y = y;
	}

	@Override
	public void undo() {
		for (MapillaryImage image : images) {
			image.move(-x, -y);
			image.stopMoving();
		}
		Main.map.repaint();
	}

	@Override
	public void redo() {
		for (MapillaryImage image : images) {
			image.move(x, y);
			image.stopMoving();
		}
		Main.map.repaint();
	}
}
