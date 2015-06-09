package org.openstreetmap.josm.plugins.mapillary.commands;

import static org.openstreetmap.josm.tools.I18n.trn;

import java.util.ArrayList;
import java.util.List;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;

/**
 * Command created when a image's direction is changed.
 * 
 * @author nokutu
 *
 */
public class CommandTurnImage extends MapillaryCommand {
	private List<MapillaryAbstractImage> images;
	private double ca;

	public CommandTurnImage(List<MapillaryAbstractImage> images, double ca) {
		this.images = new ArrayList<>(images);
		this.ca = ca;
	}

	@Override
	public void undo() {
		for (MapillaryAbstractImage image : images) {
			image.turn(-ca);
			image.stopMoving();
		}
		Main.map.repaint();
	}

	@Override
	public void redo() {
		for (MapillaryAbstractImage image : images) {
			image.turn(ca);
			image.stopMoving();
		}
		Main.map.repaint();
	}

	public String toString() {
		return trn("Turned {0} node", "Moved {0} nodes", images.size(), images.size());
	}
}
