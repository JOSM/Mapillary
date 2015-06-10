package org.openstreetmap.josm.plugins.mapillary.commands;

import java.util.List;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;

/**
 * Abstract class for any Mapillary command.
 * 
 * @author nokutu
 *
 */
public abstract class MapillaryCommand {
	protected List<MapillaryAbstractImage> images;

	public abstract void undo();

	public abstract void redo();
	
	public abstract void sum(MapillaryCommand command);
	
	public void checkModified() {
		for (MapillaryAbstractImage image : images)
			image.isModified = (image.tempLatLon == image.latLon || image.tempCa == image.ca);
	}
}
