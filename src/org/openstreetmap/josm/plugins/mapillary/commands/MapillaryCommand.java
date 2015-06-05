package org.openstreetmap.josm.plugins.mapillary.commands;


public abstract class MapillaryCommand {
	
	public abstract void undo();
	public abstract void redo();
}
