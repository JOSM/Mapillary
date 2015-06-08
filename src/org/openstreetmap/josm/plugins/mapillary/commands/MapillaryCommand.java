package org.openstreetmap.josm.plugins.mapillary.commands;

/**
 * Abstract class for any Mapillary command.
 * 
 * @author nokutu
 *
 */
public abstract class MapillaryCommand {

	public abstract void undo();

	public abstract void redo();
}
