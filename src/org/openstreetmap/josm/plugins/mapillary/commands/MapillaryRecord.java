package org.openstreetmap.josm.plugins.mapillary.commands;

import java.util.ArrayList;


public class MapillaryRecord {
	public static MapillaryRecord INSTANCE;
	
	public ArrayList<MapillaryCommand> commandList;
	/** Last written command */
	public int position;
	
	public MapillaryRecord() {
		commandList = new ArrayList<>();
		position = -1;
	}
	
	public static synchronized MapillaryRecord getInstance() {
		if (MapillaryRecord.INSTANCE == null)
			MapillaryRecord.INSTANCE = new MapillaryRecord();
		return MapillaryRecord.INSTANCE;
	}
	
	public void addCommand(MapillaryCommand command) {
		commandList.add(position + 1, command);
		position++;
		while (commandList.size() > position + 1) {
			commandList.remove(position + 1);
		}
	}
	
	public void undo() {
		if (position == -1)
			return;
		commandList.get(position).undo();
		position--;
	}
	
	public void redo() {
		if (position >= commandList.size())
			return;
		commandList.get(position).redo();
		position++;
	}
}
