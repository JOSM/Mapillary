package org.openstreetmap.josm.plugins.mapillary.command;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.Collection;

import org.openstreetmap.josm.command.Command;
import org.openstreetmap.josm.data.osm.OsmPrimitive;

/**
 * Command for adding Mapillary objects to OSM
 *
 * @author Taylor Smock
 */
public class AddMapillaryObjectCommand extends Command {

  private final Command updateTagsCommand;
  private final GenericCommand<?, ?, ?, ?, ?> deleteOriginal;

  public AddMapillaryObjectCommand(GenericCommand<?, ?, ?, ?, ?> deleteOriginal, Command updateTagsCommand) {
    super(updateTagsCommand.getAffectedDataSet());
    this.updateTagsCommand = updateTagsCommand;
    this.deleteOriginal = deleteOriginal;
  }

  @Override
  public boolean executeCommand() {
    return super.executeCommand() && updateTagsCommand.executeCommand() && deleteOriginal.executeCommand();
  }

  @Override
  public void undoCommand() {
    super.undoCommand();
    updateTagsCommand.undoCommand();
    deleteOriginal.undoCommand();
  }

  @Override
  public void fillModifiedData(Collection<OsmPrimitive> modified, Collection<OsmPrimitive> deleted,
    Collection<OsmPrimitive> added) {
    updateTagsCommand.fillModifiedData(modified, deleted, added);
  }

  @Override
  public String getDescriptionText() {
    return tr("Mapillary Smart Edit: Add objects");
  }
}
