package org.openstreetmap.josm.plugins.mapillary.command;

import static org.openstreetmap.josm.tools.I18n.tr;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collection;

import org.openstreetmap.josm.command.Command;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.gui.MainApplication;

/**
 * Command for adding Mapillary objects to OSM
 *
 * @author Taylor Smock
 */
public class AddMapillaryObjectCommand extends Command {

    @Nullable
    private final Command updateTagsCommand;
    @Nonnull
    private final GenericCommand<?, ?, ?, ?, ?> deleteOriginal;

    public AddMapillaryObjectCommand(@Nonnull final GenericCommand<?, ?, ?, ?, ?> deleteOriginal,
        @Nullable final Command updateTagsCommand) {
        super(updateTagsCommand != null ? updateTagsCommand.getAffectedDataSet()
            : MainApplication.getLayerManager().getEditDataSet());
        this.updateTagsCommand = updateTagsCommand;
        this.deleteOriginal = deleteOriginal;
    }

    @Override
    public boolean executeCommand() {
        return super.executeCommand() && (updateTagsCommand == null || updateTagsCommand.executeCommand())
            && deleteOriginal.executeCommand();
    }

    @Override
    public void undoCommand() {
        super.undoCommand();
        if (updateTagsCommand != null) {
            updateTagsCommand.undoCommand();
        }
        deleteOriginal.undoCommand();
    }

    @Override
    public void fillModifiedData(Collection<OsmPrimitive> modified, Collection<OsmPrimitive> deleted,
        Collection<OsmPrimitive> added) {
        if (updateTagsCommand != null) {
            updateTagsCommand.fillModifiedData(modified, deleted, added);
        }
    }

    @Override
    public String getDescriptionText() {
        return tr("Mapillary Smart Edit: Add objects");
    }
}
