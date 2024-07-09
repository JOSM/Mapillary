// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.Serial;

import javax.swing.JOptionPane;

import org.openstreetmap.josm.command.ChangePropertyCommand;
import org.openstreetmap.josm.data.UndoRedoHandler;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.Tag;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.geoimage.ImageViewerDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.geoimage.MapillaryImageEntry;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Add the mapillary tag to a primitive
 */
public class AddTagToPrimitiveAction extends MapillaryAction {
    @Serial
    private static final long serialVersionUID = 4834918715956633953L;

    private Tag tag;
    private transient IPrimitive target;

    /**
     * The name of the add tag action
     *
     * @param name The name to use
     */
    public AddTagToPrimitiveAction(final String name) {
        super(name, "dialogs/add", tr("Add the mapillary source tag to the selected primitive"),
            Shortcut.registerShortcut("mapillary:add_tag_to_primitive", tr("Mapillary: Add Tag to Primitive"),
                KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
            false, "mapillary:add_tag_to_primitive", false);
        this.updateEnabled();
    }

    /**
     * The tag to set
     *
     * @param tag The tag to set
     */
    public void setTag(Tag tag) {
        this.tag = tag;
        updateEnabled();
    }

    /**
     * Set the target for the tag
     *
     * @param target the target whose tags need to be added to
     */
    public void setTarget(IPrimitive target) {
        this.target = target;
        updateEnabled();
    }

    private void updateEnabled() {
        setEnabled(tag != null && target != null && ImageViewerDialog.getCurrentImage() instanceof MapillaryImageEntry);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (target != null && tag != null) {
            int conflictResolution = JOptionPane.YES_OPTION;
            if (target.hasKey(tag.getKey()) && !target.hasTag(tag.getKey(), tag.getValue())) {
                conflictResolution = JOptionPane.showConfirmDialog(MainApplication.getMainFrame(), "<html>"
                    + tr("A tag with key <i>{0}</i> is already present on the selected OSM object.", tag.getKey())
                    + "<br>"
                    + tr("Do you really want to replace the current value <i>{0}</i> with the new value <i>{1}</i>?",
                        target.get(tag.getKey()), tag.getValue())
                    + "</html>", tr("Tag conflict"), JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
            }
            if (JOptionPane.YES_OPTION == conflictResolution) {
                if (target instanceof OsmPrimitive primitive) {
                    UndoRedoHandler.getInstance()
                        .add(new ChangePropertyCommand(primitive, tag.getKey(), tag.getValue()));
                } else {
                    target.put(tag);
                    target.setModified(true);
                }
            }
        }
    }
}
