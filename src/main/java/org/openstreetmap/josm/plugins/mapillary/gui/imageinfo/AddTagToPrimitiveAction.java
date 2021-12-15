// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.command.ChangePropertyCommand;
import org.openstreetmap.josm.data.UndoRedoHandler;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.Tag;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.geoimage.ImageViewerDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.geoimage.MapillaryImageEntry;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

public class AddTagToPrimitiveAction extends AbstractAction {
    private static final long serialVersionUID = 4834918715956633953L;

    private Tag tag;
    private transient IPrimitive target;

    /**
     * The name of the add tag action
     *
     * @param name The name to use
     */
    public AddTagToPrimitiveAction(final String name) {
        super(name, ImageProvider.get("dialogs/add", ImageSizes.SMALLICON));
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
                conflictResolution = JOptionPane.showConfirmDialog(MainApplication.getMainFrame(),
                    "<html>"
                        + I18n.tr(
                            "A tag with key <i>{0}</i> is already present on the selected OSM object.", tag.getKey())
                        + "<br>"
                        + I18n.tr(
                            "Do you really want to replace the current value <i>{0}</i> with the new value <i>{1}</i>?",
                            target.get(tag.getKey()), tag.getValue())
                        + "</html>",
                    I18n.tr("Tag conflict"), JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
            }
            if (JOptionPane.YES_OPTION == conflictResolution) {
                if (target instanceof OsmPrimitive) {
                    UndoRedoHandler.getInstance()
                        .add(new ChangePropertyCommand((OsmPrimitive) target, tag.getKey(), tag.getValue()));
                } else {
                    target.put(tag);
                    target.setModified(true);
                }
            }
        }
    }
}
