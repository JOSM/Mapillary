// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.util.Locale;
import java.util.Objects;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.json.Json;
import javax.json.JsonObjectBuilder;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.gui.ExtendedDialog;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryMapFeatureUtils;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;

/**
 * Remove an object from Mapillary. This may send data to Mapillary.
 *
 * @author Taylor Smock
 */
public class SmartEditRemoveAction extends JosmAction {
    private static final String TOOLTIP = tr(
        "This currently removes the detection from the current layer, but may report back to Mapillary in the future");
    private final VectorPrimitive mapillaryObject;
    private final PointObjectLayer pointObjectLayer;

    /**
     * Create a new {@link SmartEditRemoveAction}
     *
     * @param pointObjectLayer The originating object layer
     * @param primitive The primitive that may be removed
     */
    public SmartEditRemoveAction(@Nonnull final PointObjectLayer pointObjectLayer,
        @Nonnull final VectorPrimitive primitive) {
        super(tr("Remove"), new ImageProvider("dialogs", "delete"), TOOLTIP, null, false, null, false);
        Objects.requireNonNull(primitive);
        Objects.requireNonNull(pointObjectLayer);
        this.mapillaryObject = primitive;
        this.pointObjectLayer = pointObjectLayer;
        this.updateEnabledState();
    }

    @Override
    public void actionPerformed(@Nullable ActionEvent e) {
        final RemoveDialog dialog = new RemoveDialog(this.mapillaryObject);
        dialog.showDialog();
        final Problem problem = dialog.getResponseType();
        if (problem == null) {
            return;
        }
        this.mapillaryObject.setDeleted(true);
        if (problem == Problem.DONT_REPORT) {
            return;
        }
        JsonObjectBuilder builder = Json.createObjectBuilder();
        builder.add("object_id", this.mapillaryObject.getId());
        builder.add("data_source", this.pointObjectLayer.getInfo().getSourceName());
        builder.add("feedback_type", problem.name().toLowerCase(Locale.ROOT));
        Logging.error("Mapillary feedback not yet implemented");
        Logging.error(builder.build().toString());
    }

    public String getToolTip() {
        return TOOLTIP;
    }

    private enum Problem {
        DONT_REPORT, FALSE_POSITIVE, DUPLICATE, BAD_IMAGERY, ERRONEOUS_DATA
    }

    private static class RemoveDialog extends ExtendedDialog {
        public RemoveDialog(VectorPrimitive primitive) {
            super(MainApplication.getMainFrame(), tr("Why are you removing the object?"), tr("False Positive"),
                tr("Duplicate"), tr("Bad Imagery"), tr("Erroneous Data"),
                tr("Just remove it"));
            setContent(MapillaryMapFeatureUtils.getValue(primitive));
        }

        public Problem getResponseType() {
            final int answer = getValue();
            switch (answer) {
            case 1:
                return Problem.FALSE_POSITIVE;
            case 2:
                return Problem.DUPLICATE;
            case 3:
                return Problem.BAD_IMAGERY;
            case 4:
                return Problem.ERRONEOUS_DATA;
            case 5:
                return Problem.DONT_REPORT;
            case ExtendedDialog.DialogClosedOtherwise:
                // OK. They cancelled/closed the dialog.
                return null;
            default:
                throw new IllegalArgumentException("Unknown response: " + answer);
            }
        }
    }
}
