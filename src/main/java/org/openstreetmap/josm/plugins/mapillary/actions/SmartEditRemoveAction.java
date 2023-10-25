// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.util.Locale;
import java.util.Objects;

import javax.swing.JOptionPane;
import javax.swing.text.AbstractDocument;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.json.Json;
import jakarta.json.JsonObjectBuilder;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.gui.ConditionalOptionPaneUtil;
import org.openstreetmap.josm.gui.ExtendedDialog;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.tagging.ac.MaxLengthDocumentFilter;
import org.openstreetmap.josm.gui.widgets.JosmTextField;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.smartedit.IgnoredObjects;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryMapFeatureUtils;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * Remove an object from Mapillary. This may send data to Mapillary.
 *
 * @author Taylor Smock
 */
public class SmartEditRemoveAction extends JosmAction {
    private static final String TOOLTIP = tr(
        "This currently removes the detection from the current layer, but may report back to Mapillary in the future");
    private final transient VectorPrimitive mapillaryObject;
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
        JsonObjectBuilder builder = Json.createObjectBuilder();
        if (problem == Problem.COMMENT) {
            JosmTextField textField = new JosmTextField(null, null, 64);
            final MaxLengthDocumentFilter maxLengthFilter = new MaxLengthDocumentFilter();
            maxLengthFilter.setMaxLength(255);
            ((AbstractDocument) textField.getDocument()).setDocumentFilter(maxLengthFilter);
            if (!ConditionalOptionPaneUtil.showConfirmationDialog("mapillary.feedback.comment",
                MainApplication.getMainFrame(), textField, tr("Comment to send to Mapillary"),
                JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE, JOptionPane.OK_OPTION)) {
                // This must come before the deletion of the object
                return;
            }
            builder.add("comment", textField.getText());
        }
        this.mapillaryObject.setDeleted(true);
        this.pointObjectLayer.hideWindow(this.mapillaryObject);
        if (problem == Problem.DONT_REPORT) {
            // This must come after the deletion of the object
            return;
        }
        builder.add("object_id", this.mapillaryObject.getId());
        builder.add("data_source", this.pointObjectLayer.getInfo().getSourceName());
        builder.add("feedback_type", problem.name().toLowerCase(Locale.ROOT));
        IgnoredObjects.addIgnoredObject(builder.build());
    }

    public String getToolTip() {
        return TOOLTIP;
    }

    private enum Problem {
        DONT_REPORT, FALSE_POSITIVE, DUPLICATE, BAD_IMAGERY, ERRONEOUS_DATA, COMMENT
    }

    private static class RemoveDialog extends ExtendedDialog {
        public RemoveDialog(VectorPrimitive primitive) {
            super(MainApplication.getMainFrame(),
                tr("Why are you removing the object? (Note: not yet sent to Mapillary)"), tr("False Positive"),
                tr("Duplicate"), tr("Bad Imagery"), tr("Erroneous Data"), tr("Comment"), tr("Just remove it"));
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
                return Problem.COMMENT;
            case 6:
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
