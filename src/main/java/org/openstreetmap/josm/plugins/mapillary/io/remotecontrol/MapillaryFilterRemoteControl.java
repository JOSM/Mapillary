// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.io.remotecontrol;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.time.Instant;

import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.io.remotecontrol.PermissionPrefWithDefault;
import org.openstreetmap.josm.io.remotecontrol.handler.RequestHandler;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryFilterDialog;

/**
 * Remote control for the Mapillary filter
 */
public class MapillaryFilterRemoteControl extends RequestHandler.RawURLParseRequestHandler {
    private static final String[] EMPTY_STRING_ARRAY = new String[0];
    private static final String STRING_START = "start";
    private static final String STRING_END = "end";
    private static final String STRING_ORG = "organization";
    private static final PermissionPrefWithDefault PERMISSION_PREF_WITH_DEFAULT = new PermissionPrefWithDefault(
        "mapillary.remote_control.filters", true, tr("Mapillary Filters"));

    private Instant start;
    private Instant end;
    private String organization;

    @Override
    protected void validateRequest() {
        if (this.args != null && !this.args.isEmpty()) {
            if (this.args.containsKey(STRING_START)) {
                this.start = Instant.parse(this.args.get(STRING_START));
            }
            if (this.args.containsKey(STRING_END)) {
                this.end = Instant.parse(this.args.get(STRING_END));
            }
            if (this.args.containsKey(STRING_ORG)) {
                this.organization = this.args.get(STRING_ORG);
            }
        }
    }

    @Override
    protected void handleRequest() {
        // Reset prior to setting filters
        MapillaryFilterDialog filterDialog = MapillaryFilterDialog.getInstance();
        GuiHelper.runInEDTAndWait(filterDialog::reset);
        if (this.start != null) {
            GuiHelper.runInEDT(() -> filterDialog.setStartDate(this.start));
        }
        if (this.end != null) {
            GuiHelper.runInEDTAndWait(() -> filterDialog.setEndDate(this.end));
        }
        if (this.organization != null) {
            GuiHelper.runInEDTAndWait(() -> filterDialog.setOrganization(this.organization));
        }
        GuiHelper.runInEDTAndWait(filterDialog::refresh);
    }

    @Override
    public String getPermissionMessage() {
        final String br = "<br />";
        StringBuilder sb = new StringBuilder().append(tr("Remote Control has been asked to set Mapillary filters:"));
        if (this.start != null) {
            sb.append(br).append(tr("Start: {0}", this.start));
        }
        if (this.end != null) {
            sb.append(br).append(tr("End: {0}", this.end));
        }
        if (this.organization != null) {
            sb.append(br).append(tr("Organization: {0}", this.organization));
        }
        return sb.toString();
    }

    @Override
    public PermissionPrefWithDefault getPermissionPref() {
        return PERMISSION_PREF_WITH_DEFAULT;
    }

    @Override
    public String[] getMandatoryParams() {
        return EMPTY_STRING_ARRAY;
    }

    @Override
    public String[] getOptionalParams() {
        return new String[] { STRING_START, STRING_END, STRING_ORG };
    }
}
