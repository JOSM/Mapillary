// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import org.openstreetmap.josm.actions.ExpertToggleAction;
import org.openstreetmap.josm.plugins.mapillary.gui.DeveloperToggleAction;

/**
 * Use for features that should only be shown when expert/developer
 */
public enum DataType {
    /** Only visible with developer mode and expert mode */
    TESTING(true, true),
    /** Only visible with expert mode */
    PREVIEW(true, false),
    /** Always visible */
    PRODUCTION(false, false);

    private final boolean expert;
    private final boolean developer;

    DataType(boolean expert, boolean developer) {
        this.expert = expert;
        this.developer = developer;
    }

    /**
     * Use to determine if something should be visible. Only use dynamically.
     *
     * @return {@code true} if it should be visible.
     */
    public boolean shouldBeVisible() {
        boolean e = !this.expert || ExpertToggleAction.isExpert();
        boolean d = !this.developer || DeveloperToggleAction.isDeveloper();
        return e && d;
    }
}
