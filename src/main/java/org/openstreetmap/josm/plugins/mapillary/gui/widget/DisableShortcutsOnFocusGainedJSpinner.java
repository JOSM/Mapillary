package org.openstreetmap.josm.plugins.mapillary.gui.widget;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.Action;
import javax.swing.JSpinner;
import javax.swing.SpinnerModel;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.widgets.DisableShortcutsOnFocusGainedComponent;
import org.openstreetmap.josm.tools.Pair;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * A JSpinner that disables shortcuts when focus is gained
 *
 * @author Taylor Smock
 */
public class DisableShortcutsOnFocusGainedJSpinner extends JSpinner implements DisableShortcutsOnFocusGainedComponent {
    private final Set<JosmAction> disabledMenuActions = new HashSet<>();
    private final List<Pair<Action, Shortcut>> unregisteredActionShortcuts = new ArrayList<>();

    public DisableShortcutsOnFocusGainedJSpinner(final SpinnerModel spinnerModel) {
        super(spinnerModel);
    }

    @Override
    public List<Pair<Action, Shortcut>> getUnregisteredActionShortcuts() {
        return this.unregisteredActionShortcuts;
    }

    @Override
    public Set<JosmAction> getDisabledMenuActions() {
        return this.disabledMenuActions;
    }
}
