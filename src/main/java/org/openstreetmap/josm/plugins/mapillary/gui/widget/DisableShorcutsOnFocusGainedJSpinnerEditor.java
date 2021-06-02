package org.openstreetmap.josm.plugins.mapillary.gui.widget;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.widgets.DisableShortcutsOnFocusGainedTextField;
import org.openstreetmap.josm.tools.Pair;
import org.openstreetmap.josm.tools.Shortcut;

import javax.swing.Action;
import javax.swing.JFormattedTextField;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Disable shortcuts on focus gained methods
 *
 * @author Taylor Smock
 */
public final class DisableShorcutsOnFocusGainedJSpinnerEditor {
  private DisableShorcutsOnFocusGainedJSpinnerEditor() {
    // Disable instantiation
  }

  /**
   * Disable shortcuts for a JSpinner
   *
   * @param jSpinner The JSpinner to disable shortcuts on
   * @return The JSpinner, for easy chaining
   */
  public static JSpinner disableShortcutsOnFocusGained(final JSpinner jSpinner) {
    if (jSpinner.getEditor() instanceof JSpinner.DefaultEditor) {
      JSpinner.DefaultEditor editor = (JSpinner.DefaultEditor) jSpinner.getEditor();
      JFormattedTextField oldTextField = editor.getTextField();
      editor.remove(0);
      JFormattedTextField newTextField = new DisableShortcutsOnFocusGainedJFormattedTextField();
      newTextField.setName(oldTextField.getName());
      newTextField.setHorizontalAlignment(oldTextField.getHorizontalAlignment());
      newTextField.setFormatterFactory(oldTextField.getFormatterFactory());
      newTextField.setFocusLostBehavior(oldTextField.getFocusLostBehavior());
      newTextField.setEditable(oldTextField.isEditable());
      newTextField.setText(oldTextField.getText());
      Stream.of(oldTextField.getPropertyChangeListeners()).forEach(newTextField::addPropertyChangeListener);
      if (oldTextField.getColumns() > 0) {
        newTextField.setColumns(oldTextField.getColumns());
      }
      editor.add(newTextField);
    }
    return jSpinner;
  }

  /**
   * A class similar to {@link DisableShortcutsOnFocusGainedTextField}, but with a different parent
   * ({@link JFormattedTextField} instead of {@link JTextField}).
   */
  private static class DisableShortcutsOnFocusGainedJFormattedTextField extends JFormattedTextField
    implements FocusListener {
    /**
     * @see JFormattedTextField#JFormattedTextField()
     */
    public DisableShortcutsOnFocusGainedJFormattedTextField() {
      super();
    }

    // Everything related to focus gained/lost copied from DisableShorcutsOnFocusGainedTextField (I cannot extend it
    // here)

    private final transient List<Pair<Action, Shortcut>> unregisteredActionShortcuts = new ArrayList<>();
    private final Set<JosmAction> disabledMenuActions = new HashSet<>();

    @Override
    public void focusGained(FocusEvent e) {
      disableMenuActions();
      unregisterActionShortcuts();
    }

    @Override
    public void focusLost(FocusEvent e) {
      restoreActionShortcuts();
      restoreMenuActions();
    }

    /**
     * Disables all relevant menu actions.
     *
     * @see #hasToBeDisabled
     */
    protected void disableMenuActions() {
      disabledMenuActions.clear();
      for (int i = 0; i < MainApplication.getMenu().getMenuCount(); i++) {
        JMenu menu = MainApplication.getMenu().getMenu(i);
        if (menu != null) {
          for (int j = 0; j < menu.getItemCount(); j++) {
            JMenuItem item = menu.getItem(j);
            if (item != null) {
              Action action = item.getAction();
              if (action instanceof JosmAction && action.isEnabled()) {
                Shortcut shortcut = ((JosmAction) action).getShortcut();
                if (shortcut != null) {
                  KeyStroke ks = shortcut.getKeyStroke();
                  if (hasToBeDisabled(ks)) {
                    action.setEnabled(false);
                    disabledMenuActions.add((JosmAction) action);
                  }
                }
              }
            }
          }
        }
      }
    }

    /**
     * Unregisters all relevant action shortcuts.
     *
     * @see #hasToBeDisabled
     */
    protected void unregisterActionShortcuts() {
      unregisteredActionShortcuts.clear();
      // Unregister all actions with Shift modifier or without modifiers to avoid them to be triggered by typing in this
      // text field
      for (Shortcut shortcut : Shortcut.listAll()) {
        KeyStroke ks = shortcut.getKeyStroke();
        if (hasToBeDisabled(ks)) {
          Action action = MainApplication.getRegisteredActionShortcut(shortcut);
          if (action != null) {
            MainApplication.unregisterActionShortcut(action, shortcut);
            unregisteredActionShortcuts.add(new Pair<>(action, shortcut));
          }
        }
      }
    }

    /**
     * Returns true if the given shortcut has Shift modifier or no modifier and is not an actions key.
     *
     * @param ks key stroke
     * @return {@code true} if the given shortcut has to be disabled
     * @see KeyEvent#isActionKey()
     */
    protected boolean hasToBeDisabled(KeyStroke ks) {
      return ks != null && (ks.getModifiers() == 0 || isOnlyShift(ks.getModifiers()))
        && !new KeyEvent(this, KeyEvent.KEY_PRESSED, 0, ks.getModifiers(), ks.getKeyCode(), ks.getKeyChar())
          .isActionKey();
    }

    private static boolean isOnlyShift(int modifiers) {
      return (modifiers & InputEvent.SHIFT_DOWN_MASK) != 0 && (modifiers & InputEvent.CTRL_DOWN_MASK) == 0
        && (modifiers & InputEvent.ALT_DOWN_MASK) == 0 && (modifiers & InputEvent.ALT_GRAPH_DOWN_MASK) == 0
        && (modifiers & InputEvent.META_DOWN_MASK) == 0;
    }

    /**
     * Restore all actions previously disabled
     */
    protected void restoreMenuActions() {
      for (JosmAction a : disabledMenuActions) {
        a.setEnabled(true);
      }
      disabledMenuActions.clear();
    }

    /**
     * Restore all action shortcuts previously unregistered
     */
    protected void restoreActionShortcuts() {
      for (Pair<Action, Shortcut> p : unregisteredActionShortcuts) {
        MainApplication.registerActionShortcut(p.a, p.b);
      }
      unregisteredActionShortcuts.clear();
    }
  }
}
