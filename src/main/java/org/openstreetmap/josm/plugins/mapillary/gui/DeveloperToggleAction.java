// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.event.ActionEvent;

import org.openstreetmap.josm.actions.ExpertToggleAction;
import org.openstreetmap.josm.actions.ToggleAction;
import org.openstreetmap.josm.data.preferences.BooleanProperty;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ListenerList;

/**
 * Largely similar to {@link ExpertToggleAction}, which relies upon static fields.
 */
public class DeveloperToggleAction extends ToggleAction {
  private static final ListenerList<DeveloperModeChangeListener> LISTENER_LIST = ListenerList.create();
  private static final ListenerList<Component> VISIBILITY_TOGGLE_LISTENERS = ListenerList.create();

  private static final BooleanProperty PREF_EXPERT = MapillaryProperties.DEVELOPER;

  private static final DeveloperToggleAction INSTANCE = new DeveloperToggleAction();

  private static synchronized void fireExpertModeChanged(boolean isDeveloper) {
    LISTENER_LIST.fireEvent(listener -> listener.developerChanged(isDeveloper));
    VISIBILITY_TOGGLE_LISTENERS.fireEvent(c -> c.setVisible(isDeveloper));
  }

  /**
   * Register a developer mode change listener.
   *
   * @param listener the listener. Ignored if null.
   */
  public static void addDeveloperModeChangeListener(DeveloperModeChangeListener listener) {
    addDeveloperModeChangeListener(listener, false);
  }

  /**
   * Register a developer mode change listener, and optionally fires it.
   *
   * @param listener the listener. Ignored if null.
   * @param fireWhenAdding if true, the listener will be fired immediately after added
   */
  public static synchronized void addDeveloperModeChangeListener(DeveloperModeChangeListener listener,
    boolean fireWhenAdding) {
    if (listener == null)
      return;
    LISTENER_LIST.addWeakListener(listener);
    if (fireWhenAdding) {
      listener.developerChanged(isDeveloper());
    }
  }

  /**
   * Removes a developer mode change listener
   *
   * @param listener the listener. Ignored if null.
   */
  public static synchronized void removeDeveloperModeChangeListener(DeveloperModeChangeListener listener) {
    if (listener == null)
      return;
    LISTENER_LIST.removeListener(listener);
  }

  /**
   * Marks a component to be only visible when developer mode is enabled. The visibility of the component is changed
   * automatically.
   *
   * @param c The component.
   */
  public static synchronized void addVisibilitySwitcher(Component c) {
    if (c == null)
      return;
    VISIBILITY_TOGGLE_LISTENERS.addWeakListener(c);
    c.setVisible(isDeveloper());
  }

  /**
   * Stops tracking visibility changes for the given component.
   *
   * @param c The component.
   * @see #addVisibilitySwitcher(Component)
   */
  public static synchronized void removeVisibilitySwitcher(Component c) {
    if (c == null)
      return;
    VISIBILITY_TOGGLE_LISTENERS.removeListener(c);
  }

  /**
   * Determines if the given component tracks visibility changes.
   *
   * @param c The component.
   * @return {@code true} if the given component tracks visibility changes
   * @since 15649
   */
  public static synchronized boolean hasVisibilitySwitcher(Component c) {
    if (c == null)
      return false;
    return VISIBILITY_TOGGLE_LISTENERS.containsListener(c);
  }

  /**
   * Constructs a new {@code DeveloperToggleAction}.
   */
  public DeveloperToggleAction() {
    super(tr("Enable experimental beta-features (might be unstable)"), new ImageProvider("expert").setOptional(true),
      tr("Enable/disable developer mode"), null, false /* register toolbar */, null, false);
    setToolbarId("mapillary:developermode");
    if (MainApplication.getToolbar() != null) {
      MainApplication.getToolbar().register(this);
    }
    setSelected(PREF_EXPERT.get());
    notifySelectedState();
  }

  @Override
  protected final void notifySelectedState() {
    super.notifySelectedState();
    PREF_EXPERT.put(isSelected());
    fireExpertModeChanged(isSelected());
  }

  /**
   * Forces the developer mode state to the given state.
   *
   * @param isDeveloper if developer mode should be used.
   * @since 11224
   */
  public void setDeveloper(boolean isDeveloper) {
    if (isSelected() != isDeveloper) {
      setSelected(isDeveloper);
      notifySelectedState();
    }
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    toggleSelectedState(e);
    notifySelectedState();
  }

  /**
   * Replies the unique instance of this action.
   *
   * @return The unique instance of this action
   */
  public static DeveloperToggleAction getInstance() {
    return INSTANCE;
  }

  /**
   * Determines if Developer mode is enabled.
   *
   * @return {@code true} if Developer mode is enabled, {@code false} otherwise.
   */
  public static boolean isDeveloper() {
    return INSTANCE.isSelected();
  }

  /**
   * This listener is notified whenever the developer mode setting changed.
   */
  @FunctionalInterface
  public interface DeveloperModeChangeListener {
    /**
     * The developer mode changed.
     *
     * @param isDeveloper <code>true</code> if developer mode was enabled, false otherwise.
     */
    void developerChanged(boolean isDeveloper);
  }
}
