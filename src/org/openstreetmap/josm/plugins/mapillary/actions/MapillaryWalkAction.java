package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryWalkDialog;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Walks forward at a given interval.
 *
 * @author nokutu
 *
 */
public class MapillaryWalkAction extends JosmAction implements
    MapillaryDataListener {

  private static final long serialVersionUID = 3454223919402245818L;

  private WalkThread thread = null;
  private ArrayList<WalkListener> listeners = new ArrayList<>();

  /**
   *
   */
  public MapillaryWalkAction() {
    super(tr("Walk mode"), new ImageProvider("icon24.png"), tr("Walk mode"),
        Shortcut.registerShortcut("Mapillary walk", tr("Start walk mode"),
            KeyEvent.CHAR_UNDEFINED, Shortcut.NONE), false, "mapillaryWalk",
        false);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    MapillaryWalkDialog dialog = new MapillaryWalkDialog();
    JOptionPane pane = new JOptionPane(dialog, JOptionPane.PLAIN_MESSAGE,
        JOptionPane.OK_CANCEL_OPTION);
    JDialog dlg = pane.createDialog(Main.parent, tr("Walk mode"));
    dlg.setMinimumSize(new Dimension(400, 150));
    dlg.setVisible(true);
    if (pane.getValue() != null
        && (int) pane.getValue() == JOptionPane.OK_OPTION) {
      thread = new WalkThread((int) dialog.spin.getValue(),
          dialog.waitForPicture.isSelected(), dialog.followSelection.isSelected());
      fireWalkStarted();
      thread.start();
      MapillaryMainDialog.getInstance().setMode(MapillaryMainDialog.Mode.WALK);
    }
  }

  @Override
  public void imagesAdded() {
    // Nothing
  }

  /**
   * Adds a listener.
   *
   * @param lis
   */
  public void addListener(WalkListener lis) {
    listeners.add(lis);
  }

  /**
   * Removes a listener.
   *
   * @param lis
   */
  public void removeListener(WalkListener lis) {
    listeners.remove(lis);
  }

  private void fireWalkStarted() {
    if (listeners.isEmpty())
      return;
    for (WalkListener lis : listeners)
      lis.walkStarted(thread);
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage,
      MapillaryAbstractImage newImage) {
    if (newImage != null)
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.WALK_MENU, true);
    else
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.WALK_MENU, false);
  }

}
