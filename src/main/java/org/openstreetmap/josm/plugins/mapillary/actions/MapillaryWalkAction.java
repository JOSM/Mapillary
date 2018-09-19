// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryWalkDialog;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

/**
 * Walks forward at a given interval.
 *
 * @author nokutu
 *
 */
public class MapillaryWalkAction extends JosmAction implements MapillaryDataListener {

  private static final long serialVersionUID = 3454223919402245818L;

  private WalkThread thread;
  private final List<WalkListener> listeners = new ArrayList<>();

  public MapillaryWalkAction() {
    super(tr("Walk mode"), new ImageProvider(MapillaryPlugin.LOGO).setSize(ImageSizes.DEFAULT),
        tr("Walk mode"), null,
        false, "mapillaryWalk", true);
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    MapillaryWalkDialog dialog = new MapillaryWalkDialog();
    JOptionPane pane = new JOptionPane(dialog, JOptionPane.PLAIN_MESSAGE,
        JOptionPane.OK_CANCEL_OPTION);
    JDialog dlg = pane.createDialog(MainApplication.getMainFrame(), tr("Walk mode"));
    dlg.setMinimumSize(new Dimension(400, 150));
    dlg.setVisible(true);
    if (pane.getValue() != null
        && (int) pane.getValue() == JOptionPane.OK_OPTION) {
      this.thread = new WalkThread((int) dialog.spin.getValue(),
          dialog.waitForPicture.isSelected(),
          dialog.followSelection.isSelected(), dialog.goForward.isSelected());
      fireWalkStarted();
      this.thread.start();
      MapillaryMainDialog.getInstance().setMode(MapillaryMainDialog.MODE.WALK);
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
   *          The listener to be added.
   */
  public void addListener(WalkListener lis) {
    this.listeners.add(lis);
  }

  /**
   * Removes a listener.
   *
   * @param lis
   *          The listener to be added.
   */
  public void removeListener(WalkListener lis) {
    this.listeners.remove(lis);
  }

  private void fireWalkStarted() {
    if (this.listeners.isEmpty()) {
      return;
    }
    for (WalkListener lis : this.listeners) {
      lis.walkStarted(this.thread);
    }
  }

  @Override
  protected boolean listenToSelectionChange() {
    return false;
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
    if (oldImage == null && newImage != null) {
      setEnabled(true);
    } else if (oldImage != null && newImage == null) {
      setEnabled(false);
    }
  }

  /**
   * Enabled when a mapillary image is selected.
   */
  @Override
  protected void updateEnabledState() {
    super.updateEnabledState();
    setEnabled(MapillaryLayer.hasInstance() && MapillaryLayer.getInstance().getData().getSelectedImage() != null);
  }

}
