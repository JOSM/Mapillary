package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
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
    JDialog dlg = pane.createDialog(Main.parent, tr("Export images"));
    dlg.setMinimumSize(new Dimension(400, 150));
    dlg.setVisible(true);
    if (pane.getValue() != null
        && (int) pane.getValue() == JOptionPane.OK_OPTION) {
      new WalkThread((int) dialog.spin.getValue(),
          dialog.waitForPicture.isSelected()).start();
    }
  }

  @Override
  public void imagesAdded() {
    // Nothing
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage,
      MapillaryAbstractImage newImage) {
    if (newImage != null)
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.WALK_MENU, true);
    else
      MapillaryPlugin.setMenuEnabled(MapillaryPlugin.WALK_MENU, false);
  }

  private class WalkThread extends Thread implements MapillaryDataListener {
    private int interval;
    private MapillaryData data;
    private Lock lock = new ReentrantLock();
    private boolean end = false;
    private boolean waitForPicture;
    private BufferedImage lastImage;

    private WalkThread(int interval, boolean waitForPicture) {
      this.interval = interval;
      this.waitForPicture = waitForPicture;
      data = MapillaryLayer.getInstance().getMapillaryData();
      data.addListener(this);
    }

    @Override
    public void run() {
      try {
        while (!end && data.getSelectedImage().next() != null) {
          try {
            synchronized (this) {
              if (waitForPicture) {
                while (MapillaryMainDialog.getInstance().mapillaryImageDisplay
                    .getImage() == lastImage
                    || MapillaryMainDialog.getInstance().mapillaryImageDisplay
                        .getImage() == null
                    || MapillaryMainDialog.getInstance().mapillaryImageDisplay
                        .getImage().getWidth() < 2048)
                  wait(100);
              }
              wait(interval);
            }
            lastImage = MapillaryMainDialog.getInstance().mapillaryImageDisplay
                .getImage();
            synchronized (lock) {
              data.selectNext();
            }
          } catch (InterruptedException e) {
            return;
          }
        }
      } catch (NullPointerException e) {
        return;
      }
    }

    @Override
    public void interrupt() {
      end = true;
      data.removeListener(this);
      super.interrupt();
    }

    @Override
    public void imagesAdded() {
      // Nothing
    }

    @Override
    public void selectedImageChanged(MapillaryAbstractImage oldImage,
        MapillaryAbstractImage newImage) {
      if (newImage != oldImage.next()) {
        synchronized (lock) {
          interrupt();
        }
      }
    }
  }
}
