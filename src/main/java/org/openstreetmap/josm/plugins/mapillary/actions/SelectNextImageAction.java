package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.Serializable;
import java.util.function.Supplier;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.tools.Shortcut;

public class SelectNextImageAction extends JosmAction {
  private static final long serialVersionUID = -2106549590908822237L;

  private static final String DIALOGS_SUBDIR = "dialogs";

  public static final SelectNextImageAction NEXT_ACTION = new SelectNextImageAction(
    tr("Next picture"), tr("Shows the next picture in the sequence"), DIALOGS_SUBDIR + "/next", Shortcut
      .registerShortcut("mapillary:jump_to_next", tr("Jump to next image"), KeyEvent.VK_PAGE_DOWN, Shortcut.DIRECT),
    () -> {
      final INode curImg = MapillaryMainDialog.getInstance().getImage();
      if (curImg != null) {
        final INode nextImg = MapillarySequenceUtils.getNextOrPrevious(curImg,
          MapillarySequenceUtils.NextOrPrevious.NEXT);
        if (nextImg != null && nextImg.isVisible()) {
          return nextImg;
        }
      }
      return null;
    });

  public static final SelectNextImageAction PREVIOUS_ACTION = new SelectNextImageAction(tr("Previous picture"),
    tr("Shows the previous picture in the sequence"), DIALOGS_SUBDIR + "/previous", Shortcut.registerShortcut(
      "mapillary:jump_to_previous", tr("Jump to previous image"), KeyEvent.VK_PAGE_UP, Shortcut.DIRECT),
    () -> {
      final INode curImg = MapillaryMainDialog.getInstance().getImage();
      if (curImg != null) {
        final INode prevImg = MapillarySequenceUtils.getNextOrPrevious(curImg,
          MapillarySequenceUtils.NextOrPrevious.PREVIOUS);
        if (prevImg != null && prevImg.isVisible()) {
          return prevImg;
        }
      }
      return null;
    });

  public static final SelectNextImageAction RED_ACTION = new SelectNextImageAction(tr("Jump to red"),
    tr("Jumps to the picture at the other side of the red line"), DIALOGS_SUBDIR + "/red",
    Shortcut.registerShortcut("mapillary:jump_to_red", tr("Jump to red image"), KeyEvent.VK_PAGE_DOWN, Shortcut.CTRL),
    () -> MapillaryLayer.getInstance().getNNearestImage(1));

  public static final SelectNextImageAction BLUE_ACTION = new SelectNextImageAction(tr("Jump to blue"),
    tr("Jumps to the picture at the other side of the blue line"), DIALOGS_SUBDIR + "/blue",
    Shortcut.registerShortcut("mapillary:jump_to_blue", tr("Jump to blue image"), KeyEvent.VK_PAGE_UP, Shortcut.CTRL),
    () -> MapillaryLayer.getInstance().getNNearestImage(2));

  private final SerializableSupplier<INode> destinationImgSupplier;

  private SelectNextImageAction(final String name, final String description, final String icon, final Shortcut sc,
    final SerializableSupplier<INode> destinationImgSupplier) {
    super(name, icon, description, sc, false, "mapillary:" + name.replace(" ", "_"), false);
    putValue(SHORT_DESCRIPTION, description);
    this.destinationImgSupplier = destinationImgSupplier;
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    final INode newSelectedImage = destinationImgSupplier.get();
    if (newSelectedImage != null) {
      MapillaryLayer.getInstance().getData().setSelected(newSelectedImage);
      // TODO remove if it turns out to be unneeded -- due to ids, unintended images
      // may be selected.
      if (MapillaryMainDialog.hasInstance() && !newSelectedImage.equals(MapillaryMainDialog.getInstance().getImage())) {
        MapillaryMainDialog.getInstance().setImage(newSelectedImage);
      }
      MainApplication.getMap().mapView.zoomTo(newSelectedImage.getCoor());
    }
  }

  /**
   * Get the image supplier
   *
   * @return The supplier
   */
  public Supplier<INode> getDestinationImageSupplier() {
    return this.destinationImgSupplier;
  }

  @FunctionalInterface
  private interface SerializableSupplier<T> extends Supplier<T>, Serializable {
  }
}
