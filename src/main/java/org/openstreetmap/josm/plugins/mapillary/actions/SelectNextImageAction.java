package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.Serializable;
import java.util.function.Supplier;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.Shortcut;

public class SelectNextImageAction extends JosmAction {
  private static final long serialVersionUID = -2106549590908822237L;

  private static final String DIALOGS_SUBDIR = "dialogs";

  public static final SelectNextImageAction NEXT_ACTION = new SelectNextImageAction(
    tr("Next picture"),
    tr("Shows the next picture in the sequence"),
    DIALOGS_SUBDIR + "/next",
    Shortcut.registerShortcut("mapillary:jump_to_next", tr("Jump to next image"), KeyEvent.VK_PAGE_DOWN, Shortcut.DIRECT),
    () -> {
      final MapillaryAbstractImage curImg = MapillaryLayer.getInstance().getData().getSelectedImage();
      if (curImg != null) {
        final MapillaryAbstractImage nextImg = curImg.next();
        if (nextImg != null && nextImg.isVisible()) {
          return nextImg;
        }
      }
      return null;
    }
  );

  public static final SelectNextImageAction PREVIOUS_ACTION = new SelectNextImageAction(
    tr("Previous picture"),
    tr("Shows the previous picture in the sequence"),
    DIALOGS_SUBDIR + "/previous",
    Shortcut.registerShortcut("mapillary:jump_to_previous", tr("Jump to previous image"), KeyEvent.VK_PAGE_UP, Shortcut.DIRECT),
    () -> {
      final MapillaryAbstractImage curImg = MapillaryLayer.getInstance().getData().getSelectedImage();
      if (curImg != null) {
        final MapillaryAbstractImage prevImg = curImg.previous();
        if (prevImg != null && prevImg.isVisible()) {
          return prevImg;
        }
      }
      return null;
    }
  );

  public static final SelectNextImageAction RED_ACTION = new SelectNextImageAction(
    tr("Jump to red"),
    tr("Jumps to the picture at the other side of the red line"),
    DIALOGS_SUBDIR + "/red",
    Shortcut.registerShortcut("mapillary:jump_to_red", tr("Jump to red image"), KeyEvent.VK_PAGE_DOWN, Shortcut.CTRL),
    () -> MapillaryLayer.getInstance().getNNearestImage(1)
  );

  public static final SelectNextImageAction BLUE_ACTION = new SelectNextImageAction(
    tr("Jump to blue"),
    tr("Jumps to the picture at the other side of the blue line"),
    DIALOGS_SUBDIR + "/blue",
    Shortcut.registerShortcut("mapillary:jump_to_blue", tr("Jump to blue image"), KeyEvent.VK_PAGE_UP, Shortcut.CTRL),
    () -> MapillaryLayer.getInstance().getNNearestImage(2)
  );

  private final SerializableSupplier<MapillaryAbstractImage> destinationImgSupplier;

  private SelectNextImageAction(
    final String name,
    final String description,
    final String icon,
    final Shortcut sc,
    final SerializableSupplier<MapillaryAbstractImage> destinationImgSupplier
  ) {
    super(name, icon, description, sc, false, "mapillary:" + name.replace(" ", "_"), false);
    putValue(SHORT_DESCRIPTION, description);
    this.destinationImgSupplier = destinationImgSupplier;
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    final MapillaryAbstractImage newSelectedImage = destinationImgSupplier.get();
    if (newSelectedImage != null) {
      MapillaryLayer.getInstance().getData().setSelectedImage(newSelectedImage, MapillaryProperties.MOVE_TO_IMG.get());
    }
  }

  @FunctionalInterface
  private interface SerializableSupplier<T> extends Supplier<T>, Serializable { }
}
