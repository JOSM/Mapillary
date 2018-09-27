package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.io.Serializable;
import java.util.function.Supplier;

import javax.swing.AbstractAction;

import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.ImageProvider;

public class SelectNextImageAction extends AbstractAction {
  private static final String DIALOGS_SUBDIR = "dialogs";

  public static final SelectNextImageAction NEXT_ACTION = new SelectNextImageAction(
    tr("Next picture"),
    tr("Shows the next picture in the sequence"),
    new ImageProvider(DIALOGS_SUBDIR, "next"),
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
    new ImageProvider(DIALOGS_SUBDIR, "previous"),
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
    new ImageProvider(DIALOGS_SUBDIR, "red"),
    () -> MapillaryLayer.getInstance().getNNearestImage(1)
  );

  public static final SelectNextImageAction BLUE_ACTION = new SelectNextImageAction(
    tr("Jump to blue"),
    tr("Jumps to the picture at the other side of the blue line"),
    new ImageProvider(DIALOGS_SUBDIR, "blue"),
    () -> MapillaryLayer.getInstance().getNNearestImage(2)
  );

  private final SerializableSupplier<MapillaryAbstractImage> destinationImgSupplier;

  private SelectNextImageAction(
    final String name,
    final String description,
    final ImageProvider icon,
    final SerializableSupplier<MapillaryAbstractImage> destinationImgSupplier
  ) {
    super(name);
    putValue(SHORT_DESCRIPTION, description);
    icon.getResource().attachImageIcon(this, true);
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
