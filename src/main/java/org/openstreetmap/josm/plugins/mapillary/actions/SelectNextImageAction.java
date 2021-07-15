package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.Serializable;
import java.util.function.Supplier;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

public class SelectNextImageAction extends JosmAction {

  private static final long serialVersionUID = -2106549590908822237L;
  private static final String MAPILLARY_PREF_PREFIX = marktr("Mapillary: {0}");

  private static final String DIALOGS_SUBDIR = "dialogs";

  public static final SelectNextImageAction FIRST_ACTION = new SelectNextImageRememberAction(tr("First picture"),
    tr("Show first Image"), new ImageProvider(DIALOGS_SUBDIR, "first"),
    Shortcut.registerShortcut("mapillary:jump_to_first", tr(MAPILLARY_PREF_PREFIX, tr("Show first Image")),
      KeyEvent.VK_HOME, Shortcut.DIRECT),
    () -> {
      final INode current = MapillaryMainDialog.getInstance().getImage();
      if (current != null) {
        IWay<?> sequence = MapillaryImageUtils.getSequence(current);
        if (sequence != null) {
          return sequence.firstNode();
        }
      }
      return null;
    });

  public static final SelectNextImageAction LAST_ACTION = new SelectNextImageRememberAction(tr("Last picture"),
    tr("Show last Image"), new ImageProvider(DIALOGS_SUBDIR, "last"),
    Shortcut.registerShortcut("mapillary:jump_to_last", tr(MAPILLARY_PREF_PREFIX, tr("Show last Image")),
      KeyEvent.VK_END, Shortcut.DIRECT),
    () -> {
      final INode current = MapillaryMainDialog.getInstance().getImage();
      if (current != null) {
        IWay<?> sequence = MapillaryImageUtils.getSequence(current);
        if (sequence != null) {
          return sequence.lastNode();
        }
      }
      return null;
    });

  public static final SelectNextImageAction NEXT_ACTION = new SelectNextImageAction(tr("Next picture"),
    tr("Shows the next picture in the sequence"), new ImageProvider(DIALOGS_SUBDIR, "next"),
    Shortcut.registerShortcut("mapillary:jump_to_next", tr(MAPILLARY_PREF_PREFIX, tr("Show next Image")),
      KeyEvent.VK_PAGE_DOWN, Shortcut.DIRECT),
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
    tr("Shows the previous picture in the sequence"), new ImageProvider(DIALOGS_SUBDIR, "previous"),
    Shortcut.registerShortcut("mapillary:jump_to_previous", tr(MAPILLARY_PREF_PREFIX, tr("Show previous Image")),
      KeyEvent.VK_PAGE_UP, Shortcut.DIRECT),
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

  public static final SelectNextImageAction RED_ACTION = new SelectNextImageAction(
    tr("Jump to red"), tr("Jumps to the picture at the other side of the red line"),
    new ImageProvider(DIALOGS_SUBDIR, "red"), Shortcut.registerShortcut("mapillary:jump_to_red",
      tr(MAPILLARY_PREF_PREFIX, tr("Jump to red image")), KeyEvent.VK_PAGE_DOWN, Shortcut.CTRL),
    () -> MapillaryLayer.getInstance().getNNearestImage(1));

  public static final SelectNextImageAction BLUE_ACTION = new SelectNextImageAction(
    tr("Jump to blue"), tr("Jumps to the picture at the other side of the blue line"),
    new ImageProvider(DIALOGS_SUBDIR, "blue"), Shortcut.registerShortcut("mapillary:jump_to_blue",
      tr(MAPILLARY_PREF_PREFIX, tr("Jump to blue image")), KeyEvent.VK_PAGE_UP, Shortcut.CTRL),
    () -> MapillaryLayer.getInstance().getNNearestImage(2));

  private final SerializableSupplier<INode> destinationImgSupplier;

  SelectNextImageAction(final String name, final String description, final ImageProvider icon, final Shortcut sc,
    final SerializableSupplier<INode> destinationImgSupplier) {
    super(null, icon, description, sc, false, "mapillary:" + name.replace(" ", "_"), false);
    putValue(SHORT_DESCRIPTION, description);
    this.destinationImgSupplier = destinationImgSupplier;
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    final INode newSelectedImage = this.getDestinationImageSupplier().get();
    if (newSelectedImage != null) {
      MapillaryLayer.getInstance().setSelected(newSelectedImage);
      // TODO remove if it turns out to be unneeded -- due to ids, unintended images
      // may be selected.
      if (MapillaryMainDialog.hasInstance() && !newSelectedImage.equals(MapillaryMainDialog.getInstance().getImage())) {
        MapillaryMainDialog.getInstance().setImage(newSelectedImage);
      }
      if (Boolean.TRUE.equals(MapillaryProperties.MOVE_TO_IMG.get())) {
        MainApplication.getMap().mapView.zoomTo(newSelectedImage.getCoor());
      }
    }
  }

  /**
   * Check if this action should be enabled
   *
   * @param currentImage The image to check against
   * @return {@code true} if this button should be enabled
   */
  public void updateEnabled(final Component component, final INode currentImage) {
    final INode actionNode = this.getDestinationImageSupplier().get();
    component.setEnabled(actionNode != null && !actionNode.equals(currentImage));
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
  interface SerializableSupplier<T> extends Supplier<T>, Serializable {
  }
}
