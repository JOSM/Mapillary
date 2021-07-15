package org.openstreetmap.josm.plugins.mapillary.actions;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.util.Objects;
import java.util.function.Supplier;

import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryMainDialog;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * A class to select another image, but with the ability to go back
 *
 * @author Taylor Smock
 */
final class SelectNextImageRememberAction extends SelectNextImageAction {
  private INode previousImage;
  private final ImageProvider originalIcon;
  private final Supplier<INode> getPreviousImage = () -> this.previousImage;

  SelectNextImageRememberAction(String name, String description, ImageProvider icon, Shortcut sc,
    SerializableSupplier<INode> destinationImgSupplier) {
    super(name, description, icon, sc, destinationImgSupplier);
    this.originalIcon = icon;
  }

  @Override
  public void actionPerformed(final ActionEvent e) {
    final INode current = MapillaryMainDialog.getInstance().getImage();
    final INode expected = this.getDestinationImageSupplier().get();
    super.actionPerformed(e);
    if (!Objects.equals(current, expected)) {
      this.previousImage = current;
    } else {
      this.previousImage = null;
    }
  }

  @Override
  public void updateEnabled(final Component component, final INode currentImage) {
    super.updateEnabled(component, currentImage);
    if (!Objects.equals(currentImage, this.previousImage)
      && !Objects.equals(currentImage, super.getDestinationImageSupplier().get())) {
      this.previousImage = null;
    }
    if (component.isEnabled()) {
      if (!Objects.equals(this.getDestinationImageSupplier(), super.getDestinationImageSupplier())) {
        // history or refresh
        new ImageProvider("dialogs", "history").getResource().attachImageIcon(this, true);
      } else {
        this.originalIcon.getResource().attachImageIcon(this, true);
      }
    }
  }

  @Override
  public Supplier<INode> getDestinationImageSupplier() {
    if (this.previousImage != null) {
      return this.getPreviousImage;
    }
    return super.getDestinationImageSupplier();
  }

}
