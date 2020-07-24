// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.changeset;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.mode.EditMode;

/**
 * Mark/Unmark current Image for deletion.
 *
 * @author Kishan
 */
public class DeleteImageAction extends AbstractAction {

  private MapillaryImage img;

  public DeleteImageAction() {
    super("Delete Image");
    putValue(SHORT_DESCRIPTION, tr("Mark image for deletion "));
  }

  public void setImage(MapillaryImage img) {
    this.img = img;
    updateEnabled();
    updateText();
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    if (img != null) {
      if (img.isDeleted()) {
        img.unmarkDeleted();
      } else {
        img.markDeleted();
      }
      updateText();
      MapillaryLayer.getInstance().invalidate();
    }
  }

  private void updateEnabled() {
    setEnabled(img != null && MapillaryLayer.getInstance().mode instanceof EditMode);
  }

  private void updateText() {
    if (img != null) {
      if (img.isDeleted()) {
        putValue(NAME, tr("Undelete Image"));
        putValue(SHORT_DESCRIPTION, tr("Unmark whole imguence for deletion "));
      } else {
        putValue(NAME, tr("Delete Image"));
        putValue(SHORT_DESCRIPTION, tr("Mark whole imguence for deletion "));
      }
    } else {
      putValue(NAME, tr("Delete Image"));
      putValue(SHORT_DESCRIPTION, tr("Mark whole imguence for deletion "));
    }
  }
}
