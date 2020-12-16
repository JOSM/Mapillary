// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.changeset;

import java.awt.event.ActionEvent;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryAbstractImage;

import static org.openstreetmap.josm.tools.I18n.tr;

/**
 * Mark/Unmark current Image as reviewed for uploading/changeset submission.
 *
 * @author Kishan
 */
public class ReviewImageAction extends JosmAction {
  private MapillaryAbstractImage img;

  public ReviewImageAction() {
    super(tr("Review"), null, tr("Mark image as Reviewed"), null, false);
  }

  public void setImage(MapillaryAbstractImage img) {
    this.img = img;
    updateEnabled();
    updateText();
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    if (img != null) {
      if (img.isReviewed()) {
        img.setReviewed(false);
      } else {
        img.setReviewed(true);
      }
      updateText();
    }
  }

  private void updateEnabled() {
    setEnabled(img != null);
  }

  private void updateText() {
    if (img != null) {
      if (img.isReviewed()) {
        putValue(NAME, tr("Unmark Reviewed"));
        putValue(SHORT_DESCRIPTION, tr("Unmark image as reviewed"));
      } else {
        putValue(NAME, tr("Mark Reviewed"));
        putValue(SHORT_DESCRIPTION, tr("Mark image as reviewed"));
      }
    } else {
      putValue(NAME, tr("Mark Reviewed"));
      putValue(SHORT_DESCRIPTION, tr("Mark image as reviewed"));
    }
  }
}
