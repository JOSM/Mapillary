// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.changeset;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.util.Iterator;
import javax.swing.AbstractAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.mode.EditMode;

/**
 * Mark/Unmark current sequence for deletion.
 *
 * @author Kishan
 */
public class DeleteSequenceAction extends AbstractAction {

  private MapillarySequence seq;

  public DeleteSequenceAction() {
    super("Delete Sequence");
    putValue(SHORT_DESCRIPTION, tr("Mark whole sequence for deletion "));
  }

  public void setSequence(MapillarySequence seq) {
    this.seq = seq;
    updateEnabled();
    updateText();
  }

  public boolean isSeqDeleted() {
    if (seq != null && seq.getKey() != null) {
      Iterator<MapillaryAbstractImage> iterator = seq.getImages().iterator();
      boolean isSeqDeleted = true;
      while (iterator.hasNext() && isSeqDeleted) {
        MapillaryAbstractImage img = iterator.next();
        if (img instanceof MapillaryImage) {
          isSeqDeleted = ((MapillaryImage) img).isDeleted();
        }
      }
      return isSeqDeleted;
    } else {
      return false;
    }
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    if (seq != null && seq.getKey() != null) {
      Iterator<MapillaryAbstractImage> iterator;
      iterator = seq.getImages().iterator();
      if (isSeqDeleted()) {
        while (iterator.hasNext()) {
          MapillaryAbstractImage img = iterator.next();
          if (img instanceof MapillaryImage) {
            ((MapillaryImage) img).unmarkDeleted();
          }
        }
      } else {
        while (iterator.hasNext()) {
          MapillaryAbstractImage img = iterator.next();
          if (img instanceof MapillaryImage) {
            ((MapillaryImage) img).markDeleted();
          }
        }
      }
      updateText();
      MapillaryLayer.getInstance().invalidate();
    }
  }

  private void updateEnabled() {
    setEnabled(seq != null && seq.getKey() != null && MapillaryLayer.getInstance().mode instanceof EditMode);
  }

  private void updateText() {
    if (isSeqDeleted()) {
      putValue(NAME, tr("Undelete Sequence"));
      putValue(SHORT_DESCRIPTION, tr("Unmark whole sequence for deletion "));
    } else {
      putValue(NAME, tr("Delete Sequence"));
      putValue(SHORT_DESCRIPTION, tr("Mark whole sequence for deletion "));
    }
  }
}
