// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.data.osm.AbstractPrimitive;
import org.openstreetmap.josm.data.osm.Tag;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;

public class AddTagToPrimitiveAction extends AbstractAction {
  private static final long serialVersionUID = 4834918715956633953L;

  private Tag tag;
  private AbstractPrimitive target;

  public AddTagToPrimitiveAction(final String name) {
    super(name, ImageProvider.get("dialogs/add", ImageSizes.SMALLICON));
  }

  public void setTag(Tag tag) {
    this.tag = tag;
    updateEnabled();
  }

  public void setTarget(AbstractPrimitive target) {
    this.target = target;
    updateEnabled();
  }

  private void updateEnabled() {
    setEnabled(tag != null && target != null);
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    if (target != null && tag != null) {
      int conflictResolution = JOptionPane.YES_OPTION;
      if (target.hasKey(tag.getKey()) && !target.hasTag(tag.getKey(), tag.getValue())) {
        conflictResolution = JOptionPane.showConfirmDialog(
          MainApplication.getMainFrame(),
          "<html>" +
            I18n.tr("A tag with key <i>{0}</i> is already present on the selected OSM object.", tag.getKey()) + "<br>" +
            I18n.tr(
              "Do you really want to replace the current value <i>{0}</i> with the new value <i>{1}</i>?",
              target.get(tag.getKey()),
              tag.getValue()
            ) + "</html>",
          I18n.tr("Tag conflict"),
          JOptionPane.YES_NO_OPTION,
          JOptionPane.WARNING_MESSAGE
        );
      }
      if (JOptionPane.YES_OPTION == conflictResolution) {
        target.put(tag);
        target.setModified(true);
      }
    }
  }
}
