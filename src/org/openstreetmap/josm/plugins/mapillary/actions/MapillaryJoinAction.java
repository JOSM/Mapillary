package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.mode.JoinMode;
import org.openstreetmap.josm.plugins.mapillary.mode.SelectMode;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

public class MapillaryJoinAction extends JosmAction {

  public MapillaryJoinAction() {
    super(tr("Join mode"), new ImageProvider("icon24.png"),
        tr("Join/unjoin pictures"), Shortcut.registerShortcut("Mapillary join",
            tr("Join Mapillary pictures"), KeyEvent.CHAR_UNDEFINED,
            Shortcut.NONE), false, "mapillaryJoin", false);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent arg0) {
    if (MapillaryLayer.getInstance().mode instanceof SelectMode) {
      MapillaryLayer.getInstance().setMode(new JoinMode());
    } else {
      MapillaryLayer.getInstance().setMode(new SelectMode());
    }
  }
}
