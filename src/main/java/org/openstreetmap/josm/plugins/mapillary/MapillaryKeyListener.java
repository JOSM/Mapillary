// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.JPopupMenu;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import org.openstreetmap.josm.gui.dialogs.properties.PropertiesDialog;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * Listen for Mapillary keys
 *
 * @author Taylor Smock
 */
public class MapillaryKeyListener implements PopupMenuListener, Destroyable {

  private JPopupMenu menu;
  private PropertiesDialog properties;
  private Map<JPopupMenu, List<String>> addedValues = new HashMap<>();
  private Map<JPopupMenu, List<Component>> addedComponents = new HashMap<>();

  public MapillaryKeyListener(PropertiesDialog properties, JPopupMenu menu) {
    this.menu = menu;
    this.properties = properties;
    menu.addPopupMenuListener(this);
  }

  @Override
  public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
    JPopupMenu popup = (JPopupMenu) e.getSource();
    properties.visitSelectedProperties((primitive, key, value) -> addAction(popup, key, value));
  }

  @Override
  public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
    JPopupMenu popup = (JPopupMenu) e.getSource();
    if (addedValues.containsKey(popup) && addedComponents.containsKey(popup)) {
      addedValues.remove(popup);
      addedComponents.get(popup).forEach(popup::remove);
      addedComponents.remove(popup);
    }
  }

  @Override
  public void popupMenuCanceled(PopupMenuEvent e) {
    // Do nothing
  }

  private void addAction(JPopupMenu popup, String key, String value) {
    if ("mapillary".equals(key.toLowerCase(Locale.ROOT)) && MapillaryLayer.hasInstance()) {
      List<String> strings = addedValues.computeIfAbsent(popup, p -> new ArrayList<>());
      List<Component> components = addedComponents.computeIfAbsent(popup, p -> new ArrayList<>());
      if (!strings.contains(value)) {
        strings.add(value);
        components.add(popup.add(new MapillaryKeyAction(value)));
      }
    }
  }

  @Override
  public void destroy() {
    if (menu != null) {
      menu.removePopupMenuListener(this);
    }
    properties = null;
    menu = null;
  }

  private static class MapillaryKeyAction extends AbstractAction {
    private static final long serialVersionUID = -4129937925620244251L;
    private String imageKey;

    MapillaryKeyAction(String imageKey) {
      new ImageProvider("mapillary-logo").getResource().attachImageIcon(this, true);
      this.imageKey = imageKey;
      putValue(NAME, tr("Select Mapillary Image ({0})", this.imageKey));
      putValue(SHORT_DESCRIPTION, tr("Select the Mapillary Image {0} on the {1} layer (downloads if needed)",
          this.imageKey, MapillaryLayer.getInstance().getName()));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      MapillaryData data = MapillaryLayer.getInstance().getData();
      if (data.getImage(this.imageKey) == null) {
        MapillaryDownloader.downloadSequences(
            MapillaryDownloader.downloadImages(this.imageKey.split(";", 0)).keySet().toArray(new String[0]));
      }
      GuiHelper.runInEDT(() -> data.setSelectedImage(data.getImage(this.imageKey)));
    }

  }
}
