// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.stream.Collectors;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryExportDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.io.export.MapillaryExportManager;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.ImageProvider.ImageSizes;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Action that launches a MapillaryExportDialog and lets you export the images.
 *
 * @author nokutu
 */
public class MapillaryExportAction extends JosmAction {

  private static final long serialVersionUID = 6009490043174837948L;
  private static final String EXPORT_MAPILLARY_IMAGES = marktr("Export Mapillary images");

  private MapillaryExportDialog dialog;

  /**
   * Main constructor.
   */
  public MapillaryExportAction() {
    super(tr(EXPORT_MAPILLARY_IMAGES), new ImageProvider(MapillaryPlugin.LOGO).setSize(ImageSizes.DEFAULT),
      tr(EXPORT_MAPILLARY_IMAGES), Shortcut.registerShortcut("mapillary:exportMapillaryImages",
        tr(EXPORT_MAPILLARY_IMAGES), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
      false, "mapillary:exportMapillaryImages", true);
    this.setEnabled(false);
  }

  @Override
  public void actionPerformed(ActionEvent ae) {
    JOptionPane pane = new JOptionPane();

    JButton ok = new JButton("Ok");
    ok.addActionListener(e -> pane.setValue(JOptionPane.OK_OPTION));
    JButton cancel = new JButton(tr("Cancel"));
    cancel.addActionListener(e -> pane.setValue(JOptionPane.CANCEL_OPTION));

    this.dialog = new MapillaryExportDialog(ok);
    pane.setMessage(this.dialog);
    pane.setOptions(new JButton[] { ok, cancel });

    JDialog dlg = pane.createDialog(MainApplication.getMainFrame(), tr("Export Mapillary images"));
    dlg.setMinimumSize(new Dimension(400, 150));
    dlg.setVisible(true);

    // Checks if the inputs are correct and starts the export process.
    if (pane.getValue() != null && (int) pane.getValue() == JOptionPane.OK_OPTION && this.dialog.chooser != null) {
      if (this.dialog.group.isSelected(this.dialog.all.getModel())) {
        export(MapillaryLayer.getInstance().getData().getNodes().stream().filter(MapillaryImageUtils.IS_IMAGE)
          .collect(Collectors.toSet()));
      } else if (this.dialog.group.isSelected(this.dialog.sequence.getModel())) {
        Set<INode> images = new ConcurrentSkipListSet<>();
        for (INode image : MapillaryLayer.getInstance().getData().getSelectedNodes().stream()
          .filter(MapillaryImageUtils.IS_IMAGE).collect(Collectors.toSet())) {
          if (MapillaryImageUtils.getSequenceKey(image) != null) {
            if (!images.contains(image)) {
              String sequence = MapillaryImageUtils.getSequenceKey(image);
              Set<INode> tImages = image.getDataSet().getNodes().stream().filter(MapillaryImageUtils.IS_IMAGE)
                .filter(i -> sequence.equals(MapillaryImageUtils.getSequenceKey(i))).collect(Collectors.toSet());
              images.addAll(tImages);
            }
          } else {
            images.add(image);
          }
        }
        export(images);
      } else if (this.dialog.group.isSelected(this.dialog.selected.getModel())) {
        export(MapillaryLayer.getInstance().getData().getSelectedNodes());
      }
      // This option ignores the selected directory.
    } else if (this.dialog.group.isSelected(this.dialog.rewrite.getModel())) {
      ArrayList<INode> images = new ArrayList<>();
      MapillaryLayer.getInstance().getData().getNodes().stream()
        .filter(img -> img.hasKey(MapillaryImageUtils.IMPORTED_KEY)).forEach(images::add);
      try {
        MainApplication.worker.execute(new MapillaryExportManager<>(images));
      } catch (IOException e1) {
        Logging.error(e1);
      }
    }
    dlg.dispose();
  }

  /**
   * Exports the given images from the database.
   *
   * @param images
   *        The set of images to be exported.
   */
  public void export(Collection<? extends INode> images) {
    MainApplication.worker
      .execute(new MapillaryExportManager<>(images, this.dialog.chooser.getSelectedFile().toString()));
  }

  @Override
  protected boolean listenToSelectionChange() {
    return false;
  }

  /**
   * Enabled when mapillary layer is in layer list
   */
  @Override
  protected void updateEnabledState() {
    super.updateEnabledState();
    setEnabled(MapillaryLayer.hasInstance());
  }
}
