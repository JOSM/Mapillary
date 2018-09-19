// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.GraphicsEnvironment;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.swing.AbstractListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.event.ListSelectionListener;

import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.gui.layer.geoimage.GeoImageLayer;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryImportAction;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;

public class ChooseGeoImageLayersDialog extends JDialog {
  private static final long serialVersionUID = -1793622345412435234L;
  private static final String QUESTION = I18n.marktr("Which image layers do you want to import into the Mapillary layer?");

  public ChooseGeoImageLayersDialog(final Component parent, final List<GeoImageLayer> layers) {
    super(GuiHelper.getFrameForComponent(parent), I18n.tr(QUESTION));
    final Container c = getContentPane();
    c.setLayout(new BorderLayout(10, 10));

    final JPanel questionPanel = new JPanel(new FlowLayout(FlowLayout.LEADING, 10, 10));
    questionPanel.add(new JLabel(I18n.tr(QUESTION)));
    c.add(questionPanel, BorderLayout.NORTH);

    final JList<GeoImageLayer> list = new JList<>();
    list.setModel(new BasicListModel<>(layers));
    list.setCellRenderer(new GeoImageLayerListCellRenderer());
    list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    c.add(list, BorderLayout.CENTER);

    final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 10, 10));
    final JButton cancelButton = new JButton(I18n.tr("Cancel"), new ImageProvider("cancel").get());
    cancelButton.addActionListener(e -> dispose());
    cancelButton.requestFocus();
    buttonPanel.add(cancelButton);
    final JButton importButton = new JButton(I18n.tr("Import"), new ImageProvider("copy").get());

    // Set the import button enabled/disabled depending on if there are selected items in the layer list
    final ListSelectionListener importButtonEnabler = it -> importButton.setEnabled(list.getSelectedValuesList().size() >= 1);
    importButtonEnabler.valueChanged(null);
    list.addListSelectionListener(importButtonEnabler);

    importButton.addActionListener(e -> {
      list.getSelectedValuesList().parallelStream().map(gil -> {
        MapillarySequence seq = new MapillarySequence();
        seq.add(
          gil.getImages().parallelStream()
            .map(img -> {
              try {
                return MapillaryImportedImage.createInstance(img);
              } catch (IllegalArgumentException iae) {
                final String message = I18n.tr("Could not import a geotagged image to the Mapillary layer!");
                Logging.log(Logging.LEVEL_WARN, message, iae);
                if (!GraphicsEnvironment.isHeadless()) {
                  new Notification(message).setIcon(MapillaryPlugin.LOGO.get()).show();
                }
                return null;
              }
            })
            .filter(Objects::nonNull)
            .sorted((o1, o2) -> (int) Math.signum(o1.getCapturedAt() - o2.getCapturedAt())) // order by capturedAt timestamp (ascending)
            .collect(Collectors.toList())
        );
        return seq;
      }).forEach(seq -> {
        MapillaryLayer.getInstance().getData().addAll(seq.getImages(), false);
        MapillaryImportAction.recordChanges(seq.getImages());
        if (!MainApplication.getLayerManager().containsLayer(MapillaryLayer.getInstance())) {
          MainApplication.getLayerManager().addLayer(MapillaryLayer.getInstance());
        }
      });
      MapillaryLayer.invalidateInstance();
      dispose();
    });
    buttonPanel.add(importButton);
    c.add(buttonPanel, BorderLayout.SOUTH);

    setModal(true);
    pack();
    setLocationRelativeTo(parent);
  }

  protected static class GeoImageLayerListCellRenderer implements ListCellRenderer<GeoImageLayer> {
    @Override
    public Component getListCellRendererComponent(
      JList<? extends GeoImageLayer> list, GeoImageLayer value, int index, boolean isSelected, boolean cellHasFocus
    ) {
      final JLabel result = value == null
          ? null
          : new JLabel(
            /* i18n: {0} is the layer name, {1} the number of images in it */
            I18n.trn("{0} ({1} image)", "{0} ({1} images)", value.getImages().size(), value.getName(), value.getImages().size()),
            value.getIcon(),
            SwingConstants.LEADING
          );
      if (result != null) {
        result.setOpaque(true);
        result.setBackground(isSelected ? UIManager.getColor("List.selectionBackground") : UIManager.getColor("List.background"));
      }
      return result;
    }
  }

  private static class BasicListModel<T> extends AbstractListModel<T> {
    private static final long serialVersionUID = 3107247955341855290L;
    private final List<T> list;

    public BasicListModel(List<T> list) {
      this.list = list == null ? new ArrayList<>() : list;
    }

    @Override
    public int getSize() {
      return list.size();
    }

    @Override
    public T getElementAt(int index) {
      return list.get(index);
    }
  }
}
