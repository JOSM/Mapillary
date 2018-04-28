// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.MapView;
import org.openstreetmap.josm.gui.layer.geoimage.GeoImageLayer;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.MapillarySequence;
import org.openstreetmap.josm.plugins.mapillary.actions.MapillaryImportAction;
import org.openstreetmap.josm.plugins.mapillary.utils.ImageImportUtil;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.I18n;
import org.openstreetmap.josm.tools.ImageProvider;

public class ImportMethodDialog extends JDialog {
  private static final long serialVersionUID = -1654848887884375567L;
  private static final String QUESTION = I18n.marktr("From which source do you want to import images to the Mapillary layer?");
  private static final String NO_LAYERS_MESSAGE = I18n.marktr("There are currently no layers with geotagged images!");
  private static final String IMPORT_METHOD_FILE = I18n.marktr("Images from my file system");
  private static final String FILE_IMPORT_TITLE = I18n.marktr("Select the images you want to import");
  private static final String IMPORT_METHOD_DIRECTORY = I18n.marktr("All images in a directory");
  private static final String DIRECTORY_IMPORT_TITLE = I18n.marktr("Select directory to import images from");
  private static final String IMPORT_METHOD_LAYER = I18n.marktr("From existing image layer");
  private static final String CANCEL = I18n.marktr("Cancel");

  private static final JFileChooser DIRECTORY_CHOOSER = new JFileChooser();
  private static final JFileChooser IMAGE_FILE_CHOOSER = new JFileChooser();

  static {
    DIRECTORY_CHOOSER.setCurrentDirectory(new File(MapillaryProperties.START_DIR.get()));
    DIRECTORY_CHOOSER.setDialogTitle(I18n.tr(DIRECTORY_IMPORT_TITLE));
    DIRECTORY_CHOOSER.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    DIRECTORY_CHOOSER.setAcceptAllFileFilterUsed(false);
    DIRECTORY_CHOOSER.addChoosableFileFilter(ImageImportUtil.IMAGE_FILE_FILTER);
    DIRECTORY_CHOOSER.setMultiSelectionEnabled(false);

    IMAGE_FILE_CHOOSER.setCurrentDirectory(new File(MapillaryProperties.START_DIR.get()));
    IMAGE_FILE_CHOOSER.setDialogTitle(I18n.tr(FILE_IMPORT_TITLE));
    IMAGE_FILE_CHOOSER.setFileSelectionMode(JFileChooser.FILES_ONLY);
    IMAGE_FILE_CHOOSER.setAcceptAllFileFilterUsed(false);
    IMAGE_FILE_CHOOSER.addChoosableFileFilter(ImageImportUtil.IMAGE_FILE_FILTER);
    IMAGE_FILE_CHOOSER.setMultiSelectionEnabled(true);
  }

  public ImportMethodDialog(final Component parent) {
    super(GuiHelper.getFrameForComponent(parent), I18n.tr(QUESTION));

    final Container c = getContentPane();
    c.setLayout(new BorderLayout());

    final JPanel questionPanel = new JPanel(new FlowLayout(FlowLayout.LEADING, 10, 10));
    questionPanel.add(new JLabel(I18n.tr(QUESTION)));
    c.add(questionPanel, BorderLayout.NORTH);

    c.add(getAnswerPanel(), BorderLayout.CENTER);

    final JPanel cancelPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 10, 10));
    final JButton cancelButton = new JButton(I18n.tr(CANCEL), new ImageProvider("cancel").get());
    cancelButton.addActionListener(e -> dispose());
    cancelButton.requestFocus();
    cancelPanel.add(cancelButton);
    c.add(cancelPanel, BorderLayout.SOUTH);

    setModal(true);
    pack();
    setLocationRelativeTo(parent);
  }

  private JPanel getAnswerPanel() {
    final JButton filesBtn = new JButton(I18n.tr(IMPORT_METHOD_FILE));
    filesBtn.addActionListener(e -> {
      if (JFileChooser.APPROVE_OPTION == IMAGE_FILE_CHOOSER.showOpenDialog(this)) {
        for (File f : IMAGE_FILE_CHOOSER.getSelectedFiles()) {
          final MapView mv = MapillaryPlugin.getMapView();
          final LatLon center = mv == null ? new LatLon(0, 0) : mv.getProjection().eastNorth2latlon(mv.getCenter());
          try {
            final MapillarySequence seq = new MapillarySequence();
            seq.add(ImageImportUtil.readImagesFrom(f, center));
            MapillaryLayer.getInstance().getData().addAll(seq.getImages(), false);
            if (!MainApplication.getLayerManager().containsLayer(MapillaryLayer.getInstance())) {
              MainApplication.getLayerManager().addLayer(MapillaryLayer.getInstance());
            }
            MapillaryImportAction.recordChanges(seq.getImages());
          } catch (IOException e1) {
            JOptionPane.showMessageDialog(this, I18n.tr("Could not import the image ''{0}''!", f.getAbsolutePath(), I18n.tr("Import exception"), JOptionPane.ERROR_MESSAGE));
          }
        }
        MapillaryLayer.invalidateInstance();
        dispose();
      }
    });
    final JButton directoryBtn = new JButton(I18n.tr(IMPORT_METHOD_DIRECTORY));
    directoryBtn.addActionListener(e -> {
      if (JFileChooser.APPROVE_OPTION == DIRECTORY_CHOOSER.showOpenDialog(this)) {
        final MapView mv = MapillaryPlugin.getMapView();
        final LatLon center = mv == null ? new LatLon(0, 0) : mv.getProjection().eastNorth2latlon(mv.getCenter());
        try {
          final MapillarySequence seq = new MapillarySequence();
          seq.add(ImageImportUtil.readImagesFrom(DIRECTORY_CHOOSER.getSelectedFile(), center));
          MapillaryLayer.getInstance().getData().addAll(seq.getImages(), false);
          if (!MainApplication.getLayerManager().containsLayer(MapillaryLayer.getInstance())) {
            MainApplication.getLayerManager().addLayer(MapillaryLayer.getInstance());
          }
          MapillaryImportAction.recordChanges(seq.getImages());
        } catch (IOException e1) {
          JOptionPane.showMessageDialog(this, I18n.tr("Could not import the directory ''{0}''!", DIRECTORY_CHOOSER.getSelectedFile().getAbsolutePath(), I18n.tr("Import exception"), JOptionPane.ERROR_MESSAGE));
        }
        MapillaryLayer.invalidateInstance();
        dispose();
      }
    });
    final JButton imgLayerBtn = new JButton(I18n.tr(IMPORT_METHOD_LAYER));
    imgLayerBtn.addActionListener(ae -> {
      List<GeoImageLayer> layers = MainApplication.getLayerManager().getLayersOfType(GeoImageLayer.class);
      if (layers.isEmpty()) {
        JOptionPane.showMessageDialog(this, I18n.tr(NO_LAYERS_MESSAGE), I18n.tr(NO_LAYERS_MESSAGE), JOptionPane.WARNING_MESSAGE);
      } else {
        ChooseGeoImageLayersDialog dia = new ChooseGeoImageLayersDialog(this, layers);
        dia.setVisible(true);
        dia.addWindowListener(new WindowAdapter() {
          @Override
          public void windowClosed(WindowEvent we) {
            dispose();
          }
        });
      }
    });


    final JPanel answerPanel = new JPanel(new GridLayout(1, 3, 10, 10));
    answerPanel.add(filesBtn);
    answerPanel.add(directoryBtn);
    answerPanel.add(imgLayerBtn);
    answerPanel.setPreferredSize(new Dimension(answerPanel.getPreferredSize().width, answerPanel.getPreferredSize().height*3));
    return answerPanel;
  }
}
