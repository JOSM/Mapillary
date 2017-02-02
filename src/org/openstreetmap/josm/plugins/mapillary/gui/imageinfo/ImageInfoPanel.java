// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.datatransfer.StringSelection;
import java.util.Collection;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextPane;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.openstreetmap.josm.data.SelectionChangedListener;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.Tag;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.MapillaryButton;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.SelectableLabel;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.I18n;

public final class ImageInfoPanel extends ToggleDialog implements MapillaryDataListener, SelectionChangedListener {
  private static final long serialVersionUID = 1320443250226377651L;
  private static final Log L = LogFactory.getLog(ImageInfoPanel.class);
  private static ImageInfoPanel instance;

  private final JTextPane imgKeyValue;
  private final WebLinkAction imgLinkAction;
  private final ClipboardAction copyImgKeyAction;
  private final AddTagToPrimitiveAction addMapillaryTagAction;
  private final JTextPane seqKeyValue;

  private ImageInfoPanel() {
    super(
      I18n.tr("Image info"),
      "mapillary-info",
      I18n.tr("Displays detail information on the currently selected Mapillary image"),
      null,
      150
    );
    DataSet.addSelectionListener(this);

    imgKeyValue = new SelectableLabel();

    imgLinkAction = new WebLinkAction("View in browser", null);

    copyImgKeyAction = new ClipboardAction("Copy key", null);
    MapillaryButton copyButton = new MapillaryButton(copyImgKeyAction);
    copyImgKeyAction.setPopupParent(copyButton);

    addMapillaryTagAction = new AddTagToPrimitiveAction(I18n.tr("Add Mapillary tag"));

    JPanel imgButtons = new JPanel(new FlowLayout());
    imgButtons.add(new MapillaryButton(imgLinkAction));
    imgButtons.add(copyButton);
    imgButtons.add(new MapillaryButton(addMapillaryTagAction));
    seqKeyValue = new SelectableLabel();

    JPanel root = new JPanel(new GridBagLayout());
    GridBagConstraints gbc = new GridBagConstraints();
    gbc.ipadx = 10;
    gbc.ipady = 5;

    // Left column
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.anchor = GridBagConstraints.LINE_END;
    root.add(new JLabel(I18n.tr("Image key")), gbc);
    gbc.gridy++;
    root.add(new JLabel(I18n.tr("Image actions")), gbc);
    gbc.gridy++;
    root.add(new JLabel(I18n.tr("Sequence key")), gbc);

    // Right column
    gbc.weightx = 1;
    gbc.gridx++;
    gbc.gridy = 0;
    gbc.anchor = GridBagConstraints.LINE_START;
    root.add(imgKeyValue, gbc);
    gbc.gridy++;
    root.add(imgButtons, gbc);
    gbc.gridy++;
    root.add(seqKeyValue, gbc);

    createLayout(root, true, null);
    selectedImageChanged(null, null);
  }

  public static ImageInfoPanel getInstance() {
    synchronized (ImageInfoPanel.class) {
      if (instance == null) {
        instance = new ImageInfoPanel();
      }
      return instance;
    }
  }

  /* (non-Javadoc)
   * @see org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener#imagesAdded()
   */
  @Override
  public void imagesAdded() {
    // Method is not needed, but enforcesd by the interface MapillaryDataListener
  }

  /* (non-Javadoc)
   * @see org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener#selectedImageChanged(org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage, org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage)
   */
  @Override
  public void selectedImageChanged(final MapillaryAbstractImage oldImage, final MapillaryAbstractImage newImage) {
    L.info(String.format(
      "Selected Mapillary image changed from %s to %s.",
      oldImage instanceof MapillaryImage ? ((MapillaryImage) oldImage).getKey() : "‹none›",
      newImage instanceof MapillaryImage ? ((MapillaryImage) newImage).getKey() : "‹none›"
    ));
    imgKeyValue.setEnabled(newImage instanceof MapillaryImage);
    if (newImage instanceof MapillaryImage) {
      imgKeyValue.setText(((MapillaryImage) newImage).getKey());
      imgLinkAction.setURL(MapillaryURL.browseImageURL(((MapillaryImage) newImage).getKey()));
      copyImgKeyAction.setContents(new StringSelection(((MapillaryImage) newImage).getKey()));
      addMapillaryTagAction.setTag(new Tag("mapillary", ((MapillaryImage) newImage).getKey()));
    } else {
      imgKeyValue.setText('‹' + I18n.tr("image has no key") + '›');
      imgLinkAction.setURL(null);
      copyImgKeyAction.setContents(null);
      addMapillaryTagAction.setTag(null);
    }

    final boolean partOfSequence = newImage != null && newImage.getSequence() != null && newImage.getSequence().getKey() != null;
    seqKeyValue.setEnabled(partOfSequence);
    if (partOfSequence) {
      seqKeyValue.setText(newImage.getSequence().getKey());
    } else {
      seqKeyValue.setText('‹' + I18n.tr("sequence has no key") + '›');
    }
  }
  /* (non-Javadoc)
   * @see org.openstreetmap.josm.data.SelectionChangedListener#selectionChanged(java.util.Collection)
   */
  @Override
  public void selectionChanged(final Collection<? extends OsmPrimitive> sel) {
    L.info(String.format("Selection changed. %d primitives are selected.", sel.size()));
    synchronized (sel) {
      addMapillaryTagAction.setTarget(sel != null && sel.size() == 1 ? sel.iterator().next() : null);
    }
  }
}
