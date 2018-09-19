// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.datatransfer.StringSelection;
import java.awt.image.BufferedImage;
import java.util.Collection;
import java.util.Optional;

import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextPane;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import org.openstreetmap.josm.data.osm.DataSelectionListener;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.Tag;
import org.openstreetmap.josm.data.preferences.AbstractProperty.ValueChangeListener;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.MapillaryButton;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.SelectableLabel;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.I18n;

public final class ImageInfoPanel extends ToggleDialog implements MapillaryDataListener, DataSelectionListener {
  private static final long serialVersionUID = 1320443250226377651L;
  private static final Log L = LogFactory.getLog(ImageInfoPanel.class);
  private static ImageInfoPanel instance;
  private static final ImageIcon EMPTY_USER_AVATAR = new ImageIcon(new BufferedImage(32, 32, BufferedImage.TYPE_INT_ARGB));

  private final JLabel numDetectionsLabel;
  private final JCheckBox showDetectionsCheck;
  private final JLabel usernameLabel;
  private final JTextPane imgKeyValue;
  private final WebLinkAction imgLinkAction;
  private final ClipboardAction copyImgKeyAction;
  private final AddTagToPrimitiveAction addMapillaryTagAction;
  private final JTextPane seqKeyValue;

  private ValueChangeListener<Boolean> imageLinkChangeListener;

  private ImageInfoPanel() {
    super(
      I18n.tr("Image info"),
      "mapillary-info",
      I18n.tr("Displays detail information on the currently selected Mapillary image"),
      null,
      150
    );
    MainApplication.getLayerManager().addAndFireActiveLayerChangeListener(event -> {
      try {
        Optional.ofNullable(event.getPreviousDataSet())
            .ifPresent(it -> it.removeSelectionListener(this));
      } catch (IllegalArgumentException e) {
        // The selection listener was not registered
      }
      Optional.ofNullable(MainApplication.getLayerManager().getActiveDataSet())
          .ifPresent(it -> it.addSelectionListener(this));
    });

    numDetectionsLabel = new JLabel();
    numDetectionsLabel.setFont(numDetectionsLabel.getFont().deriveFont(Font.PLAIN));

    showDetectionsCheck = new JCheckBox(I18n.tr("Show detections on top of image"));
    showDetectionsCheck.setSelected(MapillaryProperties.SHOW_DETECTED_SIGNS.get());
    showDetectionsCheck.addActionListener(
      action -> MapillaryProperties.SHOW_DETECTED_SIGNS.put(showDetectionsCheck.isSelected())
    );
    MapillaryProperties.SHOW_DETECTED_SIGNS.addListener(
      valueChange -> showDetectionsCheck.setSelected(MapillaryProperties.SHOW_DETECTED_SIGNS.get())
    );

    usernameLabel = new JLabel();
    usernameLabel.setFont(usernameLabel.getFont().deriveFont(Font.PLAIN));

    imgKeyValue = new SelectableLabel();

    imgLinkAction = new WebLinkAction(I18n.tr("View in browser"), null);

    copyImgKeyAction = new ClipboardAction(I18n.tr("Copy key"), null);
    MapillaryButton copyButton = new MapillaryButton(copyImgKeyAction, true);
    copyImgKeyAction.setPopupParent(copyButton);

    addMapillaryTagAction = new AddTagToPrimitiveAction(I18n.tr("Add Mapillary tag"));

    JPanel imgKey = new JPanel();
    imgKey.add(imgKeyValue);
    imgKey.add(copyButton);
    JPanel imgButtons = new JPanel();
    imgButtons.add(new MapillaryButton(imgLinkAction, true));
    imgButtons.add(new MapillaryButton(addMapillaryTagAction, true));
    seqKeyValue = new SelectableLabel();

    JPanel root = new JPanel(new GridBagLayout());
    GridBagConstraints gbc = new GridBagConstraints();
    gbc.insets = new Insets(0, 5, 0, 5);

    // Left column
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.anchor = GridBagConstraints.LINE_END;
    gbc.gridwidth = 1;
    gbc.gridheight = 2;
    root.add(new JLabel(I18n.tr("Image detections")), gbc);
    gbc.gridy += 2;
    gbc.gridheight = 1;
    root.add(new JLabel(I18n.tr("User")), gbc);
    gbc.gridy++;
    root.add(new JLabel(I18n.tr("Image actions")), gbc);
    gbc.gridy++;
    root.add(new JLabel(I18n.tr("Image key")), gbc);
    gbc.gridy++;
    root.add(new JLabel(I18n.tr("Sequence key")), gbc);

    // Right column
    gbc.weightx = 1;
    gbc.gridx++;
    gbc.gridy = 0;
    gbc.anchor = GridBagConstraints.LINE_START;
    root.add(numDetectionsLabel, gbc);
    gbc.gridy++;
    root.add(showDetectionsCheck, gbc);
    gbc.gridy++;
    root.add(usernameLabel, gbc);
    gbc.gridy++;
    root.add(imgButtons, gbc);
    gbc.gridy++;
    root.add(imgKey, gbc);
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

  /**
   * Destroys the unique instance of the class.
   */
  public static synchronized void destroyInstance() {
    instance = null;
  }

  /* (non-Javadoc)
   * @see org.openstreetmap.josm.gui.dialogs.ToggleDialog#stateChanged()
   */
  @Override
  protected void stateChanged() {
    super.stateChanged();
    if (isDialogShowing()) { // If the user opens the dialog once, no longer show the help message
      MapillaryProperties.IMAGEINFO_HELP_COUNTDOWN.put(0);
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
  public synchronized void selectedImageChanged(final MapillaryAbstractImage oldImage, final MapillaryAbstractImage newImage) {
    L.debug(String.format(
      "Selected Mapillary image changed from %s to %s.",
      oldImage instanceof MapillaryImage ? ((MapillaryImage) oldImage).getKey() : "‹none›",
      newImage instanceof MapillaryImage ? ((MapillaryImage) newImage).getKey() : "‹none›"
    ));

    numDetectionsLabel.setText(I18n.tr("{0} detections", newImage instanceof MapillaryImage ? ((MapillaryImage) newImage).getDetections().size() : 0));
    imgKeyValue.setEnabled(newImage instanceof MapillaryImage);
    final String newImageKey = newImage instanceof MapillaryImage ? ((MapillaryImage) newImage).getKey(): null;
    if (newImageKey != null) {
      imageLinkChangeListener = b -> imgLinkAction.setURL(
        MapillaryProperties.IMAGE_LINK_TO_BLUR_EDITOR.get()
        ? MapillaryURL.MainWebsite.blurEditImage(newImageKey)
        : MapillaryURL.MainWebsite.browseImage(newImageKey)
      );
      imageLinkChangeListener.valueChanged(null);
      MapillaryProperties.IMAGE_LINK_TO_BLUR_EDITOR.addListener(imageLinkChangeListener);

      imgKeyValue.setText(newImageKey);
      copyImgKeyAction.setContents(new StringSelection(newImageKey));
      addMapillaryTagAction.setTag(new Tag("mapillary", newImageKey));
    } else {
      if (imageLinkChangeListener != null) {
        MapillaryProperties.IMAGE_LINK_TO_BLUR_EDITOR.removeListener(imageLinkChangeListener);
        imageLinkChangeListener = null;
      }
      imgLinkAction.setURL(null);

      imgKeyValue.setText('‹' + I18n.tr("image has no key") + '›');
      copyImgKeyAction.setContents(null);
      addMapillaryTagAction.setTag(null);
    }

    final UserProfile user = newImage instanceof MapillaryImage ? ((MapillaryImage) newImage).getUser() : null;
    usernameLabel.setEnabled(user != null);
    if (user != null) {
      usernameLabel.setText(user.getUsername());
      usernameLabel.setIcon(new ImageIcon(user.getAvatar().getImage().getScaledInstance(32, 32, Image.SCALE_SMOOTH)));
    } else {
      usernameLabel.setText("‹" + I18n.tr("unknown user") + "›");
      usernameLabel.setIcon(EMPTY_USER_AVATAR);
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
  public synchronized void selectionChanged(final SelectionChangeEvent event) {
    final Collection<OsmPrimitive> sel = event.getSelection();
    L.debug(String.format("Selection changed. %d primitives are selected.", sel == null ? 0 : sel.size()));
    addMapillaryTagAction.setTarget(sel != null && sel.size() == 1 ? sel.iterator().next() : null);
  }
}
