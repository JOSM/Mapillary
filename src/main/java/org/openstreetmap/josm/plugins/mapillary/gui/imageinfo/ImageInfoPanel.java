// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.net.URL;
import java.util.Collection;
import java.util.Optional;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextPane;

import org.openstreetmap.josm.data.osm.DataSelectionListener;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.Tag;
import org.openstreetmap.josm.data.preferences.AbstractProperty.ValueChangeListener;
import org.openstreetmap.josm.gui.ExtendedDialog;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.gui.ImageColorPicker;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.MapillaryButton;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.SelectableLabel;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryURL;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Shortcut;

public final class ImageInfoPanel extends ToggleDialog implements MapillaryDataListener, DataSelectionListener {
  private static final long serialVersionUID = 1320443250226377651L;
  private static ImageInfoPanel instance;
  private static final ImageIcon EMPTY_USER_AVATAR = new ImageIcon(
    new BufferedImage(32, 32, BufferedImage.TYPE_INT_ARGB));

  private final JLabel numDetectionsLabel;
  private final JCheckBox showDetectionsCheck;
  private final JLabel usernameLabel;
  private final JTextPane imgKeyValue;
  private final WebLinkAction imgLinkAction;
  private final ClipboardAction copyImgUrlAction;
  private final ClipboardAction copyImgKeyAction;
  private final AddTagToPrimitiveAction addMapillaryTagAction;
  private final JTextPane seqKeyValue;
  private final MapillaryButton colorPickerButton;

  private ValueChangeListener<Boolean> imageLinkChangeListener;
  private boolean destroyed;

  private ImageInfoPanel() {
    super(tr("Image info"), "mapillary-info",
      tr("Displays detail information on the currently selected Mapillary image"),
      Shortcut.registerShortcut("mapillary:imageinfo", tr("Image info dialog"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
      150);
    MainApplication.getLayerManager().addAndFireActiveLayerChangeListener(event -> {
      try {
        Optional.ofNullable(event.getPreviousDataSet()).ifPresent(it -> it.removeSelectionListener(this));
      } catch (IllegalArgumentException e) {
        // The selection listener was not registered
      }
      Optional.ofNullable(MainApplication.getLayerManager().getActiveDataSet())
        .ifPresent(it -> it.addSelectionListener(this));
    });

    numDetectionsLabel = new JLabel();
    numDetectionsLabel.setFont(numDetectionsLabel.getFont().deriveFont(Font.PLAIN));

    showDetectionsCheck = new JCheckBox(tr("Show detections on top of image"));
    showDetectionsCheck.setSelected(MapillaryProperties.SHOW_DETECTED_SIGNS.get());
    showDetectionsCheck
      .addActionListener(action -> MapillaryProperties.SHOW_DETECTED_SIGNS.put(showDetectionsCheck.isSelected()));
    MapillaryProperties.SHOW_DETECTED_SIGNS
      .addListener(valueChange -> showDetectionsCheck.setSelected(MapillaryProperties.SHOW_DETECTED_SIGNS.get()));

    usernameLabel = new JLabel();
    usernameLabel.setFont(usernameLabel.getFont().deriveFont(Font.PLAIN));

    imgKeyValue = new SelectableLabel();

    imgLinkAction = new WebLinkAction(tr("View in browser"), null);

    copyImgUrlAction = new ClipboardAction(tr("Copy URL"), tr("Copied URL to clipboard …"), null);
    final MapillaryButton copyUrlButton = new MapillaryButton(copyImgUrlAction, true);
    copyImgUrlAction.setPopupParent(copyUrlButton);

    copyImgKeyAction = new ClipboardAction(tr("Copy key"), tr("Copied key to clipboard …"), null);
    final MapillaryButton copyKeyButton = new MapillaryButton(copyImgKeyAction, true);
    copyImgKeyAction.setPopupParent(copyKeyButton);

    addMapillaryTagAction = new AddTagToPrimitiveAction(tr("Add Mapillary tag"));

    colorPickerButton = new MapillaryButton(new ColorChooserAction(), true);

    JPanel imgKey = new JPanel();
    imgKey.add(imgKeyValue);
    imgKey.add(copyKeyButton);
    JPanel imgButtons = new JPanel(new GridBagLayout());
    imgButtons.add(new MapillaryButton(imgLinkAction, true), GBC.eol());
    imgButtons.add(copyUrlButton, GBC.eol());
    imgButtons.add(new MapillaryButton(addMapillaryTagAction, true), GBC.eol());
    imgButtons.add(colorPickerButton, GBC.eol());
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
    root.add(new JLabel(tr("Image detections")), gbc);
    gbc.gridy += 2;
    gbc.gridheight = 1;
    root.add(new JLabel(tr("User")), gbc);
    gbc.gridy++;
    root.add(new JLabel(tr("Image actions")), gbc);
    gbc.gridy++;
    root.add(new JLabel(tr("Image key")), gbc);
    gbc.gridy++;
    root.add(new JLabel(tr("Sequence key")), gbc);

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

  /*
   * (non-Javadoc)
   * @see org.openstreetmap.josm.gui.dialogs.ToggleDialog#stateChanged()
   */
  @Override
  protected void stateChanged() {
    super.stateChanged();
    if (isDialogShowing()) { // If the user opens the dialog once, no longer show the help message
      MapillaryProperties.IMAGEINFO_HELP_COUNTDOWN.put(0);
    }
  }

  /*
   * (non-Javadoc)
   * @see org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener#imagesAdded()
   */
  @Override
  public void imagesAdded() {
    // Method is not needed, but enforcesd by the interface MapillaryDataListener
  }

  /*
   * (non-Javadoc)
   * @see
   * org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener#selectedImageChanged(org.openstreetmap.josm.plugins.
   * mapillary.MapillaryAbstractImage, org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage)
   */
  @Override
  public synchronized void selectedImageChanged(final MapillaryAbstractImage oldImage,
    final MapillaryAbstractImage newImage) {
    Logging.debug(String.format("Selected Mapillary image changed from %s to %s.",
      oldImage instanceof MapillaryImage ? ((MapillaryImage) oldImage).getKey() : "‹none›",
      newImage instanceof MapillaryImage ? ((MapillaryImage) newImage).getKey() : "‹none›"));

    numDetectionsLabel.setText(tr("{0} detections",
      newImage instanceof MapillaryImage ? ((MapillaryImage) newImage).getDetections().size() : 0));
    imgKeyValue.setEnabled(newImage instanceof MapillaryImage);
    final String newImageKey = newImage instanceof MapillaryImage ? ((MapillaryImage) newImage).getKey() : null;
    if (newImageKey != null) {
      final URL newImageUrl = MapillaryProperties.IMAGE_LINK_TO_BLUR_EDITOR.get()
        ? MapillaryURL.MainWebsite.blurEditImage(newImageKey)
        : MapillaryURL.MainWebsite.browseImage(newImageKey);

      imageLinkChangeListener = b -> imgLinkAction.setURL(newImageUrl);
      imageLinkChangeListener.valueChanged(null);
      MapillaryProperties.IMAGE_LINK_TO_BLUR_EDITOR.addListener(imageLinkChangeListener);
      copyImgUrlAction.setContents(new StringSelection(newImageUrl.toString()));

      imgKeyValue.setText(newImageKey);
      copyImgKeyAction.setContents(new StringSelection(newImageKey));
      addMapillaryTagAction.setTag(new Tag("mapillary", newImageKey));
    } else {
      if (imageLinkChangeListener != null) {
        MapillaryProperties.IMAGE_LINK_TO_BLUR_EDITOR.removeListener(imageLinkChangeListener);
        imageLinkChangeListener = null;
      }
      imgLinkAction.setURL(null);
      copyImgUrlAction.setContents(null);

      imgKeyValue.setText('‹' + tr("image has no key") + '›');
      copyImgKeyAction.setContents(null);
      addMapillaryTagAction.setTag(null);
    }

    final UserProfile user = newImage instanceof MapillaryImage ? ((MapillaryImage) newImage).getUser() : null;
    usernameLabel.setEnabled(user != null);
    if (user != null) {
      usernameLabel.setText(user.getUsername());
      usernameLabel.setIcon(new ImageIcon(user.getAvatar().getImage().getScaledInstance(32, 32, Image.SCALE_SMOOTH)));
    } else {
      usernameLabel.setText("‹" + tr("unknown user") + "›");
      usernameLabel.setIcon(EMPTY_USER_AVATAR);
    }

    final boolean partOfSequence = newImage != null && newImage.getSequence() != null
      && newImage.getSequence().getKey() != null;
    seqKeyValue.setEnabled(partOfSequence);
    if (partOfSequence) {
      seqKeyValue.setText(newImage.getSequence().getKey());
    } else {
      seqKeyValue.setText('‹' + tr("sequence has no key") + '›');
    }
    colorPickerButton.setEnabled(newImage != null);
  }

  /*
   * (non-Javadoc)
   * @see org.openstreetmap.josm.data.SelectionChangedListener#selectionChanged(java.util.Collection)
   */
  @Override
  public synchronized void selectionChanged(final SelectionChangeEvent event) {
    final Collection<OsmPrimitive> sel = event.getSelection();
    Logging.debug(String.format("Selection changed. %d primitives are selected.", sel == null ? 0 : sel.size()));
    addMapillaryTagAction.setTarget(sel != null && sel.size() == 1 ? sel.iterator().next() : null);
  }

  @Override
  public void destroy() {
    if (!destroyed) {
      super.destroy();
      if (MainApplication.getMap() != null)
        MainApplication.getMap().removeToggleDialog(this);
      destroyed = true;
    }
    destroyInstance();
  }

  private static class ColorChooserAction extends AbstractAction {

    private static final long serialVersionUID = 8706299665735930148L;

    ColorChooserAction() {
      super(tr("Pick Color"), ImageProvider.get("mapillary-eyedropper", ImageProvider.ImageSizes.SMALLICON));
    }

    @Override
    public void actionPerformed(ActionEvent arg0) {
      ExtendedDialog abc = new ExtendedDialog(MainApplication.getMainFrame(), tr("Color Picker"));
      abc.setContent(new ImageColorPicker());
      abc.showDialog();
    }
  }
}
