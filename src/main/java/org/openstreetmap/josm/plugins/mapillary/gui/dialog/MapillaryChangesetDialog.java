// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import static org.openstreetmap.josm.tools.I18n.tr;
import static org.openstreetmap.josm.tools.I18n.trn;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.gui.MapillaryPreferenceSetting;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.DoubleSpinner;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.MapillaryButton;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.SelectableLabel;
import org.openstreetmap.josm.plugins.mapillary.gui.changeset.DeleteImageAction;
import org.openstreetmap.josm.plugins.mapillary.gui.changeset.DeleteSequenceAction;
import org.openstreetmap.josm.plugins.mapillary.gui.changeset.ReviewImageAction;
import org.openstreetmap.josm.plugins.mapillary.gui.changeset.SubmitDeletionChangesetAction;
import org.openstreetmap.josm.plugins.mapillary.gui.changeset.SubmitLocationChangesetAction;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.history.MapillaryRecord;
import org.openstreetmap.josm.plugins.mapillary.history.commands.CommandTurn;
import org.openstreetmap.josm.plugins.mapillary.mode.EditMode;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryLoginListener;
import org.openstreetmap.josm.plugins.mapillary.oauth.MapillaryUser;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryChangesetListener;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Toggle dialog that shows you the current changeset and allows you to edit images.
 */
public final class MapillaryChangesetDialog extends ToggleDialog
  implements MapillaryChangesetListener, MapillaryDataListener, MapillaryLoginListener {

  private static MapillaryChangesetDialog instance;

  private final Container rootComponent = new JPanel(new GridBagLayout());
  private final SideButton submitLocationChangesetButton = new SideButton(new SubmitLocationChangesetAction(this));
  private final SideButton submitDeletionChangesetButton = new SideButton(new SubmitDeletionChangesetAction(this));
  private final MapillaryButton setSeqOffset = new MapillaryButton(new SequenceOffsetAction(), true);
  private final MapillaryButton setImgOffset = new MapillaryButton(new ImgOffsetAction(), true);
  private final MapillaryButton normalizeSequence = new MapillaryButton(new NormalizeAction(), true);
  private final DeleteImageAction deleteImgAction = new DeleteImageAction();
  private final DeleteSequenceAction deleteSeqAction = new DeleteSequenceAction();
  private final ReviewImageAction reviewAction = new ReviewImageAction();
  private final DoubleSpinner seqOffset = new DoubleSpinner(0, 360);
  private final DoubleSpinner imgOffset = new DoubleSpinner(0, 360);
  private final JTextPane imgLat = new SelectableLabel();
  private final JTextPane imgLon = new SelectableLabel();
  private final JLabel loginLabel = new JLabel();
  private final JLabel locationChangesetLabel = new JLabel();
  private final JLabel deletionChangesetLabel = new JLabel();
  private final JProgressBar uploadPendingProgress = new JProgressBar();
  private boolean destroyed;
  private boolean isUploadPending;

  /**
   * Destroys the unique instance of the class.
   */
  public static void destroyInstance() {
    MapillaryChangesetDialog.instance = null;
  }

  private MapillaryChangesetDialog() {
    super(tr("Current Mapillary changeset"), "mapillary-upload", tr("Open Mapillary changeset dialog"), Shortcut
      .registerShortcut(tr("Mapillary changeset"), tr("Open Mapillary changeset dialog"), KeyEvent.VK_9, Shortcut.NONE),
      300, false, MapillaryPreferenceSetting.class);
    MapillaryUser.addListener(this);

    JPanel imgAnglePanel = new JPanel();
    imgAnglePanel.add(imgOffset);
    imgAnglePanel.add(setImgOffset);

    JPanel seqAnglePanel = new JPanel();
    seqAnglePanel.add(seqOffset);
    seqAnglePanel.add(setSeqOffset);
    seqAnglePanel.add(normalizeSequence);

    JPanel deleteButtons = new JPanel();
    deleteButtons.add(new MapillaryButton(deleteSeqAction, true));
    deleteButtons.add(new MapillaryButton(deleteImgAction, true));
    deleteButtons.add(new MapillaryButton(reviewAction, true));

    GridBagConstraints gbc = new GridBagConstraints();
    gbc.insets = new Insets(0, 5, 0, 5);

    gbc.anchor = GridBagConstraints.NORTH;
    gbc.gridwidth = 2;
    gbc.gridheight = 2;
    rootComponent.add(loginLabel, gbc);

    // Left column
    gbc.gridx = 0;
    gbc.gridy = 2;
    gbc.gridheight = 1;
    gbc.gridwidth = 1;
    gbc.anchor = GridBagConstraints.LINE_END;
    rootComponent.add(new JLabel("Sequence Offset"), gbc);
    gbc.gridy++;
    rootComponent.add(new JLabel("Mark as Deleted"), gbc);
    gbc.gridy++;
    rootComponent.add(new JLabel("Latitiude"), gbc);
    gbc.gridy++;
    rootComponent.add(new JLabel("Longitude"), gbc);
    gbc.gridy++;
    rootComponent.add(new JLabel("Compass Angle"), gbc);
    gbc.gridy++;
    rootComponent.add(new JLabel("Changeset"), gbc);

    // Right column
    gbc.weightx = 1;
    gbc.gridx++;
    gbc.gridy = 2;
    gbc.anchor = GridBagConstraints.LINE_START;
    rootComponent.add(seqAnglePanel, gbc);
    gbc.gridy++;
    rootComponent.add(deleteButtons, gbc);
    gbc.gridy++;
    rootComponent.add(imgLat, gbc);
    gbc.gridy++;
    rootComponent.add(imgLon, gbc);
    gbc.gridy++;
    rootComponent.add(imgAnglePanel, gbc);
    gbc.gridy++;
    rootComponent.add(locationChangesetLabel, gbc);
    gbc.gridy++;
    rootComponent.add(deletionChangesetLabel, gbc);

    gbc.gridy++;
    gbc.gridx = 0;
    gbc.gridwidth = 2;
    gbc.anchor = GridBagConstraints.SOUTH;
    rootComponent.add(uploadPendingProgress, gbc);

    createLayout(rootComponent, true, Arrays.asList(submitLocationChangesetButton, submitDeletionChangesetButton));
    uploadPendingProgress.setIndeterminate(true);
    uploadPendingProgress.setString(tr("Submitting changeset to server…"));
    uploadPendingProgress.setStringPainted(true);
    uploadPendingProgress.setVisible(isUploadPending);

    setUploadPending(false);
    selectedImageChanged(null, null);
    updateChangesetInfo();
    onLogout();
  }

  /**
   * Returns the unique instance of the class.
   *
   * @return The unique instance of the class.
   */
  public static synchronized MapillaryChangesetDialog getInstance() {
    if (instance == null) {
      instance = new MapillaryChangesetDialog();
    }
    return instance;
  }

  public void setUploadPending(final boolean isUploadPending) {
    this.isUploadPending = isUploadPending;
    uploadPendingProgress.setVisible(isUploadPending);
    updateChangesetInfo();
    rootComponent.revalidate();
    rootComponent.repaint();
  }

  @Override
  public void changesetChanged() {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(this::updateChangesetInfo);
    } else {
      updateChangesetInfo();
    }
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

  @Override
  public void imagesAdded() {
    // Do nothing.
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
    if (newImage != null) {
      imgLat.setText(Double.toString(newImage.getMovingLatLon().lat()));
      imgLon.setText(Double.toString(newImage.getMovingLatLon().lon()));
      imgOffset.setValue(newImage.getMovingCa());
      seqOffset.setValue(0.0);
      deleteImgAction.setImage(newImage instanceof MapillaryImage ? (MapillaryImage) newImage : null);
      reviewAction.setImage(newImage);
      deleteSeqAction.setSequence(newImage.getSequence());
      if (MapillaryLayer.getInstance().mode instanceof EditMode) {
        imgOffset.setEnabled(true);
        setImgOffset.setEnabled(true);
        seqOffset.setEnabled(true);
        setSeqOffset.setEnabled(true);
        normalizeSequence.setEnabled(true);
      }
    } else {
      imgLat.setText(tr("No Image Selected"));
      imgLon.setText(tr("No Image Selected"));
      imgOffset.setEnabled(false);
      imgOffset.setValue(0.0);
      seqOffset.setEnabled(false);
      seqOffset.setValue(0.0);
      setImgOffset.setEnabled(false);
      setSeqOffset.setEnabled(false);
      normalizeSequence.setEnabled(false);
      deleteSeqAction.setSequence(null);
      deleteImgAction.setImage(null);
      reviewAction.setImage(null);
    }
  }

  public void updateChangesetInfo() {
    int locationChanges = MapillaryLayer.hasInstance() ? MapillaryLayer.getInstance().getLocationChangeset().size() : 0;
    int deletionChanges = MapillaryLayer.hasInstance() ? MapillaryLayer.getInstance().getDeletionChangeset().size() : 0;
    submitLocationChangesetButton.setEnabled(!isUploadPending && locationChanges != 0);
    submitDeletionChangesetButton.setEnabled(!isUploadPending && deletionChanges != 0);
    locationChangesetLabel.setText(trn("{0} Image changed", "{0} Images changed", locationChanges, locationChanges));
    deletionChangesetLabel.setText(trn("{0} Image deleted", "{0} Images deleted", deletionChanges, deletionChanges));
  }

  @Override
  public void onLogin(String username) {
    loginLabel.setText(tr("You are logged in as ''{0}''.", username));
  }

  @Override
  public void onLogout() {
    loginLabel.setText(tr("You are currently not logged in."));
  }

  public static class SequenceOffsetAction extends JosmAction {

    public SequenceOffsetAction() {
      super(tr("Set Offset"), null, tr("Offset every angle in the sequence"), null, false);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      double turnCa = getInstance().seqOffset.getDouble();
      if (turnCa != 0.0 && turnCa != 360.0) {
        MapillaryAbstractImage image = MapillaryLayer.getInstance().getData().getSelectedImage();
        if (image.getSequence() == null) {
          return;
        }
        List<MapillaryAbstractImage> images = image.getSequence().getImages();
        images.forEach(img -> {
          img.turn(turnCa);
        });
        double from = MapillaryLayer.getInstance().getData().getSelectedImage().getTempCa();
        double to = MapillaryLayer.getInstance().getData().getSelectedImage().getMovingCa();
        MapillaryRecord.getInstance()
          .addCommand(new CommandTurn(images.stream().collect(Collectors.toSet()), to - from));
        images.forEach(MapillaryAbstractImage::stopMoving);
        MapillaryLayer.invalidateInstance();
      }
    }

  }

  public static class ImgOffsetAction extends JosmAction {

    public ImgOffsetAction() {
      super(tr("Set"), null, tr("Set compass angle"), null, false);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      double ca = getInstance().imgOffset.getDouble();
      MapillaryAbstractImage img = MapillaryLayer.getInstance().getData().getSelectedImage();
      img.setMovingCa(ca);
      MapillaryRecord.getInstance()
        .addCommand(new CommandTurn(Collections.singleton(img), img.getMovingCa() - img.getTempCa()));
      img.stopMoving();
      MapillaryLayer.invalidateInstance();
    }
  }

  public static class NormalizeAction extends JosmAction {

    public NormalizeAction() {
      super(tr("Normalize Sequence"), null, tr("Set each image direction towards next image"), null, false);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      MapillaryAbstractImage image = MapillaryLayer.getInstance().getData().getSelectedImage();
      if (image.getSequence() == null) {
        return;
      }
      image.getSequence().getImages().forEach(img -> {
        if (img.next() != null) {
          img.setMovingCa((Math.toDegrees(Math.atan2(img.next().getMovingLatLon().getX() - img.getMovingLatLon().getX(),
            img.next().getMovingLatLon().getY() - img.getMovingLatLon().getY())) + 360) % 360);
          MapillaryRecord.getInstance()
            .addCommand(new CommandTurn(Collections.singleton(img), img.getMovingCa() - img.getTempCa()));
          img.stopMoving();
        }
      });
      MapillaryLayer.invalidateInstance();
    }
  }
}
