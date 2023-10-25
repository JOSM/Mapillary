// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.datatransfer.StringSelection;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;

import jakarta.annotation.Nullable;
import org.openstreetmap.josm.data.Version;
import org.openstreetmap.josm.data.osm.DataSelectionListener;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.Tag;
import org.openstreetmap.josm.data.osm.event.IDataSelectionListener;
import org.openstreetmap.josm.data.preferences.AbstractProperty.ValueChangeListener;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.data.vector.VectorRelation;
import org.openstreetmap.josm.data.vector.VectorWay;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.gui.widgets.HtmlPanel;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.VectorDataSelectionListener;
import org.openstreetmap.josm.plugins.mapillary.gui.boilerplate.MapillaryButton;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.widget.DisableShortcutsOnFocusGainedJSpinner;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.OffsetUtils;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Shortcut;
import org.openstreetmap.josm.tools.Utils;

/**
 * A panel to show image specific information
 */
public final class ImageInfoPanel extends ToggleDialog implements DataSelectionListener, VectorDataSelectionListener {
    private static final long serialVersionUID = 1320443250226377651L;
    private static ImageInfoPanel instance;
    private static final ImageIcon EMPTY_USER_AVATAR = new ImageIcon(
        new BufferedImage(32, 32, BufferedImage.TYPE_INT_ARGB));

    private final JLabel numDetectionsLabel;
    private final JCheckBox showDetectionsCheck;
    private final JLabel usernameLabel;
    private final HtmlPanel imgKeyValue;
    private final WebLinkAction imgLinkAction;
    private final ClipboardAction copyImgUrlAction;
    private final ClipboardAction copyImgKeyAction;
    private final AddTagToPrimitiveAction addMapillaryTagAction;
    private final HtmlPanel seqKeyValue;
    private final SpinnerNumberModel offsetModel;

    private ValueChangeListener<Boolean> imageLinkChangeListener;
    private boolean destroyed;

    private ImageInfoPanel() {
        super(tr("Image info"), "mapillary-info",
            tr("Displays detail information on the currently selected Mapillary image"), Shortcut.registerShortcut(
                "mapillary:imageinfo", tr("Image info dialog"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
            150);
        MainApplication.getLayerManager().addAndFireActiveLayerChangeListener(event -> {
            try {
                Optional.ofNullable(event.getPreviousDataSet()).ifPresent(it -> it.removeSelectionListener(this));
            } catch (IllegalArgumentException e) {
                // The selection listener was not registered
                Logging.trace(e);
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
        MapillaryProperties.SHOW_DETECTED_SIGNS.addListener(valueChange -> GuiHelper
            .runInEDT(() -> showDetectionsCheck.setSelected(MapillaryProperties.SHOW_DETECTED_SIGNS.get())));

        usernameLabel = new JLabel();
        usernameLabel.setFont(usernameLabel.getFont().deriveFont(Font.PLAIN));

        imgKeyValue = new HtmlPanel();

        imgLinkAction = new WebLinkAction(tr("View in browser"), null);

        copyImgUrlAction = new ClipboardAction(tr("Copy URL"), tr("Copied URL to clipboard …"), null);
        final MapillaryButton copyUrlButton = new MapillaryButton(copyImgUrlAction, true);
        copyImgUrlAction.setPopupParent(copyUrlButton);

        copyImgKeyAction = new ClipboardAction(tr("Copy key"), tr("Copied key to clipboard …"), null);
        final MapillaryButton copyKeyButton = new MapillaryButton(copyImgKeyAction, true);
        copyImgKeyAction.setPopupParent(copyKeyButton);

        addMapillaryTagAction = new AddTagToPrimitiveAction(tr("Add Mapillary tag"));

        JPanel imgKey = new JPanel();
        imgKey.add(imgKeyValue);
        imgKey.add(copyKeyButton);
        JPanel imgButtons = new JPanel(new GridBagLayout());
        imgButtons.add(new MapillaryButton(imgLinkAction, true), GBC.eol());
        imgButtons.add(copyUrlButton, GBC.eol());
        imgButtons.add(new MapillaryButton(addMapillaryTagAction, true), GBC.eol());
        seqKeyValue = new HtmlPanel();

        JPanel offsetPanel = new JPanel();
        offsetModel = new SpinnerNumberModel(OffsetUtils.getOffset(null), -100, 100, 1);
        offsetModel.addChangeListener(l -> {
            OffsetUtils.setOffset(offsetModel.getNumber());
            if (MapillaryLayer.hasInstance()) {
                MapillaryLayer.getInstance().invalidate();
            }
        });
        offsetPanel.add(new DisableShortcutsOnFocusGainedJSpinner(offsetModel));

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
        root.add(new JLabel(tr("Organization")), gbc);
        gbc.gridy++;
        root.add(new JLabel(tr("Image actions")), gbc);
        gbc.gridy++;
        root.add(new JLabel(tr("Image key")), gbc);
        gbc.gridy++;
        root.add(new JLabel(tr("Sequence key")), gbc);
        gbc.gridy++;
        root.add(new JLabel(tr("Sequence offset")), gbc);

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
        gbc.gridy++;
        root.add(offsetPanel, gbc);

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

    @Override
    protected void stateChanged() {
        super.stateChanged();
        if (isDialogShowing()) { // If the user opens the dialog once, no longer show the help message
            MapillaryProperties.IMAGEINFO_HELP_COUNTDOWN.put(0);
        }
    }

    @Override
    public synchronized void selectionChanged(final DataSelectionListener.SelectionChangeEvent event) {
        final Collection<OsmPrimitive> sel = event.getSelection();
        Logging.debug(String.format("Selection changed. %d primitives are selected.", sel == null ? 0 : sel.size()));
        addMapillaryTagAction.setTarget(sel != null && sel.size() == 1 ? sel.iterator().next() : null);
    }

    @Override
    public synchronized void selectionChanged(
        final IDataSelectionListener.SelectionChangeEvent<VectorPrimitive, VectorNode, VectorWay, VectorRelation, VectorDataSet> selectionChanged) {
        /*
         * This predicate fixes JOSM #21049. Mapillary does not use unique primitive ids in vector tiles, so we have to
         * keep
         * track of what is selected through the image key, which is done in the MapillaryLayer instance.
         */
        final Predicate<INode> isSelected;
        if (MapillaryLayer.hasInstance()) {
            isSelected = node -> MapillaryLayer.getInstance().getData().getSelected().contains(node);
        } else {
            isSelected = node -> true;
        }
        final INode oldImage = Utils.filteredCollection(selectionChanged.getOldSelection(), VectorNode.class).stream()
            .filter(MapillaryImageUtils::isImage).findFirst().orElse(null);
        final INode newImage = Utils.filteredCollection(selectionChanged.getSelection(), VectorNode.class).stream()
            .filter(MapillaryImageUtils::isImage).filter(isSelected).findFirst().orElse(null);
        selectedImageChanged(oldImage, newImage);
    }

    private void selectedImageChanged(@Nullable INode oldImage, @Nullable INode newImage) {
        final Predicate<INode> hasKey = node -> node != null && MapillaryImageUtils.getKey(node) != 0;
        Logging.debug(String.format("Selected Mapillary image changed from %s to %s.",
            hasKey.test(oldImage) ? MapillaryImageUtils.getKey(oldImage) : "‹none›",
            hasKey.test(newImage) ? MapillaryImageUtils.getKey(newImage) : "‹none›"));

        imgKeyValue.setEnabled(newImage != null);
        final String newImageKey = newImage != null ? Long.toString(newImage.getId()) : null;
        if (newImageKey != null) {
            final boolean blur = Boolean.TRUE.equals(MapillaryProperties.IMAGE_LINK_TO_BLUR_EDITOR.get());
            final URI newImageUrl = blur ? MapillaryConfig.getUrls().blurEditImage(newImageKey)
                : MapillaryConfig.getUrls().browseImage(newImageKey);

            offsetModel.setValue(OffsetUtils.getOffset(newImage));
            if (blur) {
                imageLinkChangeListener = b -> imgLinkAction.setURI(newImageUrl);
            } else {
                try {
                    final URI newImageUrlWithLocation = new URI(newImageUrl.getScheme(), newImageUrl.getAuthority(),
                        newImageUrl.getPath(),
                        newImageUrl.getQuery() + "&z=18&lat=" + newImage.lat() + "&lng=" + newImage.lon(),
                        newImageUrl.getFragment());
                    imageLinkChangeListener = b -> imgLinkAction.setURI(newImageUrlWithLocation);
                } catch (URISyntaxException e) {
                    throw new JosmRuntimeException(e);
                }
            }
            imageLinkChangeListener.valueChanged(null);
            MapillaryProperties.IMAGE_LINK_TO_BLUR_EDITOR.addListener(imageLinkChangeListener);
            copyImgUrlAction.setContents(new StringSelection(newImageUrl.toString()));

            imgKeyValue.setText(newImageKey);

            // First, set the detections number that we currently have
            List<ImageDetection<?>> detections = ImageDetection.getDetections(MapillaryImageUtils.getKey(newImage));
            numDetectionsLabel
                .setText(tr("{0} detections", detections.stream().filter(ImageDetection::isTrafficSign).count()));
            // Then, set the detections number that we get
            ImageDetection
                .getDetectionsLaterOptional(MapillaryImageUtils.getKey(newImage),
                    (key, detectionList) -> GuiHelper.runInEDT(() -> numDetectionsLabel.setText(
                        tr("{0} detections", detectionList.stream().filter(ImageDetection::isTrafficSign).count()))),
                    1000);
            copyImgKeyAction.setContents(new StringSelection(newImageKey));
            addMapillaryTagAction.setTag(new Tag("mapillary", newImageKey));
        } else {
            if (imageLinkChangeListener != null) {
                MapillaryProperties.IMAGE_LINK_TO_BLUR_EDITOR.removeListener(imageLinkChangeListener);
                imageLinkChangeListener = null;
            }
            this.offsetModel.setValue(OffsetUtils.getOffset(null));
            imgLinkAction.setURI(null);
            copyImgUrlAction.setContents(null);

            imgKeyValue.setText('‹' + tr("image has no key") + '›');
            copyImgKeyAction.setContents(null);
            addMapillaryTagAction.setTag(null);
        }

        final OrganizationRecord organizationRecord = MapillaryImageUtils.getOrganization(newImage);
        usernameLabel.setEnabled(!OrganizationRecord.NULL_RECORD.equals(organizationRecord));
        if (usernameLabel.isEnabled()) {
            usernameLabel.setText(organizationRecord.getNiceName());
            usernameLabel.setIcon(
                new ImageIcon(organizationRecord.getAvatar().getImage().getScaledInstance(32, 32, Image.SCALE_SMOOTH)));
        } else {
            usernameLabel.setText("‹" + tr("unknown organization") + "›");
            usernameLabel.setIcon(EMPTY_USER_AVATAR);
        }

        final boolean partOfSequence = MapillaryImageUtils.getSequenceKey(newImage) != null;
        seqKeyValue.setEnabled(partOfSequence);
        if (partOfSequence) {
            seqKeyValue.setText(MapillaryImageUtils.getSequenceKey(newImage));
        } else {
            seqKeyValue.setText('‹' + tr("sequence has no key") + '›');
        }
    }

    @Override
    public void destroy() {
        if (!destroyed) {
            super.destroy();
            if (MainApplication.getMap() != null && Version.getInstance().getVersion() < 18686)
                MainApplication.getMap().removeToggleDialog(this);
            destroyed = true;
        }
        destroyInstance();
    }
}
