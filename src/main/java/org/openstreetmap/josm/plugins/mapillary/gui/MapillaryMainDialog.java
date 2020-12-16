// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.marktr;
import static org.openstreetmap.josm.tools.I18n.tr;
import static org.openstreetmap.josm.tools.I18n.trc;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.preferences.BooleanProperty;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.actions.SelectNextImageAction;
import org.openstreetmap.josm.plugins.mapillary.actions.WalkListener;
import org.openstreetmap.josm.plugins.mapillary.actions.WalkThread;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.data.image.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoHelpPopup;
import org.openstreetmap.josm.plugins.mapillary.gui.imageviewer.AbstractImageViewer;
import org.openstreetmap.josm.plugins.mapillary.gui.imageviewer.MapillaryImageViewer;
import org.openstreetmap.josm.plugins.mapillary.gui.imageviewer.PanoramicImageViewer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
import org.openstreetmap.josm.plugins.mapillary.utils.DetectionVerification;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * Toggle dialog that shows an image and some buttons.
 *
 * @author nokutu
 */
public final class MapillaryMainDialog extends ToggleDialog implements ICachedLoaderListener, MapillaryDataListener {

  private static final long serialVersionUID = 6856496736429480600L;

  private static final String BASE_TITLE = marktr("Mapillary image");
  private static final String MESSAGE_SEPARATOR = " — ";

  private static MapillaryMainDialog instance;

  private boolean destroyed;

  private transient MapillaryAbstractImage image;

  private final SideButton nextButton = new SideButton(SelectNextImageAction.NEXT_ACTION);
  private final SideButton previousButton = new SideButton(SelectNextImageAction.PREVIOUS_ACTION);
  /**
   * Button used to jump to the image following the red line
   */
  public final SideButton redButton = new SideButton(SelectNextImageAction.RED_ACTION);
  /**
   * Button used to jump to the image following the blue line
   */
  public final SideButton blueButton = new SideButton(SelectNextImageAction.BLUE_ACTION);

  private final SideButton playButton = new SideButton(new PlayAction());
  private final SideButton pauseButton = new SideButton(new PauseAction());
  private final SideButton stopButton = new SideButton(new StopAction());
  private final JPanel panel = new JPanel();

  private ImageInfoHelpPopup imageInfoHelp;

  /**
   * Buttons mode.
   *
   * @author nokutu
   */
  public enum MODE {
    /**
     * Standard mode to view pictures.
     */
    NORMAL,
    /**
     * Mode when in walk.
     */
    WALK,
    /**
     * Mode when in smart edit
     */
    SMART_EDIT
  }

  /**
   * Object containing the shown image and that handles zoom and drag
   */
  public AbstractImageViewer imageViewer;
  private final MapillaryImageViewer mapillaryViewer = new MapillaryImageViewer();
  private final PanoramicImageViewer panoramicViewer = new PanoramicImageViewer();

  private MapillaryCache imageCache;
  private MapillaryCache thumbnailCache;

  private final ShowDetectionOutlinesAction showDetectionOutlinesAction = new ShowDetectionOutlinesAction();
  private final ShowSignDetectionsAction showSignDetectionsAction = new ShowSignDetectionsAction();

  private VerifyRejectAction approveAction = new VerifyRejectAction(DetectionVerification.TYPE.APPROVE);
  private VerifyRejectAction rejectAction = new VerifyRejectAction(DetectionVerification.TYPE.REJECT);

  private MapillaryMainDialog() {
    super(tr(BASE_TITLE), "mapillary-main", tr("Open Mapillary window"),
      Shortcut.registerShortcut("mapillary:main", tr("Mapillary main dialog"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
      200, true, MapillaryPreferenceSetting.class);
    this.imageViewer = mapillaryViewer;

    this.blueButton.setForeground(Color.BLUE);
    this.redButton.setForeground(Color.RED);

    panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS));
    panel.add(imageViewer);
    // Ensure that selecting one enables the other.
    this.approveAction.setOther(this.rejectAction);
    this.rejectAction.setOther(this.approveAction);
    setMode(MODE.NORMAL);
  }

  private abstract static class JosmButtonAction extends JosmAction {
    private static final long serialVersionUID = -4009253801009731575L;
    JButton pbutton;

    public JosmButtonAction(String name, ImageProvider icon, String tooltip, Shortcut shortcut,
      boolean registerInToolbar, String toolbarId, boolean installAdapters) {
      super(name, icon, tooltip, shortcut, registerInToolbar, toolbarId, installAdapters);
    }

    void setButton(JButton button) {
      this.pbutton = button;
      setBackground();
    }

    protected void setBackground() {
      if (pbutton == null)
        return;
      if (Boolean.TRUE.equals(getProperty().get())) {
        this.pbutton.setForeground(Color.GREEN);
        this.pbutton.setBackground(Color.GREEN);
      } else {
        this.pbutton.setForeground(Color.RED);
        this.pbutton.setBackground(Color.RED);
      }
      this.pbutton.repaint();
    }

    protected abstract BooleanProperty getProperty();
  }

  private static class ShowDetectionOutlinesAction extends JosmButtonAction {
    private static final long serialVersionUID = 1943388917595255950L;

    ShowDetectionOutlinesAction() {
      super(null, new ImageProvider("mapillary_sprite_source/package_objects", "object--traffic-light--other"),
        tr("Toggle detection outlines"), Shortcut.registerShortcut("mapillary:showdetections",
          tr("Mapillary: toggle detections"), KeyEvent.VK_UNDEFINED, Shortcut.NONE),
        false, null, false);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      MapillaryProperties.SHOW_DETECTION_OUTLINES.put(!MapillaryProperties.SHOW_DETECTION_OUTLINES.get());
      this.setBackground();
    }

    @Override
    protected BooleanProperty getProperty() {
      return MapillaryProperties.SHOW_DETECTION_OUTLINES;
    }
  }

  private static class VerifyRejectAction extends JosmButtonAction {
    private final DetectionVerification.TYPE type;
    private VerifyRejectAction other;
    private boolean listenerAdded;
    private final transient MapillaryDataListener mapillaryDataListener = new MapillaryDataListener() {

      @Override
      public void imagesAdded() {
        // Do nothing
      }

      @Override
      public void selectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
        updateEnabledState();
      }
    };

    VerifyRejectAction(DetectionVerification.TYPE type) {
      super(tr("Vote: {0}", type.toString()), getIcon(type),
        tr("Tell Mapillary that you {0} this detection", type.toString()),
        Shortcut.registerShortcut("mapillary:vote:" + type.toString(), tr("Mapillary: Vote {0}", type.toString()),
          KeyEvent.VK_UNDEFINED, Shortcut.NONE),
        false, null, false);
      this.type = type;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      if (!this.listenerAdded) {
        MapillaryLayer.getInstance().getData().addListener(mapillaryDataListener);
        this.listenerAdded = true;
      }
      ImageDetection<?> detection = getDetection();
      if (detection != null) {
        MapillaryAbstractImage image = MapillaryMainDialog.getInstance().getImage();
        if (image instanceof MapillaryImage && detection.getImageKey().equals(((MapillaryImage) image).getKey())
          && DetectionVerification.vote(detection, this.type)) {
          detection.setApprovalType(this.type);
          this.updateEnabledState();
          if (this.other != null) {
            this.other.updateEnabledState();
          }
        }
      }
    }

    @Override
    public void updateEnabledState() {
      MapillaryAbstractImage image = MapillaryMainDialog.getInstance().getImage();
      ImageDetection<?> detection = getDetection();
      if (!(image instanceof MapillaryImage) || detection == null) {
        this.setEnabled(false);
      } else {
        if (this.type != detection.getApprovalType())
          this.setEnabled(((MapillaryImage) image).getKey().equals(detection.getImageKey()));
        else
          this.setEnabled(false);
      }
    }

    /**
     * @return The single detection to verify/reject
     */
    private static ImageDetection<?> getDetection() {
      Collection<ImageDetection<?>> detections = MapillaryMainDialog.getInstance().imageViewer.getShownDetections();
      if (detections.size() == 1) {
        return detections.iterator().next();
      }
      return null;
    }

    /**
     * Set the other verify action
     *
     * @param other The other verify action
     */
    public void setOther(VerifyRejectAction other) {
      this.other = other;
    }

    @Override
    protected BooleanProperty getProperty() {
      return null;
    }

    private static ImageProvider getIcon(DetectionVerification.TYPE type) {
      if (type == DetectionVerification.TYPE.APPROVE) {
        return new ImageProvider("apply");
      } else if (type == DetectionVerification.TYPE.REJECT) {
        return new ImageProvider("cancel");
      }
      throw new IllegalArgumentException("type " + type.name() + " is not recognized");
    }
  }

  private static class ShowSignDetectionsAction extends JosmButtonAction {
    private static final long serialVersionUID = -3743322064323002656L;

    ShowSignDetectionsAction() {
      super(null,
        new ImageProvider("mapillary_sprite_source/package_signs", "regulatory--go-straight-or-turn-left--g2"),
        tr("Toggle sign detection outlines"), Shortcut.registerShortcut("mapillary:showsigndetections",
          tr("Mapillary: toggle sign detections"), KeyEvent.VK_UNDEFINED, Shortcut.NONE),
        false, null, false);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      MapillaryProperties.SHOW_DETECTED_SIGNS.put(!MapillaryProperties.SHOW_DETECTED_SIGNS.get());
      this.setBackground();
    }

    @Override
    protected BooleanProperty getProperty() {
      return MapillaryProperties.SHOW_DETECTED_SIGNS;
    }
  }

  /**
   * Returns the unique instance of the class.
   *
   * @return The unique instance of the class.
   */
  public static synchronized MapillaryMainDialog getInstance() {
    if (instance == null)
      instance = new MapillaryMainDialog();
    return instance;
  }

  /**
   * @return true, iff the singleton instance is present
   */
  public static boolean hasInstance() {
    return instance != null;
  }

  public synchronized void setImageInfoHelp(ImageInfoHelpPopup popup) {
    this.imageInfoHelp = popup;
  }

  /**
   * Sets a new mode for the dialog.
   *
   * @param mode
   *        The mode to be set. Must not be {@code null}.
   */
  public void setMode(MODE mode) {
    Dimension buttonDim = new Dimension(52, 34);
    SideButton toggleSigns = new SideButton(showSignDetectionsAction);
    showSignDetectionsAction.setButton(toggleSigns);
    toggleSigns.setPreferredSize(buttonDim);
    // Mac OS X won't show background colors if buttons aren't opaque.
    toggleSigns.setOpaque(true);
    SideButton toggleDetections = null;
    toggleDetections = new SideButton(showDetectionOutlinesAction);
    showDetectionOutlinesAction.setButton(toggleDetections);
    toggleDetections.setPreferredSize(buttonDim);
    toggleDetections.setOpaque(true);

    Stream<SideButton> buttons;
    switch (mode) {
    case WALK:
      buttons = Stream.of(toggleSigns, toggleDetections, playButton, pauseButton, stopButton);
      break;
    case SMART_EDIT:
      SideButton verify = new SideButton(approveAction);
      SideButton reject = new SideButton(rejectAction);
      buttons = Stream.of(blueButton, previousButton, verify, reject, nextButton, redButton);
      break;
    case NORMAL:
    default:
      buttons = Stream.of(blueButton, previousButton, toggleSigns, toggleDetections, nextButton, redButton);
      break;
    }
    createLayout(this.panel, buttons.filter(Objects::nonNull).collect(Collectors.toList()));
    disableAllButtons();
    if (Stream.of(MODE.WALK, MODE.SMART_EDIT).anyMatch(mode::equals)) {
      updateImage();
    }
    revalidate();
    repaint();
  }

  /**
   * Destroys the unique instance of the class. You should prefer to call the destroy method on the actual instance.
   */
  public static synchronized void destroyInstance() {
    instance = null;
  }

  /**
   * Downloads the full quality picture of the selected MapillaryImage and sets in the MapillaryImageDisplay object.
   */
  public synchronized void updateImage() {
    updateImage(true);
  }

  /**
   * Downloads the picture of the selected MapillaryImage and sets in the MapillaryImageDisplay object.
   *
   * @param fullQuality
   *        If the full quality picture must be downloaded or just the thumbnail.
   */
  public synchronized void updateImage(boolean fullQuality) {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(this::updateImage);
    } else {
      if (!MapillaryLayer.hasInstance()) {
        return;
      }
      if (this.image == null) {
        setDisplayImage(null, null, false);
        setTitle(tr(BASE_TITLE));
        disableAllButtons();
        return;
      }

      if (imageInfoHelp != null && MapillaryProperties.IMAGEINFO_HELP_COUNTDOWN.get() > 0
        && imageInfoHelp.showPopup()) {
        // Count down the number of times the popup will be displayed
        MapillaryProperties.IMAGEINFO_HELP_COUNTDOWN.put(MapillaryProperties.IMAGEINFO_HELP_COUNTDOWN.get() - 1);
      }

      // Enables/disables next/previous buttons
      this.nextButton.setEnabled(false);
      this.previousButton.setEnabled(false);
      if (this.image.getSequence() != null) {
        MapillaryAbstractImage tempImage = this.image;
        while (tempImage.next() != null) {
          tempImage = tempImage.next();
          if (tempImage.isVisible()) {
            this.nextButton.setEnabled(true);
            break;
          }
        }
      }
      if (this.image.getSequence() != null) {
        MapillaryAbstractImage tempImage = this.image;
        while (tempImage.previous() != null) {
          tempImage = tempImage.previous();
          if (tempImage.isVisible()) {
            this.previousButton.setEnabled(true);
            break;
          }
        }
      }
      if (this.image instanceof MapillaryImage) {
        MapillaryImage mapillaryImage = (MapillaryImage) this.image;
        // Downloads the thumbnail.
        if (this.thumbnailCache != null)
          this.thumbnailCache.cancelOutstandingTasks();
        this.thumbnailCache = new MapillaryCache(mapillaryImage.getKey(), MapillaryCache.Type.THUMBNAIL);
        try {
          if (this.thumbnailCache.get() == null)
            this.thumbnailCache.submit(this, false);
        } catch (IOException e) {
          Logging.error(e);
        }

        // Downloads the full resolution image.
        if (fullQuality || new MapillaryCache(mapillaryImage.getKey(), MapillaryCache.Type.FULL_IMAGE).get() != null) {
          if (this.imageCache != null)
            this.imageCache.cancelOutstandingTasks();
          this.imageCache = new MapillaryCache(mapillaryImage.getKey(), MapillaryCache.Type.FULL_IMAGE);
          try {
            if (this.imageCache.get() == null)
              this.imageCache.submit(this, false);
          } catch (IOException e) {
            Logging.error(e);
          }
        }
        try {
          if (this.imageCache != null && this.imageCache.get() != null) {
            setDisplayImage(imageCache.get().getImage(), ((MapillaryImage) image).getDetections(), image.isPanorama());
          } else if (this.thumbnailCache != null && this.thumbnailCache.get() != null) {
            setDisplayImage(thumbnailCache.get().getImage(), ((MapillaryImage) image).getDetections(),
              image.isPanorama());
          } else {
            this.imageViewer.paintLoadingImage();
          }
        } catch (IOException e) {
          Logging.error(e);
          setDisplayImage(null, null, false);
        }
      } else if (this.image instanceof MapillaryImportedImage) {
        final MapillaryImportedImage importedImage = (MapillaryImportedImage) this.image;
        try {
          setDisplayImage(importedImage.getImage(), null, importedImage.isPanorama());
        } catch (IOException e) {
          Logging.error(e);
        }
      }
      updateTitle();
    }

  }

  /**
   * Disables all the buttons in the dialog
   */
  private void disableAllButtons() {
    this.nextButton.setEnabled(false);
    this.previousButton.setEnabled(false);
    this.blueButton.setEnabled(false);
    this.redButton.setEnabled(false);
  }

  /**
   * Sets a new MapillaryImage to be shown.
   *
   * @param image
   *        The image to be shown.
   */
  public synchronized void setImage(MapillaryAbstractImage image) {
    this.image = image;
    if (this.isVisible() && MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().setImageViewed(this.image);
    }
  }

  public void setDisplayImage(BufferedImage image, Collection<ImageDetection<?>> detections, Boolean pano) {
    if (image != null) {
      if (Boolean.TRUE.equals(pano)) {
        if (imageViewer instanceof MapillaryImageViewer) {
          panel.remove(imageViewer);
          imageViewer = panoramicViewer;
          panel.add(imageViewer);
          repaint();
        }
      } else {
        if (imageViewer instanceof PanoramicImageViewer) {
          panel.remove(imageViewer);
          imageViewer = mapillaryViewer;
          panel.add(imageViewer);
          repaint();
        }
      }
      imageViewer.setImage(image, detections);
    } else {
      imageViewer.setImage(null, null);
    }
  }

  /**
   * Updates the title of the dialog.
   */
  public synchronized void updateTitle() {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(this::updateTitle);
    } else if (this.image != null) {
      StringBuilder title = new StringBuilder(tr(BASE_TITLE));
      if (this.image instanceof MapillaryImage) {
        MapillaryImage mapillaryImage = (MapillaryImage) this.image;
        UserProfile user = mapillaryImage.getUser();
        if (user != null) {
          title.append(MESSAGE_SEPARATOR).append(user.getUsername());
        }
        if (mapillaryImage.getCapturedAt() != 0) {
          title.append(MESSAGE_SEPARATOR).append(mapillaryImage.getDate());
        }
        setTitle(title.toString());
      } else if (this.image instanceof MapillaryImportedImage) {
        MapillaryImportedImage mapillaryImportedImage = (MapillaryImportedImage) this.image;
        title.append(MESSAGE_SEPARATOR).append(mapillaryImportedImage.getFile().getName());
        title.append(MESSAGE_SEPARATOR).append(mapillaryImportedImage.getDate());
        setTitle(title.toString());
      }
    }
  }

  /**
   * Returns the {@link MapillaryAbstractImage} object which is being shown.
   *
   * @return The {@link MapillaryAbstractImage} object which is being shown.
   */
  public synchronized MapillaryAbstractImage getImage() {
    return this.image;
  }

  private static class StopAction extends AbstractAction implements WalkListener {

    private static final long serialVersionUID = -6561451575815789198L;

    private WalkThread thread;

    /**
     * Constructs a normal StopAction
     */
    StopAction() {
      putValue(NAME, trc("as synonym to halt or stand still", "Stop"));
      putValue(SHORT_DESCRIPTION, tr("Stops the walk."));
      new ImageProvider("dialogs/mapillaryStop").getResource().attachImageIcon(this, true);
      MapillaryPlugin.getWalkAction().addListener(this);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      if (this.thread != null)
        this.thread.stopWalk();
    }

    @Override
    public void walkStarted(WalkThread thread) {
      this.thread = thread;
    }
  }

  private static class PlayAction extends AbstractAction implements WalkListener {

    private static final long serialVersionUID = -17943404752082788L;
    private transient WalkThread thread;

    /**
     * Constructs a normal PlayAction
     */
    PlayAction() {
      putValue(NAME, tr("Play"));
      putValue(SHORT_DESCRIPTION, tr("Continues with the paused walk."));
      new ImageProvider("dialogs/mapillaryPlay").getResource().attachImageIcon(this, true);
      MapillaryPlugin.getWalkAction().addListener(this);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      if (this.thread != null)
        this.thread.play();
    }

    @Override
    public void walkStarted(WalkThread thread) {
      if (thread != null)
        this.thread = thread;
    }
  }

  private static class PauseAction extends AbstractAction implements WalkListener {

    private static final long serialVersionUID = 4400240686337741192L;

    private WalkThread thread;

    /**
     * Constructs a normal PauseAction
     */
    PauseAction() {
      putValue(NAME, tr("Pause"));
      putValue(SHORT_DESCRIPTION, tr("Pauses the walk."));
      new ImageProvider("dialogs/mapillaryPause").getResource().attachImageIcon(this, true);
      MapillaryPlugin.getWalkAction().addListener(this);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      this.thread.pause();
    }

    @Override
    public void walkStarted(WalkThread thread) {
      this.thread = thread;
    }
  }

  /**
   * When the pictures are returned from the cache, they are set in the {@link MapillaryImageDisplay} object.
   */
  @Override
  public void loadingFinished(final CacheEntry data, final CacheEntryAttributes attributes, final LoadResult result) {
    if (data != null && result == LoadResult.SUCCESS) {
      GuiHelper.runInEDT(() -> realLoadingFinished(data));
    }
  }

  private void realLoadingFinished(final CacheEntry data) {
    try {
      BufferedImage img = data instanceof BufferedImageCacheEntry ? ((BufferedImageCacheEntry) data).getImage()
        : ImageIO.read(new ByteArrayInputStream(data.getContent()));
      if (img == null) {
        return;
      }
      if ((imageCache == null || data.equals(imageCache.get()) || thumbnailCache == null
        || data.equals(thumbnailCache.get()))
        && (imageViewer.getImage() == null || img.getHeight() >= this.imageViewer.getImage().getHeight())) {
        final MapillaryAbstractImage mai = getImage();
        setDisplayImage(img, mai instanceof MapillaryImage ? ((MapillaryImage) getImage()).getDetections() : null,
          mai != null && mai.isPanorama());
      }
    } catch (IOException e) {
      Logging.error(e);
    }
  }

  /**
   * Creates the layout of the dialog.
   *
   * @param data
   *        The content of the dialog
   * @param buttons
   *        The buttons where you can click
   */
  private void createLayout(Component data, List<SideButton> buttons) {
    removeAll();
    clearButtonActions(); // Fixes JOSM-18912
    createLayout(data, true, buttons);
    add(titleBar, BorderLayout.NORTH);
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage, MapillaryAbstractImage newImage) {
    setImage(newImage);
    updateImage();
  }

  @Override
  public void imagesAdded() {
    // This method is enforced by MapillaryDataListener, but only selectedImageChanged() is needed
  }

  @Override
  public void showDialog() {
    super.showDialog();
    if (this.image != null)
      MapillaryLayer.getInstance().setImageViewed(this.image);
  }

  @Override
  public void destroy() {
    if (!destroyed) {
      super.destroy();
      showDetectionOutlinesAction.destroy();
      showSignDetectionsAction.destroy();
      playButton.destroy();
      pauseButton.destroy();
      stopButton.destroy();
      if (MainApplication.getMap() != null)
        MainApplication.getMap().removeToggleDialog(this);
      destroyed = true;
    }
    destroyInstance();
  }
}
