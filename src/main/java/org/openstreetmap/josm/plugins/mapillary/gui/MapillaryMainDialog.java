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
import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.TimeZone;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import javax.annotation.Nonnull;
import javax.imageio.ImageIO;
import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.cache.BufferedImageCacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.osm.DataSelectionListener;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.preferences.BooleanProperty;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.actions.SelectNextImageAction;
import org.openstreetmap.josm.plugins.mapillary.actions.WalkListener;
import org.openstreetmap.josm.plugins.mapillary.actions.WalkThread;
import org.openstreetmap.josm.plugins.mapillary.cache.CacheUtils;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.OrganizationRecord;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.VectorDataSelectionListener;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.model.ImageDetection;
import org.openstreetmap.josm.plugins.mapillary.utils.DetectionVerification;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillarySequenceUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryUtils;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.Shortcut;
import org.openstreetmap.josm.tools.date.DateUtils;

/**
 * Toggle dialog that shows an image and some buttons.
 *
 * @author nokutu
 */
public final class MapillaryMainDialog extends ToggleDialog
  implements ICachedLoaderListener, VectorDataSelectionListener {

  private static final long serialVersionUID = 6856496736429480600L;

  private static final String BASE_TITLE = marktr("Mapillary image");
  private static final String MESSAGE_SEPARATOR = " — ";

  private static MapillaryMainDialog instance;

  private boolean destroyed;

  private transient INode image;
  /** The key for the current image -- used for when there are multiple images with the same MVT id */

  private final PlayAction playAction = new PlayAction();
  private final PauseAction pauseAction = new PauseAction();
  private final StopAction stopAction = new StopAction();
  private ImageDetection.ImageDetectionForkJoinTask futureDetections;
  private Future<?> imagePainter;

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
  public final MapillaryImageDisplay imageViewer = new MapillaryImageDisplay();

  private MapillaryCache imageCache;
  private MapillaryCache thumbnailCache;

  private final ShowDetectionOutlinesAction showDetectionOutlinesAction = new ShowDetectionOutlinesAction();
  private final ShowSignDetectionsAction showSignDetectionsAction = new ShowSignDetectionsAction();
  private final Collection<AbstractButton> buttonCollection = new ArrayList<>();

  private final VerifyRejectAction approveAction = new VerifyRejectAction(DetectionVerification.TYPE.APPROVE);
  private final VerifyRejectAction rejectAction = new VerifyRejectAction(DetectionVerification.TYPE.REJECT);

  /**
   * This is copied from {@link ImageViewerDialog#createNaviationButton}
   *
   * @param action The action to put in a {@link JButton}
   * @param buttonDim The dimensions of the button
   * @return A new button, appropriately sized
   */
  private static JButton createNavigationButton(JosmAction action, Dimension buttonDim) {
    JButton btn = new JButton(action);
    btn.setPreferredSize(buttonDim);
    btn.setEnabled(false);
    return btn;
  }

  private MapillaryMainDialog() {
    super(tr(BASE_TITLE), "mapillary-main", tr("Open Mapillary window"),
      Shortcut.registerShortcut("mapillary:main", tr("Mapillary main dialog"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
      200, true, MapillaryPreferenceSetting.class);
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
    private final transient DataSelectionListener mapillaryDataListener = event -> updateEnabledState();

    VerifyRejectAction(DetectionVerification.TYPE type) {
      super(tr("Vote: {0}", type.toString()), getIcon(type), tr("Tell Mapillary that you {0} this detection", type),
        Shortcut.registerShortcut("mapillary:vote:" + type, tr("Mapillary: Vote {0}", type.toString()),
          KeyEvent.VK_UNDEFINED, Shortcut.NONE),
        false, null, false);
      this.type = type;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      if (!this.listenerAdded) {
        MapillaryLayer.getInstance().getData().addSelectionListener(mapillaryDataListener);
        this.listenerAdded = true;
      }
      ImageDetection<?> detection = getDetection();
      if (detection != null) {
        INode image = MapillaryMainDialog.getInstance().getImage();
        if (detection.getImageKey().equals(MapillaryImageUtils.getKey(image))
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
      INode image = MapillaryMainDialog.getInstance().getImage();
      ImageDetection<?> detection = getDetection();
      if (MapillaryImageUtils.getKey(image) != null || detection == null) {
        this.setEnabled(false);
      } else {
        if (this.type != detection.getApprovalType())
          this.setEnabled(MapillaryImageUtils.getKey(image).equals(detection.getImageKey()));
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
    if (instance == null || instance.destroyed)
      instance = new MapillaryMainDialog();
    return instance;
  }

  /**
   * Check if there is an instance of the dialog
   *
   * @return true, iff the singleton instance is present
   */
  public static boolean hasInstance() {
    return instance != null;
  }

  /**
   * Sets a new mode for the dialog.
   *
   * @param mode
   *        The mode to be set. Must not be {@code null}.
   */
  public void setMode(@Nonnull final MODE mode) {
    final Dimension buttonDim = ImageProvider.ImageSizes.CURSOR.getImageDimension();
    final Dimension nextPrevButtonDim = new Dimension(2 * buttonDim.width, buttonDim.height);
    JToggleButton toggleSigns = new JToggleButton(showSignDetectionsAction);
    toggleSigns.setPreferredSize(buttonDim);
    // Mac OS X won't show background colors if buttons aren't opaque.
    toggleSigns.setOpaque(true);
    JToggleButton toggleDetections = new JToggleButton(showDetectionOutlinesAction);
    toggleDetections.setPreferredSize(buttonDim);
    toggleDetections.setOpaque(true);

    toggleSigns.setSelected(Boolean.TRUE.equals(MapillaryProperties.SHOW_DETECTED_SIGNS.get()));
    toggleDetections.setSelected(Boolean.TRUE.equals(MapillaryProperties.SHOW_DETECTION_OUTLINES.get()));

    final JButton nextButton = createNavigationButton(SelectNextImageAction.NEXT_ACTION, nextPrevButtonDim);
    final JButton previousButton = createNavigationButton(SelectNextImageAction.PREVIOUS_ACTION, nextPrevButtonDim);
    final JButton redButton = createNavigationButton(SelectNextImageAction.RED_ACTION, buttonDim);
    final JButton blueButton = createNavigationButton(SelectNextImageAction.BLUE_ACTION, buttonDim);
    final JToggleButton jumpToCurrent = new JToggleButton(new JosmAction(null,
      new ImageProvider("dialogs/autoscale", "selection"), tr("Center view"), null, false, null, false) {
      @Override
      public void actionPerformed(ActionEvent e) {
        INode currentImage = MapillaryMainDialog.getInstance().getImage();
        if (MapillaryImageUtils.IS_IMAGE.test(currentImage)
          && Boolean.TRUE.equals(MapillaryProperties.MOVE_TO_IMG.get())) {
          MainApplication.getMap().mapView.zoomTo(currentImage.getCoor());
        }
      }
    });
    jumpToCurrent.setPreferredSize(buttonDim);

    jumpToCurrent.setSelected(Boolean.TRUE.equals(MapillaryProperties.MOVE_TO_IMG.get()));
    jumpToCurrent.addChangeListener(l -> {
      MapillaryProperties.MOVE_TO_IMG.put(jumpToCurrent.isSelected());
    });
    blueButton.setForeground(Color.BLUE);
    redButton.setForeground(Color.RED);

    Stream<AbstractButton> buttons;
    switch (mode) {
    case WALK:
      final JButton playButton = createNavigationButton(this.playAction, buttonDim);
      final JButton pauseButton = createNavigationButton(this.pauseAction, buttonDim);
      final JButton stopButton = createNavigationButton(this.stopAction, buttonDim);
      buttons = Stream.of(toggleSigns, toggleDetections, playButton, pauseButton, stopButton, jumpToCurrent);
      break;
    case SMART_EDIT:
      JButton verify = createNavigationButton(this.approveAction, buttonDim);
      JButton reject = createNavigationButton(this.rejectAction, buttonDim);
      buttons = Stream.of(blueButton, previousButton, verify, reject, nextButton, redButton, jumpToCurrent);
      break;
    case NORMAL:
    default:
      final JButton lastButton = createNavigationButton(SelectNextImageAction.LAST_ACTION, buttonDim);
      final JButton firstButton = createNavigationButton(SelectNextImageAction.FIRST_ACTION, buttonDim);
      buttons = Stream.of(firstButton, previousButton, nextButton, lastButton, jumpToCurrent, blueButton, redButton,
        toggleSigns, toggleDetections);
      break;
    }
    final JPanel content = new JPanel(new BorderLayout());
    final JPanel buttonsPanel = new JPanel();

    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS));
    panel.add(imageViewer, BorderLayout.CENTER);
    content.add(panel);
    this.buttonCollection.clear();
    buttons.forEach(this.buttonCollection::add);
    this.buttonCollection.forEach(buttonsPanel::add);
    content.add(buttonsPanel, BorderLayout.SOUTH);
    createLayout(content, false, null);
    disableAllButtons();
    if (Arrays.asList(MODE.WALK, MODE.SMART_EDIT).contains(mode)) {
      updateImage();
    }
    revalidate();
    repaint();
  }

  /**
   * Destroys the unique instance of the class. You should prefer to call the destroy method on the actual instance.
   */
  private static synchronized void destroyInstance() {
    instance = null;
  }

  /**
   * Downloads the full quality picture of the selected MapillaryImage and sets in the MapillaryImageDisplay object.
   */
  public synchronized void updateImage() {
    if (!SwingUtilities.isEventDispatchThread()) {
      GuiHelper.runInEDT(this::updateImage);
    } else {
      if (!MapillaryLayer.hasInstance()) {
        return;
      }

      final INode currentImage = this.image;
      if (currentImage == null) {
        setDisplayImage((BufferedImage) null, null, false);
        setTitle(tr(BASE_TITLE));
        disableAllButtons();
        return;
      }

      this.updateButtonStates(currentImage);

      if (MapillaryImageUtils.getKey(currentImage) != null) {
        if (currentImage.getNumKeys() <= CacheUtils.MAPILLARY_DEFAULT_KEY_LENGTH) {
          MainApplication.worker.submit(() -> {
            MapillaryDownloader.downloadImages(MapillaryImageUtils.getKey(currentImage));
            this.updateImage();
          });
          return;
        }
        // Use this to avoid race conditions
        final MapillaryCache imageFullCache = this.cacheFullImage(currentImage);

        MapillaryCache.cacheSurroundingImages(currentImage);
        Function<BufferedImageCacheEntry, BufferedImage> getImage = cache -> {
          try {
            return cache.getImage();
          } catch (IOException e) {
            Logging.error(e);
          }
          return null;
        };

        ForkJoinPool pool = MapillaryUtils.getForkJoinPool();
        if (this.futureDetections != null && !this.futureDetections.isDone()
          && !MapillaryImageUtils.getKey(image).equals(this.futureDetections.key)) {
          this.futureDetections.cancel(false);
        }
        this.futureDetections = ImageDetection.getDetections(MapillaryImageUtils.getKey(this.image),
          (key, detections) -> {
            INode tImage = this.image;
            if (tImage != null && key.equals(MapillaryImageUtils.getKey(tImage))) {
              this.updateDetections(imageFullCache, tImage, detections);
            }
          });
        List<ImageDetection<?>> detections = ImageDetection.getDetections(MapillaryImageUtils.getKey(image), false);
        if (imageFullCache.get() != null) {
          setDisplayImage(() -> getImage.apply(imageFullCache.get()), detections,
            MapillaryImageUtils.IS_PANORAMIC.test(currentImage));
          pool.execute(() -> updateDetections(imageFullCache, currentImage, detections));
        } else {
          this.imageViewer.paintLoadingImage();
        }
        if (!currentImage.hasKeys()) {
          pool.execute(() -> {
            MapillaryDownloader.downloadImages(MapillaryImageUtils.getKey(currentImage));
            if (currentImage.equals(this.image)) {
              updateTitle();
            }
          });
        }

      } else if (currentImage.hasKey(MapillaryImageUtils.IMPORTED_KEY)) {
        try {
          setDisplayImage(ImageIO.read(new File(currentImage.get(MapillaryImageUtils.IMPORTED_KEY))), null,
            MapillaryImageUtils.IS_PANORAMIC.test(currentImage));
        } catch (IOException e) {
          Logging.error(e);
        }
      }
      updateTitle();
    }
  }

  private MapillaryCache cacheFullImage(INode currentImage) {
    // Downloads the full resolution image.
    final MapillaryCache imageFullCache = new MapillaryCache(currentImage);
    // Use this variable to avoid race conditions
    if (this.imageCache != null)
      this.imageCache.cancelOutstandingTasks();
    this.imageCache = imageFullCache;
    try {
      if (imageFullCache.getUrl() != null && imageFullCache.get() == null)
        imageFullCache.submit(this, false);
    } catch (IOException e) {
      Logging.error(e);
    }
    return imageFullCache;
  }

  /**
   * Update button states. Synchronized to avoid locks.
   *
   * @param currentImage The current image
   */
  public synchronized void updateButtonStates(INode currentImage) {
    // Enables/disables next/previous buttons
    final boolean isImage = MapillaryImageUtils.IS_IMAGE.test(currentImage);
    for (AbstractButton jButton : this.buttonCollection) {
      Action action = jButton.getAction();
      if (action instanceof SelectNextImageAction) {
        SelectNextImageAction josmAction = (SelectNextImageAction) action;
        josmAction.updateEnabled(jButton, currentImage);
      } else {
        jButton.setEnabled(isImage);
      }
    }
  }

  private <T extends INode> void updateDetections(MapillaryCache cache, T image,
    Collection<ImageDetection<?>> detections) {
    Objects.requireNonNull(cache);
    Objects.requireNonNull(image);
    Objects.requireNonNull(detections);
    GuiHelper.runInEDT(() -> updateDisplayImage(cache, image, detections));
  }

  private <T extends INode> void updateDisplayImage(MapillaryCache cache, T image,
    Collection<ImageDetection<?>> detections) {
    Objects.requireNonNull(image, "Image cannot be null");
    Objects.requireNonNull(cache, "Cache cannot be null");
    Objects.requireNonNull(detections, "Detections cannot be null");
    final Object syncObject = this.image != null ? this.image : MapillaryMainDialog.class;
    synchronized (syncObject) {
      if (image.equals(this.image)) {
        // Comprehensively fix Github #165
        if (cache.get() != null) {
          this.setDisplayImage(() -> {
            try {
              return cache.get().getImage();
            } catch (IOException e) {
              Logging.error(e);
            }
            return null;
          }, detections, MapillaryImageUtils.IS_PANORAMIC.test(image));
        }
      }
    }
  }

  /**
   * Disables all the buttons in the dialog
   */
  private void disableAllButtons() {
    for (AbstractButton jButton : this.buttonCollection) {
      jButton.setEnabled(false);
    }
  }

  /**
   * Sets a new MapillaryImage to be shown.
   *
   * @param image
   *        The image to be shown.
   */
  public void setImage(INode image) {
    this.image = image;
    // Avoid blocking on HTTP GET -- this replaces the sequences across tiles
    if (!MapillaryUtils.getForkJoinPool().isShutdown()) {
      MapillaryUtils.getForkJoinPool().execute(() -> {
        MapillarySequenceUtils.getSequence(MapillaryImageUtils.getSequenceKey(image));
        GuiHelper.runInEDT(() -> {
          this.updateImage();
          this.invalidate();
        });
      });
    } else {
      this.updateImage();
    }
    if (this.isVisible() && MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().setImageViewed(this.image);
    }
  }

  public void setDisplayImage(Supplier<BufferedImage> imageSupplier, Collection<ImageDetection<?>> detections,
    Boolean pano) {
    if (SwingUtilities.isEventDispatchThread()) {
      MainApplication.worker.execute(() -> setDisplayImageNonEdt(imageSupplier, detections, pano));
    } else {
      setDisplayImageNonEdt(imageSupplier, detections, pano);
    }
  }

  private void setDisplayImageNonEdt(Supplier<BufferedImage> imageSupplier, Collection<ImageDetection<?>> detections,
    Boolean pano) {
    final BufferedImage bufferedImage = imageSupplier.get();
    GuiHelper.runInEDT(() -> setDisplayImage(bufferedImage, detections, pano));
  }

  public void setDisplayImage(BufferedImage image, Collection<ImageDetection<?>> detections, Boolean pano) {
    if (image != null) {
      imageViewer.setImage(image, detections, Boolean.TRUE.equals(pano));
    } else {
      imageViewer.setImage(null, null, false);
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
      if (MapillaryImageUtils.getKey(this.image) != null) {
        INode mapillaryImage = this.image;
        OrganizationRecord organizationRecord = MapillaryImageUtils.getOrganization(mapillaryImage);
        if (organizationRecord != OrganizationRecord.NULL_RECORD) {
          title.append(MESSAGE_SEPARATOR).append(organizationRecord.getNiceName());
        }
        if (!Instant.EPOCH.equals(MapillaryImageUtils.getDate(mapillaryImage))) {
          final boolean showHour = Boolean.TRUE.equals(MapillaryProperties.DISPLAY_HOUR.get());
          final Instant pictureTime = MapillaryImageUtils.getDate(mapillaryImage);
          title.append(MESSAGE_SEPARATOR);
          final DateFormat formatter;
          if (showHour) {
            formatter = DateUtils.getDateTimeFormat(DateFormat.DEFAULT, DateFormat.DEFAULT);
          } else {
            formatter = DateUtils.getDateFormat(DateFormat.DEFAULT);
          }
          // Use UTC, since mappers may be outside of "their" timezone, which would be even more confusing.
          formatter.setTimeZone(TimeZone.getTimeZone(ZoneOffset.UTC));
          title.append(formatter.format(Date.from(pictureTime)));
        }
        setTitle(title.toString());
      } else if (this.image.hasKey(MapillaryImageUtils.IMPORTED_KEY)) {
        INode mapillaryImportedImage = this.image;
        title.append(MESSAGE_SEPARATOR).append(mapillaryImportedImage.get(MapillaryImageUtils.IMPORTED_KEY));
        title.append(MESSAGE_SEPARATOR).append(MapillaryImageUtils.getDate(mapillaryImportedImage));
        setTitle(title.toString());
      }
    }
  }

  /**
   * Returns the {@link INode} object which is being shown.
   *
   * @return The {@link INode} object which is being shown.
   */
  public synchronized INode getImage() {
    return this.image;
  }

  private static class StopAction extends JosmAction implements WalkListener {

    private static final long serialVersionUID = -6561451575815789198L;

    private WalkThread thread;

    /**
     * Constructs a normal StopAction
     */
    StopAction() {
      super(trc("as synonym to halt or stand still", "Stop"), "dialogs/mapillaryStop", tr("Stop the walk."),
        Shortcut.registerShortcut("mapillary:image:walk_stop", tr("Mapillary: {0}", tr("Stop Image Walk")),
          KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
        false, false);
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

  private static class PlayAction extends JosmAction implements WalkListener {

    private static final long serialVersionUID = -17943404752082788L;
    private transient WalkThread thread;

    /**
     * Constructs a normal PlayAction
     */
    PlayAction() {
      super(tr("Play"), "dialogs/mapillaryPlay", tr("Continues with the paused walk."),
        Shortcut.registerShortcut("mapillary:image:walk", tr("Mapillary: {0}", tr("Image Walk")),
          KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
        false, false);
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

  private static class PauseAction extends JosmAction implements WalkListener {

    private static final long serialVersionUID = 4400240686337741192L;

    private WalkThread thread;

    /**
     * Constructs a normal PauseAction
     */
    PauseAction() {
      super(tr("Pause"), "dialogs/mapillaryPause", tr("Pause the walk."),
        Shortcut.registerShortcut("mapillary:image:walk_pause", tr("Mapillary: {0}", tr("Pause Image Walk")),
          KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
        false, false);
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
    if ((imageCache == null || data.equals(imageCache.get()) || thumbnailCache == null
      || data.equals(thumbnailCache.get()))) {
      final INode mai = getImage();
      setDisplayImage(() -> {
        try {
          return data instanceof BufferedImageCacheEntry ? ((BufferedImageCacheEntry) data).getImage()
            : ImageIO.read(new ByteArrayInputStream(data.getContent()));
        } catch (IOException e) {
          Logging.error(e);
        }
        return null;
      }, ImageDetection.getDetections(MapillaryImageUtils.getKey(mai), false),
        MapillaryImageUtils.IS_PANORAMIC.test(mai));
      if (mai != null) {
        ImageDetection.getDetections(MapillaryImageUtils.getKey(mai), (key, detections) -> this
          .updateDetections(this.imageCache != null ? this.imageCache : this.thumbnailCache, mai, detections));
      }
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
  public void selectionChanged(SelectionChangeEvent event) {
    INode newImage = MapillaryLayer.getInstance().getSelected().filter(MapillaryImageUtils.IS_IMAGE).findFirst()
      .orElse(null);
    setImage(newImage);
  }

  @Override
  public void showDialog() {
    super.showDialog();
    if (this.image != null)
      MapillaryLayer.getInstance().setImageViewed(this.image);
  }

  @Override
  public synchronized void destroy() {
    if (!destroyed) {
      super.destroy();
      showDetectionOutlinesAction.destroy();
      showSignDetectionsAction.destroy();
      playAction.destroy();
      pauseAction.destroy();
      stopAction.destroy();
      if (MainApplication.getMap() != null)
        MainApplication.getMap().removeToggleDialog(this);
      destroyed = true;
    }
    destroyInstance();
  }
}
