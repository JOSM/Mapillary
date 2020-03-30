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
import java.util.Arrays;
import java.util.List;

import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.data.preferences.BooleanProperty;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.actions.SelectNextImageAction;
import org.openstreetmap.josm.plugins.mapillary.actions.WalkListener;
import org.openstreetmap.josm.plugins.mapillary.actions.WalkThread;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.plugins.mapillary.gui.imageinfo.ImageInfoHelpPopup;
import org.openstreetmap.josm.plugins.mapillary.model.UserProfile;
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

  private volatile MapillaryAbstractImage image;

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
    WALK
  }

  /**
   * Object containing the shown image and that handles zoom and drag
   */
  public MapillaryImageDisplay mapillaryImageDisplay;

  private MapillaryCache imageCache;
  private MapillaryCache thumbnailCache;

  private final ShowSignDetectionsAction showSignDetectionsAction = new ShowSignDetectionsAction();

  private MapillaryMainDialog() {
    super(
      tr(BASE_TITLE), "mapillary-main", tr("Open Mapillary window"), null, 200, true, MapillaryPreferenceSetting.class
    );
    addShortcuts();
    this.mapillaryImageDisplay = new MapillaryImageDisplay();

    this.blueButton.setForeground(Color.BLUE);
    this.redButton.setForeground(Color.RED);

    panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS));
    panel.add(mapillaryImageDisplay);
    JPanel buttons = new JPanel();
    buttons.setLayout(new BoxLayout(buttons, BoxLayout.LINE_AXIS));
    Dimension buttonDim = new Dimension(52, 34);
    JButton toggleSigns = new JButton(showSignDetectionsAction);
    showSignDetectionsAction.setButton(toggleSigns);
    toggleSigns.setPreferredSize(buttonDim);
    // Mac OS X won't show background colors if buttons aren't opaque.
    toggleSigns.setOpaque(true);
    buttons.add(toggleSigns);
    panel.add(buttons);

    setMode(MODE.NORMAL);
  }

  private static abstract class JosmButtonAction extends JosmAction {
    private static final long serialVersionUID = -4009253801009731575L;
    JButton pbutton;

    public JosmButtonAction(
      String name, ImageProvider icon, String tooltip, Shortcut shortcut, boolean registerInToolbar, String toolbarId,
      boolean installAdapters
    ) {
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

  private static class ShowSignDetectionsAction extends JosmButtonAction {
    private static final long serialVersionUID = -3743322064323002656L;

    ShowSignDetectionsAction() {
      super(
        null, new ImageProvider("mapillary_sprite_source/package_signs", "regulatory--go-straight-or-turn-left--g2"), tr("Toggle sign detection outlines"), Shortcut.registerShortcut("mapillary:showsigndetections", tr("Mapillary: toggle sign detections"), KeyEvent.VK_UNDEFINED, Shortcut.NONE), false, null, false
      );
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
   * Adds the shortcuts to the buttons.
   */
  private void addShortcuts() {
    this.nextButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("PAGE_DOWN"), "next");
    this.nextButton.getActionMap().put("next", SelectNextImageAction.NEXT_ACTION);
    this.previousButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
      .put(KeyStroke.getKeyStroke("PAGE_UP"), "previous");
    this.previousButton.getActionMap().put("previous", SelectNextImageAction.PREVIOUS_ACTION);
    this.blueButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
      .put(KeyStroke.getKeyStroke("control PAGE_UP"), "blue");
    this.blueButton.getActionMap().put("blue", SelectNextImageAction.BLUE_ACTION);
    this.redButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
      .put(KeyStroke.getKeyStroke("control PAGE_DOWN"), "red");
    this.redButton.getActionMap().put("red", SelectNextImageAction.RED_ACTION);
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
   *          The mode to be set. Must not be {@code null}.
   */
  public void setMode(MODE mode) {
    switch (mode) {
    case WALK:
      createLayout(this.panel, Arrays.asList(playButton, pauseButton, stopButton));
      break;
    case NORMAL:
    default:
      createLayout(this.panel, Arrays.asList(blueButton, previousButton, nextButton, redButton));
      break;
    }
    disableAllButtons();
    if (MODE.NORMAL.equals(mode)) {
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
   *          If the full quality picture must be downloaded or just the thumbnail.
   */
  public synchronized void updateImage(boolean fullQuality) {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(this::updateImage);
    } else {
      if (!MapillaryLayer.hasInstance()) {
        return;
      }
      if (this.image == null) {
        this.mapillaryImageDisplay.setImage(null, null, false);
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
        this.mapillaryImageDisplay.setImage(null, null, false);
        if (this.thumbnailCache != null)
          this.thumbnailCache.cancelOutstandingTasks();
        this.thumbnailCache = new MapillaryCache(mapillaryImage.getKey(), MapillaryCache.Type.THUMBNAIL);
        try {
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
            this.imageCache.submit(this, false);
          } catch (IOException e) {
            Logging.error(e);
          }
        }
      } else if (this.image instanceof MapillaryImportedImage) {
        final MapillaryImportedImage mapillaryImage = (MapillaryImportedImage) this.image;
        try {
          this.mapillaryImageDisplay.setImage(mapillaryImage.getImage(), null, mapillaryImage.isPanorama());
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
   *          The image to be shown.
   */
  public synchronized void setImage(MapillaryAbstractImage image) {
    this.image = image;
    if (this.isVisible() && MapillaryLayer.hasInstance()) {
      MapillaryLayer.getInstance().setImageViewed(this.image);
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
      new ImageProvider("dialogs/mapillaryStop.png").getResource().attachImageIcon(this, true);
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
      new ImageProvider("dialogs/mapillaryPlay.png").getResource().attachImageIcon(this, true);
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
      new ImageProvider("dialogs/mapillaryPause.png").getResource().attachImageIcon(this, true);
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
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(() -> loadingFinished(data, attributes, result));
    } else if (data != null && result == LoadResult.SUCCESS) {
      try {
        BufferedImage img = ImageIO.read(new ByteArrayInputStream(data.getContent()));
        if (img == null) {
          return;
        }
        if (mapillaryImageDisplay.getImage() == null
          || img.getHeight() > this.mapillaryImageDisplay.getImage().getHeight()) {
          final MapillaryAbstractImage mai = getImage();
          this.mapillaryImageDisplay.setImage(
            img, mai instanceof MapillaryImage ? ((MapillaryImage) getImage()).getDetections() : null,
            mai != null && mai.isPanorama()
          );
        }
      } catch (IOException e) {
        Logging.error(e);
      }
    }
  }

  /**
   * Creates the layout of the dialog.
   *
   * @param data
   *          The content of the dialog
   * @param buttons
   *          The buttons where you can click
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
      showSignDetectionsAction.destroy();
      playButton.destroy();
      pauseButton.destroy();
      stopButton.destroy();
      MainApplication.getMap().removeToggleDialog(this);
      destroyed = true;
    }
    destroyInstance();
  }
}
