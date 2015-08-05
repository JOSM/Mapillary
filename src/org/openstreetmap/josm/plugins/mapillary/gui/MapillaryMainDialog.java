package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.data.cache.CacheEntry;
import org.openstreetmap.josm.data.cache.CacheEntryAttributes;
import org.openstreetmap.josm.data.cache.ICachedLoaderListener;
import org.openstreetmap.josm.gui.dialogs.ToggleDialog;
import org.openstreetmap.josm.gui.SideButton;
import org.openstreetmap.josm.plugins.mapillary.MapillaryAbstractImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryDataListener;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.actions.WalkListener;
import org.openstreetmap.josm.plugins.mapillary.actions.WalkThread;
import org.openstreetmap.josm.plugins.mapillary.cache.MapillaryCache;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

import javax.imageio.ImageIO;
import javax.swing.JComponent;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.AbstractAction;
import javax.swing.JPanel;

/**
 * Toggle dialog that shows an image and some buttons.
 *
 * @author nokutu
 *
 */
public class MapillaryMainDialog extends ToggleDialog implements
    ICachedLoaderListener, MapillaryDataListener {

  private static final long serialVersionUID = 6856496736429480600L;

  private final static String BASE_TITLE = "Mapillary picture";

  private static MapillaryMainDialog INSTANCE;

  private volatile MapillaryAbstractImage image;

  private final SideButton nextButton = new SideButton(new nextPictureAction());
  private final SideButton previousButton = new SideButton(
      new previousPictureAction());
  /** Button used to jump to the image following the red line */
  public final SideButton redButton = new SideButton(new redAction());
  /** Button used to jump to the image following the blue line */
  public final SideButton blueButton = new SideButton(new blueAction());

  private final SideButton playButton = new SideButton(new playAction());
  private final SideButton pauseButton = new SideButton(new pauseAction());
  private final SideButton stopButton = new SideButton(new stopAction());

  /**
   * Buttons mode.
   *
   * @author nokutu
   *
   */
  public static enum Mode {
    /** Standard mode to view pictures. */
    NORMAL,
    /** Mode when in walk. */
    WALK;
  }

  private JPanel buttonsPanel;

  /** Object containing the shown image and that handles zoom and drag */
  public MapillaryImageDisplay mapillaryImageDisplay;

  private MapillaryCache imageCache;
  private MapillaryCache thumbnailCache;

  private MapillaryMainDialog() {
    super(tr(BASE_TITLE), "mapillary.png", tr("Open Mapillary window"),
        Shortcut.registerShortcut(tr("Mapillary dialog"),
            tr("Open Mapillary main dialog"), KeyEvent.VK_M, Shortcut.NONE),
        200, false, MapillaryPreferenceSetting.class);
    MapillaryLayer.getInstance().getData().addListener(this);
    addShortcuts();
    this.mapillaryImageDisplay = new MapillaryImageDisplay();

    this.blueButton.setForeground(Color.BLUE);
    this.redButton.setForeground(Color.RED);

    createLayout(
        this.mapillaryImageDisplay,
        Arrays.asList(new SideButton[] { this.blueButton, this.previousButton,
            this.nextButton, this.redButton }),
        Main.pref.getBoolean("mapillary.reverse-buttons"));
    disableAllButtons();

  }

  /**
   * Adds the shortcuts to the buttons.
   */
  private void addShortcuts() {
    this.nextButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
        KeyStroke.getKeyStroke("PAGE_DOWN"), "next");
    this.nextButton.getActionMap().put("next", new nextPictureAction());
    this.previousButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
        KeyStroke.getKeyStroke("PAGE_UP"), "previous");
    this.previousButton.getActionMap().put("previous",
        new previousPictureAction());
    this.blueButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
        KeyStroke.getKeyStroke("control PAGE_UP"), "blue");
    this.blueButton.getActionMap().put("blue", new blueAction());
    this.redButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
        KeyStroke.getKeyStroke("control PAGE_DOWN"), "red");
    this.redButton.getActionMap().put("red", new redAction());
  }

  /**
   * Returns the unique instance of the class.
   *
   * @return The unique instance of the class.
   */
  public static MapillaryMainDialog getInstance() {
    if (INSTANCE == null)
      INSTANCE = new MapillaryMainDialog();
    return INSTANCE;
  }

  /**
   * Sets a new mode for the dialog.
   *
   * @param mode
   *          The mode to be set.
   */
  public void setMode(Mode mode) {
    switch (mode) {
      case NORMAL:
        createLayout(
            this.mapillaryImageDisplay,
            Arrays.asList(new SideButton[] { this.blueButton,
                this.previousButton, this.nextButton, this.redButton }),
            Main.pref.getBoolean("mapillary.reverse-buttons"));
        break;
      case WALK:
        createLayout(
            this.mapillaryImageDisplay,
            Arrays.asList(new SideButton[] { this.playButton, this.pauseButton,
                this.stopButton }),
            Main.pref.getBoolean("mapillary.reverse-buttons"));
        break;
    }
    disableAllButtons();

  }

  /**
   * Destroys the unique instance of the class.
   */
  public static void destroyInstance() {
    INSTANCE = null;
  }

  /**
   * Downloads the full quality picture of the selected MapillaryImage and sets
   * in the MapillaryImageDisplay object.
   */
  public synchronized void updateImage() {
    updateImage(true);
  }

  /**
   * Downloads the picture of the selected MapillaryImage and sets in the
   * MapillaryImageDisplay object.
   *
   * @param fullQuality
   *          If the full quality picture must be downloaded or just the
   *          thumbnail.
   */
  public synchronized void updateImage(boolean fullQuality) {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          updateImage();
        }
      });
    } else {
      if (MapillaryLayer.INSTANCE == null) {
        return;
      }
      if (this.image == null) {
        this.mapillaryImageDisplay.setImage(null);
        setTitle(tr(BASE_TITLE));
        disableAllButtons();
        return;
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
        this.mapillaryImageDisplay.hyperlink.setVisible(true);
        MapillaryImage mapillaryImage = (MapillaryImage) this.image;
        this.mapillaryImageDisplay.hyperlink.setURL(mapillaryImage.getKey());
        // Downloads the thumbnail.
        this.mapillaryImageDisplay.setImage(null);
        if (this.thumbnailCache != null)
          this.thumbnailCache.cancelOutstandingTasks();
        this.thumbnailCache = new MapillaryCache(mapillaryImage.getKey(),
            MapillaryCache.Type.THUMBNAIL);
        this.thumbnailCache.submit(this, false);

        // Downloads the full resolution image.
        if (fullQuality) {
          if (this.imageCache != null)
            this.imageCache.cancelOutstandingTasks();
          this.imageCache = new MapillaryCache(mapillaryImage.getKey(),
              MapillaryCache.Type.FULL_IMAGE);
          this.imageCache.submit(this, false);
        }
      } else if (this.image instanceof MapillaryImportedImage) {
        this.mapillaryImageDisplay.hyperlink.setVisible(false);
        this.mapillaryImageDisplay.hyperlink.setURL(null);
        MapillaryImportedImage mapillaryImage = (MapillaryImportedImage) this.image;
        try {
          this.mapillaryImageDisplay.setImage(mapillaryImage.getImage());
        } catch (IOException e) {
          Main.error(e);
        }
      }
      updateTitle();
    }

  }

  private void disableAllButtons() {
    this.nextButton.setEnabled(false);
    this.previousButton.setEnabled(false);
    this.blueButton.setEnabled(false);
    this.redButton.setEnabled(false);
    this.mapillaryImageDisplay.hyperlink.setVisible(false);
  }

  /**
   * Sets a new MapillaryImage to be shown.
   *
   * @param image
   *          The image to be shown.
   */
  public synchronized void setImage(MapillaryAbstractImage image) {
    this.image = image;
  }

  /**
   * Updates the title of the dialog.
   */
  public synchronized void updateTitle() {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          updateTitle();
        }
      });
    } else {
      if (this.image != null) {
        if (this.image instanceof MapillaryImage) {
          MapillaryImage mapillaryImage = (MapillaryImage) this.image;
          String title = tr(BASE_TITLE);
          if (mapillaryImage.getUser() != null)
            title += " -- " + mapillaryImage.getUser();
          if (mapillaryImage.getCapturedAt() != 0)
            title += " -- " + mapillaryImage.getDate();
          setTitle(title);
        } else if (this.image instanceof MapillaryImportedImage) {
          MapillaryImportedImage mapillaryImportedImage = (MapillaryImportedImage) this.image;
          String title = tr(BASE_TITLE);
          title += " -- " + mapillaryImportedImage.getFile().getName();
          title += " -- " + mapillaryImportedImage.getDate();
          setTitle(title);
        }
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

  /**
   * Action class form the next image button.
   *
   * @author nokutu
   *
   */
  private class nextPictureAction extends AbstractAction {

    private static final long serialVersionUID = 3023827221453154340L;

    public nextPictureAction() {
      putValue(NAME, tr("Next picture"));
      putValue(SHORT_DESCRIPTION, tr("Shows the next picture in the sequence"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      MapillaryLayer.getInstance().getData().selectNext();
    }
  }

  /**
   * Action class for the previous image button.
   *
   * @author nokutu
   *
   */
  private class previousPictureAction extends AbstractAction {

    private static final long serialVersionUID = -6420511632957956012L;

    public previousPictureAction() {
      putValue(NAME, tr("Previous picture"));
      putValue(SHORT_DESCRIPTION,
          tr("Shows the previous picture in the sequence"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      MapillaryLayer.getInstance().getData().selectPrevious();
    }
  }

  /**
   * Action class to jump to the image following the red line.
   *
   * @author nokutu
   *
   */
  private class redAction extends AbstractAction {

    private static final long serialVersionUID = -6480229431481386376L;

    public redAction() {
      putValue(NAME, tr("Jump to red"));
      putValue(SHORT_DESCRIPTION,
          tr("Jumps to the picture at the other side of the red line"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      if (MapillaryMainDialog.getInstance().getImage() != null) {
        MapillaryLayer.getInstance().getData()
            .setSelectedImage(MapillaryLayer.RED, true);
      }
    }
  }

  /**
   * Action class to jump to the image following the blue line.
   *
   * @author nokutu
   *
   */
  private class blueAction extends AbstractAction {

    private static final long serialVersionUID = 6250690644594703314L;

    public blueAction() {
      putValue(NAME, tr("Jump to blue"));
      putValue(SHORT_DESCRIPTION,
          tr("Jumps to the picture at the other side of the blue line"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      if (MapillaryMainDialog.getInstance().getImage() != null) {
        MapillaryLayer.getInstance().getData()
            .setSelectedImage(MapillaryLayer.BLUE, true);
      }
    }
  }

  private class stopAction extends AbstractAction implements WalkListener {

    private static final long serialVersionUID = -6561451575815789198L;

    private WalkThread thread;

    public stopAction() {
      putValue(NAME, tr("Stop"));
      putValue(SHORT_DESCRIPTION, tr("Stops the walk."));
      putValue(SMALL_ICON, ImageProvider.get("dialogs/mapillaryStop.png"));
      MapillaryPlugin.walkAction.addListener(this);
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

  private class playAction extends AbstractAction implements WalkListener {

    private static final long serialVersionUID = -17943404752082788L;
    private WalkThread thread;

    public playAction() {
      putValue(NAME, tr("Play"));
      putValue(SHORT_DESCRIPTION, tr("Continues with the paused walk."));
      putValue(SMALL_ICON, ImageProvider.get("dialogs/mapillaryPlay.png"));
      MapillaryPlugin.walkAction.addListener(this);
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

  private class pauseAction extends AbstractAction implements WalkListener {

    private static final long serialVersionUID = 4400240686337741192L;

    private WalkThread thread;

    public pauseAction() {
      putValue(NAME, tr("Pause"));
      putValue(SHORT_DESCRIPTION, tr("Pauses the walk."));
      putValue(SMALL_ICON, ImageProvider.get("dialogs/mapillaryPause.png"));
      MapillaryPlugin.walkAction.addListener(this);
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
   * When the pictures are returned from the cache, they are set in the
   * {@link MapillaryImageDisplay} object.
   */
  @Override
  public void loadingFinished(final CacheEntry data,
      final CacheEntryAttributes attributes, final LoadResult result) {
    if (!SwingUtilities.isEventDispatchThread()) {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          loadingFinished(data, attributes, result);
        }
      });
    } else if (data != null && result == LoadResult.SUCCESS) {
      System.out.println(attributes.getMetadata());
      try {
        BufferedImage img = ImageIO.read(new ByteArrayInputStream(data
            .getContent()));
        if (img == null)
          return;
        if (this.mapillaryImageDisplay.getImage() == null)
          this.mapillaryImageDisplay.setImage(img);
        else if (img.getHeight() > this.mapillaryImageDisplay.getImage()
            .getHeight()) {
          this.mapillaryImageDisplay.setImage(img);
        }
      } catch (IOException e) {
        Main.error(e);
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
   * @param reverse
   *          {@code true} if the buttons should go at the top; {@code false}
   *          otherwise.
   */
  public void createLayout(Component data, List<SideButton> buttons,
      boolean reverse) {
    this.removeAll();
    JPanel panel = new JPanel();
    panel.setLayout(new BorderLayout());
    panel.add(data, BorderLayout.CENTER);
    if (reverse) {
      this.buttonsPanel = new JPanel(new GridLayout(1, 1));
      if (!buttons.isEmpty() && buttons.get(0) != null) {
        final JPanel buttonRowPanel = new JPanel(Main.pref.getBoolean(
            "dialog.align.left", false) ? new FlowLayout(FlowLayout.LEFT)
            : new GridLayout(1, buttons.size()));
        this.buttonsPanel.add(buttonRowPanel);
        for (SideButton button : buttons)
          buttonRowPanel.add(button);
      }
      panel.add(this.buttonsPanel, BorderLayout.NORTH);
      createLayout(panel, true, null);
    } else
      createLayout(panel, true, buttons);
    this.add(this.titleBar, BorderLayout.NORTH);
  }

  @Override
  public void selectedImageChanged(MapillaryAbstractImage oldImage,
      MapillaryAbstractImage newImage) {
    setImage(newImage);
    updateImage();
  }

  @Override
  public void imagesAdded() {
  }
}
