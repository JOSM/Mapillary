// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.Serial;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Popup;
import javax.swing.PopupFactory;

import org.openstreetmap.josm.gui.datatransfer.ClipboardUtils;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryColorScheme;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * An action for copying items to the clipboard
 */
public class ClipboardAction extends MapillaryAction {
    @Serial
    private static final long serialVersionUID = 3323536079627210533L;
    /**
     * The duration in milliseconds for which the popup will be shown
     */
    private static final long POPUP_DURATION = 3000;
    /**
     * A small popup that shows up when the text has been moved to the clipboard
     */
    private final JComponent popupContent;
    /**
     * The component which is used as parent of the shown popup.
     * If this is <code>null</code>, no popup will be shown.
     */
    private Component popupParent;
    /**
     * The UNIX epoch time when the popup for this action was shown the last time
     */
    private long lastCopyTime;
    /**
     * The contents that are transfered into the clipboard when the action is executed.
     * If this is <code>null</code>, the clipboard won't be changed.
     */
    private Transferable contents;

    /**
     * Create a new action for clipboards
     *
     * @param name The name of the action (use {@link org.openstreetmap.josm.tools.I18n#marktr(String)})
     * @param successMessage The message for success
     * @param contents The initial contents to use
     */
    public ClipboardAction(final String name, final String successMessage, final Transferable contents) {
        super(tr(name), "copy", tr("Copy {0} to clipboard", tr(name)),
            Shortcut.registerShortcut("mapillary:copy_to_clipboard_" + name.replace(' ', '_'),
                tr("Mapillary: {0}", tr(name)), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
            false, "mapillary:copy_to_clipboard_" + name.replace(' ', '_'), false);
        this.contents = contents;

        // Init popup
        this.popupContent = new JPanel();
        popupContent.setLayout(new FlowLayout(FlowLayout.CENTER, 10, 10));
        popupContent.setBackground(MapillaryColorScheme.TOOLBAR_DARK_GREY);
        final JLabel label = new JLabel(successMessage);
        label.setForeground(Color.WHITE);
        popupContent.add(label);
    }

    /**
     * Set the potential contents of the clipboard
     *
     * @param contents the contents, which should be copied to the clipboard when the {@link Action} is executed
     */
    public void setContents(Transferable contents) {
        this.contents = contents;
        setEnabled(contents != null);
    }

    /**
     * Sets the component, under which the popup will be shown, which indicates that the text was copied to the
     * clipboard.
     *
     * @param popupParent the component to set as parent of the popup
     */
    public void setPopupParent(Component popupParent) {
        this.popupParent = popupParent;
    }

    /*
     * (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (contents != null) {
            ClipboardUtils.copy(contents);
            if (popupParent != null && lastCopyTime + POPUP_DURATION < System.currentTimeMillis()) {
                final PopupFactory popupFactory = new PopupFactory();
                final Popup popup = popupFactory.getPopup(popupParent, popupContent,
                    popupParent.getLocationOnScreen().x,
                    popupParent.getLocationOnScreen().y + popupParent.getHeight() + 2);
                popup.show();
                Timer timer = new Timer();
                timer.schedule(new TimerPopupTask(popup), POPUP_DURATION);
            }
            lastCopyTime = System.currentTimeMillis();
        }
    }

    /**
     * A timer task to hide the popup
     */
    private static class TimerPopupTask extends TimerTask {
        private final Popup popup;

        public TimerPopupTask(final Popup popup) {
            this.popup = popup;
        }

        @Override
        public void run() {
            GuiHelper.runInEDT(this.popup::hide);
        }
    }
}
