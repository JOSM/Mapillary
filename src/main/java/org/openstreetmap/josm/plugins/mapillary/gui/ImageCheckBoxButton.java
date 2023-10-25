// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.Collection;
import java.util.stream.Stream;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryExpertFilterDialog;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.GBC;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * Buttons for filtering detections
 *
 * @author Taylor Smock
 */
public class ImageCheckBoxButton extends JPanel implements Destroyable, TableModelListener {
    private static final long serialVersionUID = 3659377445718790107L;
    private final transient Filter filter;
    private final JCheckBox jcheckbox;
    private final JButton image;
    private final String detection;
    private final String[] splitName;
    private final ObjectDetections[] detections;
    private final ImageProvider icon;

    public ImageCheckBoxButton(@Nullable ImageProvider icon, @Nonnull String detection,
        @Nonnull ObjectDetections... detections) {
        super(new GridBagLayout());
        this.detection = detection;
        this.detections = detections.clone();
        MapillaryExpertFilterDialog.getInstance().getFilterModel().addTableModelListener(this);
        splitName = detection.split("--", -1);
        image = new JButton();
        this.icon = icon;
        add(image, GBC.std().anchor(GridBagConstraints.WEST));
        String name = splitName[splitName.length - 1].replace(".svg", "");
        if (name.matches("g\\d+")) {
            name = splitName[splitName.length - 2];
        }

        name = name.replace("-", " ");
        String filterText = "value:\"" + detection + "\"";
        filter = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().parallelStream()
            .filter(f -> f.text.equals(filterText)).findAny().orElse(makeNewFilter());
        filter.text = filterText;

        jcheckbox = new JCheckBox(name);
        add(jcheckbox, GBC.eol().fill());
        image.addActionListener(l -> {
            updateCheckBox(jcheckbox, filter);
            updateTooltips(detection, image, jcheckbox);
        });
        jcheckbox.addActionListener(l -> {
            updateFilters(jcheckbox, filter);
            updateTooltips(detection, image, jcheckbox);
        });

        updateTooltips(detection, image, jcheckbox);

        tableChanged(null);
    }

    @Override
    public void paint(Graphics g) {
        if (this.image.getIcon() == null) {
            if (this.icon == null) {
                this.image.setIcon(ObjectDetections.NO_ICON);
            } else {
                this.icon.getAsync(i -> GuiHelper.runInEDT(() -> this.image.setIcon(i)));
            }
        }
        super.paint(g);
    }

    private static void updateTooltips(String detection, JButton image, JCheckBox jcheckbox) {
        image.setToolTipText(tr("{0}: {1}", detection, jcheckbox.isSelected() ? tr("hidden") : tr("shown")));
        jcheckbox.setToolTipText(image.getToolTipText());
    }

    private static Filter makeNewFilter() {
        Filter filter = new Filter();
        filter.hiding = true;
        return filter;
    }

    private static void updateCheckBox(JCheckBox jcheckbox, Filter filter) {
        jcheckbox.setSelected(!jcheckbox.isSelected());
        updateFilters(jcheckbox, filter);
    }

    private static void updateFilters(JCheckBox jcheckbox, Filter filter) {
        int index = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().indexOf(filter);
        if (jcheckbox.isSelected()) {
            filter.enable = true;
        } else if (!jcheckbox.isSelected()) {
            filter.enable = false;
        }
        if (index < 0 && filter.enable) {
            MapillaryExpertFilterDialog.getInstance().getFilterModel().addFilter(filter);
        } else if (index >= 0 && !filter.enable) {
            MapillaryExpertFilterDialog.getInstance().getFilterModel().removeFilter(index);
        }
    }

    @Override
    public void destroy() {
        MapillaryExpertFilterDialog.getInstance().getFilterModel().removeTableModelListener(this);
    }

    @Override
    public void tableChanged(TableModelEvent e) {
        if (filter == null) {
            return;
        }
        final int index = MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters().indexOf(filter);
        if (index < 0) {
            filter.enable = false;
        }
        if (e != null && (e.getFirstRow() > index || e.getLastRow() < index) && e.getType() != TableModelEvent.DELETE) {
            return;
        }
        if (SwingUtilities.isEventDispatchThread()) {
            jcheckbox.setSelected(filter.enable);
        } else {
            // Invoke in GUI, but don't wait for it (may be blocked)
            GuiHelper.runInEDT(() -> jcheckbox.setSelected(filter.enable));
        }
    }

    /**
     * Get the name of the detection
     *
     * @return The detection name
     */
    public String getDetectionName() {
        return this.detection;
    }

    /**
     * Get the detections for this button
     *
     * @return The detections
     */
    public ObjectDetections[] getDetections() {
        return this.detections.clone();
    }

    /**
     * Check if the button is filtered
     *
     * @param searchString The string
     * @return true if the button shouldn't be visible
     */
    public boolean isFiltered(String searchString) {
        String[] searchSplit = searchString.split(" ", -1);
        return Stream.of(splitName).parallel()
            .anyMatch(n -> Stream.of(searchSplit).parallel().anyMatch(s -> s.contains(n) || n.contains(s)));
    }

    /**
     * Set the box as selected
     *
     * @param selected Set the checkbox state to the selected boolean
     */
    public void setSelected(boolean selected) {
        GuiHelper.runInEDT(() -> {
            jcheckbox.setSelected(selected);
            updateTooltips(this.detection, this.image, this.jcheckbox);
        });
        updateFilters(jcheckbox, filter);
    }

    /**
     * Check if the box is selected
     *
     * @return {@code true} if selected (see {@link JCheckBox#isSelected}).
     */
    public boolean isSelected() {
        return jcheckbox.isSelected();
    }

    /**
     * Check if the button is relevant
     *
     * @param detections The detections to filter on
     * @return {@code true} if a point object layer has it. Or if there are no point object layers.
     */
    public boolean isRelevant(Collection<String> detections) {
        return detections.contains(this.detection) || detections.stream().anyMatch(det -> det.contains(detection));
    }

    @Override
    public String toString() {
        return this.detection;
    }
}
