// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.preferences.display;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.GridBagLayout;
import java.util.Collection;

import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.gui.widgets.JosmComboBox;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayerDrawTypes;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryProperties;
import org.openstreetmap.josm.tools.GBC;

/**
 * A class for setting how Mapillary traces are shown
 */
public class MapillaryGPXSettingsPanel extends JPanel {
    private static final long serialVersionUID = 360735482359208499L;

    private final JRadioButton colorTypeDefault = new JRadioButton(tr("Mapillary default"));
    private final JRadioButton colorTypeVelocity = new JRadioButton(tr("Velocity (red = slow, green = fast)"));
    private final JRadioButton colorTypeDirection = new JRadioButton(
        tr("Track Direction (red = west, yellow = north, green = east, blue = south)"));
    private final JRadioButton colorTypeTrackTime = new JRadioButton(tr("Track date"));
    private final JosmComboBox<String> colorTypeVelocityTune = new JosmComboBox<>(
        new String[] { tr("Car"), tr("Bicycle"), tr("Foot") });
    private final Collection<MapillaryLayer> layers;

    /**
     * Create a new settings panel
     *
     * @param layers The layers to invalidate on save
     */
    public MapillaryGPXSettingsPanel(Collection<MapillaryLayer> layers) {
        super(new GridBagLayout());
        this.layers = layers;
        // colorTracks
        ButtonGroup colorGroup = new ButtonGroup();
        colorGroup.add(colorTypeDefault);
        colorGroup.add(colorTypeVelocity);
        colorGroup.add(colorTypeDirection);
        colorGroup.add(colorTypeTrackTime);

        colorTypeDefault.setToolTipText(tr("Default"));
        colorTypeVelocity.setToolTipText(tr("Colors points and track segments by velocity."));
        colorTypeDirection.setToolTipText(tr("Colors points and track segments by direction."));
        colorTypeTrackTime.setToolTipText(tr("Colors points and track segments by its timestamp."));

        // color Tracks by Velocity Tune
        colorTypeVelocityTune.setToolTipText(tr("Allows to tune the track coloring for different average speeds."));

        add(Box.createVerticalGlue(), GBC.eol().insets(0, 20, 0, 0));

        add(new JLabel(tr("Track and Point Coloring")), GBC.eol().insets(20, 0, 0, 0));
        add(colorTypeDefault, GBC.eol().insets(40, 0, 0, 0));
        // add(colorTypeVelocity, GBC.std().insets(40, 0, 0, 0));
        // add(colorTypeVelocityTune, GBC.eop().fill(GBC.HORIZONTAL).insets(5, 0, 0, 5));
        add(colorTypeDirection, GBC.eol().insets(40, 0, 0, 0));
        add(colorTypeTrackTime, GBC.eol().insets(40, 0, 0, 0));

        switch (MapillaryProperties.MAPILLARY_LAYER_DRAW_TYPE.get()) {
        case DEFAULT:
            colorGroup.setSelected(colorTypeDefault.getModel(), true);
            break;
        case VELOCITY_CAR:
            colorGroup.setSelected(colorTypeVelocity.getModel(), true);
            colorTypeVelocityTune.setSelectedIndex(0); // car
            break;
        case VELOCITY_BIKE:
            colorGroup.setSelected(colorTypeVelocity.getModel(), true);
            colorTypeVelocityTune.setSelectedIndex(1); // bike
            break;
        case VELOCITY_FOOT:
            colorGroup.setSelected(colorTypeVelocity.getModel(), true);
            colorTypeVelocityTune.setSelectedIndex(2); // foot
            break;
        case DIRECTION:
            colorGroup.setSelected(colorTypeDirection.getModel(), true);
            break;
        case DATE:
            colorGroup.setSelected(colorTypeTrackTime.getModel(), true);
        }
    }

    /**
     * Save to preferences
     */
    public void savePreferences() {
        if (colorTypeDefault.isSelected()) {
            MapillaryProperties.MAPILLARY_LAYER_DRAW_TYPE.put(MapillaryLayerDrawTypes.DEFAULT);
        } else if (colorTypeVelocity.isSelected()) {
            switch (colorTypeVelocityTune.getSelectedIndex()) {
            default:
            case 0: // car
                MapillaryProperties.MAPILLARY_LAYER_DRAW_TYPE.put(MapillaryLayerDrawTypes.VELOCITY_CAR);
                break;
            case 1: // bike
                MapillaryProperties.MAPILLARY_LAYER_DRAW_TYPE.put(MapillaryLayerDrawTypes.VELOCITY_BIKE);
                break;
            case 2: // foot
                MapillaryProperties.MAPILLARY_LAYER_DRAW_TYPE.put(MapillaryLayerDrawTypes.VELOCITY_FOOT);
                break;
            }
        } else if (colorTypeDirection.isSelected()) {
            MapillaryProperties.MAPILLARY_LAYER_DRAW_TYPE.put(MapillaryLayerDrawTypes.DIRECTION);
        } else if (colorTypeTrackTime.isSelected()) {
            MapillaryProperties.MAPILLARY_LAYER_DRAW_TYPE.put(MapillaryLayerDrawTypes.DATE);
        }
        this.layers.forEach(Layer::invalidate);
    }
}
