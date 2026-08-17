// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URI;

import javax.swing.JOptionPane;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.Notification;
import org.openstreetmap.josm.plugins.mapillary.MapillaryPlugin;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.pointcloud.MapillaryPointCloudLayer;
import org.openstreetmap.josm.plugins.mapillary.oauth.OAuthUtils;
import org.openstreetmap.josm.plugins.mapillary.spi.preferences.MapillaryConfig;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

/**
 * An action to add point clouds as aerial imagery
 */
public class MapillaryPointCloudAction extends JosmAction {
    /**
     * Create a new action
     */
    public MapillaryPointCloudAction() {
        super(tr("Mapillary Point Cloud (experimental)"),
            new ImageProvider(MapillaryPlugin.LOGO).setSize(ImageProvider.ImageSizes.DEFAULT),
            tr("Open Mapillary Point Cloud layer"), Shortcut.registerShortcut("mapillary:pointcloud",
                tr("Mapillary Point Cloud"), KeyEvent.CHAR_UNDEFINED, Shortcut.NONE),
            false, "mapillary:pointcloud", true);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        final var selected = MapillaryLayer.getInstance().getImage();
        try {
            var obj = OAuthUtils
                .getWithHeader(URI.create(MapillaryConfig.getUrls().getImageInformation(selected.getUniqueId(),
                    MapillaryImageUtils.ImageProperties.SFM_CLUSTER)))
                .getJsonObject(MapillaryImageUtils.ImageProperties.SFM_CLUSTER.toString());
            if (obj.containsKey("id") && obj.containsKey("url")) {
                MainApplication.getLayerManager()
                    .addLayer(new MapillaryPointCloudLayer(obj.getString("id"), obj.getString("url"), selected));
            } else {
                new Notification(tr("Could not find point cloud for Mapillary image {0}", selected.getUniqueId()))
                    .setIcon(JOptionPane.ERROR_MESSAGE).show();
            }
        } catch (IOException ioException) {
            throw new UncheckedIOException(ioException);
        }
    }

    @Override
    protected void updateEnabledState() {
        setEnabled(MapillaryLayer.hasInstance() && MapillaryLayer.getInstance().getImage() != null || true);
    }
}
