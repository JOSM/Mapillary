// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.stream.Collectors;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import javax.swing.AbstractAction;
import javax.swing.JPopupMenu;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.OsmData;
import org.openstreetmap.josm.data.osm.OsmPrimitiveType;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.dialogs.properties.PropertiesDialog;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.io.download.MapillaryDownloader;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryImageUtils;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryMapFeatureUtils;
import org.openstreetmap.josm.tools.Destroyable;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.JosmRuntimeException;
import org.openstreetmap.josm.tools.Logging;

/**
 * Listen for Mapillary keys
 *
 * @author Taylor Smock
 */
public class MapillaryKeyListener implements PopupMenuListener, Destroyable {

    private JPopupMenu menu;
    private PropertiesDialog properties;
    private final Map<JPopupMenu, List<String>> addedValues = new HashMap<>();
    private final Map<JPopupMenu, List<Component>> addedComponents = new HashMap<>();

    public MapillaryKeyListener(PropertiesDialog properties, JPopupMenu menu) {
        this.menu = menu;
        this.properties = properties;
        menu.addPopupMenuListener(this);
    }

    @Override
    public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
        JPopupMenu popup = (JPopupMenu) e.getSource();
        properties.visitSelectedProperties((primitive, key, value) -> addAction(popup, key, value));
    }

    @Override
    public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
        JPopupMenu popup = (JPopupMenu) e.getSource();
        if (addedValues.containsKey(popup) && addedComponents.containsKey(popup)) {
            addedValues.remove(popup);
            addedComponents.get(popup).forEach(popup::remove);
            addedComponents.remove(popup);
        }
    }

    @Override
    public void popupMenuCanceled(PopupMenuEvent e) {
        // Do nothing
    }

    private void addAction(JPopupMenu popup, String key, String value) {
        if (value == null || value.trim().isEmpty() || !MapillaryLayer.hasInstance()) {
            return;
        }
        String lowerKey = key.toLowerCase(Locale.ENGLISH);
        if (Arrays.asList("mapillary", "mapillary:image").contains(lowerKey)) {
            List<String> strings = addedValues.computeIfAbsent(popup, p -> new ArrayList<>());
            List<Component> components = addedComponents.computeIfAbsent(popup, p -> new ArrayList<>());
            if (!strings.contains(value)) {
                strings.add(value);
                components.add(popup.add(new MapillaryImageKeyAction(value)));
            }
            // TODO only if not filtering map features
        } else if ("mapillary:map_feature".equals(lowerKey)
            && !MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).isEmpty()) {
            List<String> strings = addedValues.computeIfAbsent(popup, p -> new ArrayList<>());
            List<Component> components = addedComponents.computeIfAbsent(popup, p -> new ArrayList<>());
            if (!strings.contains(value)) {
                strings.add(value);
                components.add(popup.add(new MapillaryDetectionKeyAction(value)));
            }
        }
    }

    @Override
    public void destroy() {
        if (menu != null) {
            menu.removePopupMenuListener(this);
        }
        properties = null;
        menu = null;
    }

    static class MapillaryImageKeyAction extends AbstractAction {
        private static final long serialVersionUID = -4129937925620244251L;
        private final String imageKey;

        MapillaryImageKeyAction(String imageKey) {
            new ImageProvider("mapillary-logo").getResource().attachImageIcon(this, true);
            this.imageKey = imageKey;
            putValue(NAME, tr("Select Mapillary Image ({0})", this.imageKey));
            putValue(SHORT_DESCRIPTION, tr("Select the Mapillary Image {0} on the {1} layer (downloads if needed)",
                this.imageKey, MapillaryLayer.getInstance().getName()));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            final VectorDataSet data = MapillaryLayer.getInstance().getData();
            final Lock lock = data.getReadLock();
            final Map<Long, VectorNode> newMap;
            try {
                lock.lockInterruptibly();
                Map<Long, VectorNode> map = data.getNodes().stream()
                    .filter(image -> MapillaryImageUtils.getKey(image) != 0)
                    .collect(Collectors.toMap(MapillaryImageUtils::getKey, i -> i));
                long[] missingImages = Stream.of(this.imageKey.split(";", 0)).mapToLong(Long::parseLong)
                    .filter(i -> !map.containsKey(i)).toArray();
                map.clear(); // deallocate map
                if (missingImages.length != 0) {
                    MapillaryDownloader.downloadSequences(
                        MapillaryDownloader.downloadImages(missingImages).keySet().toArray(new String[0]));
                }
                newMap = data.getNodes().stream().filter(image -> MapillaryImageUtils.getKey(image) != 0)
                    .collect(Collectors.toMap(MapillaryImageUtils::getKey, i -> i));
            } catch (InterruptedException exception) {
                Logging.error(exception);
                Thread.currentThread().interrupt();
                throw new JosmRuntimeException(exception);
            } finally {
                lock.unlock();
            }
            GuiHelper.runInEDT(
                () -> MapillaryLayer.getInstance().getData().setSelected(Stream.of(this.imageKey.split(";", 0))
                    .mapToLong(Long::parseLong).mapToObj(newMap::get).collect(Collectors.toSet())));
        }
    }

    static class MapillaryDetectionKeyAction extends AbstractAction {
        private static final long serialVersionUID = -4129937925620244252L;
        private final long[] detection;

        MapillaryDetectionKeyAction(String detection) {
            new ImageProvider("mapillary-logo").getResource().attachImageIcon(this, true);
            this.detection = Stream.of(detection.split(";", 0)).mapToLong(Long::valueOf).toArray();
            putValue(NAME, tr("Select Mapillary Detection ({0})", detection));
            putValue(SHORT_DESCRIPTION, tr("Select the Mapillary Detection {0}", detection));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            final Collection<IPrimitive> detections = MainApplication.getLayerManager()
                .getLayersOfType(PointObjectLayer.class).stream().map(PointObjectLayer::getData)
                .flatMap(d -> d.allNonDeletedPrimitives().stream()).filter(p -> p.getId() != 0)
                .filter(p -> LongStream.of(this.detection).anyMatch(d -> d == p.getId())).collect(Collectors.toSet());
            final long[] images = detections.stream()
                .flatMapToLong(d -> LongStream.of(MapillaryMapFeatureUtils.getImageIds(d))).distinct().toArray();

            final VectorDataSet data = MapillaryLayer.getInstance().getData();
            final long[] missingImages;
            final Lock lock = MapillaryLayer.getInstance().getData().getReadLock();
            try {
                lock.lockInterruptibly();
                missingImages = LongStream.of(images)
                    .filter(i -> data.getPrimitiveById(i, OsmPrimitiveType.NODE) == null).toArray();
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
                throw new JosmRuntimeException(exception);
            } finally {
                lock.unlock();
            }
            if (missingImages.length != 0) {
                MapillaryDownloader.downloadSequences(
                    MapillaryDownloader.downloadImages(missingImages).keySet().toArray(new String[0]));
            }
            Map<OsmData<?, ?, ?, ?>, List<IPrimitive>> selections = detections.stream()
                .collect(Collectors.groupingBy(IPrimitive::getDataSet));
            GuiHelper.runInEDT(() -> {
                for (Map.Entry<OsmData<?, ?, ?, ?>, List<IPrimitive>> selection : selections.entrySet()) {
                    selection.getKey().setSelected(selection.getValue());
                }
            });
        }
    }
}
