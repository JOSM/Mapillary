// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.osm.event;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.swing.SwingUtilities;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.data.osm.FilterMatcher;
import org.openstreetmap.josm.data.osm.FilterWorker;
import org.openstreetmap.josm.data.osm.IFilterablePrimitive;
import org.openstreetmap.josm.data.osm.OsmData;
import org.openstreetmap.josm.data.osm.search.SearchParseError;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.layer.Layer;
import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryExpertFilterDialog;
import org.openstreetmap.josm.tools.Logging;

/**
 * This class allows layers to listen for changes to the filter dialog, and have the filters applied to an arbitrary
 * dataset.
 *
 * @author Taylor Smock
 */
public class FilterEventListener implements TableModelListener {
    private final Layer layer;
    private final OsmData<? extends IFilterablePrimitive, ?, ?, ?> data;
    public final FilterMatcher matcher;

    public FilterEventListener(Layer layer, OsmData<? extends IFilterablePrimitive, ?, ?, ?> data) {
        this.layer = layer;
        this.data = data;
        matcher = new FilterMatcher();
    }

    @Override
    public void tableChanged(TableModelEvent e) {
        updateAndRunFilters();
    }

    public synchronized void updateAndRunFilters() {
        matcher.reset();
        if (MainApplication.getMap() != null) {
            for (List<Filter> filters : Arrays.asList(
                MapillaryExpertFilterDialog.getInstance().getFilterModel().getFilters(),
                MainApplication.getMap().filterDialog.getFilterModel().getFilters())) {
                for (Filter filter : filters) {
                    try {
                        matcher.add(filter);
                    } catch (SearchParseError e1) {
                        Logging.error(e1);
                    }
                }
            }
        }
        FilterWorker.executeFilters(
            data.allPrimitives().stream().filter(p -> p.getReferrers().isEmpty()).collect(Collectors.toList()),
            matcher);
        SwingUtilities.invokeLater(layer::invalidate);
    }
}
