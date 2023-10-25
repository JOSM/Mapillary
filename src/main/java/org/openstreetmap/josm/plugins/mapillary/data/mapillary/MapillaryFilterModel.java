// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.stream.Collectors;

import jakarta.annotation.Nullable;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.data.osm.FilterMatcher;
import org.openstreetmap.josm.data.osm.FilterModel;
import org.openstreetmap.josm.data.osm.FilterWorker;
import org.openstreetmap.josm.data.osm.IFilterablePrimitive;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.OsmData;
import org.openstreetmap.josm.data.osm.OsmDataManager;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.search.SearchParseError;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.tools.Logging;

/**
 * A model to filter data
 */
public class MapillaryFilterModel extends FilterModel {
    /**
     * Runs the filters on the current edit data set.
     */
    @Override
    public void executeFilters() {
        MainApplication.getLayerManager().getLayersOfType(PointObjectLayer.class).stream()
            .map(PointObjectLayer::getData).forEach(this::executeFilters);
    }

    /**
     * Runs the filters on a data set
     *
     * @param <O> The base primitive type of the dataset
     * @param ds The dataset to run filters on
     */
    public <O extends IPrimitive & IFilterablePrimitive> void executeFilters(OsmData<O, ?, ?, ?> ds) {
        if (ds != null) {
            this.executeFilters(ds, ds.allNonDeletedCompletePrimitives());
        }
    }

    /**
     * Runs the filter on a list of primitives that are part of a specified dataset.
     *
     * @param primitives The primitives
     */
    @Override
    public void executeFilters(@Nullable Collection<? extends OsmPrimitive> primitives) {
        executeFilters(OsmDataManager.getInstance().getActiveDataSet(), primitives);
    }

    /**
     * Runs the filter on a list of primitives that are part of the edit data set.
     *
     * @param ds The dataset with the primitives
     * @param primitives The primitives
     * @param <O> The base primitive type for the dataset
     */
    public <O extends IPrimitive & IFilterablePrimitive> void executeFilters(@Nullable OsmData<O, ?, ?, ?> ds,
        @Nullable Collection<? extends O> primitives) {
        if (ds == null || primitives == null)
            return;

        final FilterMatcher filterMatcher = new FilterMatcher();
        for (Filter filter : super.getFilters()) {
            try {
                filterMatcher.add(filter);
            } catch (SearchParseError e) {
                // This should never happen, as the other filter should throw
                Logging.error(e);
            }
        }

        Lock lock = null;
        if (ds instanceof DataSet) {
            ((DataSet) ds).beginUpdate();
        } else {
            lock = ds.getReadLock();
        }

        Set<IPrimitive> deselect;
        try {
            if (lock != null) {
                lock.lockInterruptibly();
            }
            // The executeFilters method properly orders primitives
            if (FilterWorker.executeFilters(primitives, filterMatcher)) {
                deselect = primitives.parallelStream().filter(IPrimitive::isSelected).filter(IPrimitive::isDisabled)
                    .collect(Collectors.toSet());
            } else {
                deselect = Collections.emptySet();
            }
        } catch (InterruptedException e) {
            Logging.error(e);
            Thread.currentThread().interrupt();
            deselect = Collections.emptySet();
        } finally {
            if (ds instanceof DataSet) {
                ((DataSet) ds).endUpdate();
            }
            if (lock != null) {
                lock.unlock();
            }
        }

        if (!deselect.isEmpty()) {
            ds.clearSelection(deselect);
        }
    }
}
