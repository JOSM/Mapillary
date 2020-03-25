// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.FilterMatcher;
import org.openstreetmap.josm.data.osm.FilterModel;
import org.openstreetmap.josm.data.osm.FilterWorker;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.OsmDataManager;
import org.openstreetmap.josm.data.osm.OsmPrimitive;

/**
 *
 */
public class MapillaryFilterModel extends FilterModel {
  private final FilterMatcher filterMatcher = new FilterMatcher();

  /**
   * Runs the filters on the current edit data set.
   */
  @Override
  public void executeFilters() {
    executeFilters(OsmDataManager.getInstance().getActiveDataSet());
  }

  /**
   * Runes the filters on a data set
   *
   * @param ds The dataset to run filters on
   */
  public void executeFilters(DataSet ds) {
    if (ds != null) {
      final Collection<OsmPrimitive> deselect = new HashSet<>();

      ds.beginUpdate();
      try {

        final Collection<OsmPrimitive> all = ds.allNonDeletedCompletePrimitives();

        FilterWorker.executeFilters(all, filterMatcher);

        // collect disabled and selected the primitives
        for (OsmPrimitive osm : all) {
          if (osm.isDisabled() && osm.isSelected()) {
            deselect.add(osm);
          }
        }
      } finally {
        ds.endUpdate();
      }

      if (!deselect.isEmpty()) {
        ds.clearSelection(deselect);
      }
    }
  }

  /**
   * Runs the filter on a list of primitives that are part of a specified dataset.
   *
   * @param ds         The dataset to run filters on
   * @param primitives The primitives
   */
  @Override
  public void executeFilters(Collection<? extends OsmPrimitive> primitives) {
    executeFilters(OsmDataManager.getInstance().getActiveDataSet(), primitives);
  }

  /**
   * Runs the filter on a list of primitives that are part of the edit data set.
   *
   * @param primitives The primitives
   */
  public void executeFilters(DataSet ds, Collection<? extends OsmPrimitive> primitives) {
    if (ds == null)
      return;

    List<OsmPrimitive> deselect = new ArrayList<>();

    ds.beginUpdate();
    try {
      for (int i = 0; i < 2; i++) {
        for (OsmPrimitive primitive : primitives) {
          if ((i == 0 && primitive instanceof Node) || (i == 1 && !(primitive instanceof Node))) {
            continue;
          }

          FilterWorker.executeFilters(primitive, filterMatcher);
          if (primitive.isSelected() && primitive.isDisabled()) {
            deselect.add(primitive);
          }
        }
      }
    } finally {
      ds.endUpdate();
    }

    if (!deselect.isEmpty()) {
      ds.clearSelection(deselect);
    }
  }
}
