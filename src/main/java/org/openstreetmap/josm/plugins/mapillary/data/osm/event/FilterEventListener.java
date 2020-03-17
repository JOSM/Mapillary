// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.osm.event;

import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.data.osm.FilterMatcher;
import org.openstreetmap.josm.data.osm.FilterWorker;
import org.openstreetmap.josm.data.osm.search.SearchParseError;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.tools.Logging;

/**
 * This class allows layers to listen for changes to the filter dialog, and have the filters applied to an arbitrary
 * dataset.
 *
 * @author Taylor Smock
 */
public class FilterEventListener implements TableModelListener {
  private final DataSet data;
  public final FilterMatcher matcher;
  public FilterEventListener(DataSet data) {
    this.data = data;
    matcher = new FilterMatcher();
  }

  @Override
  public void tableChanged(TableModelEvent e) {
    updateAndRunFilters();
  }

  public void updateAndRunFilters() {
    matcher.reset();
    for (Filter filter : MainApplication.getMap().filterDialog.getFilterModel().getFilters()) {
      try {
        matcher.add(filter);
      } catch (SearchParseError e1) {
        Logging.error(e1);
      }
    }
    FilterWorker.executeFilters(data.allNonDeletedPrimitives(), matcher);
  }
}
