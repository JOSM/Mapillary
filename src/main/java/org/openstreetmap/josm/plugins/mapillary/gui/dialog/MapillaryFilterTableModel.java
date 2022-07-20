// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.dialog;

import static org.openstreetmap.josm.tools.I18n.trc;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.table.AbstractTableModel;

import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.data.osm.FilterWorker;
import org.openstreetmap.josm.data.osm.IFilterablePrimitive;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.search.SearchParseError;
import org.openstreetmap.josm.gui.autofilter.AutoFilterManager;
import org.openstreetmap.josm.gui.dialogs.FilterTableModel;
import org.openstreetmap.josm.gui.util.GuiHelper;
import org.openstreetmap.josm.gui.util.SortableTableModel;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryFilterModel;
import org.openstreetmap.josm.tools.Logging;

/**
 * A model for arbitrary filters
 *
 * @author Taylor Smock
 */
public class MapillaryFilterTableModel extends AbstractTableModel implements SortableTableModel<Filter> {
    private static final long serialVersionUID = -5965135291510758363L;

    /**
     * The filter enabled column
     */
    public static final int COL_ENABLED = FilterTableModel.COL_ENABLED;
    /**
     * The column indicating if the filter is hiding.
     */
    public static final int COL_HIDING = FilterTableModel.COL_HIDING;
    /**
     * The column that displays the filter text
     */
    public static final int COL_TEXT = FilterTableModel.COL_TEXT;
    /**
     * The column to invert the filter
     */
    public static final int COL_INVERTED = FilterTableModel.COL_INVERTED;

    /**
     * The filter model
     */
    private final MapillaryFilterModel model = new MapillaryFilterModel();

    /**
     * The selection model
     */
    final ListSelectionModel selectionModel;

    private boolean manyUpdates;

    /**
     * Constructs a new {@code FilterTableModel}.
     *
     * @param listSelectionModel selection model
     */
    public MapillaryFilterTableModel(ListSelectionModel listSelectionModel) {
        this.selectionModel = listSelectionModel;
        loadPrefs();
    }

    /**
     * Check if we are not doing many changes
     *
     * @return {@code true} if we are not doing many changes
     */
    public boolean notManyChanges() {
        return !this.manyUpdates;
    }

    private void updateFilters() {
        executeFilters(true);
    }

    /**
     * Runs the filters on the current edit data set, if any. Does nothing if no filter is enabled.
     */
    public void executeFilters() {
        executeFilters(false);
    }

    /**
     * Runs the filter on a list of primitives that are part of the edit data set, if any. Does nothing if no filter is
     * enabled.
     *
     * @param primitives The primitives
     */
    public <O extends IPrimitive & IFilterablePrimitive> void executeFilters(Collection<O> primitives) {
        executeFilters(primitives, false);
    }

    /**
     * Runs the filters on the current edit data set, if any.
     *
     * @param force force execution of filters even if no filter is enabled. Useful to reset state after change of
     *        filters
     * @since 14206
     */
    public synchronized void executeFilters(boolean force) {
        if (force || model.hasFilters()) {
            model.executeFilters();
        }
    }

    /**
     * Runs the filter on a list of primitives that are part of the edit data set, if any.
     *
     * @param <O> The base primitive type
     * @param force force execution of filters even if no filter is enabled. Useful to reset state after change of
     *        filters
     * @param primitives The primitives
     * @since 14206
     */
    public <O extends IPrimitive & IFilterablePrimitive> void executeFilters(Collection<O> primitives, boolean force) {
        if (AutoFilterManager.getInstance().getCurrentAutoFilter() == null && (force || model.hasFilters())) {
            try {
                FilterWorker.executeFilters(primitives, model.getFilters().toArray(new Filter[0]));
            } catch (SearchParseError searchParseError) {
                Logging.error(searchParseError);
            }
        }
    }

    private synchronized void loadPrefs() {
        model.loadPrefs("mapillary.filters.entries");
        cleanup();
    }

    private synchronized void savePrefs() {
        model.savePrefs("mapillary.filters.entries");
    }

    private synchronized void cleanup() {
        List<Filter> originalFilters = getFilters();
        List<Integer> toRemove = getFilters().parallelStream().filter(f -> f.text.isEmpty())
            .map(originalFilters::indexOf).sorted(Collections.reverseOrder()).collect(Collectors.toList());
        for (int i : toRemove) {
            removeFilter(i);
        }
    }

    /**
     * Adds a new filter to the filter list.
     *
     * @param filter The new filter
     */
    public synchronized void addFilter(Filter filter) {
        if (model.addFilter(filter) && this.notManyChanges()) {
            savePrefs();
            updateFilters();
            int size = model.getFiltersCount();
            fireTableRowsInserted(size - 1, size - 1);
        }
    }

    @Override
    public boolean doMove(int delta, int... selectedRows) {
        return model.moveFilters(delta, selectedRows);
    }

    @Override
    public boolean move(int delta, int... selectedRows) {
        if (!SortableTableModel.super.move(delta, selectedRows))
            return false;
        if (this.notManyChanges()) {
            savePrefs();
            updateFilters();
            int rowIndex = selectedRows[0];
            if (delta < 0) {
                fireTableRowsUpdated(rowIndex + delta, rowIndex);
            } else if (delta > 0) {
                fireTableRowsUpdated(rowIndex, rowIndex + delta);
            }
        }
        return true;
    }

    /**
     * Removes the filter that is displayed in the given row
     *
     * @param rowIndex The index of the filter to remove
     */
    public synchronized void removeFilter(int rowIndex) {
        if (rowIndex >= 0 && model.getFiltersCount() > rowIndex && model.removeFilter(rowIndex) != null
            && this.notManyChanges()) {
            savePrefs();
            updateFilters();
            GuiHelper.runInEDT(() -> fireTableRowsDeleted(rowIndex, rowIndex));
        }
    }

    /**
     * See {@link MapillaryFilterModel#clearFilters()}
     */
    public synchronized void clearFilters() {
        this.model.clearFilters();
    }

    @Override
    public Filter setValue(int rowIndex, Filter filter) {
        Filter result = model.setValue(rowIndex, filter);
        if (this.notManyChanges()) {
            savePrefs();
            updateFilters();
            fireTableRowsUpdated(rowIndex, rowIndex);
        }
        return result;
    }

    @Override
    public Filter getValue(int rowIndex) {
        return model.getValue(rowIndex);
    }

    @Override
    public ListSelectionModel getSelectionModel() {
        return selectionModel;
    }

    @Override
    public int getRowCount() {
        return model.getFiltersCount();
    }

    @Override
    public int getColumnCount() {
        return 5;
    }

    @Override
    public String getColumnName(int column) {
        String[] names = { /* translators notes must be in front */
            /* column header: enable filter */trc("filter", "E"), /* column header: hide filter */trc("filter", "H"),
            /* column header: filter text */trc("filter", "Text"),
            /* column header: inverted filter */trc("filter", "I"),
            /* column header: filter mode */trc("filter", "M") };
        return names[column];
    }

    @Override
    public Class<?> getColumnClass(int column) {
        Class<?>[] classes = { Boolean.class, Boolean.class, String.class, Boolean.class, String.class };
        return classes[column];
    }

    /**
     * Determines if a cell is enabled.
     *
     * @param row row index
     * @param column column index
     * @return {@code true} if the cell at (row, column) is enabled
     */
    public boolean isCellEnabled(int row, int column) {
        return (model.getFiltersCount() > row && model.getValue(row).enable) || column == 0;
    }

    @Override
    public boolean isCellEditable(int row, int column) {
        return column < 4 && isCellEnabled(row, column);
    }

    @Override
    public void setValueAt(Object aValue, int row, int column) {
        if (row >= model.getFiltersCount()) {
            return;
        }
        Filter f = model.getValue(row);
        switch (column) {
        case COL_ENABLED:
            f.enable = (Boolean) aValue;
            break;
        case COL_HIDING:
            f.hiding = (Boolean) aValue;
            break;
        case COL_TEXT:
            f.text = (String) aValue;
            break;
        case COL_INVERTED:
            f.inverted = (Boolean) aValue;
            break;
        default: // Do nothing
        }
        setValue(row, f);
    }

    @Override
    public Object getValueAt(int row, int column) {
        if (row >= model.getFiltersCount()) {
            return null;
        }
        Filter f = model.getValue(row);
        switch (column) {
        case COL_ENABLED:
            return f.enable;
        case COL_HIDING:
            return f.hiding;
        case COL_TEXT:
            return f.text;
        case COL_INVERTED:
            return f.inverted;
        case 4:
            switch (f.mode) { /* translators notes must be in front */
            case replace: /* filter mode: replace */
                return trc("filter", "R");
            case add: /* filter mode: add */
                return trc("filter", "A");
            case remove: /* filter mode: remove */
                return trc("filter", "D");
            case in_selection: /* filter mode: in selection */
                return trc("filter", "F");
            default:
                Logging.warn("Unknown filter mode: " + f.mode);
            }
            break;
        default: // Do nothing
        }
        return null;
    }

    /**
     * Returns the list of filters.
     *
     * @return the list of filters
     */
    public List<Filter> getFilters() {
        return model.getFilters();
    }

    @Override
    public void sort() {
        model.sort();
        fireTableDataChanged();
    }

    @Override
    public void reverse() {
        model.reverse();
        fireTableDataChanged();
    }

    public synchronized void doManyUpdates(Runnable runnable) {
        this.manyUpdates = true;
        runnable.run();
        this.manyUpdates = false;
        savePrefs();
        updateFilters();
        SwingUtilities.invokeLater(this::fireTableDataChanged);
    }
}
