// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.data.mapillary;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.util.Collection;
import java.util.Collections;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.Filter;
import org.openstreetmap.josm.data.osm.IFilterablePrimitive;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.OsmData;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.vector.VectorDataSet;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.testutils.annotations.BasicPreferences;

/**
 * Test class for {@link MapillaryFilterModel}
 *
 * @author Taylor Smock
 */
@BasicPreferences
class MapillaryFilterModelTest {
    static Stream<Arguments> testExecuteFilters() {
        final VectorNode vectorNode = new VectorNode("testExecuteFilters");
        vectorNode.setCoor(LatLon.ZERO);
        final Node node = new Node(LatLon.ZERO);
        node.setOsmId(1, 1);
        return Stream.of(Arguments.of(new DataSet(), node), Arguments.of(new VectorDataSet(), vectorNode));
    }

    @ParameterizedTest(name = "[{index}] {0}")
    @MethodSource
    <O extends IPrimitive & IFilterablePrimitive> void testExecuteFilters(final OsmData<O, ?, ?, ?> dataSet,
        final O primitive) {
        assumeTrue(primitive instanceof OsmPrimitive, "Hiding filter ");
        final MapillaryFilterModel mapillaryFilterModel = new MapillaryFilterModel();
        final Filter filter = new Filter(Filter.readFromString("type:node and highway=crossing"));
        filter.enable = true;
        mapillaryFilterModel.addFilter(filter);
        dataSet.addPrimitive(primitive);

        assertTrue(dataSet.allPrimitives().stream().noneMatch(IPrimitive::isDisabled));
        mapillaryFilterModel.executeFilters(dataSet);
        assertTrue(dataSet.allPrimitives().stream().noneMatch(IPrimitive::isDisabled));

        dataSet.allPrimitives().iterator().next().put("highway", "crossing");
        assertTrue(dataSet.allPrimitives().stream().noneMatch(IPrimitive::isDisabled));
        mapillaryFilterModel.executeFilters(dataSet);
        assertEquals(1, dataSet.allPrimitives().stream().filter(IPrimitive::isDisabled).count());
    }

    static Stream<Arguments> testExecuteFiltersNull() {
        final MapillaryFilterModel mapillaryFilterModel = new MapillaryFilterModel();
        return Stream.of(
            Arguments.of("Null dataset", (Runnable) () -> mapillaryFilterModel.executeFilters((DataSet) null)),
            Arguments.of("Null collection",
                (Runnable) () -> mapillaryFilterModel.executeFilters((Collection<? extends OsmPrimitive>) null)),
            Arguments.of("Null dataset and collection",
                (Runnable) () -> mapillaryFilterModel.executeFilters(null, null)),
            Arguments.of("Null dataset and non-null collection",
                (Runnable) () -> mapillaryFilterModel.executeFilters(null, Collections.emptyList())),
            Arguments.of("Non-null dataset and null collection",
                (Runnable) () -> mapillaryFilterModel.executeFilters(new DataSet(), null)));
    }

    @ParameterizedTest(name = "[{index}] {0}")
    @MethodSource
    void testExecuteFiltersNull(final String message, final Runnable supplier) {
        assertDoesNotThrow(supplier::run, message);
    }
}
