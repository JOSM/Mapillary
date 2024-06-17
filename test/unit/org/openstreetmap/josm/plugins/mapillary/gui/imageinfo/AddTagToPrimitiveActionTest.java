// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.imageinfo;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import javax.swing.JOptionPane;

import mockit.Mock;
import mockit.MockUp;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openstreetmap.josm.TestUtils;
import org.openstreetmap.josm.data.UndoRedoHandler;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.data.imagery.street_level.IImageEntry;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.Tag;
import org.openstreetmap.josm.data.vector.VectorNode;
import org.openstreetmap.josm.gui.layer.geoimage.ImageViewerDialog;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.MapillaryNode;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.geoimage.MapillaryImageEntry;
import org.openstreetmap.josm.testutils.annotations.FullPreferences;
import org.openstreetmap.josm.testutils.mockers.JOptionPaneSimpleMocker;
import org.openstreetmap.josm.tools.I18n;

@FullPreferences
class AddTagToPrimitiveActionTest {
    private AddTagToPrimitiveAction addTagToPrimitiveAction;

    static class ImageViewerDialogMock extends MockUp<ImageViewerDialog> {
        @Mock
        public static IImageEntry<?> getCurrentImage() {
            MapillaryNode node = new MapillaryNode();
            node.setCoor(LatLon.ZERO);
            node.setOsmId(135511895288847L, 1);
            return new MapillaryImageEntry(node);
        }
    }

    @BeforeEach
    void beforeEach() {
        this.addTagToPrimitiveAction = new AddTagToPrimitiveAction("test");
    }

    /**
     * Check that tags are properly set
     */
    @Test
    void setTag() {
        TestUtils.assumeWorkingJMockit();
        new ImageViewerDialogMock();
        assertFalse(this.addTagToPrimitiveAction.isEnabled());

        this.addTagToPrimitiveAction.setTag(new Tag("mapillary", "tag"));
        assertFalse(this.addTagToPrimitiveAction.isEnabled());

        final Node iPrimitive = new Node(LatLon.ZERO);
        TestUtils.addFakeDataSet(iPrimitive);
        assertDoesNotThrow(() -> this.addTagToPrimitiveAction.actionPerformed(null));

        this.addTagToPrimitiveAction.setTarget(iPrimitive);
        assertTrue(this.addTagToPrimitiveAction.isEnabled());
        assertFalse(iPrimitive.hasKey("mapillary"));
        assertDoesNotThrow(() -> this.addTagToPrimitiveAction.actionPerformed(null));
        assertEquals("tag", iPrimitive.get("mapillary"));

        UndoRedoHandler.getInstance().undo();
        assertFalse(iPrimitive.hasKey("mapillary"));

        this.addTagToPrimitiveAction.setTag(new Tag("random", "tag"));
        assertTrue(this.addTagToPrimitiveAction.isEnabled());
        assertFalse(iPrimitive.hasKey("random"));
        assertDoesNotThrow(() -> this.addTagToPrimitiveAction.actionPerformed(null));
        assertEquals("tag", iPrimitive.get("random"));

        UndoRedoHandler.getInstance().undo();
        assertFalse(iPrimitive.hasKey("random"));

        this.addTagToPrimitiveAction.setTag(null);
        assertDoesNotThrow(() -> this.addTagToPrimitiveAction.actionPerformed(null));
        assertTrue(iPrimitive.getKeys().isEmpty());
        assertFalse(UndoRedoHandler.getInstance().hasUndoCommands());
    }

    @Test
    void setTarget() {
        TestUtils.assumeWorkingJMockit();
        new ImageViewerDialogMock();
        final Node one = new Node(LatLon.ZERO);
        TestUtils.addFakeDataSet(one);
        final VectorNode two = new VectorNode("setTarget");
        two.setCoor(LatLon.ZERO);
        final Tag tag = new Tag("highway", "stop");

        assertFalse(this.addTagToPrimitiveAction.isEnabled());
        this.addTagToPrimitiveAction.setTarget(one);
        assertFalse(this.addTagToPrimitiveAction.isEnabled());
        assertTrue(one.getKeys().isEmpty());
        assertTrue(two.getKeys().isEmpty());

        assertDoesNotThrow(() -> this.addTagToPrimitiveAction.actionPerformed(null));
        assertTrue(one.getKeys().isEmpty());
        assertTrue(two.getKeys().isEmpty());

        this.addTagToPrimitiveAction.setTag(tag);
        assertTrue(this.addTagToPrimitiveAction.isEnabled());
        assertDoesNotThrow(() -> this.addTagToPrimitiveAction.actionPerformed(null));
        assertEquals(tag.getValue(), one.get(tag.getKey()));
        assertTrue(two.getKeys().isEmpty());

        UndoRedoHandler.getInstance().undo();
        assertTrue(one.getKeys().isEmpty());
        assertTrue(two.getKeys().isEmpty());

        this.addTagToPrimitiveAction.setTarget(two);
        assertDoesNotThrow(() -> this.addTagToPrimitiveAction.actionPerformed(null));
        // Only OsmPrimitive currently supports UndoRedoHandler
        assertFalse(UndoRedoHandler.getInstance().hasUndoCommands());
        assertEquals(tag.getValue(), two.get(tag.getKey()));
        assertTrue(one.getKeys().isEmpty());
    }

    @Test
    void testConfirmDialog() {
        TestUtils.assumeWorkingJMockit();
        new ImageViewerDialogMock();
        JOptionPaneSimpleMocker jOptionPaneSimpleMocker = new JOptionPaneSimpleMocker();
        final Tag tag = new Tag("highway", "residential");
        final Tag original_tag = new Tag("highway", "motorway");
        final Node target = new Node(LatLon.ZERO);
        target.put(original_tag);
        TestUtils.addFakeDataSet(target);
        String expected = "<html>"
            + I18n.tr("A tag with key <i>{0}</i> is already present on the selected OSM object.", tag.getKey()) + "<br>"
            + I18n.tr("Do you really want to replace the current value <i>{0}</i> with the new value <i>{1}</i>?",
                target.get(tag.getKey()), tag.getValue())
            + "</html>";

        this.addTagToPrimitiveAction.setTag(tag);
        this.addTagToPrimitiveAction.setTarget(target);

        jOptionPaneSimpleMocker.getMockResultMap().put(expected, JOptionPane.NO_OPTION);
        assertDoesNotThrow(() -> this.addTagToPrimitiveAction.actionPerformed(null));
        assertEquals(original_tag.getValue(), target.get(original_tag.getKey()));
        assertEquals(1, target.getNumKeys());

        jOptionPaneSimpleMocker.getMockResultMap().put(expected, JOptionPane.CLOSED_OPTION);
        assertDoesNotThrow(() -> this.addTagToPrimitiveAction.actionPerformed(null));
        assertEquals(original_tag.getValue(), target.get(original_tag.getKey()));
        assertEquals(1, target.getNumKeys());

        jOptionPaneSimpleMocker.getMockResultMap().put(expected, JOptionPane.YES_OPTION);
        assertDoesNotThrow(() -> this.addTagToPrimitiveAction.actionPerformed(null));
        assertEquals(tag.getValue(), target.get(original_tag.getKey()));
        assertEquals(1, target.getNumKeys());

        jOptionPaneSimpleMocker.getMockResultMap().put(expected, JOptionPane.CANCEL_OPTION);
        assertDoesNotThrow(() -> this.addTagToPrimitiveAction.actionPerformed(null));
        assertEquals(tag.getValue(), target.get(original_tag.getKey()));
        assertEquals(1, target.getNumKeys());
    }
}
