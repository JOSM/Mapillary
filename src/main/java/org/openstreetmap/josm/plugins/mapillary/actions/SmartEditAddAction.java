package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.OptionalLong;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.command.AddPrimitivesCommand;
import org.openstreetmap.josm.command.Command;
import org.openstreetmap.josm.data.UndoRedoHandler;
import org.openstreetmap.josm.data.osm.DataSet;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.osm.Node;
import org.openstreetmap.josm.data.osm.OsmPrimitive;
import org.openstreetmap.josm.data.osm.Way;
import org.openstreetmap.josm.data.vector.VectorPrimitive;
import org.openstreetmap.josm.gui.MainApplication;
import org.openstreetmap.josm.gui.tagging.presets.TaggingPreset;
import org.openstreetmap.josm.plugins.mapillary.command.AddMapillaryObjectCommand;
import org.openstreetmap.josm.plugins.mapillary.command.GenericCommand;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.AdditionalInstructions;
import org.openstreetmap.josm.plugins.mapillary.data.mapillary.ObjectDetections;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.MapillaryMapFeatureUtils;
import org.openstreetmap.josm.tools.ImageProvider;

/**
 * Add an item to the map from a {@link org.openstreetmap.josm.plugins.mapillary.gui.layer.PointObjectLayer}
 *
 * @author Taylor Smock
 */
public class SmartEditAddAction extends JosmAction {
    private final transient IPrimitive mapillaryObject;
    private final transient PointObjectLayer pointObjectLayer;
    private final ObjectDetections detection;

    public SmartEditAddAction(final PointObjectLayer pointObjectLayer, final IPrimitive primitive) {
        super(tr("Add"), new ImageProvider("dialogs", "add"), tr("Add Map Feature to OSM"), null, false, null, false);
        this.mapillaryObject = primitive;
        this.pointObjectLayer = pointObjectLayer;
        this.detection = ObjectDetections.valueOfMapillaryValue(mapillaryObject.get("value"));
        this.updateEnabledState();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        addMapillaryPrimitiveToOsm(this.mapillaryObject, this.detection);
        this.updateEnabledState();
    }

    @Override
    public void updateEnabledState() {
        this.setEnabled(!this.detection.getTaggingPresets().isEmpty());
    }

    /**
     * Add a Mapillary Object Detection Primitive to OSM
     *
     * @param mapillaryObject The primitive to add to OSM
     * @param detection The detections for the primitive
     */
    void addMapillaryPrimitiveToOsm(final IPrimitive mapillaryObject, final ObjectDetections detection) {
        this.pointObjectLayer.hideWindow(mapillaryObject);
        final Collection<TaggingPreset> presets = detection.getTaggingPresets();
        final DataSet dataSet = MainApplication.getLayerManager().getActiveDataSet();
        if (dataSet != null && !dataSet.isLocked() && presets.size() == 1) {
            final TaggingPreset preset = presets.iterator().next();
            final List<OsmPrimitive> toAdd = generateToAdd();
            if (toAdd.isEmpty()) {
                return;
            }
            final OsmPrimitive basePrimitive = toAdd.get(0);
            basePrimitive.removeAll();
            detection.getOsmKeys().forEach(basePrimitive::put);

            final AddPrimitivesCommand add = new AddPrimitivesCommand(
                toAdd.stream().map(OsmPrimitive::save).collect(Collectors.toList()), dataSet);
            UndoRedoHandler.getInstance().add(add);
            final OsmPrimitive addedPrimitive = dataSet.getPrimitiveById(basePrimitive.getPrimitiveId());
            final int[] tSelection = new int[1];
            final Command[] tCommand = new Command[1];
            this.pointObjectLayer.hideAdditionalActionsWindow(() -> {
                tSelection[0] = preset.showDialog(Collections.singleton(addedPrimitive), false);
                tCommand[0] = TaggingPreset.createCommand(Collections.singleton(addedPrimitive),
                    preset.getChangedTags());
            });
            final int userSelection = tSelection[0];
            final Command updateTagsCommand = tCommand[0];
            // Closing the window returns 0. Not in the TaggingPreset public answers at this time.
            if ((userSelection == 0 || userSelection == TaggingPreset.DIALOG_ANSWER_CANCEL)
                && UndoRedoHandler.getInstance().hasUndoCommands()) {
                // Technically, it would be easier to do one undo, but this avoids corner cases
                // where a user makes some modifications while the preset window is open.
                List<Command> undoCommands = UndoRedoHandler.getInstance().getUndoCommands();
                int index = undoCommands.size() - undoCommands.indexOf(add);
                UndoRedoHandler.getInstance().undo(index);
                return;
            } else if (!addedPrimitive.isTagged()) {
                return;
            }
            addMapillaryTags(addedPrimitive);

            generateCommands(addedPrimitive, updateTagsCommand);
            this.pointObjectLayer.getData().setSelected(this.pointObjectLayer.getData().getSelected().stream()
                .filter(n -> !n.equals(this.mapillaryObject)).collect(Collectors.toList()));
        }
    }

    /**
     * Generate all commands and add them to the UndoRedo stack
     *
     * @param basePrimitive The base primitive
     * @param updateTagsCommand The tag add command
     */
    private void generateCommands(final OsmPrimitive basePrimitive, final Command updateTagsCommand) {
        GenericCommand<?, ?, ?, ?, ?> deleteOriginal;
        if (mapillaryObject instanceof VectorPrimitive) {
            deleteOriginal = new org.openstreetmap.josm.plugins.mapillary.command.DeleteCommand<>(
                ((VectorPrimitive) mapillaryObject).getDataSet(), (VectorPrimitive) mapillaryObject);
        } else {
            throw new IllegalArgumentException(
                "Unknown primitive type for mapillaryObject: " + mapillaryObject.getClass().getName());
        }
        if (updateTagsCommand != null) {
            UndoRedoHandler.getInstance().add(new AddMapillaryObjectCommand(deleteOriginal, updateTagsCommand));
            UndoRedoHandler.getInstance().add(updateTagsCommand);
        }
        final AdditionalInstructions additionalInstructions = detection.getAdditionalInstructions();
        if (additionalInstructions != null) {
            final Command additionalCommand = additionalInstructions.apply(basePrimitive);
            if (additionalCommand != null) {
                UndoRedoHandler.getInstance().add(additionalCommand);
            }
        }
    }

    /**
     * Add mapillary tags to a primitive
     *
     * @param basePrimitive The primitive to add tags to
     */
    private void addMapillaryTags(final IPrimitive basePrimitive) {
        long[] imageIds = MapillaryMapFeatureUtils.getImageIds(mapillaryObject);
        if (imageIds.length > 0) {
            final OptionalLong imageId = MapillaryLayer.getInstance().getData().getSelectedNodes().stream()
                .mapToLong(IPrimitive::getUniqueId).distinct()
                .filter(i -> LongStream.of(imageIds).anyMatch(id -> id == i)).findFirst();
            if (imageId.isPresent()) {
                basePrimitive.put("mapillary:image", Long.toString(imageId.getAsLong()));
            }
        }
        if (mapillaryObject.getId() != 0) {
            basePrimitive.put("mapillary:map_feature", Long.toString(mapillaryObject.getId()));
        }

    }

    /**
     * Generate the objects to add
     *
     * @return The objects to add (first object in list is what should have the tags)
     */
    private List<OsmPrimitive> generateToAdd() {
        if (mapillaryObject instanceof INode) {
            final Node tNode = new Node();
            tNode.setCoor(((INode) mapillaryObject).getCoor());
            mapillaryObject.getKeys().forEach(tNode::put);
            return Collections.singletonList(tNode);
        } else if (mapillaryObject instanceof IWay) {
            Way way = new Way((Way) mapillaryObject);
            way.removeAll();
            way.setNodes(Collections.emptyList());
            ((IWay<?>) mapillaryObject).getNodes().stream().map(INode::getCoor).map(Node::new).forEach(way::addNode);
            List<OsmPrimitive> toAdd = new ArrayList<>(way.getNodesCount() + 1);
            toAdd.add(way);
            toAdd.addAll(way.getNodes());
            return toAdd;
        } else {
            return Collections.emptyList();
        }
    }
}
