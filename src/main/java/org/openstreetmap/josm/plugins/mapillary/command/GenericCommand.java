package org.openstreetmap.josm.plugins.mapillary.command;

import java.util.Collection;

import org.openstreetmap.josm.command.PseudoCommand;
import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IRelation;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.osm.OsmData;

/**
 * A generic command class
 *
 * @param <O> The base primitive class
 * @param <N> The base node class
 * @param <W> The base way class
 * @param <R> The base relation class
 * @param <D> The data set class
 */
public abstract class GenericCommand<O extends IPrimitive, N extends INode, W extends IWay<N>, R extends IRelation<?>, D extends OsmData<O, N, W, R>>
  implements PseudoCommand {

  private final D data;

  protected GenericCommand(D dataset) {
    this.data = dataset;
  }

  /**
   * Executes the command on the dataset. This implementation will remember all
   * primitives returned by fillModifiedData for restoring them on undo.
   * <p>
   * The layer should be invalidated after execution so that it can be re-painted.
   *
   * @return true
   */
  public abstract boolean executeCommand();

  /**
   * Undoes the command.
   * It can be assumed that all objects are in the same state they were before.
   * It can also be assumed that executeCommand was called exactly once before.
   * This implementation undoes all objects stored by a former call to executeCommand.
   */
  public abstract void undoCommand();

  /**
   * Gets the data set this command affects.
   *
   * @return The data set. May be <code>null</code> if no layer was set and no edit layer was found.
   */
  public D getAffectedDataSet() {
    return data;
  }

  /**
   * Return the primitives that take part in this command.
   *
   * @return primitives that take part in this command
   */
  public abstract Collection<? extends O> getParticipatingIPrimitives();

  /**
   * Fill collections with modified data
   *
   * @param modified The modified primitives
   * @param deleted The deleted primitives
   * @param added The added primitives
   */
  public abstract void fillModifiedData(Collection<O> modified, Collection<O> deleted, Collection<O> added);
}
