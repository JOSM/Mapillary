package org.openstreetmap.josm.plugins.mapillary.command;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.util.Collection;
import java.util.Collections;

import org.openstreetmap.josm.data.osm.INode;
import org.openstreetmap.josm.data.osm.IPrimitive;
import org.openstreetmap.josm.data.osm.IRelation;
import org.openstreetmap.josm.data.osm.IWay;
import org.openstreetmap.josm.data.osm.OsmData;
import org.openstreetmap.josm.data.osm.OsmPrimitive;

/**
 * A generic command class
 *
 * @param <O> The base primitive class
 * @param <N> The base node class
 * @param <W> The base way class
 * @param <R> The base relation class
 * @param <D> The data set class
 */
public class DeleteCommand<O extends IPrimitive, N extends INode, W extends IWay<N>, R extends IRelation<?>, D extends OsmData<O, N, W, R>>
  extends GenericCommand<O, N, W, R, D> {
  private final O primitive;

  /**
   * Create a new delete command for a dataset
   *
   * @param dataSet The dataset to use
   * @param primitive The primitive to delete
   */
  public DeleteCommand(final D dataSet, final O primitive) {
    super(dataSet);
    this.primitive = primitive;
  }

  @Override
  public String getDescriptionText() {
    return tr("Delete Mapillary object");
  }

  @Override
  public Collection<? extends OsmPrimitive> getParticipatingPrimitives() {
    if (this.primitive instanceof OsmPrimitive) {
      return Collections.singleton((OsmPrimitive) this.primitive);
    }
    return Collections.emptyList();
  }

  @Override
  public boolean executeCommand() {
    final D data = this.getAffectedDataSet();
    final boolean locked = data.isLocked();
    try {
      data.unlock();
      this.primitive.setDeleted(true);
    } finally {
      if (locked) {
        data.lock();
      }
    }
    return true;
  }

  @Override
  public void undoCommand() {
    final D data = this.getAffectedDataSet();
    final boolean locked = data.isLocked();
    try {
      data.unlock();
      this.primitive.setDeleted(false);
    } finally {
      if (locked) {
        data.lock();
      }
    }
  }

  @Override
  public Collection<O> getParticipatingIPrimitives() {
    return Collections.singleton(this.primitive);
  }

  @Override
  public void fillModifiedData(Collection<O> modified, Collection<O> deleted, Collection<O> added) {
    deleted.add(this.primitive);
  }
}
