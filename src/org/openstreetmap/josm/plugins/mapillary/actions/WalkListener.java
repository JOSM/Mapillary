// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.actions;

/**
 * Implemented by those classes that need to listen to the creation of the walk
 * threads.
 *
 * @author nokutu
 *
 */
public interface WalkListener {

  /**
   * Called when a new walk thread is started.
   *
   * @param thread The thread executing the walk.
   */
  void walkStarted(WalkThread thread);
}
