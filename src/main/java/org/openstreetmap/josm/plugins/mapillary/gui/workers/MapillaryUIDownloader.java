// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.workers;

import javax.swing.SwingWorker;

/**
 * A common class for downloaders
 *
 * @param <T> The primary return type
 * @param <V> The updating type
 * @see SwingWorker
 */
abstract class MapillaryUIDownloader<T, V> extends SwingWorker<T, V> {
}
