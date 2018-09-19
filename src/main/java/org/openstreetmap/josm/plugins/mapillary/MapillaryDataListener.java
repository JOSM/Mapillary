// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary;

/**
 * Interface for listeners of the class {@link MapillaryData}.
 *
 * @author nokutu
 *
 */
public interface MapillaryDataListener {

  /**
   * Fired when any image is added to the database.
   */
  void imagesAdded();

  /**
   * Fired when the selected image is changed by something different from
   * manually clicking on the icon.
   *
   * @param oldImage
   *          Old selected {@link MapillaryAbstractImage}
   * @param newImage
   *          New selected {@link MapillaryAbstractImage}
   */
  void selectedImageChanged(MapillaryAbstractImage oldImage,
      MapillaryAbstractImage newImage);
}
