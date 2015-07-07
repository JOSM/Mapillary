package org.openstreetmap.josm.plugins.mapillary;

public interface MapillaryDataListener {

  /**
   * Fired when any image is added to the database.
   */
  public void imagesAdded();

  /**
   * Fired when the selected image is changed by something different from
   * manually clicking on the icon.
   */
  public void selectedImageChanged(MapillaryAbstractImage oldImage,
      MapillaryAbstractImage newImage);

}
