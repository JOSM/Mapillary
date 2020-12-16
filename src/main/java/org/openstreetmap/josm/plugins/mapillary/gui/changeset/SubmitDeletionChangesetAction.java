// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.gui.changeset;

import org.openstreetmap.josm.plugins.mapillary.gui.dialog.MapillaryChangesetDialog;
import org.openstreetmap.josm.plugins.mapillary.gui.layer.MapillaryLayer;
import org.openstreetmap.josm.plugins.mapillary.utils.api.JsonChangesetEncoder;

/**
 * Submits current deletion changeset to Mapillary.
 */
public class SubmitDeletionChangesetAction extends AbstractChangesetAction {

  private static final long serialVersionUID = 4995924098228082806L;

  /**
   * Main constructor.
   *
   * @param changesetDialog Mapillary changeset dialog
   */
  public SubmitDeletionChangesetAction(MapillaryChangesetDialog changesetDialog) {
    super(changesetDialog, "deletion", () -> MapillaryLayer.getInstance().getDeletionChangeset(),
      changeset -> JsonChangesetEncoder.encodeDeletionChangeset(changeset).build().toString());
  }
}
