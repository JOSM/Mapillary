## What's Changed
### [v2.0.0-beta.2](https://github.com/JOSM/Mapillary/compare/v2.0.0-beta.1...v2.0.0-beta.2)
* JOSM [#20274](https://josm.openstreetmap.de/ticket/20274): Cannot zoom in to detail.
  * Mapillary has added an API field for thumb_original_url, which has the original (processed) image.
  * Breaking change: getWidth/getHeight from MapillaryCache now require a parameter (can be null).
* JOSM [#21717](https://josm.openstreetmap.de/ticket/21717): add filter to display only images that are not panoramas
  * The filter allows users to see all images, panoramic images, or non-panoramic images, depending upon their choice.
* JOSM [#21817](https://josm.openstreetmap.de/ticket/21817): NPE in MapillaryImageEntry#read
  * This is caused by old image urls. We've reset the maximum age for the image metadata to be cached to be the same as `mirror.maxtime` (advanced preferences). This defaults to 7 days (down from 700 days).

### [v2.0.0-beta.1](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.50...v2.0.0-beta.1)
* JOSM [#21791](https://josm.openstreetmap.de/ticket/21791): IOOBE in OffsetUtils#getOffsetLocation
* JOSM [#21530](https://josm.openstreetmap.de/ticket/21530): Disable Mapillary tag insert when no Mapillary image selected.

### [v2.0.0-alpha.50](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.49...v2.0.0-alpha.50)
UI and OOM improvements
* MapillaryImageEntry: Use SoftReferences to avoid OOM
* Avoid blocking EDT when image is initially clicked
### [v2.0.0-alpha.49](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.48...v2.0.0-alpha.49)
* JOSM [#21603](https://josm.openstreetmap.de/ticket/21603): AE: ImageViewerDialog instance not created
* JOSM [#21604](https://josm.openstreetmap.de/ticket/21604): MapillaryImageUtils: Check for null
* ImageDetection: Don't get detections for unknown images.
* MapillaryLayer: Improve rendering performance

  Fix an expensive recurrent call by storing initial results and passing
  to required locations.

  getSelectedNodes took up 60%+ of time spent in drawImageMarker, which
  took up ~75% of the time in paintWithLock. This means getSelectedNodes
  took up ~46% of the processing time in the paintWithLock method.

  getSelectedNodes now takes up 0.15% of the time in the paintWithLock
  method.

### [v2.0.0-alpha.48](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.47...v2.0.0-alpha.48)
* JOSM [#21571](https://josm.openstreetmap.de/ticket/21571): NPE

### [v2.0.0-alpha.47](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.46...v2.0.0-alpha.47)
Swallow JosmRuntimeException when removing primitive (various checks did not fix this, so now
we're just going to ignore it).

### [v2.0.0-alpha.46](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.45...v2.0.0-alpha.46)
Attempt to be more robust on reindexing of sequences

### [v2.0.0-alpha.45](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.44...v2.0.0-alpha.45)
Fix [#21565](https://josm.openstreetmap.de/ticket/21565): NPE in MapillarySequenceUtils#getNextOrPrevious
Also add ability to offset images within a sequence (see Mapillary Image Info dialog).

### [v2.0.0-alpha.44](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.43...v2.0.0-alpha.44)
MapillaryDownloader: Attempt to fix [#21553](https://josm.openstreetmap.de/ticket/21553): Failed to remove primitive

### [v2.0.0-alpha.43](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.41...v2.0.0-alpha.43)
Fix various CME's

### [v2.0.0-alpha.41](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.40...v2.0.0-alpha.41)
Import: Drop image imports

Since Mapillary does not have a public upload API, and we now use JOSM's
image viewer, it is pointless for users to "import" images into the
Mapillary layer. JOSM also supports opening directories of images now.

In the event upload is re-added, it will most likely be on a GeoImage by
GeoImage layer basis, and will not import the images into the Mapillary
layer.

Also improve speed and viewing experience for images.

### [v2.0.0-alpha.40](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.39...v2.0.0-alpha.40)
Fix JOSM [#21481](https://josm.openstreetmap.de/ticket/21481): MapillaryLayer not repainting on image change

### [v2.0.0-alpha.39](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.38...v2.0.0-alpha.39)
JOSM ImageViewer: Enable copy button (link to Mapillary.com)

### [v2.0.0-alpha.38](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.37...v2.0.0-alpha.38)
Fix JOSM [#21443](https://josm.openstreetmap.de/ticket/21443): Mapillary Filter changes interpreted as JOSM hotkey

### [v2.0.0-alpha.37](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.36...v2.0.0-alpha.37)
Use JOSM image viewer

* See JOSM [#16472](https://josm.openstreetmap.de/ticket/16472): "Add support to 360/spherical image" for the
  enablement of 360 imagery in JOSM core. Painting of detections is now
  down by drawing on the buffered image, which isn't ideal, but will be
  fixed as soon as the math and ui results for 360 images makes sense.
* May fix [#21295](https://josm.openstreetmap.de/ticket/21295): Mapillary shows grey sequence links which cannot be
  followed
* Object Detections are no longer shown by default -- there is no button
  for it right now, so they are hidden to avoid cluttering the image
  viewer
* May fix [#21406](https://josm.openstreetmap.de/ticket/21406): Crash on Mapillary password change
* May fix [#21264](https://josm.openstreetmap.de/ticket/21264): null in sequence nodes

### [v2.0.0-alpha.36](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.35...v2.0.0-alpha.36)
* Fix [#21329](https://josm.openstreetmap.de/ticket/21329): IAE: id cannot be 0
* ImageDetections: Wait some time prior to fetching

### [v2.0.0-alpha.35](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.34...v2.0.0-alpha.35)
Fix JOSM [#21296](https://josm.openstreetmap.de/ticket/21296) and [#21312](https://josm.openstreetmap.de/ticket/21312)

### [v2.0.0-alpha.34](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.33...v2.0.0-alpha.34)
Fix JOSM [#21281](https://josm.openstreetmap.de/ticket/21281):  Applying the filter creates a bunch of staight lines

### [v2.0.0-alpha.33](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.30...v2.0.0-alpha.33)
* Fix [#21254](https://josm.openstreetmap.de/ticket/21254): IAE: POLYGON cannot have zero area (root cause fixed in
  JOSM, workaround in Mapillary for older JOSM versions)
* Fix [#21265](https://josm.openstreetmap.de/ticket/21265): NPE when no computed geometry is found

### [v2.0.0-alpha.30](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.29...v2.0.0-alpha.30)
Fix [#21178](https://josm.openstreetmap.de/ticket/21178): failed to remove primitive: VectorNode in MapillaryVectorTileWorkarounds

### [v2.0.0-alpha.29](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.28...v2.0.0-alpha.29)
Fix JOSM-[21170](https://josm.openstreetmap.de/ticket/21170): Mouse selection area to large

### [v2.0.0-alpha.28](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.27...v2.0.0-alpha.28)
JOSM [#21127](https://josm.openstreetmap.de/ticket/21127): NPE in Mapillary login
JOSM [#21121](https://josm.openstreetmap.de/ticket/21121): Hopefully fix failing tests (JOSM Jenkins)

### [v2.0.0-alpha.27](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.26...v2.0.0-alpha.27)
* GitHub #186: UOE with modifying a BBox
* JOSM [#21086](https://josm.openstreetmap.de/ticket/21086): ConcurrentModificationException in PointObjectLayer
* JOSM [#21118](https://josm.openstreetmap.de/ticket/21118): Failed to remove primitive
* Fix an issue where logging in would result in the login button still
  being present after JOSM restart

### [v2.0.0-alpha.26](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.25...v2.0.0-alpha.26)
Attempt to fix JOSM [#21075](https://josm.openstreetmap.de/ticket/21075): Probable concurrent modification

### [v2.0.0-alpha.25](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.23...v2.0.0-alpha.25)
Fix JOSM [#21070](https://josm.openstreetmap.de/ticket/21070) and [#21072](https://josm.openstreetmap.de/ticket/21072) (hopefully)

The issue occurred when there were two objects in the stream with the
same key. I don't know how it happened, and was unable to reproduce even
when adding a second key into the json.

This patch hopefully fixes the issue, but due to not being able to
reproduce, it is unknown if this fixes the issue.

### [v2.0.0-alpha.23](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.22...v2.0.0-alpha.23)
360 Images: Now properly show what is 360 and what isn't

### [v2.0.0-alpha.22](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.21...v2.0.0-alpha.22)
Image Detections and misc fixups

* Image Detections now draw properly, and are properly highlighted when
  selected
* Users can now log in (*application secret is not in source code*,
  currently requires source code modification)
* Fix JOSM [#21049](https://josm.openstreetmap.de/ticket/21049): Wrong image URL in image info pane
* Drop camera make/model filters (no API)

### [v2.0.0-alpha.21](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.20...v2.0.0-alpha.21)
Point object detections: update max zoom, fix rendering issues

This also allows the user to select an object and (hopefully) get an
image that shows it. The selection code is not yet stable, so you might
not get the "right" image.

### [v2.0.0-alpha.20](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.19...v2.0.0-alpha.20)
MapillaryURL: Default to getting computed and non-computed info

### [v2.0.0-alpha.19](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.17...v2.0.0-alpha.19)
Rework preferences, fix some bugs

The preferences panel has been reworked to remove most obsolete/unused
options.

GitHub #184 is fixed (image viewer was not being moved back to the main
dialog after using the color picker)

### [v2.0.0-alpha.17](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.16...v2.0.0-alpha.17)
No longer pre-cache nearest Mapillary images

The next/previous images on the current sequence are still pre-cached,
but the nearby images (red/blue buttons) are not precached. This is due
to the significant amplification of API calls from the caching the
imagery.

### [v2.0.0-alpha.16](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.15...v2.0.0-alpha.16)
MapillaryCache: Fix JOSM [#21035](https://josm.openstreetmap.de/ticket/21035): IAE when rate limited

### [v2.0.0-alpha.15](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.14...v2.0.0-alpha.15)
Test updates, fixes for some exceptions thrown when api rate limits reached

See JOSM [#21028](https://josm.openstreetmap.de/ticket/21028), [#21029](https://josm.openstreetmap.de/ticket/21029), [#21030](https://josm.openstreetmap.de/ticket/21030) for the rate limit issues.

### [v2.0.0-alpha.14](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.13...v2.0.0-alpha.14)
API Rate Limiting: Notify when reached

* Notify user when API rate limit is reached
* Notify user when something else happens that is not handled
* Fix JOSM [#20999](https://josm.openstreetmap.de/ticket/20999)/#21015: IAE when creating a new array list
* Avoid caching most bad returns
* Disable ImageDetection until v4 API gives me geometry
* FIXUP: Highlight selected image as well as hovered images
* Mapillary API: Use OAuth Authorization headers for non-tiles
* Fix JOSM [#21002](https://josm.openstreetmap.de/ticket/21002), [#21020](https://josm.openstreetmap.de/ticket/21020), [#21024](https://josm.openstreetmap.de/ticket/21024) (partial): Mapillary image date is not displayed
* ImageInfoPanel: Correctly account for no organization

### [v2.0.0-alpha.13](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.12.3...v2.0.0-alpha.13)
Mapillary v4 API support

* Most API calls should only be called once (this should reduce network
  traffic)
* It currently appears that detection tiles are not shown
* There is no current method to get detection geometry to show in the
  image viewer
* Image loading has been moved off of the EDT (this should improve
  performance of the map view -- time taken to image shown is still the
  same)
* Some naive handling for server errors exist (i.e., we don't crash).
  * Hitting API limits is not well handled -- this may mean that caches
    need to be cleared. If this happens, the exact error is logged and
    can be seen in the JOSM status report.
    This might not appear with the production key (testing was done with
      a custom API key)

### [v2.0.0-alpha.12.3](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.11...v2.0.0-alpha.12.3)
Fix JOSM [#20999](https://josm.openstreetmap.de/ticket/20999) and [#21015](https://josm.openstreetmap.de/ticket/21015): IAE when creating a new array list

### [v2.0.0-alpha.11](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.10...v2.0.0-alpha.11)
Performance enhancements

* No longer get multiple image info in singletons (batch them)
* Avoid calling detection endpoints multiple times

### [v2.0.0-alpha.10](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.8...v2.0.0-alpha.10)
Fix JOSM [#20961](https://josm.openstreetmap.de/ticket/20961): CME in ImageDetections#getDetections

### [v2.0.0-alpha.8](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.7...v2.0.0-alpha.8)
Enable organization filtering for non-logged in users

* Fix JOSM [#20951](https://josm.openstreetmap.de/ticket/20951): Webstart does not allow file access in common ForkJoinPool
* Disable user filter to avoid confusion

### [v2.0.0-alpha.7](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.6...v2.0.0-alpha.7)
UI changes and bug fixes

MapillaryMainDialog:
* Increase next/previous button sizes
* First/last buttons now remember which image they came from, and will
  go back if clicked prior to the next navigation action
* Fix potential deadlock

SelectNextImageAction:
* Prefix most shortcuts with `Mapillary: ` to make them easier to find

Caches:
* Fix a crash in Java WebStart (ForkJoinPool common pool does not allow
  file access).

### [v2.0.0-alpha.6](https://github.com/JOSM/Mapillary/compare/v2.0.0-alpha.3...v2.0.0-alpha.6)
Fix CME (JOSM [#20948](https://josm.openstreetmap.de/ticket/20948)) and image deselection (GitHub #180).

### [v2.0.0-alpha.3](https://github.com/JOSM/Mapillary/compare/v1.5.37...v2.0.0-alpha.3)
MapillaryFilterDialog: Update to work with new primitive types

* Fix JOSM [#20769](https://josm.openstreetmap.de/ticket/20769) (Mapillary Expert Filter/Mapillary Filter dialogs
  shared preferences)
* Move from LocalDate to Instant for many things (JOSM is also doing
  that)
* Fix a couple of crashing issues
