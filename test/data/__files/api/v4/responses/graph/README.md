# Entity responses
All entity responses are formatted with `jq`. Please keep in mind that the formatting from the server is typically less human readable (i.e., no spacing, no newlines, etc.).

## Image
This returns the metadata for an image.

When used in tests, the output should be appropriately filtered if it is the return from an API call.

### Endpoint
* https://graph.mapillary.com/:image\_id

### Fields
* altitude
* atomic\_scale
* camera\_parameters
* camera\_parameters
* camera\_type
* captured\_at
* compass\_angle
* computed\_altitude
* computed\_compass\_angle
* computed\_geometry
* computed\_rotation
* exif\_orientation
* geometry
* height
* thumb\_256\_url
* thumb\_1024\_url
* thumb\_2048\_url
* merge\_cc
* mesh
* quality\_score
* sequence
* sfm\_cluster
* width

## Sequence
Sequence entities represent a sequence of image IDS ordered by capture time.

These are stored in image\_ids/:sequence\_id


### Endpoints
* https://graph.mapillary.com/image\_ids?sequence\_id=:sequence\_id -- this is a collection endpoint, so the `data` value is an array.

### Fields
* `id`: The identifier for the image in the sequence
