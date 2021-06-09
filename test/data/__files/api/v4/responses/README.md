This directory contains example data representative for what the APIv4 typically returns for certain queries.

The data responses are organized like this:
* graph (for entities)
  * entity\_id.json
* coverageTiles (for tiles)
  * endpoint (minus commonalities). If there are non coordinate paths after commonalities, they are included as subdirs.
    * {z}/{x}/{y}.mvt
