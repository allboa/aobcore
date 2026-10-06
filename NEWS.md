# aobcore 0.0.0.9000

* Chunk references from R, and the codecs already bundled (#63). New
  `cog_chunks()` describes a COG as a scene spec 0.6 `chunks` data
  reference: the grid of every level (GDAL's own geotransform and size for
  each overview), the codec chain mapped from the TIFF's compression and
  predictor (`bytes` with the file's byte order, then `predictor`
  `horizontal` or `floating_point`, then `deflate`, `zstd` or `lzw`; JPEG
  as the whole chain with its shared tables, YCbCr or greyscale only), and
  a ref for every tile GDAL says is stored (its TileOffsets and
  TileByteCounts), so a sparse tile has none. Bands stored separately get
  a ref per band. Compressions with no 0.6 codec (PACKBITS, LERC, WEBP),
  or different compressions across levels, are an error that says to draw
  the COG as a `cog`. `scene_add_tiled_raster(format = "chunks")` draws a
  COG over its chunk references with the same plan as the `cog` path
  (levels, tiles, footprints and meshes in the view CRS), each plan level
  cut down to what 0.6 asks; an embedded page carries only the planned
  chunks' bytes, one blob per chunk keyed `"<source>@<offset>+<length>"`
  as a cog's tiles are, and `embed = FALSE` registers the file for
  `serve_scene()` as for a cog. The renderer looks up that per-chunk blob
  (in the page, or listed by a served page) for a ref at the reference's
  own `url` before a blob keyed by the url, and decodes `zstd` and `lzw`
  with the decoders the COG path bundles (fzstd, geotiff.js's TIFF LZW),
  `deflate` and `gzip` synchronously with the bundled fflate instead of
  the browser's `DecompressionStream` (so trailing bytes and Safari before
  16.4 are no longer a problem), and `jpeg` with the browser's image
  decoder and the chain's tables; `blosc` is still a layer error.
  `write_scene_html()` keeps blobs named by a refs table's `url` column (an
  Arrow stream blob) and per-chunk blobs, not only inline rows' urls. The
  renderer tests check zstd and lzw GeoTIFFs written by GDAL (a uint16
  zstd file with its bands stored separately, and a float32 lzw file)
  against GDAL's values, a jpeg chunk against GDAL's read, and the colour
  drawn at one cell of scenespec's `tiny.zarr` (row 0 at the bottom); CI
  validates written 0.6 scenes against scenespec 898419b. A Zarr producer
  is left for later: gdalraster's multidimensional API does not give chunk
  keys or byte ranges.

* Renderer: draws scene spec 0.6 chunk references (#61; allboa/design
  decision 0010 item 2). A `tiled_raster` whose source is a `chunks` data
  reference reads each planned chunk's bytes by its ref's `url`, `offset`
  and `length` (an HTTP range request, or a slice of a blob keyed by that
  `url` when the page carries one or, on a served page, lists it), undoes
  the codec chain in reverse and draws the chunk as a COG tile is drawn
  (palette or `rgb`, `nodata`, `scale` and `offset`, mesh). Codecs: `bytes`
  (either endian), `deflate` and `gzip` through the browser's own
  `DecompressionStream`, and the `horizontal` and `floating_point`
  predictors with a stride of `bands` for `pixel` interleave. `zstd`,
  `lzw`, `blosc` and `jpeg` chains are a layer error naming the codec. A
  planned chunk with no ref is no data and is not drawn; edge chunks are
  padded; `pixel`, `plane` and `separate` interleave and the layer's `band`
  follow the 0.6 README; refs may be inline rows or an Arrow table. Tested
  against GDAL's reads of scenespec's `tiny.zarr` (vendored in
  `js/test/fixtures/scenespec`) and of a deflate COG of 2 int16 bands
  interleaved by pixel (`tools/make-chunk-fixtures.R`). In R,
  `scene_spec_version()` knows 0.6 (a scene with a `chunks` reference), and
  `write_scene_html()` accepts a 0.6 scene and embeds the blobs named by
  its chunk refs' urls. No producer writes `chunks` yet.

* `scene_add_data()` and `scene_add_vector()` check IPC bytes and native
  GeoArrow streams against scene spec's explicit-data contract, so data the
  page would refuse are an error in R (#59). The CRS in the geometry
  column's GeoArrow metadata must match the view CRS by the contract's rule
  (equal JSON values, or one authority code compared case-insensitively,
  so `"EPSG:3031"` matches the PROJJSON 'geoarrow' writes), with a
  `crs_type` that fits it and planar edges; a CRS that does not match is an
  error, since nothing is reprojected. A native stream with no CRS is given
  the view CRS; IPC bytes are never rewritten, so bytes with no CRS are an
  error in a view that has one. `scene_add_layer()` checks that popup
  columns are attribute types (boolean, 8 to 64 bit integers, float32 or
  float64, string, date or timestamp), read from the blob's schema, and
  names each column that is not with its type (a duration, time, binary,
  list or dictionary column). `vector_ipc()` and `scene_add_data()` accept
  separated coordinates (a struct of `x`, `y` and optionally `z`) as well
  as interleaved, which the contract allows and the renderer reads, and
  write them as they are; M coordinates and 64-bit list offsets are
  refused by `vector_ipc()` and in IPC bytes, and converted from a stream
  by `scene_add_data()`. Renderer: a vector layer whose bytes are not the
  declared format (an IPC file, which starts with `ARROW1`, declared
  `arrow-ipc-stream`, or the reverse) is a layer error, as scenespec's
  `check-data.js` reports it; the check applies to the data of vector
  layers (a raster's mesh and value tables are read as before).

* Renderer: vector data are checked against scene spec's explicit-data
  contract (allboa/scenespec#11) before they are drawn. The geometry
  column's `ARROW:extension:name` must be the scene's `geometry.encoding`
  and one of the six native GeoArrow types (WKB, WKT, geometry collections
  and unknown extensions are refused, not guessed from the storage); the
  `crs` in its `ARROW:extension:metadata` must fit its `crs_type` and match
  `view.crs` (and `geometry.crs`, when given) as equal JSON values or by one
  authority code, compared case-insensitively, so `"EPSG:3031"` matches
  the PROJJSON geoarrow writes; edges must be planar; the storage must be
  the extension's List layout with interleaved (`xy`, `xyz`) or separated
  (`x`, `y`, `z`) doubles, no M; and a `fill` or `stroke` column must be
  RGBA. Data that fail are an error for each layer that draws them, which
  is not drawn, and the rest of the scene draws; nothing is reprojected.
  A popup column that is not an attribute type (a binary or dictionary
  column, say) is an error for that layer's popup, as a missing one was.
  Data written by aobcore's producers (`vector_stream()`,
  `gdal_vector_stream()`, and `scene_add_vector()` or `scene_add_data()`
  given anything they convert) are unchanged and draw as before; IPC bytes
  and native streams made elsewhere are checked in R before the page sees
  them (#59). The renderer test reads scenespec's fixtures (copied into
  `js/test/fixtures/scenespec` from scenespec 898419b): the six streams
  draw with their feature counts, and each invalid-data scene is a layer
  error (#57).

* `mosaic_members()` reads the members of a VRT (a `.vrt` file or a
  `vrt://` connection string, from the XML GDAL serialises for it) or a
  GDAL Tile Index (GTI: a `.gti` file, a `.gti.gpkg` or `.gti.fgb` index or
  a `GTI:` path, from its index layer) without reading any member: each
  member's path or URL as GDAL opens it (relative names resolved against
  the VRT or index) with its placement in the mosaic, and `NULL` for a
  plain file. `mosaic_plan()` plans a mosaic of COGs across its members for
  a view: one `cog_plan()` per member, probing (`cog_info()`) only the
  members whose placement meets `extent`, so a remote member costs one
  range request for its header and nothing is copied; a member that
  cannot be drawn in place (not a tiled GeoTIFF with overviews, in another
  CRS, placed by a window or stretched, rescaled by the VRT) is reported
  with a reason for the caller to fall back on. aobview draws a mosaic as
  one `tiled_raster` layer per member (allboa/aobview#41, allboa/design
  decision 0011).

* `renderer_dependency()` is exported: the renderer as an HTML dependency,
  for hosts that call `aob.render()` themselves, such as aobview's Shiny
  output binding (decision 0009).

* Renderer: `aob.render(el, scene, {channel})` takes a channel made by the
  host (`send()`, `onMessage()`, `close()`, and "open" reported through
  `onState`) instead of a websocket, and speaks protocol 1 over it; a
  Shiny output binding passes one that forwards the page's messages to
  input values (decision 0009). The channel is always given message
  objects (the websocket channel writes them as JSON text), "open" is
  never reported synchronously, and a `reload` over a channel is ignored
  rather than reloading the host page. In a fragment or over a channel,
  Escape clears only the view with the keyboard focus, so one key press
  no longer clears every view on a page. Pages are unchanged.

* `scene_tag()` returns a scene as an htmltools fragment (a sized `<div>`,
  the scene JSON and its blobs embedded as `write_scene_html()` embeds
  them) with the renderer attached as an `htmlDependency()` named
  `"aob-renderer"`, so a knitted document, a Shiny app or
  `htmltools::save_html()` carries the renderer once however many scenes
  it shows (allboa/design decision 0009). It ends with a one-line script
  that calls `aob.boot()`, so a fragment inserted after the page loaded
  (Shiny's `renderUI()`) draws too. `write_scene_html()` and served pages
  are byte-for-byte as before apart from the rebuilt bundle and one page
  CSS rule (below).

* Renderer: an element with class `aob-fragment` takes its theme from its
  own `data-theme` (`"light"` or `"dark"`), and its theme button sets the
  fragment's theme rather than the host document's root element. The
  renderer no longer sets `color-scheme` on the root element (it set it
  there, which in a host page with a dark preference turned the host's
  own text and inputs dark): it sets it on its own element, and a whole
  page's CSS sets the root's (`html { color-scheme: var(--aob-scheme) }`),
  so whole pages look as before.

* `serve_scene()` handles have `running()` (`TRUE` while the server runs,
  `FALSE` after `stop()`) and `status()`: `running`, `socket`, `serial`,
  `connections` and `received` (counts of accepted `hello`, `select` and
  `view` messages), read without running the event loop and on a stopped
  server too. Callers such as aobview need not read `srv$state`.

* Renderer: `handle.view()` has the shape of the initial view (`target`
  of length 3, `zoom`, `minZoom`, `maxZoom`, clamped to `view.bounds`)
  after a double-click zoom or any interaction, not deck's last transition
  frame or controller state. `handle.setView()` returns the same shape.

* Renderer: a press on anything in the map's container other than deck's
  canvas (such as a future deck widget) never selects the feature under it
  (#29 item 2).

* Renderer: a double click zooms in one level about the pointer in a
  scene with `view.bounds` (#28). It did nothing: the bounds clamp handed
  each frame of deck's zoom transition back with a changed target, which
  ends the transition. A frame of a transition now passes through as it
  is (its end is clamped when it starts), and the clamp keeps the target's
  length and deck's `zoomX`/`zoomY`.

* Renderer: moving the pointer off the map clears the amber hover outline
  (#45). deck picks no layers for a point outside its viewport, so on
  `pointerleave` the outline stayed on the last feature hovered.

* Renderer: two clicks are a double click (zoom, no select) by deck's own
  rules, decided at the second release: both presses under 250 ms and
  released within 300 ms (#29 item 1). A slow double click selects instead
  of doing nothing, and the third click of a triple click selects. A press
  just before `finalize()` no longer calls `setProps()` on the finalized
  deck (#29 item 5).

* Factor columns are written as character in vector blobs (#25 item 3):
  the IPC writer has no dictionary types and failed with "dictionary types
  unsupported". This covers data frames given to `vector_stream()` or
  `scene_add_vector()` and streams with a dictionary column given to
  `vector_ipc()`. The caller's data keep their factors and level order.

* `vector_densify()` densifies lines and polygon edges of any wk-handleable
  input, linearly in its own coordinates, with no GDAL: the wk densify that
  allboa/design decision 0004 names as the default for in-memory data.
  aobview uses it before reprojecting with PROJ.

* `write_scene_html()` encodes blobs as base64 about five times faster and
with about a third of the peak memory: a 32 MB blob took 8.2 s and 1.2 GB
(process high-water mark) and now takes 1.6 s and 0.4 GB. The text is
built a chunk at a time through raw bytes instead of one R string per
character. This was most of the time and memory of an embedded view of
a few million vertices.

* An R session that ended with a page's websocket connected crashed (exit
  status 139), at the end of a script or on an error: the exit finalizer
  that stops the servers closed each socket, and httpuv 1.6.17 may already
  have finalized the socket's handle by then. At exit the sockets are now
  dropped without a close frame and the server stopped; `srv$stop()`,
  `stop_scene_servers()` and unloading aobcore still close each socket with
  1001.

* Selections from a served page come back to R over a websocket (decision
  0007 in allboa/design; #39, #40). It needs 'jsonlite' (in Suggests);
  without it pages are served as before, with no socket, and
  `serve_scene()` says so once per session. See `?serve_scene_socket`.
  - `serve_scene()`'s server takes a websocket at `/<token>/ws` only. The
    path (else 404, silent), the `Host` (else 403, warned as for every
    route) and the `Origin` (else 403) are checked in `onHeaders()` and
    again, first, in `onWSOpen()`, which closes a refused socket with 1008
    before registering or counting anything, since httpuv 1.6.17 switches
    protocols even after a refusal. Allowed origins are
    `http://127.0.0.1:<port>`, `http://localhost:<port>`, and `http://H`
    and `https://H` for each `aobcore.serve_hosts` value `H`; a missing
    `Origin` is refused. `Origin` refusals warn with the value escaped and
    cut to 80 bytes, for the first 5 distinct values per server, then once
    more. Every other upgrade answers 404 and is closed, as before.
  - Text frames only (binary: 1003), at most
    `getOption("aobcore.ws_max_message", 2^20)` bytes (1009, warned),
    UTF-8 JSON objects with a string `type` (else 1007, never httpuv's
    1011), `hello` first (else 1008), protocol 1 (else 4000), at most 8
    pages at once (1013, warned once per server). An error in R closes
    with 1011 and a warning. `stop()` closes every socket with 1001.
    Warnings about pages (closes, dropped messages, a spec mismatch) are
    given for the first 5 per server, then once more.
  - Protocol 1: the page sends `hello`, `select` (its whole selection, as
    0-based Arrow rows per layer) and `view`; R answers `hello` (connection
    number, scene serial, spec version, selectable layers and size cap) and
    sends `reload` when `serve_scene(server = srv)` replaces the scene,
    which gives it a new serial and clears the selection. Messages for
    another serial or with a stale `seq` are dropped silently; bad fields
    drop the message with one warning per page and type.
  - The served page carries `data-aob-scene-serial` and, with a socket,
    `data-aob-socket="ws"` on its page element. Embedded pages are
    unchanged.
  - The handle gains `selection()` (1-based Arrow rows per layer id, with
    `at`, `trigger`, `connection`, `seq`, `time` and `scene` attributes),
    `view_state()`, `wait()`, `on()` and `connections()`, and
    `serve_scene(select = )` names the selectable layers (default every
    vector layer). `wait()` inside an `on()` callback is an error, and the
    reads there skip running the event loop.
  - 'later' (already a dependency of httpuv) joins Suggests. The selection
    reads run every callback that is due (`later::run_now(0, all = TRUE)`),
    so messages queued while R was busy are all counted.
  - The renderer (0.0.5 bundle, #41) opens the socket once the scene is
    drawn, on pages whose element has `data-aob-socket`. The layers R names
    in its `hello` become selectable, popup or not: a click selects one
    feature, Shift or Cmd click adds or removes one (and closes the popup),
    a click on nothing or Escape clears (a Shift or Cmd click on nothing
    keeps the selection), and each change sends the whole selection; selected
    features are drawn highlighted in light and dark. The settled camera is
    sent as `view` (250 ms after its last change). `reload` reloads the
    page with its camera kept (only for a newer scene serial). A lost
    socket shows "Not connected to R: selections stay in this page" (or
    that too many pages are connected, for 1013) and retries (1 s doubling
    to 30 s, back to 1 s only once R has answered and the socket stayed
    open 5 s); a
    refusal that cannot change (1003, 1007, 1008, 4000) is not retried and
    the note says why. A selection over R's size cap stays in the page
    with a note. Embedded pages open no socket and behave as before.
  - An empty selection keeps its `layer` column in `srv$selection()`.

* `serve_scene()` follow-ups from the #37 review (#38): refused-`Host`
  warnings are given for the first 5 distinct values per server, then once
  more, and the record of them stops growing; `own = "."` (or any
  directory) is refused as not a regular file rather than as a link; and
  on macOS, as on Windows, an `own` path given in another case than the
  file's is not taken for a link.

* `serve_scene()` serves a scene from a local HTTP server (decision 0006 in
  allboa/design; #33, #34, #35), with httpuv in Suggests. The browser
  reads a local COG added with `scene_add_tiled_raster(embed = FALSE)` by
  HTTP range requests instead of carrying its bytes in the page.
  - Routes under `http://127.0.0.1:<port>/<token>/`: the linked page, the
    renderer, `blob/<encoded key>` (content type by the data reference's
    format) and `files/<data id>/<base name>` for registered files only.
    `/<token>` redirects to `/<token>/`; anything else is 404, methods
    other than `GET` and `HEAD` are 405, and there are no CORS headers.
  - Files answer one `bytes=` range with 206 (`a-b`, `a-`, `-n`); past the
    end, several ranges, malformed or longer than
    `getOption("aobcore.serve_range_max", 64 * 2^20)` is 416. A file
    changed since registration answers 409 and a missing one 404, each
    with a warning; `serve_scene()` on such a file is an error.
  - The served copy of the scene gives each registered file whose `url`
    was defaulted the relative URL `files/<data id>/<base name>`, so a
    served page never holds the local path. A cog with a relative `url`
    and neither a registered file nor embedded tiles is an error, as are
    the blob keys `"."` and `".."`.
  - Loopback only, a random 128-bit path token from `/dev/urandom` on
    Unix-alikes (elsewhere a weaker fallback, announced by a message, that
    does not resist other local users and makes the port a hint to the
    token) and a random port from separate bytes; neither touches
    `.Random.seed`. A
    `Host` other than `127.0.0.1:<port>` or `localhost:<port>` is refused
    (403, with one warning per distinct `Host`, the value escaped and cut
    short) unless listed in `getOption("aobcore.serve_hosts")`. Responses
    carry `X-Content-Type-Options: nosniff`, the page sends no referrer,
    `HEAD` sends headers only, and an undecodable path segment (such as
    `%00`) is 404.
  - The server accepted no websockets (until decision 0007 added one): an
    upgrade request on any path, with or without the token, answers 404,
    and httpuv's socket (which httpuv 1.6.17 opens even after a refusal)
    is closed at once. Before, any page, from any origin, could open one
    and make httpuv print "attempt to apply non-function" to the console.
  - The `"aob_server"` handle has `url`, `port`, `token` and an idempotent
    `stop()`, which also deletes files given in `own` (existing regular
    files, not symbolic links, kept by absolute path, and deleted only if
    their size and modification time are unchanged). `serve_scene(scene,
    server = srv)` replaces the scene on a running server with the same
    URL. `scene_servers()` lists running servers and
    `stop_scene_servers()` stops them; all stop at session end and when
    aobcore is unloaded. A non-interactive call warns that the server only
    answers while R is idle.
  - `write_scene_html()`'s warning for a registered file now names
    `serve_scene()`. The renderer stops a render that was replaced while
    its data loaded. `tools/serve-screenshots.R` draws the polar 3031 COG
    through `serve_scene()` and checks that every tile came as a 206 range.

* First steps of the local server transport (decision 0006 in
  allboa/design; #30, #31, #32). Nothing is served yet; embedded pages are
  byte-identical to before, blob order included.
  - `write_scene_html()` builds its page with an internal page builder that
    has an inline mode (the embedded page, unchanged) and a linked mode (the
    page a server answers: the renderer by `src`, no blob scripts, a
    `data-aob-blob-base` attribute on the page element and the served blob
    keys in one JSON script).
  - The bundled renderer fetches a blob the page does not carry from the
    blob base plus `encodeURIComponent(key)`, so keys with `/`, `@` and `+`
    are one path segment. A tiled raster takes a tile from the blob base
    only when the page lists its key, and otherwise reads the COG by range
    requests as before. A missing blob with no blob base is an error as
    before.
  - `scene_add_tiled_raster(embed = FALSE)` on a local COG registers the
    file in the scene's `"files"` attribute (normalized path, size,
    modification time, and whether `url` was given explicitly) and reads no
    tile bytes. The registry is never written to the scene JSON; the
    layer's `url` stays the `file://` URL (the full path) until a server
    replaces it, unless `url` was given. `write_scene_html()` on
    such a scene warns that the page cannot read the file from disk, and
    `embed = FALSE` on a `/vsimem/` COG (with no `url`) is an error that
    says to embed it or write it to a file. `cog_info()` records a local
    file's size and modification time, and `scene_add_tiled_raster()`
    refuses a file that has changed since (its tile plan is out of date).
  - `tools/write-scenes.R` also writes served pages in the server's route
    layout, and `js/screenshots.mjs --serve` draws them over loopback HTTP.

* The bundled renderer no longer drops clicks when picking is slow (#26).
  deck.gl picked on every press and dropped a click whose press outlasted
  its tap time limit, which a pick of a heavy polygon layer with software
  rendering could take by itself. The press no longer picks; a click is a
  primary press and release that moves less than 9 pixels, of any
  duration, picked once after the release. A drag pans and never selects,
  and a double click still zooms without selecting.

* Legends and popups (scene spec 0.5, #23). New `scene_add_legend()` adds a
  key to a layer's colours: a continuous ramp from a palette name or from
  colour stops (with the range ends labelled), discrete classes, and an
  optional no-data entry, checked against the 0.5 rules (the layer exists,
  ramp ends differ, stops run from 0 to 1 strictly increasing, a palette
  ramp only keys a palette layer and equals its palette and range). With no
  `ramp` or `classes`, a palette raster gets a ramp copied from its palette.
  `scene_add_layer()` and `scene_add_vector()` take `popup`: attribute
  columns shown for one feature at a time, on select (default) or while
  pointed at (`trigger = "point"`); the columns must be in the layer's data
  and not its geometry column. A scene is written as 0.5 only when it has
  legends or popups. The bundled renderer draws 0.5 legends (light and
  dark, labelled in text) and shows a popup for a selected point, path or
  polygon, closed by Escape or its close button; a popup column missing
  from the data is an error for that layer. Before 0.5 a palette raster's
  ramp is still drawn from its palette as before; in a 0.5 scene only the
  scene's own legends are drawn, so once a scene has any legend or popup,
  add `scene_add_legend()` for each palette layer that should keep its key.

* `crs_domain()` no longer crashes R when GDAL cannot find its PROJ
  database (`proj.db`) and the view CRS is a PROJ string (#21): it checks
  the database first and stops with an error, so `scene()` quietly writes
  no bounds. `scene()` also no longer prints GDAL's "Cannot find proj.db"
  error while it tries the domain (#17).
* New `rgba_array()` builds a per-row colour column
  (`FixedSizeList<uint8, 4>`) for a layer's `stroke` or `fill` (#21).
* `plan_extent()` is exported: the extent of a tile plan's finest level,
  for a default initial view (#17).

* Scenes have a view domain by default (allboa/design decision 0005):
  `scene()`, `cog_scene()` and `view_cog()` take `domain`, by default the
  new `crs_domain()` of the view CRS, which walks out from the projection
  centre until the stretch along each bearing passes `k = 2`. It is written
  as `view.bounds` (scene spec 0.4): the renderer keeps the camera within it
  plus a quarter of its size on each side, and a scene with no `extent`
  opens on its data clipped to it. It limits the camera only; data outside
  it still load. `domain = FALSE`, or `options(aobcore.domain = FALSE)`,
  turns it off and keeps the 0.1 to 0.3 output as before. The domain needs
  'gdalraster'; without it `scene()` writes no bounds, so the same code can
  write 0.4 with 'gdalraster' installed and 0.1 to 0.3 without.
* A view CRS can be any definition GDAL reads, not only an
  `"authority:code"` string: WKT, a PROJ string, PROJJSON text, an EPSG
  number or an `sf` `crs`. `scene()`, `cog_plan()`, `cog_scene()`,
  `view_cog()`, `vector_stream()` and `gdal_vector_stream()` resolve it with
  the new `scene_crs()`: a definition that is exactly an authority's CRS
  becomes its code, and one with no code is carried in the scene as a
  PROJJSON object, as the scene spec already allows. The renderer labels a
  PROJJSON CRS named "unknown" by its projection method.
* RGB(A) COGs are drawn as colour images (scene spec 0.3 `rgb`), not one
  band through a palette. `scene_add_tiled_raster()`, `cog_scene()` and
  `view_cog()` do so by default when the COG has 3 or 4 Byte bands whose
  colour interpretation is Red, Green, Blue (and Alpha); `band =`,
  `palette =` or `rgb = FALSE` keep the single-band palette path, and
  `rgb = c(r, g, b[, a])` picks the bands. Alpha 0, or all colour bands at
  nodata, is transparent. Non-Byte bands take `range` (by default the
  data's) (#13).
* JPEG-compressed COGs are no longer refused: each level's shared JPEG
  tables are carried in the plan (`encoding$jpeg_tables`, scene spec 0.3)
  and the renderer decodes each tile, joined to its tables, with the
  browser's JPEG decoder. Only YCbCr (3 bands) and greyscale (1 band) JPEG
  are allowed; other photometric interpretations are refused with an error
  naming the layer and level (#13).
* `cog_info()` records each band's colour interpretation (`color_interp`)
  and each level's TIFF photometric interpretation (`photometric`), and
  reads JPEGTables, from the TIFF image directories (classic TIFF and
  BigTIFF, through GDAL's virtual file layer, falling back to GDAL's
  metadata when they cannot be read) (#13).
* Scenes are written at the lowest spec version that expresses them: 0.3
  only when a tiled raster uses `rgb` or JPEG tiles, else 0.2 or 0.1 as
  before (#13).
* New fixtures `polar_rgba.tif` (RGBA, LZW) and `polar_ycbcr.tif` (YCbCr
  JPEG), made by `tools/make-rgb-cogs.R` (#13).
* With `embed = TRUE` (the default for a local COG), the scene's `cog`
  reference is the file's base name, not its absolute `file://` path, so a
  shared page no longer reveals the local directory.
  `scene_add_tiled_raster()`, `cog_scene()` and `view_cog()` take `url =`
  to write another reference (#11).
* `cog_plan()` has a tile budget, `max_tiles` (default 1024): an
  `all_levels` plan adds levels coarse to fine and leaves out, with a
  warning, a level that would pass it and every finer level. Pass `extent`
  to plan part of a large COG at full resolution, or `max_tiles = Inf` (#11).
* `view_cog()` and `cog_scene()` add the south-of-40S coastline by default
  only when the view CRS is centred on the South Pole (`coastline = NULL`);
  `TRUE` and `FALSE` still force it (#11).
* The renderer keeps at most 256 idle decoded tiles, dropping the least
  recently drawn, aborts tile fetches the view no longer needs, chooses the
  level and tiles again when the page is resized, and, when a server
  ignores `Range` and sends the whole COG, downloads it once and cuts every
  tile from it rather than downloading the file per tile (#11). Rendering
  again into the same element stops the previous scene, and the handle
  `aob.render()` returns has `finalize()`.
* CI runs the scenespec validator on the scenes `tools/write-scenes.R`
  writes, including `cog_scene()` output (#11).
* `gdal_vector_stream()`: `-nlt` in `options` counts as a native type only
  for point, line and polygon types (single or multi, with any Z, M or 25D
  suffix); `-nlt GEOMETRY` and other non-native types go through WKB and
  are converted in R instead of failing in GDAL, and `PROMOTE_TO_MULTI` and
  `CONVERT_TO_LINEAR` defer to the layer's type. The Arrow route drops Z
  and M in GDAL (`-dim XY`), as its driver writes 2D only (#9).
* `gdal_vector_stream()`'s Arrow route writes the PROJJSON CRS to the
  geometry field's `ARROW:extension:metadata`, as the R route does, so both
  routes give the same field (#9).
* Zero features (for example a clip that removes everything) give an empty
  stream of the declared geometry type: the layer's type in
  `gdal_vector_stream()`, and the `sfc` class or 'geoarrow' type in
  `vector_stream()`. Empty input with no declared native type is still an
  error, now documented (#9).

* A global lon/lat COG in a polar view no longer asks the browser for every
  full-resolution tile. `cog_plan()` measures each level's pixel size as the
  median over the grid (the far pole had pushed it to around 1e20 m, so the
  renderer always chose full resolution), and leaves out tiles stretched past
  `max_stretch` (default 8) times that size, with a message (the far polar
  cap of global data). The extent passed to `view_cog()` / `cog_scene()`,
  widened by half its size on each side, now also culls the plan's tiles.
* `cog_info()` and `view_cog()` accept a COG URL given as GDAL's
  `"/vsicurl/https://..."` path and give the renderer the plain URL; before,
  the page asked the browser for the `/vsicurl/` path and every tile failed.
  GDAL's `/vsicurl?url=...` form works too. `/vsis3/`, `/vsigs/` and
  `/vsiaz/` paths are given to the renderer as their public https URLs
  (public objects only); another `/vsi` path that is not embedded is an
  error, since the browser cannot fetch it.
* Tiled COGs per gate A (design decision 0003, R-planned tiles) and scene
  spec 0.2 (#5): `cog_info()` reads each level's grid and tile byte ranges
  through GDAL, `cog_plan()` plans tiles with meshes projected to the view
  CRS (one Arrow vertex table and one index table), `scene_add_tiled_raster()`
  adds a `tiled_raster` layer and a `cog` data reference, and `view_cog()`
  (with `cog_scene()`) draws a COG from R in one call. Local COGs travel with
  their planned tiles' bytes embedded in the page.
* A scene with a tiled raster is written as scene spec 0.2; other scenes are
  still 0.1 (`scene_spec_version(scene)`).
* The renderer draws 0.2 `tiled_raster` layers: level selection by the
  plan's rule, HTTP range requests or embedded tile bytes, codecs none,
  deflate, lzw, zstd and packbits (others are a layer error), predictors,
  scale/offset, nodata and palettes.
* Raster meshes are now drawn unlit; deck.gl lit them with a camera-dependent
  highlight despite `material: false`.
* Two small polar COG fixtures in `inst/extdata` (`polar_3031.tif`,
  `polar_lonlat.tif`), made by `tools/make-polar-cogs.R`.

* Vector producers to native, interleaved GeoArrow: `vector_stream()` for
  wk-handleable input, `gdal_vector_stream()` for GDAL sources (gdalraster,
  in Suggests), and `vector_ipc()` for Arrow IPC bytes (#3).
* `scene_add_data()`, `scene_add_layer()`, `scene_add_vector()`,
  `scene_blobs()` and `scene_json()` build scene spec 0.1 documents (#3).
* A Natural Earth 50m coastline fixture south of 40S in `inst/extdata`.
* `write_scene_html()` writes a self-contained page: the scene JSON, its
  Arrow IPC blobs as base64 and the bundled scene spec 0.1 renderer
  (deck.gl, built from `js/`), with no CDN (#4).
* `probe_scene()` is the polar probe conformance scene with its data in
  `inst/extdata/probe` (#4).
* `scene_json()` writes numbers with as many digits as they need to read
  back exactly (#4).
