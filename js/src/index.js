// allonboard renderer for scene spec 0.1 to 0.5, on deck.gl. 0.4 view.bounds keep
// the camera within them plus a quarter of their size on each side. 0.5
// legends are drawn from the scene's legends array (legend.js) and 0.5
// popups show a picked feature's attributes (popup.js).
//
// aob.render(container, scene, {blobs}) draws one scene into an element.
// blobs maps each data reference's blob key to Arrow IPC bytes (Uint8Array,
// ArrayBuffer or a base64 string); data references with a url are fetched.
// A 0.2 cog reference is not fetched whole: its tiled_raster layers fetch
// the planned tiles' byte ranges, or use blobs keyed
// "<source>@<offset>+<length>" that carry those bytes (see tiles.js). 0.3
// adds colour images (rgb) and jpeg tiles to tiled_raster layers.
// On load, every element with a data-aob-scene attribute is rendered from
// the JSON script it names and the blob scripts that point at it.
//
// A served page (decision 0006) carries no blob scripts. Its element has
// data-aob-blob-base (for example "blob/"), and render() is given that as
// options.blobBase: a blob key with no delivered bytes is then fetched from
// blobBase + encodeURIComponent(key), relative to the page, so a key with
// "/", "@" or "+" is one path segment. options.blobKeys lists the keys the
// server has (a <script type="application/json" data-aob-blob-keys>); a
// tiled raster fetches a tile from the blob base only when its key is
// listed, and otherwise reads the cog by range requests as before.
//
// Selections (decision 0007). A served page whose element also has
// data-aob-socket (a URL relative to the page, "ws") and
// data-aob-scene-serial (an integer) opens a websocket to R once the scene
// is drawn (options.socket and options.serial; see link.js for protocol 1).
// The layers R names in its hello become selectable: a click selects a
// feature, Shift or Cmd click adds or removes one (and closes the popup),
// a click on nothing or Escape clears (with Shift or Cmd held a click on
// nothing keeps the selection), and every change sends the whole selection; the settled
// camera is sent as a view message. R's reload reloads the page with its
// camera kept (sessionStorage, under the page's path). A page without
// options.socket (every embedded page) opens no socket and has no
// selection mode. For tools and tests the element carries the link's state
// (data-aob-link: connecting, open, ready, closed, refused or reloading),
// the selectable layer ids (data-aob-selectable) and the selection
// (data-aob-selection, "layer:row,row;layer:row").
import { Deck, OrthographicView, _GlobeView as GlobeView, COORDINATE_SYSTEM } from "@deck.gl/core";
import { decodeBase64, readTable } from "./arrow.js";
import { buildLayer } from "./layers.js";
import { buildTiledRaster } from "./tiles.js";
import { decodeTileSamples } from "./jpeg.js";
import { cssGradient } from "./palettes.js";
import { legendElement } from "./legend.js";
import { popupBox, popupRows } from "./popup.js";
import { CSS } from "./style.js";
import { socketChannel, socketUrl } from "./channel.js";
import { connectLink } from "./link.js";
import { selectionState, clickSelection, selectionText } from "./selection.js";

const VERSION = "0.0.5";
const SPECS = ["0.1", "0.2", "0.3", "0.4", "0.5"];
const atLeast = (v, min) => SPECS.indexOf(v) >= SPECS.indexOf(min);


function injectStyle() {
  if (document.getElementById("aob-style")) return;
  const el = document.createElement("style");
  el.id = "aob-style";
  el.textContent = CSS;
  document.head.appendChild(el);
}

function el(tag, cls, text) {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text !== undefined) e.textContent = text;
  return e;
}

// An extent [xmin, xmax, ymin, ymax] widened by `f` of its size on each side.
function padBounds(e, f) {
  const dx = (e[1] - e[0]) * f;
  const dy = (e[3] - e[2]) * f;
  return [e[0] - dx, e[1] + dx, e[2] - dy, e[3] + dy];
}

// The overlap of two extents, or null when they do not overlap.
function intersect(a, b) {
  const e = [Math.max(a[0], b[0]), Math.min(a[1], b[1]), Math.max(a[2], b[2]), Math.min(a[3], b[3])];
  return e[0] < e[1] && e[2] < e[3] ? e : null;
}

function crsLabel(crs) {
  if (crs === undefined) return "no CRS";
  if (typeof crs === "string") return crs;
  if (crs.id && crs.id.authority) return `${crs.id.authority}:${crs.id.code}`;
  // A CRS read from a PROJ string is named "unknown"; its projection
  // method says more.
  const method = crs.conversion && crs.conversion.method && crs.conversion.method.name;
  if (crs.name && crs.name !== "unknown") return crs.name;
  return method || crs.name || "PROJJSON";
}

// The URL of a blob under a blob base: the key is one path segment.
function blobUrl(base, key) {
  return base + encodeURIComponent(key);
}

// Fetch one blob from the blob base; `what` names it in errors. A network
// failure is an error naming the key and URL; an abort stays an abort.
async function fetchBlob(base, key, what, signal) {
  const url = blobUrl(base, key);
  let res;
  try {
    res = await fetch(url, signal ? { signal } : undefined);
  } catch (err) {
    if (signal && signal.aborted) throw err;
    throw new Error(`${what}: blob "${key}" could not be fetched from ${url}: ${err && err.message ? err.message : err}`);
  }
  if (!res.ok) throw new Error(`${what}: blob "${key}" was not delivered (${url} returned ${res.status})`);
  return new Uint8Array(await res.arrayBuffer());
}

async function loadBytes(ref, id, blobs, blobBase, signal) {
  if (ref.blob !== undefined) {
    const b = blobs[ref.blob];
    if (b === undefined) {
      if (typeof blobBase === "string") return fetchBlob(blobBase, ref.blob, `data ${id}`, signal);
      throw new Error(`data ${id}: blob "${ref.blob}" was not delivered`);
    }
    if (typeof b === "string") return decodeBase64(b.trim());
    return b instanceof Uint8Array ? b : new Uint8Array(b);
  }
  const res = await fetch(ref.url, signal ? { signal } : undefined);
  if (!res.ok) throw new Error(`data ${id}: ${ref.url} returned ${res.status}`);
  return new Uint8Array(await res.arrayBuffer());
}

function checkScene(scene) {
  if (!scene || !SPECS.includes(scene.version)) {
    throw new Error(`this renderer draws scene spec ${SPECS.join(", ")}; got ${JSON.stringify(scene && scene.version)}`);
  }
  for (const k of ["view", "data", "layers"]) {
    if (!scene[k]) throw new Error(`scene has no ${k}`);
  }
}

// Theme: follows prefers-color-scheme unless the root element carries
// data-theme="light" or "dark". The button cycles auto, light, dark.
function themeButton(onChange) {
  const root = document.documentElement;
  const order = ["auto", "light", "dark"];
  const b = el("button", "aob-theme", "");
  b.type = "button";
  const show = () => {
    const t = root.dataset.theme || "auto";
    b.textContent = `Theme: ${t}`;
    b.setAttribute("aria-label", `Colour theme: ${t}. Press to change.`);
  };
  b.addEventListener("click", () => {
    const t = root.dataset.theme || "auto";
    const next = order[(order.indexOf(t) + 1) % order.length];
    if (next === "auto") delete root.dataset.theme;
    else root.dataset.theme = next;
    show();
    if (onChange) onChange();
  });
  show();
  return b;
}

// A CSS colour token as RGBA 0-255: "#rgb", "#rrggbb" or "#rrggbbaa".
function tokenColor(node, name, fallback) {
  const v = getComputedStyle(node).getPropertyValue(name).trim();
  let m = /^#([0-9a-f]{6})([0-9a-f]{2})?$/i.exec(v);
  if (m) {
    const n = parseInt(m[1], 16);
    return [(n >> 16) & 255, (n >> 8) & 255, n & 255, m[2] ? parseInt(m[2], 16) : 255];
  }
  m = /^#([0-9a-f])([0-9a-f])([0-9a-f])$/i.exec(v);
  if (m) return [1, 2, 3].map((i) => parseInt(m[i] + m[i], 16)).concat(255);
  return fallback;
}

// Where a served page keeps its camera across a reload from R: under a
// hash of the page's path (which holds the server's token), not the path.
function hashText(s) {
  let h1 = 0xdeadbeef;
  let h2 = 0x41c6ce57;
  for (let i = 0; i < s.length; i++) {
    const c = s.charCodeAt(i);
    h1 = Math.imul(h1 ^ c, 2654435761);
    h2 = Math.imul(h2 ^ c, 1597334677);
  }
  h1 = Math.imul(h1 ^ (h1 >>> 16), 2246822507) ^ Math.imul(h2 ^ (h2 >>> 13), 3266489909);
  h2 = Math.imul(h2 ^ (h2 >>> 16), 2246822507) ^ Math.imul(h1 ^ (h1 >>> 13), 3266489909);
  return (h2 >>> 0).toString(16).padStart(8, "0") + (h1 >>> 0).toString(16).padStart(8, "0");
}
const cameraKey = () => `aob-camera:${hashText(new URL(".", location.href).pathname)}`;
function readCamera() {
  try {
    const s = sessionStorage.getItem(cameraKey());
    sessionStorage.removeItem(cameraKey());
    return s ? JSON.parse(s) : null;
  } catch (err) {
    return null;
  }
}
function writeCamera(v) {
  try {
    sessionStorage.setItem(cameraKey(), JSON.stringify(v));
  } catch (err) {
    // no storage: the reloaded page starts from the scene's view
  }
}

function fmt(v, span) {
  if (!isFinite(v)) return "-";
  const digits = span > 1000 ? 0 : span > 10 ? 2 : 5;
  return v.toLocaleString("en-US", { minimumFractionDigits: digits, maximumFractionDigits: digits });
}

// Teardown of the scene last rendered into each container.
const teardowns = new WeakMap();

export async function render(container, scene, options = {}) {
  injectStyle();
  // Rendering again into a container replaces its scene: stop the old one.
  if (teardowns.has(container)) teardowns.get(container)();
  // Aborts this render's data fetches when the container is rendered again
  // (or options.signal aborts) before they finish.
  const loading = new AbortController();
  if (options.signal) {
    if (options.signal.aborted) loading.abort(options.signal.reason);
    else options.signal.addEventListener("abort", () => loading.abort(options.signal.reason), { once: true });
  }
  const stopLoading = () => loading.abort();
  teardowns.set(container, stopLoading);
  const blobs = options.blobs || {};
  const blobBase = typeof options.blobBase === "string" ? options.blobBase : null;
  const blobKeys = new Set(blobBase !== null && options.blobKeys ? options.blobKeys : []);
  const socketRel = typeof options.socket === "string" && options.socket ? options.socket : null;
  const serial = socketRel !== null && Number.isInteger(options.serial) ? options.serial : null;
  const warnings = [];
  const errors = [];
  const warn = (m) => {
    warnings.push(m);
    console.warn(`aob: ${m}`);
  };
  // A layer-level error: that layer is not drawn, the rest of the scene is.
  const layerError = (m) => {
    errors.push(m);
    console.warn(`aob: error: ${m}`);
    showNotes();
  };
  container.classList.add("aob-root");
  container.textContent = "";
  const map = el("div", "aob-map");
  const canvasHost = el("div", "aob-canvas");
  const tag = el("div", "aob-tag");
  const readout = el("div", "aob-readout");
  const status = el("div", "aob-status");
  status.setAttribute("role", "status");
  const panel = el("aside", "aob-panel");
  map.append(canvasHost, tag, readout);
  // The link to R's note (served pages with a socket only).
  const linkNote = socketRel !== null ? el("div", "aob-link") : null;
  if (linkNote) {
    linkNote.setAttribute("role", "status");
    linkNote.hidden = true;
    map.append(linkNote);
  }
  container.append(map, panel, status);
  const setStatus = (msg, isError) => {
    status.textContent = msg;
    status.classList.toggle("aob-error", !!isError);
    status.hidden = !msg;
  };
  let showNotes = () => {};

  try {
    checkScene(scene);
    const view = scene.view;
    const globe = view.type === "globe";
    tag.textContent = `${crsLabel(view.crs)} / ${view.type}`;

    const t0 = performance.now();
    const tables = {};
    // Arrow tables are read whole; a cog is read tile by tile by its layers.
    const ids = Object.keys(scene.data).filter((id) => scene.data[id].format !== "cog");
    const bytes = await Promise.all(ids.map((id) => loadBytes(scene.data[id], id, blobs, blobBase, loading.signal)));
    // Rendered again while loading: this render stops here.
    if (loading.signal.aborted) throw loading.signal.reason;
    let total = 0;
    ids.forEach((id, i) => {
      total += bytes[i].length;
      tables[id] = readTable(bytes[i]);
    });
    const decodeMs = performance.now() - t0;

    let pending = 0;
    const layerStatus = {};
    // deck layer id -> how its picked parts map to feature rows (0.5 popups).
    const picks = new Map();
    // scene layer id -> functions drawing its selected rows, per batch.
    const highlights = new Map();
    const ctx = {
      scene,
      tables,
      blobs,
      // A tile blob the page does not carry but the server has (decision 0006).
      servedBlob: (key, what, signal) => (blobKeys.has(key) ? fetchBlob(blobBase, key, what, signal) : null),
      warn,
      error: layerError,
      pending: (d) => {
        pending += d;
        container.dataset.aobPending = String(pending);
      },
      redraw: () => update(),
      status: (id, text) => {
        layerStatus[id] = text;
      },
      coordinateSystem: globe ? COORDINATE_SYSTEM.LNGLAT : COORDINATE_SYSTEM.CARTESIAN,
      bounds: [Infinity, -Infinity, Infinity, -Infinity],
      pickable: (deckId, info) => picks.set(deckId, info),
      highlight: socketRel !== null ? (layerId, f) => {
        if (!highlights.has(layerId)) highlights.set(layerId, []);
        highlights.get(layerId).push(f);
      } : null,
    };
    const built = scene.layers.map((L) => {
      if (globe && (L.kind === "raster" || L.kind === "tiled_raster")) {
        warn(`layer ${L.id}: rasters are not drawn on a globe by this renderer yet`);
        return { layers: [], summary: "not drawn on a globe" };
      }
      if (L.kind === "tiled_raster") return buildTiledRaster(L, ctx);
      return buildLayer(L, ctx);
    });
    const visible = scene.layers.map((L) => L.visible !== false);

    // Layer list (top of the list draws on top) and legends. From 0.5 the
    // legends are the scene's own (shown while their layer is shown);
    // before 0.5 a palette raster gets a ramp from its palette.
    panel.append(el("h2", "aob-h", "Layers"));
    const list = el("div", "aob-layers");
    const legends = el("div", "aob-legends");
    const specLegends = atLeast(scene.version, "0.5");
    const legendEls = [];
    if (specLegends) {
      const byId = new Map(scene.layers.map((L, i) => [L.id, i]));
      (scene.legends || []).forEach((lg) => {
        const i = byId.get(lg.layer);
        const box = legendElement(lg, scene.layers[i], layerError);
        box.hidden = i === undefined || !visible[i];
        legendEls.push([i, box]);
        legends.append(box);
      });
    }
    const showLegends = () => legendEls.forEach(([i, box]) => {
      box.hidden = i === undefined || !visible[i];
    });
    scene.layers.forEach((L, i) => {
      const lab = el("label", "aob-layer");
      const cb = el("input");
      cb.type = "checkbox";
      cb.checked = visible[i];
      cb.addEventListener("change", () => {
        visible[i] = cb.checked;
        if (!cb.checked && popup.current && popup.current.layer === L) popup.hide();
        showLegends();
        update();
      });
      lab.append(cb, el("span", "aob-name", L.label || L.id), el("small", "aob-count", built[i].summary));
      list.prepend(lab);
      const lg = specLegends ? null : built[i].legend;
      if (lg) {
        const box = el("div", "aob-legend");
        box.append(el("div", "aob-legend-title", lg.label));
        const bar = el("div", "aob-legend-bar");
        bar.style.background = cssGradient(lg.stops);
        const scale = el("div", "aob-legend-scale");
        scale.append(el("span", "", String(lg.range[0])), el("span", "", String(lg.range[1])));
        box.append(bar, scale);
        legends.append(box);
      }
    });
    if (legends.childElementCount) {
      legends.setAttribute("role", "group");
      legends.setAttribute("aria-label", "Legends");
    }
    panel.append(list, legends, themeButton(() => update()));

    // 0.5 popups. "point" shows while the pointer is over a feature; a
    // pointer that cannot hover (touch) makes it act as select. Decided per
    // event from its pointer type, since one page can see mouse and touch;
    // with no pointer type, from whether the device can hover.
    const canHover = () => typeof matchMedia === "function" && matchMedia("(hover: hover)").matches;
    const triggerOf = (L, event) => {
      const t = (L.popup && L.popup.trigger) || "select";
      if (t !== "point") return t;
      const kind = event && event.srcEvent && event.srcEvent.pointerType;
      const hovers = kind ? kind !== "touch" : canHover();
      return hovers ? "point" : "select";
    };
    map.tabIndex = -1;
    const popup = popupBox(map, () => delete container.dataset.aobSelected);
    // The feature under a picking result, or null.
    const featureAt = (info) => {
      if (!info || !info.picked || !info.layer) return null;
      const p = picks.get(info.layer.id);
      if (!p || info.index < 0 || info.index >= p.feature.length) return null;
      return { layer: p.layer, table: p.table, row: p.rowOffset + p.feature[info.index], popup: p.popup };
    };
    // A feature of a layer picked only for selection has no popup.
    const showFeature = (f, x, y, trigger, focus) => {
      if (!f.popup) return;
      const rows = popupRows(f.table, f.layer.popup.columns, f.row);
      popup.show({ layer: f.layer, row: f.row, rows, x, y, trigger }, focus);
      container.dataset.aobSelected = `${f.layer.id}:${f.row}`;
    };

    // Initial view: extent, then center, then the data bounds (clipped to
    // view.bounds when the scene has them), then the bounds themselves.
    const b = ctx.bounds;
    let ext = view.extent;
    if (!ext && isFinite(b[0])) ext = b[1] > b[0] && b[3] > b[2] ? b : [b[0] - 1, b[1] + 1, b[2] - 1, b[3] + 1];
    // 0.4 view.bounds: the camera is kept within them plus a margin of a
    // quarter of their size on each side, so their edge can be seen.
    const limits = !globe && Array.isArray(view.bounds) ? padBounds(view.bounds, 0.25) : null;
    if (limits && !view.extent) ext = (ext && intersect(ext, view.bounds)) || view.bounds;
    const center = view.center || (ext ? [(ext[0] + ext[1]) / 2, (ext[2] + ext[3]) / 2] : [0, 0]);
    const span = ext ? Math.max(ext[1] - ext[0], ext[3] - ext[2]) : 1;
    const fitZoom = () => {
      if (!ext) return 0;
      const w = canvasHost.clientWidth || 800;
      const h = canvasHost.clientHeight || 600;
      return Math.log2(Math.min(w / (ext[1] - ext[0]), h / (ext[3] - ext[2])) * 0.95);
    };
    // A camera kept by a reload from R (item 5): only from an older scene
    // serial of this page, in the same view CRS.
    let kept = null;
    if (serial !== null) {
      const c = readCamera();
      if (c && Number.isInteger(c.serial) && c.serial < serial && c.type === view.type &&
          c.crs === JSON.stringify(view.crs === undefined ? null : view.crs)) {
        const z = Number(c.zoom);
        if (globe && Array.isArray(c.center) && c.center.every(isFinite) && isFinite(z)) {
          kept = { globe: { longitude: c.center[0], latitude: c.center[1], zoom: z } };
        } else if (!globe && Array.isArray(c.target) && c.target.every(isFinite) && isFinite(z)) {
          kept = { ortho: { target: [c.target[0], c.target[1], 0], zoom: z } };
        }
        if (kept) container.dataset.aobCameraKept = "true";
      }
    }
    let deckView;
    let initialViewState;
    if (globe) {
      deckView = new GlobeView({ id: "aob", controller: true });
      const z = ext ? Math.log2(360 / Math.max(ext[1] - ext[0], (ext[3] - ext[2]) * 2)) : 0;
      initialViewState = { longitude: center[0], latitude: center[1], zoom: Math.max(0, z) };
      if (kept && kept.globe) initialViewState = { ...initialViewState, ...kept.globe };
    } else {
      deckView = new OrthographicView({ id: "aob", flipY: false, controller: true });
      const z = fitZoom();
      initialViewState = { target: [center[0], center[1], 0], zoom: z, minZoom: z - 4, maxZoom: z + 12 };
      if (kept && kept.ortho) initialViewState = { ...initialViewState, ...kept.ortho };
      if (limits) initialViewState = clampView(initialViewState);
    }

    // Keep an orthographic camera within `limits`: zooming out stops when
    // they fill the canvas, and panning stops at their edges. deck's
    // controller reads zoomX and zoomY before zoom, so those follow zoom
    // when present, and the target keeps its length (deck compares it).
    function clampView(vs) {
      const w = canvasHost.clientWidth || 800;
      const h = canvasHost.clientHeight || 600;
      const minZoom = Math.log2(Math.min(w / (limits[1] - limits[0]), h / (limits[3] - limits[2])));
      let zoom = Array.isArray(vs.zoom) ? vs.zoom[0] : vs.zoom;
      zoom = Math.max(zoom, minZoom);
      const u = Math.pow(2, -zoom);
      const axis = (t, lo, hi, half) => (hi - lo <= 2 * half ? (lo + hi) / 2 : Math.min(Math.max(t, lo + half), hi - half));
      const t = vs.target || [0, 0, 0];
      const target = [axis(t[0], limits[0], limits[1], (w / 2) * u), axis(t[1], limits[2], limits[3], (h / 2) * u)];
      if (t.length !== 2) target.push(t[2] || 0);
      const out = { ...vs, zoom, target, minZoom };
      if ("zoomX" in vs) Object.assign(out, { zoomX: zoom, zoomY: zoom, minZoomX: minZoom, minZoomY: minZoom });
      return out;
    }

    // The part of the view CRS on screen and the size of one device pixel
    // in view units, for tiled rasters. OrthographicView: 2^zoom CSS pixels
    // per view unit.
    let viewState = initialViewState;
    const currentView = () => {
      const w = canvasHost.clientWidth || 800;
      const h = canvasHost.clientHeight || 600;
      const z = Array.isArray(viewState.zoom) ? viewState.zoom[0] : viewState.zoom;
      const u = Math.pow(2, -z);
      const t = viewState.target || [0, 0];
      return {
        bounds: [t[0] - (w / 2) * u, t[0] + (w / 2) * u, t[1] - (h / 2) * u, t[1] + (h / 2) * u],
        unitsPerPixel: u / (window.devicePixelRatio || 1),
      };
    };

    // Selection mode (served pages with a socket): the layers R lets the
    // viewer select, the selection, and the link to R.
    let selectable = new Set();
    const selection = selectionState();
    let link = null;
    const selectionChanged = (trigger, at) => {
      const items = selection.items();
      if (items.length) container.dataset.aobSelection = selectionText(items);
      else delete container.dataset.aobSelection;
      update();
      if (link) link.selected(trigger, at);
    };

    let ready = false;
    let frames = 0;
    let deck = null;
    let finalized = false;
    const deckProps = {
      parent: canvasHost,
      views: deckView,
      initialViewState,
      layers: [],
      onViewStateChange: ({ viewState: vs, interactionState }) => setView(vs, interactionState),
      onHover: (info, event) => {
        const c = info.coordinate;
        readout.textContent = c ? `x ${fmt(c[0], span)}   y ${fmt(c[1], span)}` : "";
        const f = featureAt(info);
        const cur = popup.current;
        if (f && triggerOf(f.layer, event) === "point") {
          if (!cur || cur.trigger === "point") showFeature(f, info.x, info.y, "point", false);
        } else if (cur && cur.trigger === "point") {
          popup.hide();
        }
      },
      // No onClick: selection is our own press and release (see clicks
      // below), so a slow pick cannot make deck's tap time out.
      getCursor: ({ isDragging, isHovering }) => (isDragging ? "grabbing" : isHovering ? "pointer" : "grab"),
      onError: (err) => setStatus(`Rendering error: ${err && err.message ? err.message : err}`, true),
      onAfterRender: () => {
        if (!ready && ++frames >= 2 && pending === 0) {
          ready = true;
          container.dataset.aobStatus = "ready";
          if (options.onReady) options.onReady(handle);
          // The socket opens once the scene is drawn, so it never delays it.
          if (socketRel !== null && !finalized) startLink();
        }
      },
    };
    // Every camera change goes through here, so view.bounds hold for
    // interaction, resize and handle.setView() alike. The one exception is
    // a frame of one of deck's own transitions (a double click or a key
    // zooms over 300 ms): its end was clamped when it started, and deck
    // carries on only while it is handed its own frames back unchanged; a
    // frame it does not recognise ends the transition where it is (#28).
    function setView(vs, interaction) {
      const frame = !!(interaction && interaction.inTransition && !vs.transitionDuration);
      viewState = limits && !frame ? clampView(vs) : vs;
      if (limits && deck) deck.setProps({ viewState });
      update();
      if (link) link.viewChanged();
      return viewState;
    }
    function update() {
      if (!deck || finalized) return;
      const layers = [];
      const view = globe ? null : currentView();
      built.forEach((B, i) => {
        // A tiled raster's layers depend on the view; a hidden one loads nothing.
        const ls = B.dynamic ? (visible[i] ? B.dynamic(view) : []) : B.layers;
        const sel = selectable.has(scene.layers[i].id);
        ls.forEach((l) => {
          // A layer R lets the viewer select is pickable, popup or not.
          const p = picks.get(l.id);
          const more = sel && p && !p.popup ? { pickable: true, autoHighlight: true } : {};
          layers.push(l.clone({ visible: visible[i], ...more }));
        });
      });
      // Selected features, over every layer.
      if (selection.size) {
        const colors = {
          line: tokenColor(container, "--aob-sel", [194, 24, 91, 255]),
          halo: tokenColor(container, "--aob-sel-halo", [255, 255, 255, 255]),
          fill: tokenColor(container, "--aob-sel-fill", [194, 24, 91, 77]),
        };
        scene.layers.forEach((L, i) => {
          const rows = selection.rowsOf(L.id);
          if (!visible[i] || !rows || !highlights.has(L.id)) return;
          for (const f of highlights.get(L.id)) layers.push(...f(rows, colors));
        });
      }
      deck.setProps({ layers });
      const tiled = Object.entries(layerStatus).map(([id, t]) => `${id}: ${t}`).join("; ");
      if (tiled) container.dataset.aobTiles = tiled;
      else delete container.dataset.aobTiles;
    }
    deck = new Deck(deckProps);
    update();

    // Clicks (#26). deck.gl picks synchronously on every pointerdown, to
    // hand the pressed object to onClick and the onDrag* callbacks, whether
    // or not anything uses them. That pick draws every pickable layer and
    // reads back a pixel; with software rendering and a heavy polygon layer
    // it takes seconds, and its tap recognizer then sees a press longer
    // than its time limit and drops the click. Nothing here is dragged, so
    // picking is switched off for the length of deck's own pointerdown
    // handler (on the canvas, between our capture and bubble listeners on
    // its parent), and a click is our own: a primary press and release
    // that never moves more than `slop` pixels, of any duration. A drag
    // moves further, so it pans and never selects. The pick runs once,
    // after the release, at the pressed point. Like deck's click, it waits
    // for the double-click interval, and a double click (which zooms)
    // cancels it. Whether two clicks are a double click is decided at the
    // second release by deck's own rules (#29): both presses shorter than
    // `tapMs`, released within `dblMs` of each other, `near` pixels apart.
    // Two clicks that are not a double click both select, and a third
    // click after a double click is a first click again, as it is to deck.
    const slop = 9; // px, deck's tap threshold
    const dblMs = 300; // deck's double-tap interval, release to release
    const tapMs = 250; // deck's longest tap
    const near = 10; // px, deck's double-tap distance
    let press = null;
    let pendingSelect = null;
    let lastUp = null;
    const offPress = () => {
      window.removeEventListener("pointermove", onPressMove, true);
      window.removeEventListener("pointerup", onPressUp, true);
      window.removeEventListener("pointercancel", onPressCancel, true);
      press = null;
    };
    const selectAt = (cx, cy, srcEvent) => {
      if (finalized) return;
      const r = (deck.getCanvas() || canvasHost).getBoundingClientRect();
      const x = cx - r.left;
      const y = cy - r.top;
      const info = deck.pickObject({ x, y, radius: deck.props.pickingRadius || 0 });
      const f = featureAt(info);
      const multi = selectable.size > 0 && !!(srcEvent && (srcEvent.shiftKey || srcEvent.metaKey));
      // A Shift or Cmd click edits the selection: no popup over the next
      // feature to pick.
      if (multi) popup.hide();
      else if (f && triggerOf(f.layer, { srcEvent }) === "select") showFeature(f, x, y, "select", true);
      else if (!f) popup.hide();
      if (selectable.size) {
        const hit = f && selectable.has(f.layer.id) ? { layer: f.layer.id, row: f.row } : null;
        selectionChanged(clickSelection(selection, hit, multi), pointAt(x, y));
      }
    };
    // The pressed point in view CRS units, or [lon, lat] on a globe.
    const pointAt = (x, y) => {
      const vp = deck.getViewports()[0];
      const c = vp ? vp.unproject([x, y]) : null;
      return c && isFinite(c[0]) && isFinite(c[1]) ? [c[0], c[1]] : undefined;
    };
    // Escape clears the selection (a popup with focus takes Escape first).
    const onKey = (e) => {
      if (e.key !== "Escape" || !selectable.size || e.defaultPrevented) return;
      if (e.type === "keydown" && e.currentTarget === window &&
          e.target !== document.body && e.target !== document.documentElement) return;
      if (selection.clear()) selectionChanged("clear");
    };
    container.addEventListener("keydown", onKey);
    window.addEventListener("keydown", onKey);
    function onPressMove(e) {
      if (press && e.pointerId === press.id &&
          Math.hypot(e.clientX - press.x, e.clientY - press.y) >= slop) press.moved = true;
    }
    function onPressUp(e) {
      if (!press || e.pointerId !== press.id) return;
      const p = press;
      offPress();
      if (p.moved || Math.hypot(e.clientX - p.x, e.clientY - p.y) >= slop) {
        lastUp = null;
        return;
      }
      const t = e.timeStamp;
      const short = t - p.t < tapMs;
      const prev = lastUp;
      if (short && prev && prev.short && t - prev.t < dblMs &&
          Math.hypot(e.clientX - prev.x, e.clientY - prev.y) < near) {
        // A double click: deck zooms, and neither click selects.
        clearTimeout(pendingSelect);
        pendingSelect = null;
        lastUp = null;
        return;
      }
      lastUp = { t, x: e.clientX, y: e.clientY, short };
      // An earlier click whose select is still waiting keeps it.
      const id = setTimeout(() => {
        if (pendingSelect === id) pendingSelect = null;
        selectAt(p.x, p.y, e);
      }, dblMs);
      pendingSelect = id;
    }
    function onPressCancel(e) {
      if (press && e.pointerId === press.id) offPress();
    }
    const onDownCapture = (e) => {
      if (deck.props._pickable !== false) {
        deck.setProps({ _pickable: false });
        setTimeout(onDownBubble, 0); // in case the bubble listener is not reached
      }
      if (!e.isPrimary || e.button !== 0) {
        if (press) press.moved = true; // a second finger (pinch) or button
        return;
      }
      offPress();
      press = { id: e.pointerId, x: e.clientX, y: e.clientY, t: e.timeStamp, moved: false };
      window.addEventListener("pointermove", onPressMove, true);
      window.addEventListener("pointerup", onPressUp, true);
      window.addEventListener("pointercancel", onPressCancel, true);
    };
    const onDownBubble = () => {
      // Also run from a timer, which can come after finalize() (#29).
      if (!finalized && deck.props._pickable === false) deck.setProps({ _pickable: true });
    };
    canvasHost.addEventListener("pointerdown", onDownCapture, true);
    canvasHost.addEventListener("pointerdown", onDownBubble);
    // Leaving the canvas ends the hover (#45). deck calls onHover with
    // nothing, but picks no layers for a point outside every viewport, so
    // an autoHighlight outline would stay on the last feature hovered. Clear
    // it as deck's own hover pick does, and forget that feature, so the
    // next hover over it highlights it again.
    const leaveCanvas = deck.getCanvas() || canvasHost;
    const onLeave = () => {
      const last = deck.deckPicker && deck.deckPicker.lastPickedInfo;
      if (finalized || !last || last.layerId === null) return;
      let layer = deck.layerManager.getLayers().find((l) => l.props.id === last.layerId);
      while (layer && layer.parent) layer = layer.parent;
      if (layer) layer.updateAutoHighlight({ picked: false, color: null, index: -1, layer, object: null });
      last.layerId = null;
      last.index = -1;
      last.info = null;
    };
    leaveCanvas.addEventListener("pointerleave", onLeave);
    // The view's extent and pixel size change with the element's size, so
    // tiled rasters choose their level and tiles again on resize.
    const onResize = () => {
      if (limits && deck) {
        viewState = clampView(viewState);
        deck.setProps({ viewState });
      }
      update();
    };
    // Highlight colours follow the theme.
    const darkQuery = typeof matchMedia === "function" ? matchMedia("(prefers-color-scheme: dark)") : null;
    const onScheme = () => update();
    if (darkQuery) darkQuery.addEventListener("change", onScheme);
    let observer = null;
    if (typeof ResizeObserver !== "undefined") {
      observer = new ResizeObserver(onResize);
      observer.observe(canvasHost);
    } else {
      window.addEventListener("resize", onResize);
    }
    // The link to R (decision 0007): protocol 1 over the page's socket.
    const viewMessage = () => {
      const w = canvasHost.clientWidth || 800;
      const h = canvasHost.clientHeight || 600;
      const z = Array.isArray(viewState.zoom) ? viewState.zoom[0] : viewState.zoom;
      if (globe) return { center: [viewState.longitude, viewState.latitude], zoom: z, size_px: [w, h] };
      const v = currentView();
      return { extent: v.bounds, zoom: z, units_per_pixel: v.unitsPerPixel, size_px: [w, h] };
    };
    const reloadPage = () => {
      const z = Array.isArray(viewState.zoom) ? viewState.zoom[0] : viewState.zoom;
      const cam = { serial, type: view.type, crs: JSON.stringify(view.crs === undefined ? null : view.crs), zoom: z };
      if (globe) cam.center = [viewState.longitude, viewState.latitude];
      else cam.target = [viewState.target[0], viewState.target[1]];
      writeCamera(cam);
      container.dataset.aobLink = "reloading";
      location.reload();
    };
    function startLink() {
      if (link) return;
      link = connectLink((onState) => socketChannel(socketUrl(socketRel, location.href), { onState }), {
        serial,
        renderer: VERSION,
        specs: SPECS,
        selection: () => selection.items(),
        view: viewMessage,
        setSelectable: (ids) => {
          const vector = new Set(scene.layers.filter((L) => ["polygon", "path", "point"].includes(L.kind)).map((L) => L.id));
          selectable = new Set(ids.filter((id) => vector.has(id)));
          container.dataset.aobSelectable = [...selectable].join(",");
          selection.keepLayers(selectable);
          update();
        },
        reload: reloadPage,
        note: (text) => {
          linkNote.textContent = text || "";
          linkNote.hidden = !text;
        },
        state: (st) => {
          container.dataset.aobLink = st;
        },
      });
    }
    const finalize = () => {
      if (finalized) return;
      finalized = true;
      if (link) link.close();
      container.removeEventListener("keydown", onKey);
      window.removeEventListener("keydown", onKey);
      if (darkQuery) darkQuery.removeEventListener("change", onScheme);
      loading.abort();
      offPress();
      clearTimeout(pendingSelect);
      canvasHost.removeEventListener("pointerdown", onDownCapture, true);
      canvasHost.removeEventListener("pointerdown", onDownBubble);
      leaveCanvas.removeEventListener("pointerleave", onLeave);
      if (observer) observer.disconnect();
      else window.removeEventListener("resize", onResize);
      popup.hide();
      delete container.dataset.aobSelected;
      deck.finalize();
      if (teardowns.get(container) === finalize) teardowns.delete(container);
    };
    teardowns.set(container, finalize);
    const kib = (total / 1024).toFixed(0);
    showNotes = () => {
      const notes = errors.map((m) => `error: ${m}`).concat(warnings);
      setStatus(notes.join("; "), errors.length > 0);
      if (errors.length) container.dataset.aobErrors = String(errors.length);
    };
    showNotes();
    // handle.finalize() stops the scene: resize tracking and the deck.
    // handle.view() is the camera; handle.setView(vs) moves it, clamped to
    // view.bounds as interaction is, and returns where it ended up.
    // handle.selected() is the feature whose popup is open, as
    // {layer, row}, or null; handle.closePopup() dismisses it.
    // handle.selection() is the selection mode's selection, [{layer, rows}]
    // (empty on a page with no link to R).
    const handle = { deck, scene, tables, decodeMs, bytes: total, warnings, errors, finalize,
                     view: () => viewState, setView, selection: () => selection.items(),
                     selected: () => (popup.current ? { layer: popup.current.layer.id, row: popup.current.row } : null),
                     closePopup: () => popup.hide() };
    container.dataset.aobInfo = `${kib} KiB Arrow decoded in ${decodeMs.toFixed(1)} ms`;
    return handle;
  } catch (err) {
    // Replaced by a newer render of this container: leave it alone.
    if (loading.signal.aborted && teardowns.get(container) !== stopLoading) throw err;
    if (teardowns.get(container) === stopLoading) teardowns.delete(container);
    container.dataset.aobStatus = "error";
    setStatus(`This scene could not be drawn: ${err && err.message ? err.message : err}`, true);
    console.error(err);
    throw err;
  }
}

// Read a scene and its blobs from script elements in the page. A served
// page's element (`container`, by default the first non-script element
// naming the scene) gives a blob base, and a JSON script lists the keys the
// server has; both are returned only when the page has them.
export function fromPage(sceneId, container) {
  const s = document.getElementById(sceneId);
  if (!s) throw new Error(`no scene script with id ${sceneId}`);
  const scene = JSON.parse(s.textContent);
  const blobs = {};
  let blobKeys;
  document.querySelectorAll("script[data-aob-blob]").forEach((b) => {
    if (b.getAttribute("data-aob-scene") === sceneId) blobs[b.getAttribute("data-aob-blob")] = b.textContent;
  });
  document.querySelectorAll("script[data-aob-blob-keys]").forEach((b) => {
    if (b.getAttribute("data-aob-scene") === sceneId) blobKeys = JSON.parse(b.textContent);
  });
  const host = container ||
    [...document.querySelectorAll("[data-aob-scene]:not(script)")].find((e) => e.getAttribute("data-aob-scene") === sceneId);
  const out = { scene, blobs };
  if (host && host.hasAttribute("data-aob-blob-base")) out.blobBase = host.getAttribute("data-aob-blob-base");
  // A served page with a link to R (decision 0007).
  // (Read through dataset, so the bundle inlined in an embedded page holds
  // no "data-aob-socket" text.)
  const ds = host ? host.dataset : null;
  if (ds && typeof ds.aobSocket === "string" && typeof ds.aobSceneSerial === "string" && ds.aobSceneSerial.trim() !== "") {
    const n = Number(ds.aobSceneSerial);
    if (Number.isInteger(n)) {
      out.socket = ds.aobSocket;
      out.serial = n;
    }
  }
  if (blobKeys !== undefined) {
    if (!Array.isArray(blobKeys)) throw new Error(`scene ${sceneId}: data-aob-blob-keys is not an array`);
    out.blobKeys = blobKeys;
  }
  return out;
}

export function boot() {
  const out = [];
  document.querySelectorAll("[data-aob-scene]:not(script)").forEach((c) => {
    if (c.dataset.aobStatus) return;
    c.dataset.aobStatus = "loading";
    let p;
    try {
      const { scene, blobs, blobBase, blobKeys, socket, serial } = fromPage(c.getAttribute("data-aob-scene"), c);
      // The handle is kept on the element for tools and tests.
      p = render(c, scene, { blobs, blobBase, blobKeys, socket, serial }).then((h) => {
        c.aob = h;
        return h;
      });
    } catch (err) {
      c.dataset.aobStatus = "error";
      c.textContent = `This scene could not be drawn: ${err.message}`;
      p = Promise.reject(err);
    }
    p.catch(() => {});
    out.push(p);
  });
  return out;
}

export { VERSION as version, SPECS as specVersions };
// For tests: decode one tile's bytes to {samples, spp} as a layer does
// (jpeg through the browser's decoder). Not a stable interface.
export { decodeTileSamples as _decodeTileSamples };

if (typeof document !== "undefined") {
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", boot);
  else boot();
}
