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
import { Deck, OrthographicView, _GlobeView as GlobeView, COORDINATE_SYSTEM } from "@deck.gl/core";
import { decodeBase64, readTable } from "./arrow.js";
import { buildLayer } from "./layers.js";
import { buildTiledRaster } from "./tiles.js";
import { decodeTileSamples } from "./jpeg.js";
import { cssGradient } from "./palettes.js";
import { legendElement } from "./legend.js";
import { popupBox, popupRows } from "./popup.js";
import { CSS } from "./style.js";

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

async function loadBytes(ref, id, blobs) {
  if (ref.blob !== undefined) {
    const b = blobs[ref.blob];
    if (b === undefined) throw new Error(`data ${id}: blob "${ref.blob}" was not delivered`);
    if (typeof b === "string") return decodeBase64(b.trim());
    return b instanceof Uint8Array ? b : new Uint8Array(b);
  }
  const res = await fetch(ref.url);
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
function themeButton() {
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
  });
  show();
  return b;
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
  const blobs = options.blobs || {};
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
    const bytes = await Promise.all(ids.map((id) => loadBytes(scene.data[id], id, blobs)));
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
    const ctx = {
      scene,
      tables,
      blobs,
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
    if (legends.childElementCount) legends.setAttribute("aria-label", "Legends");
    panel.append(list, legends, themeButton());

    // 0.5 popups. "point" shows while the pointer is over a feature, where
    // the device has a pointer that can hover; otherwise it acts as select.
    const canHover = typeof matchMedia === "function" && matchMedia("(hover: hover)").matches;
    const triggerOf = (L) => {
      const t = (L.popup && L.popup.trigger) || "select";
      return t === "point" && !canHover ? "select" : t;
    };
    map.tabIndex = -1;
    const popup = popupBox(map, () => delete container.dataset.aobSelected);
    // The feature under a picking result, or null.
    const featureAt = (info) => {
      if (!info || !info.picked || !info.layer) return null;
      const p = picks.get(info.layer.id);
      if (!p || info.index < 0 || info.index >= p.feature.length) return null;
      return { layer: p.layer, table: p.table, row: p.rowOffset + p.feature[info.index] };
    };
    const showFeature = (f, x, y, trigger, focus) => {
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
    let deckView;
    let initialViewState;
    if (globe) {
      deckView = new GlobeView({ id: "aob", controller: true });
      const z = ext ? Math.log2(360 / Math.max(ext[1] - ext[0], (ext[3] - ext[2]) * 2)) : 0;
      initialViewState = { longitude: center[0], latitude: center[1], zoom: Math.max(0, z) };
    } else {
      deckView = new OrthographicView({ id: "aob", flipY: false, controller: true });
      const z = fitZoom();
      initialViewState = { target: [center[0], center[1], 0], zoom: z, minZoom: z - 4, maxZoom: z + 12 };
      if (limits) initialViewState = clampView(initialViewState);
    }

    // Keep an orthographic camera within `limits`: zooming out stops when
    // they fill the canvas, and panning stops at their edges.
    function clampView(vs) {
      const w = canvasHost.clientWidth || 800;
      const h = canvasHost.clientHeight || 600;
      const minZoom = Math.log2(Math.min(w / (limits[1] - limits[0]), h / (limits[3] - limits[2])));
      let zoom = Array.isArray(vs.zoom) ? vs.zoom[0] : vs.zoom;
      zoom = Math.max(zoom, minZoom);
      const u = Math.pow(2, -zoom);
      const axis = (t, lo, hi, half) => (hi - lo <= 2 * half ? (lo + hi) / 2 : Math.min(Math.max(t, lo + half), hi - half));
      const t = vs.target || [0, 0, 0];
      const target = [axis(t[0], limits[0], limits[1], (w / 2) * u), axis(t[1], limits[2], limits[3], (h / 2) * u), t[2] || 0];
      return { ...vs, zoom, target, minZoom };
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

    let ready = false;
    let frames = 0;
    let deck = null;
    let finalized = false;
    const deckProps = {
      parent: canvasHost,
      views: deckView,
      initialViewState,
      layers: [],
      onViewStateChange: ({ viewState: vs }) => setView(vs),
      onHover: (info) => {
        const c = info.coordinate;
        readout.textContent = c ? `x ${fmt(c[0], span)}   y ${fmt(c[1], span)}` : "";
        const f = featureAt(info);
        const cur = popup.current;
        if (f && triggerOf(f.layer) === "point") {
          if (!cur || cur.trigger === "point") showFeature(f, info.x, info.y, "point", false);
        } else if (cur && cur.trigger === "point") {
          popup.hide();
        }
      },
      onClick: (info) => {
        const f = featureAt(info);
        if (f && triggerOf(f.layer) === "select") showFeature(f, info.x, info.y, "select", true);
        else if (!f) popup.hide();
      },
      getCursor: ({ isDragging, isHovering }) => (isDragging ? "grabbing" : isHovering ? "pointer" : "grab"),
      onError: (err) => setStatus(`Rendering error: ${err && err.message ? err.message : err}`, true),
      onAfterRender: () => {
        if (!ready && ++frames >= 2 && pending === 0) {
          ready = true;
          container.dataset.aobStatus = "ready";
          if (options.onReady) options.onReady(handle);
        }
      },
    };
    // Every camera change goes through here, so view.bounds hold for
    // interaction, resize and handle.setView() alike.
    function setView(vs) {
      viewState = limits ? clampView(vs) : vs;
      if (limits && deck) deck.setProps({ viewState });
      update();
      return viewState;
    }
    function update() {
      if (!deck || finalized) return;
      const layers = [];
      const view = globe ? null : currentView();
      built.forEach((B, i) => {
        // A tiled raster's layers depend on the view; a hidden one loads nothing.
        const ls = B.dynamic ? (visible[i] ? B.dynamic(view) : []) : B.layers;
        ls.forEach((l) => layers.push(l.clone({ visible: visible[i] })));
      });
      deck.setProps({ layers });
      const tiled = Object.entries(layerStatus).map(([id, t]) => `${id}: ${t}`).join("; ");
      if (tiled) container.dataset.aobTiles = tiled;
      else delete container.dataset.aobTiles;
    }
    deck = new Deck(deckProps);
    update();
    // The view's extent and pixel size change with the element's size, so
    // tiled rasters choose their level and tiles again on resize.
    const onResize = () => {
      if (limits && deck) {
        viewState = clampView(viewState);
        deck.setProps({ viewState });
      }
      update();
    };
    let observer = null;
    if (typeof ResizeObserver !== "undefined") {
      observer = new ResizeObserver(onResize);
      observer.observe(canvasHost);
    } else {
      window.addEventListener("resize", onResize);
    }
    const finalize = () => {
      if (finalized) return;
      finalized = true;
      if (observer) observer.disconnect();
      else window.removeEventListener("resize", onResize);
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
    const handle = { deck, scene, tables, decodeMs, bytes: total, warnings, errors, finalize,
                     view: () => viewState, setView,
                     selected: () => (popup.current ? { layer: popup.current.layer.id, row: popup.current.row } : null),
                     closePopup: () => popup.hide() };
    container.dataset.aobInfo = `${kib} KiB Arrow decoded in ${decodeMs.toFixed(1)} ms`;
    return handle;
  } catch (err) {
    container.dataset.aobStatus = "error";
    setStatus(`This scene could not be drawn: ${err && err.message ? err.message : err}`, true);
    console.error(err);
    throw err;
  }
}

// Read a scene and its blobs from script elements in the page.
export function fromPage(sceneId) {
  const s = document.getElementById(sceneId);
  if (!s) throw new Error(`no scene script with id ${sceneId}`);
  const scene = JSON.parse(s.textContent);
  const blobs = {};
  document.querySelectorAll("script[data-aob-blob]").forEach((b) => {
    if (b.getAttribute("data-aob-scene") === sceneId) blobs[b.getAttribute("data-aob-blob")] = b.textContent;
  });
  return { scene, blobs };
}

export function boot() {
  const out = [];
  document.querySelectorAll("[data-aob-scene]:not(script)").forEach((c) => {
    if (c.dataset.aobStatus) return;
    c.dataset.aobStatus = "loading";
    let p;
    try {
      const { scene, blobs } = fromPage(c.getAttribute("data-aob-scene"));
      // The handle is kept on the element for tools and tests.
      p = render(c, scene, { blobs }).then((h) => {
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
