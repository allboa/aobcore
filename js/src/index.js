// allonboard renderer for scene spec 0.1, on deck.gl.
//
// aob.render(container, scene, {blobs}) draws one scene into an element.
// blobs maps each data reference's blob key to Arrow IPC bytes (Uint8Array,
// ArrayBuffer or a base64 string); data references with a url are fetched.
// On load, every element with a data-aob-scene attribute is rendered from
// the JSON script it names and the blob scripts that point at it.
import { Deck, OrthographicView, _GlobeView as GlobeView, COORDINATE_SYSTEM } from "@deck.gl/core";
import { decodeBase64, readTable } from "./arrow.js";
import { buildLayer } from "./layers.js";
import { cssGradient } from "./palettes.js";
import { CSS } from "./style.js";

const VERSION = "0.0.1";
const SPEC = "0.1";

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

function crsLabel(crs) {
  if (crs === undefined) return "no CRS";
  if (typeof crs === "string") return crs;
  if (crs.id && crs.id.authority) return `${crs.id.authority}:${crs.id.code}`;
  return crs.name || "PROJJSON";
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
  if (!scene || scene.version !== SPEC) {
    throw new Error(`this renderer draws scene spec ${SPEC}; got ${JSON.stringify(scene && scene.version)}`);
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

export async function render(container, scene, options = {}) {
  injectStyle();
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

  try {
    checkScene(scene);
    const view = scene.view;
    const globe = view.type === "globe";
    tag.textContent = `${crsLabel(view.crs)} / ${view.type}`;

    const t0 = performance.now();
    const tables = {};
    const ids = Object.keys(scene.data);
    const bytes = await Promise.all(ids.map((id) => loadBytes(scene.data[id], id, blobs)));
    let total = 0;
    ids.forEach((id, i) => {
      total += bytes[i].length;
      tables[id] = readTable(bytes[i]);
    });
    const decodeMs = performance.now() - t0;

    const ctx = {
      scene,
      tables,
      warn,
      error: layerError,
      coordinateSystem: globe ? COORDINATE_SYSTEM.LNGLAT : COORDINATE_SYSTEM.CARTESIAN,
      bounds: [Infinity, -Infinity, Infinity, -Infinity],
    };
    const built = scene.layers.map((L) => {
      if (globe && L.kind === "raster") {
        warn(`layer ${L.id}: rasters are not drawn on a globe by this renderer yet`);
        return { layers: [], summary: "not drawn on a globe" };
      }
      return buildLayer(L, ctx);
    });
    const visible = scene.layers.map((L) => L.visible !== false);

    // Layer list (top of the list draws on top) and legends.
    panel.append(el("h2", "aob-h", "Layers"));
    const list = el("div", "aob-layers");
    const legends = el("div", "aob-legends");
    scene.layers.forEach((L, i) => {
      const lab = el("label", "aob-layer");
      const cb = el("input");
      cb.type = "checkbox";
      cb.checked = visible[i];
      cb.addEventListener("change", () => {
        visible[i] = cb.checked;
        update();
      });
      lab.append(cb, el("span", "aob-name", L.label || L.id), el("small", "aob-count", built[i].summary));
      list.prepend(lab);
      const lg = built[i].legend;
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
    panel.append(list, legends, themeButton());

    // Initial view: extent, then center, then the data bounds.
    const b = ctx.bounds;
    let ext = view.extent;
    if (!ext && isFinite(b[0])) ext = b[1] > b[0] && b[3] > b[2] ? b : [b[0] - 1, b[1] + 1, b[2] - 1, b[3] + 1];
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
    }

    let ready = false;
    let frames = 0;
    const deck = new Deck({
      parent: canvasHost,
      views: deckView,
      initialViewState,
      layers: [],
      onHover: (info) => {
        const c = info.coordinate;
        readout.textContent = c ? `x ${fmt(c[0], span)}   y ${fmt(c[1], span)}` : "";
      },
      onError: (err) => setStatus(`Rendering error: ${err && err.message ? err.message : err}`, true),
      onAfterRender: () => {
        if (!ready && ++frames >= 2) {
          ready = true;
          container.dataset.aobStatus = "ready";
          if (options.onReady) options.onReady(handle);
        }
      },
    });
    function update() {
      const layers = [];
      built.forEach((B, i) => B.layers.forEach((l) => layers.push(l.clone({ visible: visible[i] }))));
      deck.setProps({ layers });
    }
    update();
    const kib = (total / 1024).toFixed(0);
    const notes = errors.map((m) => `error: ${m}`).concat(warnings);
    setStatus(notes.join("; "), errors.length > 0);
    if (errors.length) container.dataset.aobErrors = String(errors.length);
    const handle = { deck, scene, tables, decodeMs, bytes: total, warnings, errors };
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
      p = render(c, scene, { blobs });
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

export { VERSION as version, SPEC as specVersion };

if (typeof document !== "undefined") {
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", boot);
  else boot();
}
