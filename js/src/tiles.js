// Scene spec 0.2 and 0.3 tiled_raster layers to deck.gl layers. Like
// layers.js, this is where spec concepts meet deck.gl names.
//
// The producer planned everything: levels, tiles, byte ranges and meshes in
// the view CRS. Here the renderer picks a level for the current zoom (the
// plan's selection rule), keeps the tiles whose footprint meets the
// viewport, gets each tile's bytes (an embedded blob keyed
// "<source>@<offset>+<length>" when the page carries one, the same blob
// from the server's blob base when a served page lists that key, else an
// HTTP range request to the cog URL), decodes them and draws each tile's mesh slice
// with the tile as its texture. A layer colours one band through a palette,
// or (0.3) draws three bands as red, green and blue, with an optional alpha
// band (rgb). jpeg tiles (0.3) are decoded by the browser (jpeg.js).
//
// Scene spec 0.6: the source may instead be a chunks reference. Each plan
// tile is then a chunk of the source grid: its bytes are the chunk's ref
// (url, offset and length; a page that carries the url's bytes as a blob
// keyed by the url, or a served page that lists that key, gives the bytes
// to slice), decoded by the source's codec chain (chunks.js), and its size
// and valid cells come from the grid. A planned chunk with no ref is not
// stored: it is no data and is not drawn.
//
// Tiles no longer in view stay cached, up to MAX_CACHED decoded tiles; past
// that the least recently drawn are dropped (and fetched again if needed).
// A tile still loading when the view moves off it has its fetch aborted.
import { SimpleMeshLayer } from "@deck.gl/mesh-layers";
import { numericColumn, listColumn, decodeBase64 } from "./arrow.js";
import { paletteStops, ramp, UnknownPaletteError, UNLIT } from "./palettes.js";
import { pickBand, colorizeTile, colorizeRGB, encodingProblem } from "./decode.js";
import { decodeTileSamples } from "./jpeg.js";
import { chunksProblem, chunkRefs, chunkKey, chunkLevel, chunkWindow, chunkLayout, chunkEncoding, decodeChunk } from "./chunks.js";

const textureParameters = { minFilter: "nearest", magFilter: "nearest", mipmapFilter: "none" };

// Decoded tiles kept beyond those the current view draws.
export const MAX_CACHED = 256;

export function tileBlobKey(source, t) {
  return `${source}@${t.byte_offset}+${t.byte_length}`;
}

// Pick one level for a screen pixel size (view units per device pixel).
// levels: [{pixel_size}, ...]. Returns an index into levels.
export function selectLevel(levels, upp, rule) {
  let best = -1;
  if (rule === "nearest_pixel_size") {
    let d = Infinity;
    levels.forEach((L, i) => {
      const e = Math.abs(L.pixel_size - upp);
      if (e < d || (e === d && L.pixel_size < levels[best].pixel_size)) {
        d = e;
        best = i;
      }
    });
    return best;
  }
  // coarsest_sufficient: the coarsest level whose pixel is no larger than a
  // screen pixel, or the finest level when none is.
  levels.forEach((L, i) => {
    if (L.pixel_size <= upp && (best < 0 || L.pixel_size > levels[best].pixel_size)) best = i;
  });
  if (best < 0) {
    levels.forEach((L, i) => {
      if (best < 0 || L.pixel_size < levels[best].pixel_size) best = i;
    });
  }
  return best;
}

const meets = (fp, v) => !(fp[1] < v[0] || fp[0] > v[1] || fp[3] < v[2] || fp[2] > v[3]);

// A range reader: fetchRange(url, offset, length, signal) resolves to the
// bytes. A server that ignores Range answers 200 with the whole file; that
// body is kept (per URL) and later tiles are cut from it, rather than
// downloading the whole file again for every tile. Until the first response
// for a URL shows whether the server honours Range, other requests to it
// wait, so tiles asked for together do not each get the whole file.
// fetchFn is for tests.
export function rangeReader(fetchFn = (u, o) => fetch(u, o)) {
  const whole = new Map();
  const probes = new Map();
  return async function fetchRange(url, offset, length, signal) {
    const cut = (all) => {
      if (offset + length > all.length) throw new Error(`${url} has ${all.length} bytes; tile needs ${offset + length}`);
      return all.subarray(offset, offset + length);
    };
    // Wait for a whole-file body another tile's request is reading. That
    // read runs under the other tile's signal; if it fails or is aborted
    // while this caller still wants its tile, start again.
    const shared = async (p) => {
      try {
        return cut(await p);
      } catch (err) {
        if (signal && signal.aborted) throw err;
        if (whole.get(url) === p) {
          whole.delete(url);
          probes.delete(url);
        }
        return fetchRange(url, offset, length, signal);
      }
    };
    if (!whole.has(url) && probes.has(url)) await probes.get(url);
    if (whole.has(url)) return shared(whole.get(url));
    let answered = null;
    if (!probes.has(url)) probes.set(url, new Promise((r) => { answered = r; }));
    let res;
    try {
      res = await fetchFn(url, { headers: { Range: `bytes=${offset}-${offset + length - 1}` }, signal });
    } finally {
      if (answered) answered(); // waiters resume after whole is set below
    }
    if (res.status === 206) return new Uint8Array(await res.arrayBuffer());
    if (res.ok) {
      if (whole.has(url)) {
        // Another tile's request is already reading the whole file.
        if (res.body && res.body.cancel) res.body.cancel().catch(() => {});
        return shared(whole.get(url));
      }
      const body = res.arrayBuffer().then((b) => new Uint8Array(b));
      whole.set(url, body);
      body.catch(() => whole.delete(url));
      return cut(await body);
    }
    throw new Error(`${url} returned ${res.status} for bytes ${offset}-${offset + length - 1}`);
  };
}

const fetchRange = rangeReader();

// The cached tiles to drop: all but the max most recently used of those
// not used by the current view (tile.used < now).
export function tilesToEvict(cached, now, max) {
  const idle = cached.filter((x) => x.used < now);
  if (idle.length <= max) return [];
  return idle.sort((a, b) => a.used - b.used).slice(0, idle.length - max);
}

export function buildTiledRaster(L, ctx) {
  const { scene, blobs } = ctx;
  const what = `layer ${L.id}`;
  const fail = (msg, summary) => {
    ctx.error(`${what}: ${msg}; not drawn`);
    return { summary: `error: ${summary || msg}`, layers: [] };
  };
  const rgb = L.rgb || null;
  if (!rgb === !L.palette) return fail("a tiled raster needs exactly one of palette and rgb", "needs palette or rgb");
  let stops = null;
  if (!rgb) {
    try {
      stops = paletteStops(L.palette.name);
    } catch (err) {
      if (!(err instanceof UnknownPaletteError)) throw err;
      return fail(err.message, "unknown palette");
    }
  }
  const plan = L.plan;
  const src = scene.data[L.source];
  if (src && src.format === "chunks") return buildChunkRaster(L, ctx, src, rgb, stops, fail);
  for (const lv of plan.levels) {
    const problem = encodingProblem(lv.encoding);
    if (problem) return fail(`level ${lv.level}: ${problem}`, problem.replace(/ \(.*$/, ""));
    if (rgb) {
      const enc = lv.encoding;
      const spp = enc.samples_per_pixel || 1;
      const need = Math.max(...rgb.bands, rgb.alpha || 0);
      if ((enc.planar || "interleaved") !== "interleaved") return fail(`level ${lv.level}: rgb needs interleaved samples`, "rgb needs interleaved samples");
      if (need > spp) return fail(`level ${lv.level}: rgb names band ${need} of ${spp}`, "rgb band out of range");
      if (enc.dtype !== "uint8" && !rgb.range) return fail(`level ${lv.level}: rgb of ${enc.dtype} samples needs a range`, "rgb needs a range");
    }
  }
  const url = typeof document !== "undefined" ? new URL(src.url, document.baseURI).href : src.url;

  return drawPlan(L, ctx, rgb, stops, async (tile, signal) => {
    const { t, lv } = tile;
    const key = tileBlobKey(L.source, t);
    let bytes;
    if (blobs[key] !== undefined) {
      const b = blobs[key];
      bytes = typeof b === "string" ? decodeBase64(b.trim()) : b instanceof Uint8Array ? b : new Uint8Array(b);
    } else {
      // A tile blob the server has (a served page), else a range request.
      const served = ctx.servedBlob ? ctx.servedBlob(key, `tile ${tile.key}`, signal) : null;
      bytes = served ? await served : await fetchRange(url, t.byte_offset, t.byte_length, signal);
    }
    if (signal.aborted) throw signal.reason;
    if (bytes.length !== t.byte_length) throw new Error(`tile ${tile.key}: got ${bytes.length} bytes, expected ${t.byte_length}`);
    const [w, h] = t.size;
    const { samples, spp } = await decodeTileSamples(bytes, lv.encoding, w, h);
    return { samples, spp, w, h, win: t.window, enc: lv.encoding, nodata: lv.grid.nodata, band: lv.encoding.band || 1 };
  });
}

// A tiled_raster over a 0.6 chunks source.
function buildChunkRaster(L, ctx, src, rgb, stops, fail) {
  const what = `layer ${L.id}`;
  const problem = chunksProblem(src, L);
  if (problem) return fail(problem, problem.replace(/ \(.*$/, ""));
  let refs;
  try {
    refs = chunkRefs(src, ctx.tables, what);
  } catch (err) {
    return fail(err.message.replace(`${what}: `, ""), "refs not read");
  }
  const { interleave } = chunkLayout(src);
  const enc = chunkEncoding(src);
  const band = rgb ? 1 : L.band || 1;
  // Bytes a page carries for a url (a blob keyed by the url as the scene
  // writes it), decoded once.
  const carried = new Map();
  const blobBytes = (k) => {
    if (!carried.has(k)) {
      const b = ctx.blobs[k];
      carried.set(k, typeof b === "string" ? decodeBase64(b.trim()) : b instanceof Uint8Array ? b : new Uint8Array(b));
    }
    return carried.get(k);
  };
  const resolve = (u) => (typeof document !== "undefined" ? new URL(u, document.baseURI).href : u);
  return drawPlan(L, ctx, rgb, stops, async (tile, signal) => {
    const { t, lv } = tile;
    const ref = refs.get(chunkKey(lv.level, t.col, t.row, interleave === "separate" ? band : 0));
    // Not stored: every cell is no data, so nothing is drawn.
    if (!ref) return null;
    let bytes;
    if (ctx.blobs[ref.url] !== undefined) {
      const all = blobBytes(ref.url);
      if (ref.offset + ref.length > all.length) {
        throw new Error(`chunk ${tile.key}: blob "${ref.url}" has ${all.length} bytes; the chunk needs ${ref.offset + ref.length}`);
      }
      bytes = all.subarray(ref.offset, ref.offset + ref.length);
    } else {
      const served = ctx.servedUrl ? ctx.servedUrl(ref.url) : null;
      bytes = await fetchRange(served || resolve(ref.url), ref.offset, ref.length, signal);
    }
    if (signal.aborted) throw signal.reason;
    if (bytes.length !== ref.length) throw new Error(`chunk ${tile.key}: got ${bytes.length} bytes, expected ${ref.length}`);
    const level = chunkLevel(src, lv.level);
    const [w, h] = level.size;
    const { samples, spp } = await decodeChunk(bytes, src, w, h);
    return { samples, spp, w, h, win: chunkWindow(level, t.col, t.row), enc, nodata: src.nodata,
      band: interleave === "separate" ? 1 : band };
  });
}

// Draw a plan: pick a level for the view, load the tiles in view with
// get(tile, signal), which resolves to decoded samples ({samples, spp, w,
// h, win, enc, nodata, band}) or null for a tile with nothing to draw, and
// draw each as a textured mesh.
function drawPlan(L, ctx, rgb, stops, get) {
  const { tables } = ctx;
  const what = `layer ${L.id}`;
  const plan = L.plan;
  const m = plan.mesh;
  const pos = listColumn(tables[m.vertices], m.position_column || "position", what);
  const uv = listColumn(tables[m.vertices], m.uv_column || "uv", what);
  let idx = numericColumn(tables[m.indices], m.index_column || "index", what).values;
  if (!(idx instanceof Uint32Array)) idx = Uint32Array.from(idx, Number);
  const texAll = uv.values instanceof Float32Array ? uv.values : Float32Array.from(uv.values);

  const lut = new Uint8Array(256 * 3);
  if (stops) for (let k = 0; k < 256; k++) lut.set(ramp(stops, k / 255), k * 3);

  // Every tile of the plan, with its level.
  const acc = ctx.bounds;
  const levels = plan.levels.map((lv) => ({
    lv,
    pixel_size: lv.pixel_size,
    tiles: lv.tiles.map((t) => {
      const fp = t.footprint;
      acc[0] = Math.min(acc[0], fp[0]);
      acc[1] = Math.max(acc[1], fp[1]);
      acc[2] = Math.min(acc[2], fp[2]);
      acc[3] = Math.max(acc[3], fp[3]);
      return { t, lv, key: `${lv.level}/${t.col}/${t.row}`, state: "new", layer: null, used: 0, ctrl: null };
    }),
  }));
  const rule = plan.coverage === "view" ? null : (plan.selection && plan.selection.rule) || "coarsest_sufficient";
  let failedOnce = false;
  // Tiles loading or ready, and a counter of dynamic() calls for LRU order.
  const live = new Set();
  let clock = 0;

  function meshOf(t) {
    const r = t.mesh;
    const n = r.vertex_count;
    const positions = new Float32Array(n * 3);
    for (let i = 0; i < n; i++) {
      const j = (r.first_vertex + i) * pos.size;
      positions[i * 3] = pos.values[j];
      positions[i * 3 + 1] = pos.values[j + 1];
      positions[i * 3 + 2] = pos.size > 2 ? pos.values[j + 2] : 0;
    }
    return {
      attributes: {
        positions: { value: positions, size: 3 },
        texCoords: { value: texAll.subarray(r.first_vertex * 2, (r.first_vertex + n) * 2), size: 2 },
      },
      indices: { value: idx.subarray(r.first_index, r.first_index + r.index_count), size: 1 },
    };
  }

  async function load(tile, signal) {
    const d = await get(tile, signal);
    if (d === null) return null;
    if (signal.aborted) throw signal.reason;
    const { samples, spp, w, h, win, enc, nodata } = d;
    const px = rgb
      ? colorizeRGB(samples, spp, w, h, win, enc, nodata, rgb)
      : colorizeTile(pickBand(samples, spp, d.band, w * h), w, h, win, enc, nodata, L.palette.range, lut);
    const image = document.createElement("canvas");
    image.width = w;
    image.height = h;
    image.getContext("2d").putImageData(new ImageData(px, w, h), 0, 0);
    return new SimpleMeshLayer({
      id: `${L.id}--${tile.key}`,
      data: [0],
      mesh: meshOf(tile.t),
      texture: image,
      coordinateSystem: ctx.coordinateSystem,
      getPosition: [0, 0, 0],
      getColor: [255, 255, 255, 255],
      sizeScale: 1,
      material: UNLIT,
      textureParameters,
    });
  }

  function request(tile) {
    if (tile.state !== "new") return;
    tile.state = "loading";
    const ctrl = new AbortController();
    tile.ctrl = ctrl;
    live.add(tile);
    ctx.pending(+1);
    load(tile, ctrl.signal)
      .then((layer) => {
        if (tile.ctrl !== ctrl) return;
        tile.layer = layer;
        tile.state = "ready";
      })
      .catch((err) => {
        if (tile.ctrl !== ctrl) return; // aborted: the view moved on
        tile.state = "failed";
        live.delete(tile);
        if (!failedOnce) {
          failedOnce = true;
          ctx.error(`${what}: tile ${tile.key}: ${err && err.message ? err.message : err}`);
        } else {
          console.warn(`aob: ${what}: tile ${tile.key}: ${err && err.message ? err.message : err}`);
        }
      })
      .finally(() => {
        if (tile.ctrl === ctrl) tile.ctrl = null;
        ctx.pending(-1);
        ctx.redraw();
      });
  }

  // Back to "new": abort a load in flight, or drop a decoded tile.
  function forget(tile) {
    if (tile.state === "loading" && tile.ctrl) tile.ctrl.abort();
    tile.ctrl = null;
    tile.layer = null;
    tile.state = "new";
    live.delete(tile);
  }

  // The deck.gl layers for a view: {bounds: [xmin, xmax, ymin, ymax],
  // unitsPerPixel}. Starts loading the tiles it needs.
  function dynamic(view) {
    const li = rule === null ? 0 : selectLevel(levels, view.unitsPerPixel, rule);
    const chosen = levels[li];
    const want = chosen.tiles.filter((x) => meets(x.t.footprint, view.bounds));
    const now = ++clock;
    want.forEach((x) => {
      x.used = now;
      request(x);
    });
    const out = [];
    // While the chosen level loads, draw loaded tiles of coarser levels
    // beneath it (coarsest first), so the view never goes blank.
    if (want.some((x) => x.state !== "ready")) {
      levels
        .filter((l) => l.pixel_size > chosen.pixel_size)
        .sort((a, b) => b.pixel_size - a.pixel_size)
        .forEach((l) => l.tiles.forEach((x) => {
          if (x.state === "ready" && meets(x.t.footprint, view.bounds)) {
            x.used = now;
            if (x.layer) out.push(x.layer);
          }
        }));
    }
    want.forEach((x) => x.state === "ready" && x.layer && out.push(x.layer));
    // Loads the view no longer needs are aborted; idle decoded tiles past
    // the cache size are dropped, least recently drawn first.
    for (const x of live) if (x.state === "loading" && x.used < now) forget(x);
    const ready = [...live].filter((x) => x.state === "ready");
    tilesToEvict(ready, now, MAX_CACHED).forEach(forget);
    ctx.status(L.id, `level ${chosen.lv.level}, ${want.length} tile${want.length === 1 ? "" : "s"}`);
    return out;
  }

  const nTiles = levels.reduce((s, l) => s + l.tiles.length, 0);
  return {
    legend: rgb ? null : { id: L.id, label: L.label || L.id, stops, range: L.palette.range },
    summary: `${nTiles} tiles in ${levels.length} level${levels.length === 1 ? "" : "s"}`,
    layers: [],
    dynamic,
  };
}
