// Scene spec 0.2 tiled_raster layers to deck.gl layers. Like layers.js, this
// is where spec concepts meet deck.gl names.
//
// The producer planned everything: levels, tiles, byte ranges and meshes in
// the view CRS. Here the renderer picks a level for the current zoom (the
// plan's selection rule), keeps the tiles whose footprint meets the
// viewport, gets each tile's bytes (an embedded blob keyed
// "<source>@<offset>+<length>" when the page carries one, else an HTTP range
// request to the cog URL), decodes them and draws each tile's mesh slice
// with the tile as its texture.
import { SimpleMeshLayer } from "@deck.gl/mesh-layers";
import { numericColumn, listColumn, decodeBase64 } from "./arrow.js";
import { paletteStops, ramp, UnknownPaletteError, UNLIT } from "./palettes.js";
import { decodeTile, colorizeTile, encodingProblem } from "./decode.js";

const textureParameters = { minFilter: "nearest", magFilter: "nearest", mipmapFilter: "none" };

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

async function fetchRange(url, offset, length) {
  const res = await fetch(url, { headers: { Range: `bytes=${offset}-${offset + length - 1}` } });
  if (res.status === 206) return new Uint8Array(await res.arrayBuffer());
  if (res.ok) {
    // The server ignored the range and sent the whole file.
    const all = new Uint8Array(await res.arrayBuffer());
    return all.subarray(offset, offset + length);
  }
  throw new Error(`${url} returned ${res.status} for bytes ${offset}-${offset + length - 1}`);
}

export function buildTiledRaster(L, ctx) {
  const { scene, tables, blobs } = ctx;
  const what = `layer ${L.id}`;
  const fail = (msg, summary) => {
    ctx.error(`${what}: ${msg}; not drawn`);
    return { summary: `error: ${summary || msg}`, layers: [] };
  };
  let stops;
  try {
    stops = paletteStops(L.palette.name);
  } catch (err) {
    if (!(err instanceof UnknownPaletteError)) throw err;
    return fail(err.message, "unknown palette");
  }
  const plan = L.plan;
  for (const lv of plan.levels) {
    const problem = encodingProblem(lv.encoding);
    if (problem) return fail(`level ${lv.level}: ${problem}`, problem.replace(/ \(.*$/, ""));
  }
  const src = scene.data[L.source];
  const url = typeof document !== "undefined" ? new URL(src.url, document.baseURI).href : src.url;

  const m = plan.mesh;
  const pos = listColumn(tables[m.vertices], m.position_column || "position", what);
  const uv = listColumn(tables[m.vertices], m.uv_column || "uv", what);
  let idx = numericColumn(tables[m.indices], m.index_column || "index", what).values;
  if (!(idx instanceof Uint32Array)) idx = Uint32Array.from(idx, Number);
  const texAll = uv.values instanceof Float32Array ? uv.values : Float32Array.from(uv.values);

  const lut = new Uint8Array(256 * 3);
  for (let k = 0; k < 256; k++) lut.set(ramp(stops, k / 255), k * 3);

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
      return { t, lv, key: `${lv.level}/${t.col}/${t.row}`, state: "new", layer: null };
    }),
  }));
  const rule = plan.coverage === "view" ? null : (plan.selection && plan.selection.rule) || "coarsest_sufficient";
  let failedOnce = false;

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

  async function load(tile) {
    const { t, lv } = tile;
    const key = tileBlobKey(L.source, t);
    let bytes;
    if (blobs[key] !== undefined) {
      const b = blobs[key];
      bytes = typeof b === "string" ? decodeBase64(b.trim()) : b instanceof Uint8Array ? b : new Uint8Array(b);
    } else {
      bytes = await fetchRange(url, t.byte_offset, t.byte_length);
    }
    if (bytes.length !== t.byte_length) throw new Error(`tile ${tile.key}: got ${bytes.length} bytes, expected ${t.byte_length}`);
    const [w, h] = t.size;
    const values = decodeTile(bytes, lv.encoding, w, h);
    const px = colorizeTile(values, w, h, t.window, lv.encoding, lv.grid.nodata, L.palette.range, lut);
    const image = document.createElement("canvas");
    image.width = w;
    image.height = h;
    image.getContext("2d").putImageData(new ImageData(px, w, h), 0, 0);
    return new SimpleMeshLayer({
      id: `${L.id}--${tile.key}`,
      data: [0],
      mesh: meshOf(t),
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
    ctx.pending(+1);
    load(tile)
      .then((layer) => {
        tile.layer = layer;
        tile.state = "ready";
      })
      .catch((err) => {
        tile.state = "failed";
        if (!failedOnce) {
          failedOnce = true;
          ctx.error(`${what}: tile ${tile.key}: ${err && err.message ? err.message : err}`);
        } else {
          console.warn(`aob: ${what}: tile ${tile.key}: ${err && err.message ? err.message : err}`);
        }
      })
      .finally(() => {
        ctx.pending(-1);
        ctx.redraw();
      });
  }

  // The deck.gl layers for a view: {bounds: [xmin, xmax, ymin, ymax],
  // unitsPerPixel}. Starts loading the tiles it needs.
  function dynamic(view) {
    const li = rule === null ? 0 : selectLevel(levels, view.unitsPerPixel, rule);
    const chosen = levels[li];
    const want = chosen.tiles.filter((x) => meets(x.t.footprint, view.bounds));
    want.forEach(request);
    const out = [];
    // While the chosen level loads, draw loaded tiles of coarser levels
    // beneath it (coarsest first), so the view never goes blank.
    if (want.some((x) => x.state !== "ready")) {
      levels
        .filter((l) => l.pixel_size > chosen.pixel_size)
        .sort((a, b) => b.pixel_size - a.pixel_size)
        .forEach((l) => l.tiles.forEach((x) => {
          if (x.state === "ready" && meets(x.t.footprint, view.bounds)) out.push(x.layer);
        }));
    }
    want.forEach((x) => x.state === "ready" && out.push(x.layer));
    ctx.status(L.id, `level ${chosen.lv.level}, ${want.length} tile${want.length === 1 ? "" : "s"}`);
    return out;
  }

  const nTiles = levels.reduce((s, l) => s + l.tiles.length, 0);
  return {
    legend: { id: L.id, label: L.label || L.id, stops, range: L.palette.range },
    summary: `${nTiles} tiles in ${levels.length} level${levels.length === 1 ? "" : "s"}`,
    layers: [],
    dynamic,
  };
}
