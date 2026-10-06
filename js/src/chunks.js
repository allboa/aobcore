// Scene spec 0.6 chunk references: a `chunks` data reference is a grid
// whose chunks are byte ranges (url, offset, length) decoded by a codec
// chain. A tiled_raster over one draws each planned chunk as a COG tile is
// drawn (tiles.js); here are the pure parts: checking that a source can be
// drawn, its levels, the index of its refs, and decoding one chunk's bytes
// to samples. No DOM, so they run in Node tests.
//
// The decoding rule (scenespec README, "0.6: chunk references"): read bytes
// [offset, offset + length) and undo the codec chain in reverse, the last
// codec first. That gives one full chunk of width x height x bands samples
// of dtype, laid out as interleave says. Codecs read here: bytes (endian),
// the horizontal and floating_point predictors (stride: bands for pixel
// interleave, 1 otherwise), and deflate (a zlib stream) and gzip through
// the browser's own DecompressionStream. zstd, lzw, blosc and jpeg chains
// are a layer error naming the codec.
import { decodeSamples, encodingProblem } from "./decode.js";

export const CHUNK_CODECS = ["bytes", "predictor", "deflate", "gzip"];

const DECOMPRESS = { deflate: "deflate", gzip: "gzip" };

function config(c) {
  return (c && c.configuration) || {};
}

// The bands and interleave of a source, with their defaults.
export function chunkLayout(src) {
  return { bands: src.bands || 1, interleave: src.interleave || "pixel" };
}

// The predictor type of a chain ("none", "horizontal" or "floating_point").
function predictorOf(codecs) {
  const p = codecs.find((c) => c.name === "predictor");
  return p ? config(p).type : "none";
}

// The tile encoding (decode.js) that undoes the bytes codec and predictor
// of a chain, for samples that are already decompressed.
export function chunkEncoding(src) {
  const codecs = src.codecs || [];
  return {
    codec: "none",
    dtype: src.dtype,
    byte_order: config(codecs[0]).endian || "little",
    predictor: predictorOf(codecs),
    scale: src.scale,
    offset: src.offset,
  };
}

// Level k of a source's grid: {dim, size} (size is the chunk [width,
// height]), or null when the grid has no such level.
export function chunkLevel(src, k) {
  const g = src.grid;
  if (k === 0) return { dim: g.dim, size: g.chunk_size };
  const lv = (g.levels || []).find((l) => l.level === k);
  return lv ? { dim: lv.dim, size: lv.chunk_size || g.chunk_size } : null;
}

// The valid cells of chunk (col, row) of a level: a window like a COG
// tile's, {x, y, width, height}, inside the full padded chunk.
export function chunkWindow(level, col, row) {
  const [w, h] = level.size;
  return {
    x: 0,
    y: 0,
    width: Math.max(0, Math.min(w, level.dim[0] - col * w)),
    height: Math.max(0, Math.min(h, level.dim[1] - row * h)),
  };
}

// Why layer L cannot draw chunks source src, or null when it can.
export function chunksProblem(src, L) {
  const codecs = src.codecs || [];
  for (const c of codecs) {
    if (!CHUNK_CODECS.includes(c.name)) {
      return `codec ${c.name} is not supported by this renderer (supported for chunks: ${CHUNK_CODECS.join(", ")})`;
    }
  }
  if (!codecs.length || codecs[0].name !== "bytes") return "a codec chain must start with bytes";
  if (codecs.slice(1).some((c) => c.name === "bytes")) return "a codec chain has one bytes codec";
  const pi = codecs.findIndex((c) => c.name === "predictor");
  if (pi >= 0 && (pi !== 1 || codecs.slice(2).some((c) => c.name === "predictor"))) {
    return "a predictor must come directly after bytes";
  }
  const endian = config(codecs[0]).endian || "little";
  if (endian !== "little" && endian !== "big") return `bytes endian ${endian} is not little or big`;
  const problem = encodingProblem(chunkEncoding(src));
  if (problem) return problem;
  const { bands, interleave } = chunkLayout(src);
  if (!["pixel", "plane", "separate"].includes(interleave)) return `interleave ${interleave} is not pixel, plane or separate`;
  if (L.rgb) {
    if (interleave === "separate" && bands > 1) return "rgb needs every band in one chunk (pixel or plane interleave)";
    const need = Math.max(...L.rgb.bands, L.rgb.alpha || 0);
    if (need > bands) return `rgb names band ${need} of ${bands}`;
    if (src.dtype !== "uint8" && !L.rgb.range) return `rgb of ${src.dtype} samples needs a range`;
  } else if ((L.band || 1) > bands) {
    return `band ${L.band} is not in the source, which has ${bands} band${bands === 1 ? "" : "s"}`;
  }
  for (const lv of L.plan.levels) {
    if (!chunkLevel(src, lv.level)) return `plan level ${lv.level} is not a level of the source grid`;
  }
  return null;
}

// Every stored chunk of a source: a Map from chunkKey(level, col, row,
// band) to {url, offset, length}. Refs are inline rows or an Arrow table
// (refs.table, read from tables). band is 0 unless interleave is separate.
export function chunkKey(level, col, row, band) {
  return `${level}/${col}/${row}/${band}`;
}

export function chunkRefs(src, tables, what) {
  const out = new Map();
  const separate = chunkLayout(src).interleave === "separate";
  const add = (level, col, row, band, url, offset, length) => {
    const u = url === null || url === undefined ? src.url : url;
    if (u === undefined) throw new Error(`${what}: chunk ${level}/${col}/${row} has no url and the source has none`);
    out.set(chunkKey(level, col, row, separate ? band || 1 : 0), { url: u, offset, length });
  };
  const refs = src.refs || {};
  if (refs.rows) {
    for (const r of refs.rows) add(r.level || 0, r.col, r.row, r.band, r.url, r.offset, r.length);
  } else if (refs.table !== undefined) {
    const t = tables[refs.table];
    if (!t) throw new Error(`${what}: refs table ${refs.table} was not read`);
    const col = (name, needed) => {
      const c = t.getChild(name);
      if (!c && needed) throw new Error(`${what}: refs table ${refs.table} has no column "${name}"`);
      return c;
    };
    const num = (c, i, dflt) => {
      if (!c) return dflt;
      const v = c.get(i);
      return v === null || v === undefined ? dflt : Number(v);
    };
    const [lc, cc, rc, bc, uc, oc, nc] = [col("level"), col("col", true), col("row", true), col("band"),
      col("url"), col("offset", true), col("length", true)];
    for (let i = 0; i < t.numRows; i++) {
      add(num(lc, i, 0), num(cc, i), num(rc, i), num(bc, i, 0), uc ? uc.get(i) : null, num(oc, i), num(nc, i));
    }
  }
  return out;
}

// Decompress with the platform's DecompressionStream (every current
// browser, and Node 18 and later).
async function decompress(bytes, format) {
  if (typeof DecompressionStream === "undefined") {
    throw new Error(`this browser has no DecompressionStream to undo ${format}`);
  }
  try {
    const stream = new Blob([bytes]).stream().pipeThrough(new DecompressionStream(format));
    return new Uint8Array(await new Response(stream).arrayBuffer());
  } catch (err) {
    throw new Error(`${format} did not decode (${err && err.message ? err.message : err})`);
  }
}

// One chunk's bytes, w x h cells (padding included), to {samples, spp}:
// every band, pixel interleaved (spp samples per cell, spp is 1 for a
// separate chunk), row 0 first, as decode.js gives a COG tile's samples.
export async function decodeChunk(raw, src, w, h) {
  const codecs = src.codecs;
  let bytes = raw;
  for (let i = codecs.length - 1; i >= 1; i--) {
    const c = codecs[i];
    if (c.name === "predictor") break;
    if (!DECOMPRESS[c.name]) throw new Error(`codec ${c.name} is not supported by this renderer`);
    bytes = await decompress(bytes, DECOMPRESS[c.name]);
  }
  const { bands, interleave } = chunkLayout(src);
  const enc = chunkEncoding(src);
  if (interleave === "separate" || bands === 1) {
    return decodeSamples(bytes, { ...enc, samples_per_pixel: 1 }, w, h);
  }
  if (interleave === "pixel") {
    return decodeSamples(bytes, { ...enc, samples_per_pixel: bands, planar: "interleaved" }, w, h);
  }
  // plane: one band's full chunk after another, so the predictor runs over
  // rows of one band (h * bands rows of w samples). Interleave them.
  const { samples } = decodeSamples(bytes, { ...enc, samples_per_pixel: 1 }, w, h * bands);
  const n = w * h;
  const out = new samples.constructor(n * bands);
  for (let b = 0; b < bands; b++) for (let i = 0; i < n; i++) out[i * bands + b] = samples[b * n + i];
  return { samples: out, spp: bands };
}
