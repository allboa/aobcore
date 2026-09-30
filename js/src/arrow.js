// Arrow tables to flat typed arrays. Geometry is read from native GeoArrow
// buffers as decoded; nothing here parses coordinates one by one.
import { tableFromIPC, Type } from "apache-arrow";

export function decodeBase64(s) {
  if (typeof Uint8Array.fromBase64 === "function") return Uint8Array.fromBase64(s);
  const bin = atob(s);
  const out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
  return out;
}

export function readTable(bytes) {
  return tableFromIPC(bytes);
}

function column(table, name, what) {
  const idx = table.schema.fields.findIndex((f) => f.name === name);
  if (idx < 0) throw new Error(`${what}: column "${name}" not found`);
  return idx;
}

// The chunks (one Data per record batch) of a named column.
export function columnChunks(table, name, what) {
  const idx = column(table, name, what);
  return table.batches.map((b) => b.data.children[idx]);
}

function concatTyped(parts, Ctor) {
  if (parts.length === 1) return parts[0];
  let n = 0;
  for (const p of parts) n += p.length;
  const out = new (Ctor || parts[0].constructor)(n);
  let o = 0;
  for (const p of parts) {
    out.set(p, o);
    o += p.length;
  }
  return out;
}

function isFixedSizeList(d) {
  return d.type.typeId === Type.FixedSizeList;
}

function isStruct(d) {
  return d.type.typeId === Type.Struct;
}

// A coordinate node (FixedSizeList interleaved, or Struct separated) as
// {values: interleaved array, size, count}.
function coordValues(d) {
  if (isFixedSizeList(d)) {
    const size = d.type.listSize;
    const child = d.children[0];
    const start = (d.offset + child.offset) * size;
    return { values: child.values.subarray(start, start + d.length * size), size, count: d.length };
  }
  if (isStruct(d)) {
    const size = d.children.length;
    const out = new Float64Array(d.length * size);
    d.children.forEach((c, k) => {
      const v = c.values;
      const base = d.offset + c.offset;
      for (let i = 0; i < d.length; i++) out[i * size + k] = v[base + i];
    });
    return { values: out, size, count: d.length };
  }
  throw new Error("geometry coordinates must be a FixedSizeList or Struct of doubles");
}

// List offsets for a List Data, as absolute indexes into its child.
function offsets(d) {
  return d.valueOffsets.subarray(d.offset, d.offset + d.length + 1);
}

const LEVELS = {
  "geoarrow.point": 0,
  "geoarrow.linestring": 1,
  "geoarrow.multipoint": 1,
  "geoarrow.polygon": 2,
  "geoarrow.multilinestring": 2,
  "geoarrow.multipolygon": 3,
};

// Walk the nested lists of one geometry chunk. Returns the coordinates and
// the offsets of each nesting level, outermost first; all offsets index the
// next level down, and the innermost index coordinates.
export function geometryChunk(d, encoding) {
  const depth = LEVELS[encoding];
  if (depth === undefined) throw new Error(`unsupported geometry encoding ${encoding}`);
  const levels = [];
  let node = d;
  for (let k = 0; k < depth; k++) {
    levels.push(offsets(node));
    node = node.children[0];
  }
  const c = coordValues(node);
  return { rows: d.length, levels, coords: c.values, size: c.size, count: c.count };
}

// Parts at a given depth with the feature (row) each belongs to. For a
// linestring chunk the parts are the rows themselves; for a multilinestring
// they are the member linestrings.
export function partsOf(g, outer) {
  // outer: number of list levels above the parts
  if (outer === 0) {
    const feature = new Uint32Array(g.rows);
    for (let i = 0; i < g.rows; i++) feature[i] = i;
    return { count: g.rows, feature };
  }
  const offs = g.levels[0];
  const n = offs[g.rows] - offs[0];
  const feature = new Uint32Array(n);
  for (let r = 0; r < g.rows; r++) {
    for (let j = offs[r]; j < offs[r + 1]; j++) feature[j - offs[0]] = r;
  }
  return { count: n, feature };
}

// A plain numeric column as one typed array (all batches), plus a validity
// test when it has nulls.
export function numericColumn(table, name, what) {
  const chunks = columnChunks(table, name, what);
  let values = concatTyped(chunks.map((c) => c.values.subarray(c.offset, c.offset + c.length)));
  if (typeof BigInt64Array !== "undefined" && (values instanceof BigInt64Array || values instanceof BigUint64Array)) {
    values = Float64Array.from(values, Number);
  }
  let valid = null;
  if (chunks.some((c) => c.nullCount > 0)) {
    valid = new Uint8Array(values.length);
    let o = 0;
    for (const c of chunks) {
      for (let i = 0; i < c.length; i++) valid[o + i] = c.getValid(i) ? 1 : 0;
      o += c.length;
    }
  }
  return { values, valid };
}

// A FixedSizeList<number, k> column as one interleaved typed array.
export function listColumn(table, name, what) {
  const chunks = columnChunks(table, name, what);
  let size = null;
  const parts = chunks.map((c) => {
    if (!isFixedSizeList(c)) throw new Error(`${what}: column "${name}" must be a FixedSizeList`);
    const cv = coordValues(c);
    size = cv.size;
    return cv.values;
  });
  return { values: concatTyped(parts), size };
}

// The RGBA column of one batch, as a Uint8Array with 4 values per row.
export function rgbaChunk(batchData, fields, name, what) {
  const idx = fields.findIndex((f) => f.name === name);
  if (idx < 0) throw new Error(`${what}: color column "${name}" not found`);
  const c = batchData.children[idx];
  if (!isFixedSizeList(c) || c.type.listSize !== 4) {
    throw new Error(`${what}: color column "${name}" must be FixedSizeList<uint8, 4>`);
  }
  const child = c.children[0];
  const start = (c.offset + child.offset) * 4;
  return child.values.subarray(start, start + c.length * 4);
}
