// Arrow tables to flat typed arrays. Geometry is read from native GeoArrow
// buffers as decoded; nothing here parses coordinates one by one.
import { tableFromIPC, Type, Precision } from "apache-arrow";

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

// The bytes against the declared format (scene spec README, "Explicit
// data"): an IPC file starts with the magic "ARROW1" and a stream does not.
// apache-arrow reads either, so a mismatch is found here or not at all.
export function ipcFormatProblem(bytes, format) {
  const isFile = bytes.length >= 6 && String.fromCharCode(...bytes.subarray(0, 6)) === "ARROW1";
  if (format === "arrow-ipc-file" && !isFile) return "declared arrow-ipc-file but the bytes are not an IPC file";
  if (format === "arrow-ipc-stream" && isFile) return "declared arrow-ipc-stream but the bytes are an IPC file";
  return null;
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

// ---- scene spec's explicit-data contract ------------------------------------
// The checks a reader makes on a vector data reference before drawing it
// (allboa/scenespec README, "Explicit data"; scripts/check-data.js there is
// the reference). Each returns an error message, or null when the data
// meet the contract. A reader does not reproject or guess: a table that
// fails is an error for each layer that draws it.

const CRS_TYPES = ["projjson", "authority_code"];
const AUTH_CODE = /^[A-Za-z][A-Za-z0-9_]*:[A-Za-z0-9_.-]+$/;

const isDouble = (t) => t.typeId === Type.Float && t.precision === Precision.DOUBLE;

export function typeName(t) {
  if (t.typeId === Type.Dictionary) return `Dictionary<${typeName(t.dictionary)}>`;
  if (t.typeId === Type.FixedSizeList) return `FixedSizeList<${typeName(t.children[0].type)}, ${t.listSize}>`;
  if (t.typeId === Type.Int) return `${t.isSigned ? "Int" : "Uint"}${t.bitWidth}`;
  if (t.typeId === Type.Float) return ["Float16", "Float32", "Float64"][t.precision];
  return Type[t.typeId] || String(t);
}

// A colour column: FixedSizeList<uint8, 4>.
export function isColourType(t) {
  if (t.typeId !== Type.FixedSizeList || t.listSize !== 4) return false;
  const c = t.children[0].type;
  return c.typeId === Type.Int && c.bitWidth === 8 && !c.isSigned;
}

// An attribute, what a popup shows as text: boolean, integer, float32 or
// float64, string, date or timestamp (not a dictionary).
export function isAttributeType(t) {
  switch (t.typeId) {
    case Type.Bool: case Type.Int: case Type.Utf8: case Type.LargeUtf8: case Type.Date: case Type.Timestamp:
      return true;
    case Type.Float:
      return t.precision !== Precision.HALF;
    default:
      return false;
  }
}

// JSON values equal regardless of object key order.
function sameValue(a, b) {
  if (a === b) return true;
  if (typeof a !== "object" || typeof b !== "object" || a === null || b === null) return false;
  if (Array.isArray(a) !== Array.isArray(b)) return false;
  const ka = Object.keys(a).sort();
  const kb = Object.keys(b).sort();
  return ka.join("\n") === kb.join("\n") && ka.every((k) => sameValue(a[k], b[k]));
}

// The "AUTHORITY:CODE" names of a CRS (upper case): the string itself, or a
// PROJJSON object's top-level id (or ids).
function crsCodes(c) {
  if (typeof c === "string") return AUTH_CODE.test(c) ? [c.toUpperCase()] : [];
  if (!c || typeof c !== "object") return [];
  const ids = c.ids || (c.id ? [c.id] : []);
  return ids.filter((i) => i && i.authority !== undefined && i.code !== undefined)
    .map((i) => `${i.authority}:${i.code}`.toUpperCase());
}

// Two CRSs match when they are equal as JSON values (PROJJSON's "$schema"
// aside), or when one authority code names both.
export function sameCrs(a, b) {
  const bare = (c) => {
    if (!c || typeof c !== "object" || Array.isArray(c)) return c;
    const { $schema, ...rest } = c;
    return rest;
  };
  if (sameValue(bare(a), bare(b))) return true;
  const cb = crsCodes(b);
  return crsCodes(a).some((k) => cb.includes(k));
}

function crsLabel(c) {
  if (c === undefined || c === null) return "no CRS";
  const codes = crsCodes(c);
  if (codes.length) return codes[0];
  return typeof c === "string" ? JSON.stringify(c.slice(0, 40)) : `PROJJSON "${c.name || "unnamed"}"`;
}

// Coordinates: interleaved (FixedSizeList named xy or xyz) or separated
// (Struct of x, y and optionally z) doubles; no M.
function coordProblem(t) {
  if (t.typeId === Type.FixedSizeList) {
    const child = t.children[0];
    if (/m/.test(child.name) || t.listSize === 4) return `coordinates are ${child.name || `${t.listSize} values`}: M coordinates are not drawn`;
    if (!((child.name === "xy" && t.listSize === 2) || (child.name === "xyz" && t.listSize === 3))) {
      return `interleaved coordinates must be a FixedSizeList named xy (2 values) or xyz (3), not "${child.name}" (${t.listSize})`;
    }
    return isDouble(child.type) ? null : `coordinates must be doubles, not ${typeName(child.type)}`;
  }
  if (t.typeId === Type.Struct) {
    const names = t.children.map((c) => c.name).join(",");
    if (/(^|,)m(,|$)/.test(names)) return `coordinates are ${names.replace(/,/g, "")}: M coordinates are not drawn`;
    if (names !== "x,y" && names !== "x,y,z") return `separated coordinates must be a Struct of x, y and optionally z, not ${names}`;
    const bad = t.children.find((c) => !isDouble(c.type));
    return bad ? `coordinate ${bad.name} must be double, not ${typeName(bad.type)}` : null;
  }
  return `coordinates must be a FixedSizeList (interleaved) or Struct (separated), not ${typeName(t)}`;
}

function storageProblem(type, ext) {
  let t = type;
  for (let k = 0; k < LEVELS[ext]; k++) {
    if (t.typeId === Type.LargeList) return `${ext} storage uses LargeList; offsets must be 32-bit (List)`;
    if (t.typeId !== Type.List) {
      return `${ext} storage must be ${LEVELS[ext]} nested List level(s) above the coordinates; found ${typeName(t)} at level ${k + 1}`;
    }
    t = t.children[0].type;
  }
  return coordProblem(t);
}

// The geometry column of a vector data reference `ref`, against the
// contract and the scene's view: its extension name is the native GeoArrow
// type the scene declares, its metadata CRS (form fitting crs_type) is the
// view's and geometry.crs's, its edges are planar, and its storage is the
// layout the extension names.
export function geometryProblem(field, ref, view) {
  const meta = field.metadata;
  const ext = meta.get("ARROW:extension:name");
  const name = `geometry column "${field.name}"`;
  const declared = ref.geometry.encoding;
  if (ext === undefined) return `${name} has no GeoArrow extension type (${typeName(field.type)} storage)`;
  if (/wk[bt]/.test(ext)) return `${name} is ${ext}: serialised geometry (WKB or WKT) is not drawn; write a native GeoArrow type`;
  if (ext === "geoarrow.geometrycollection" || ext === "geoarrow.geometry") {
    return `${name} is ${ext}: geometry collections and mixed geometry types are not drawn`;
  }
  if (!(ext in LEVELS)) return `${name} has extension ${ext}, which is not one of the six native GeoArrow types`;
  if (ext !== declared) return `${name} is ${ext} but the scene declares ${declared}`;
  let m = {};
  const text = meta.get("ARROW:extension:metadata");
  if (text !== undefined && text !== "") {
    try {
      m = JSON.parse(text);
    } catch (e) {
      return `ARROW:extension:metadata of ${name} is not JSON`;
    }
    if (!m || typeof m !== "object" || Array.isArray(m)) return `ARROW:extension:metadata of ${name} is not a JSON object`;
  }
  if (m.edges !== undefined && m.edges !== "planar") return `edges are ${m.edges}; only planar edges are drawn`;
  if (m.crs_type !== undefined && !CRS_TYPES.includes(m.crs_type)) return `crs_type ${m.crs_type} is not ${CRS_TYPES.join(" or ")}`;
  const hasCrs = m.crs !== undefined && m.crs !== null;
  if (hasCrs && m.crs_type === "projjson" && (typeof m.crs !== "object" || Array.isArray(m.crs))) {
    return "crs_type is projjson but crs is not a JSON object";
  }
  if (hasCrs && m.crs_type === "authority_code" && !(typeof m.crs === "string" && AUTH_CODE.test(m.crs))) {
    return "crs_type is authority_code but crs is not an \"authority:code\" string";
  }
  if (view && view.crs !== undefined && view.crs !== null) {
    if (!hasCrs) return `${name} has no crs in its extension metadata (view CRS ${crsLabel(view.crs)})`;
    if (!sameCrs(m.crs, view.crs)) return `geometry CRS ${crsLabel(m.crs)} is not the view CRS ${crsLabel(view.crs)}`;
  }
  const declaredCrs = ref.geometry.crs;
  if (declaredCrs !== undefined && !(hasCrs && sameCrs(m.crs, declaredCrs))) {
    return `geometry CRS ${crsLabel(m.crs)} is not the scene's geometry.crs ${crsLabel(declaredCrs)}`;
  }
  return storageProblem(field.type, ext);
}
