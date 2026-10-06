// Scene spec 0.1 layers to deck.gl layers. This file is the only place where
// spec concepts meet deck.gl names. A 0.5 popup makes a vector layer's
// features pickable; ctx.pickable() records how a picked part maps to its
// feature row, for every vector layer, since a served page may make a layer
// without a popup selectable (decision 0007). ctx.highlight() receives, per
// record batch, a function that draws the selected rows of that batch.
import { COORDINATE_SYSTEM } from "@deck.gl/core";
import { SolidPolygonLayer, PathLayer, ScatterplotLayer, BitmapLayer } from "@deck.gl/layers";
import { SimpleMeshLayer } from "@deck.gl/mesh-layers";
import { earcut } from "@math.gl/polygon";
import { geometryChunk, partsOf, numericColumn, listColumn, rgbaChunk, geometryProblem, isColourType, isAttributeType,
  typeName } from "./arrow.js";
import { paletteStops, colorize, UnknownPaletteError, UNLIT } from "./palettes.js";

const DEFAULT_FILL = [128, 128, 128, 255];
const DEFAULT_STROKE = [60, 66, 72, 255];

const sameJson = (a, b) => JSON.stringify(a) === JSON.stringify(b);

// Positions in the view: vector coordinates go to deck.gl as absolute
// float64 view CRS units (deck.gl splits them into high and low float32
// parts and offsets them in the shader), so data marked origin_subtracted
// get view.local_origin added back.
function absolute(coords, size, origin) {
  if (!origin) return coords;
  const out = new Float64Array(coords.length);
  for (let i = 0; i < coords.length; i += size) {
    out[i] = coords[i] + origin[0];
    out[i + 1] = coords[i + 1] + origin[1];
    for (let k = 2; k < size; k++) out[i + k] = coords[i + k];
  }
  return out;
}

// A color given as data: a constant RGBA, or a per-feature RGBA column.
// Returns a deck.gl accessor for objects whose feature row is feature[index].
function colorAccessor(spec, fallback, batch, fields, feature, what) {
  if (spec === undefined || spec === null) return fallback;
  if (Array.isArray(spec)) return spec;
  const rgba = rgbaChunk(batch, fields, spec.column, what);
  return (_, info) => {
    const r = feature[info.index] * 4;
    return [rgba[r], rgba[r + 1], rgba[r + 2], rgba[r + 3]];
  };
}

// A colour column a layer names (fill or stroke) that is missing or not
// FixedSizeList<uint8, 4>, as an error message; null when there is none.
function colourProblem(L, keys, fields) {
  for (const key of keys) {
    const spec = L[key];
    if (!spec || Array.isArray(spec)) continue;
    const f = fields.find((x) => x.name === spec.column);
    if (!f) return `${key} colour column "${spec.column}" not found`;
    if (!isColourType(f.type)) return `${key} colour column "${spec.column}" is ${typeName(f.type)}, not FixedSizeList<Uint8, 4>`;
  }
  return null;
}

function bounds(coords, size, acc) {
  for (let i = 0; i < coords.length; i += size) {
    const x = coords[i];
    const y = coords[i + 1];
    if (x < acc[0]) acc[0] = x;
    if (x > acc[1]) acc[1] = x;
    if (y < acc[2]) acc[2] = y;
    if (y > acc[3]) acc[3] = y;
  }
}

export function buildLayer(L, ctx) {
  if (L.kind === "raster") return buildRaster(L, ctx);
  const ref = ctx.scene.data[L.data];
  const table = ctx.tables[L.data];
  const enc = ref.geometry.encoding;
  const fields = table.schema.fields;
  // The explicit-data contract (scene spec README, "Explicit data"): the
  // geometry column's GeoArrow extension is the declared encoding, its CRS
  // is the view's, its storage is the extension's, and colour columns are
  // RGBA. Data that fail it are an error for this layer, which is not drawn;
  // the rest of the scene is.
  const geomIdx = fields.findIndex((f) => f.name === ref.geometry.column);
  const colourKeys = L.kind === "path" ? ["stroke"] : ["fill", "stroke"];
  const problem = geomIdx < 0 ? `geometry column "${ref.geometry.column}" not found`
    : geometryProblem(fields[geomIdx], ref, ctx.scene.view) || colourProblem(L, colourKeys, fields);
  if (problem) {
    ctx.error(`layer ${L.id}: data ${L.data}: ${problem}; not drawn`);
    return { layers: [], summary: "error: data not drawn" };
  }
  const origin = ref.origin_subtracted ? ctx.scene.view.local_origin : null;
  // 0.5 popup: features can be picked when every named column is present
  // and an attribute (what a popup shows as text); a missing or other
  // column is an error for this layer's popup (the layer still draws).
  let popup = false;
  if (L.popup) {
    const missing = L.popup.columns.filter((c) => !fields.some((f) => f.name === c));
    const other = fields.filter((f) => L.popup.columns.includes(f.name) && !isAttributeType(f.type));
    if (missing.length) {
      ctx.error(`layer ${L.id}: popup column${missing.length > 1 ? "s" : ""} ${missing.map((c) => `"${c}"`).join(", ")} not found in data ${L.data}; popup not shown`);
    } else if (other.length) {
      ctx.error(`layer ${L.id}: popup column${other.length > 1 ? "s" : ""} ${other.map((f) => `"${f.name}" (${typeName(f.type)})`).join(", ")} in data ${L.data} ${other.length > 1 ? "are" : "is"} not an attribute type; popup not shown`);
    } else {
      popup = true;
    }
  }
  const out = [];
  let count = 0;
  let rowOffset = 0;
  table.batches.forEach((b, bi) => {
    const g = geometryChunk(b.data.children[geomIdx], enc);
    g.coords = absolute(g.coords, g.size, origin);
    bounds(g.coords, g.size, ctx.bounds);
    const base = { coordinateSystem: ctx.coordinateSystem, pickable: popup, autoHighlight: popup,
                   highlightColor: [255, 196, 0, 160] };
    const id = `${L.id}--${bi}`;
    // A picked object's index is a part (a member linestring, polygon or
    // point); its feature row in the table is rowOffset + feature[index].
    const pick = (deckId, feature) => {
      ctx.pickable(deckId, { layer: L, table, feature, rowOffset, popup });
    };
    // Selected rows (absolute, 0-based) of this batch, drawn over the scene.
    const hl = (kind, parts) => {
      if (ctx.highlight) ctx.highlight(L.id, highlighter(`${id}-sel`, kind, g, parts, rowOffset, b.numRows, L, ctx.coordinateSystem));
    };
    const what = `layer ${L.id}`;
    const widthPx = L.stroke_width_px === undefined ? 1 : L.stroke_width_px;
    if (L.kind === "path") {
      const multi = enc === "geoarrow.multilinestring";
      const parts = partsOf(g, multi ? 1 : 0);
      const starts = g.levels[multi ? 1 : 0];
      count += g.rows;
      pick(id, parts.feature);
      hl("path", { feature: parts.feature, count: parts.count, starts });
      out.push(new PathLayer({
        ...base,
        id,
        _pathType: "open",
        positionFormat: g.size === 3 ? "XYZ" : "XY",
        data: {
          length: parts.count,
          startIndices: starts,
          attributes: { getPath: { value: g.coords.subarray(0, starts[parts.count] * g.size), size: g.size } },
        },
        getColor: colorAccessor(L.stroke, DEFAULT_STROKE, b.data, fields, parts.feature, what),
        getWidth: widthPx,
        widthUnits: "pixels",
      }));
    } else if (L.kind === "polygon") {
      const multi = enc === "geoarrow.multipolygon";
      const polys = partsOf(g, multi ? 1 : 0);
      const polyRings = g.levels[multi ? 1 : 0];
      const ringVerts = g.levels[multi ? 2 : 1];
      count += g.rows;
      hl("polygon", { feature: polys.feature, count: polys.count, polyRings, ringVerts });
      const hasFill = L.fill !== undefined || L.stroke === undefined;
      if (hasFill) {
        const starts = new Uint32Array(polys.count + 1);
        const tris = [];
        let nIdx = 0;
        for (let p = 0; p < polys.count; p++) {
          const r0 = polyRings[p];
          const r1 = polyRings[p + 1];
          const v0 = ringVerts[r0];
          const v1 = ringVerts[r1];
          starts[p] = v0;
          if (v1 - v0 < 3) continue;
          const holes = [];
          for (let r = r0 + 1; r < r1; r++) holes.push(ringVerts[r] - v0);
          const t = earcut(g.coords.subarray(v0 * g.size, v1 * g.size), holes.length ? holes : null, g.size);
          for (let k = 0; k < t.length; k++) t[k] += v0;
          tris.push(t);
          nIdx += t.length;
        }
        starts[polys.count] = ringVerts[polyRings[polys.count]];
        const indices = new Uint32Array(nIdx);
        let o = 0;
        for (const t of tris) {
          indices.set(t, o);
          o += t.length;
        }
        pick(`${id}-fill`, polys.feature);
        out.push(new SolidPolygonLayer({
          ...base,
          id: `${id}-fill`,
          _normalize: false,
          positionFormat: g.size === 3 ? "XYZ" : "XY",
          data: {
            length: polys.count,
            startIndices: starts,
            attributes: {
              getPolygon: { value: g.coords.subarray(0, starts[polys.count] * g.size), size: g.size },
              indices,
            },
          },
          getFillColor: colorAccessor(L.fill, DEFAULT_FILL, b.data, fields, polys.feature, what),
        }));
      }
      if (L.stroke !== undefined) {
        // Outlines: every ring as an open path (GeoArrow rings are closed).
        const nRings = polyRings[polys.count] - polyRings[0];
        const ringFeature = new Uint32Array(nRings);
        for (let p = 0; p < polys.count; p++) {
          for (let r = polyRings[p]; r < polyRings[p + 1]; r++) ringFeature[r - polyRings[0]] = polys.feature[p];
        }
        pick(`${id}-stroke`, ringFeature);
        out.push(new PathLayer({
          ...base,
          id: `${id}-stroke`,
          _pathType: "open",
          positionFormat: g.size === 3 ? "XYZ" : "XY",
          data: {
            length: nRings,
            startIndices: ringVerts,
            attributes: { getPath: { value: g.coords.subarray(0, ringVerts[nRings] * g.size), size: g.size } },
          },
          getColor: colorAccessor(L.stroke, DEFAULT_STROKE, b.data, fields, ringFeature, what),
          getWidth: widthPx,
          widthUnits: "pixels",
        }));
      }
    } else if (L.kind === "point") {
      const multi = enc === "geoarrow.multipoint";
      let feature;
      if (multi) {
        const offs = g.levels[0];
        feature = new Uint32Array(g.count);
        for (let r = 0; r < g.rows; r++) for (let j = offs[r]; j < offs[r + 1]; j++) feature[j] = r;
      } else {
        feature = partsOf(g, 0).feature;
      }
      count += g.rows;
      const filled = L.fill !== undefined || L.stroke === undefined;
      const stroked = L.stroke !== undefined;
      pick(id, feature);
      hl("point", { feature, count: g.count });
      out.push(new ScatterplotLayer({
        ...base,
        id,
        data: { length: g.count, attributes: { getPosition: { value: g.coords, size: g.size } } },
        filled,
        stroked,
        radiusUnits: "pixels",
        getRadius: L.radius_px === undefined ? 3 : L.radius_px,
        lineWidthUnits: "pixels",
        getLineWidth: widthPx,
        getFillColor: colorAccessor(L.fill, DEFAULT_FILL, b.data, fields, feature, what),
        getLineColor: colorAccessor(L.stroke, DEFAULT_STROKE, b.data, fields, feature, what),
      }));
    }
    rowOffset += b.numRows;
  });
  const noun = { path: "line", polygon: "polygon", point: "point" }[L.kind];
  return { layers: out, summary: `${count.toLocaleString("en-US")} ${noun}${count === 1 ? "" : "s"}` };
}

// The highlight of selected features in one record batch: a function of
// (rows, colors) that returns deck.gl layers drawing only the parts whose
// feature row is in `rows` (a Set of absolute rows), or none. colors has
// line, halo and fill RGBA arrays. Polygons get a translucent fill and an
// outline over a halo; lines a wider line over a halo; points a ring.
function highlighter(id, kind, g, parts, rowOffset, numRows, L, coordinateSystem) {
  const size = g.size;
  const positionFormat = size === 3 ? "XYZ" : "XY";
  const widthPx = L.stroke_width_px === undefined ? 1 : L.stroke_width_px;
  const lineW = Math.max(2.5, widthPx + 2);
  const base = { coordinateSystem, pickable: false };
  return (rows, colors) => {
    let any = false;
    for (const r of rows) {
      if (r >= rowOffset && r < rowOffset + numRows) {
        any = true;
        break;
      }
    }
    if (!any) return [];
    const on = (p) => rows.has(rowOffset + parts.feature[p]);
    const lines = (paths, key) => [
      // Rounded joints and caps: mitred ones spike at sharp turns (a
      // detailed coastline).
      new PathLayer({ ...base, id: `${id}-${key}-halo`, data: paths, getPath: (d) => d, positionFormat, _pathType: "open",
                      getColor: colors.halo, getWidth: lineW + 3, widthUnits: "pixels", jointRounded: true, capRounded: true }),
      new PathLayer({ ...base, id: `${id}-${key}`, data: paths, getPath: (d) => d, positionFormat, _pathType: "open",
                      getColor: colors.line, getWidth: lineW, widthUnits: "pixels", jointRounded: true, capRounded: true }),
    ];
    if (kind === "path") {
      const paths = [];
      for (let p = 0; p < parts.count; p++) {
        if (on(p)) paths.push(g.coords.subarray(parts.starts[p] * size, parts.starts[p + 1] * size));
      }
      return lines(paths, "line");
    }
    if (kind === "polygon") {
      const polys = [];
      const rings = [];
      for (let p = 0; p < parts.count; p++) {
        if (!on(p)) continue;
        const r0 = parts.polyRings[p];
        const r1 = parts.polyRings[p + 1];
        const v0 = parts.ringVerts[r0];
        const v1 = parts.ringVerts[r1];
        if (v1 - v0 < 3) continue;
        const holeIndices = [];
        for (let r = r0 + 1; r < r1; r++) holeIndices.push((parts.ringVerts[r] - v0) * size);
        polys.push({ positions: g.coords.subarray(v0 * size, v1 * size), holeIndices });
        for (let r = r0; r < r1; r++) rings.push(g.coords.subarray(parts.ringVerts[r] * size, parts.ringVerts[r + 1] * size));
      }
      return [
        new SolidPolygonLayer({ ...base, id: `${id}-fill`, data: polys, getPolygon: (d) => d, positionFormat,
                                getFillColor: colors.fill }),
        ...lines(rings, "ring"),
      ];
    }
    const pts = [];
    for (let p = 0; p < parts.count; p++) if (on(p)) pts.push(g.coords.subarray(p * size, (p + 1) * size));
    const radius = (L.radius_px === undefined ? 3 : L.radius_px) + 3;
    return [new ScatterplotLayer({ ...base, id: `${id}-point`, data: pts, getPosition: (d) => d,
                                   radiusUnits: "pixels", getRadius: radius, filled: true, stroked: true,
                                   lineWidthUnits: "pixels", getLineWidth: 2.5,
                                   getFillColor: colors.fill, getLineColor: colors.line })];
  };
}

function buildRaster(L, ctx) {
  const { scene, tables } = ctx;
  const what = `layer ${L.id}`;
  let stops;
  try {
    stops = paletteStops(L.palette.name);
  } catch (err) {
    if (!(err instanceof UnknownPaletteError)) throw err;
    ctx.error(`layer ${L.id}: ${err.message}; not drawn`);
    return { summary: "error: unknown palette", layers: [] };
  }
  const vals = numericColumn(tables[L.values], L.values_column || "value", what);
  const [nx, ny] = L.grid.dim;
  const pixels = colorize(vals.values, vals.valid, L.grid, L.palette, stops);
  const image = document.createElement("canvas");
  image.width = nx;
  image.height = ny;
  image.getContext("2d").putImageData(new ImageData(pixels, nx, ny), 0, 0);
  const textureParameters = { minFilter: "nearest", magFilter: "nearest", mipmapFilter: "none" };
  const legend = { id: L.id, label: L.label || L.id, stops, range: L.palette.range };
  if (L.mesh) {
    const m = L.mesh;
    const pos = listColumn(tables[m.vertices], m.position_column || "position", what);
    const uv = listColumn(tables[m.vertices], m.uv_column || "uv", what);
    let idx = numericColumn(tables[m.indices], m.index_column || "index", what).values;
    if (!(idx instanceof Uint32Array)) idx = Uint32Array.from(idx, Number);
    // Mesh vertices are float32 model positions drawn relative to an anchor:
    // view.local_origin when the scene has one, else the CRS origin.
    const ref = scene.data[m.vertices];
    const origin = scene.view.local_origin || null;
    const n = pos.values.length / pos.size;
    const positions = new Float32Array(n * 3);
    const shift = origin && !ref.origin_subtracted ? origin : [0, 0];
    const back = origin && ref.origin_subtracted ? origin : [0, 0];
    const acc = ctx.bounds;
    for (let i = 0; i < n; i++) {
      const x = pos.values[i * pos.size];
      const y = pos.values[i * pos.size + 1];
      positions[i * 3] = x - shift[0];
      positions[i * 3 + 1] = y - shift[1];
      positions[i * 3 + 2] = pos.size > 2 ? pos.values[i * pos.size + 2] : 0;
      const ax = x + back[0];
      const ay = y + back[1];
      if (ax < acc[0]) acc[0] = ax;
      if (ax > acc[1]) acc[1] = ax;
      if (ay < acc[2]) acc[2] = ay;
      if (ay > acc[3]) acc[3] = ay;
    }
    const texCoords = uv.values instanceof Float32Array ? uv.values : Float32Array.from(uv.values);
    const mesh = {
      attributes: { positions: { value: positions, size: 3 }, texCoords: { value: texCoords, size: 2 } },
      indices: { value: idx, size: 1 },
    };
    const anchor = origin ? [origin[0], origin[1], 0] : [0, 0, 0];
    return {
      legend,
      summary: `${(idx.length / 3).toLocaleString("en-US")} triangles`,
      layers: [new SimpleMeshLayer({
        id: L.id,
        data: [0],
        mesh,
        texture: image,
        coordinateSystem: ctx.coordinateSystem,
        getPosition: anchor,
        getColor: [255, 255, 255, 255],
        sizeScale: 1,
        material: UNLIT,
        textureParameters,
      })],
    };
  }
  // No mesh: the grid is placed in the view CRS as an axis-aligned image,
  // which is only right when the grid is already in the view CRS.
  const sameCrs = scene.view.type === "cartesian" || sameJson(L.grid.crs, scene.view.crs);
  if (!sameCrs) {
    ctx.warn(`layer ${L.id}: grid CRS differs from view CRS and there is no mesh; not drawn`);
    return { legend, summary: "not drawn (needs a mesh)", layers: [] };
  }
  const [xmin, xmax, ymin, ymax] = L.grid.extent;
  const acc = ctx.bounds;
  acc[0] = Math.min(acc[0], xmin);
  acc[1] = Math.max(acc[1], xmax);
  acc[2] = Math.min(acc[2], ymin);
  acc[3] = Math.max(acc[3], ymax);
  return {
    legend,
    summary: `${nx} x ${ny} cells`,
    layers: [new BitmapLayer({
      id: L.id,
      image,
      bounds: [xmin, ymin, xmax, ymax],
      coordinateSystem: ctx.coordinateSystem,
      textureParameters,
    })],
  };
}

export { COORDINATE_SYSTEM };
