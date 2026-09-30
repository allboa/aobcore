// Scene spec 0.1 layers to deck.gl layers. This file is the only place where
// spec concepts meet deck.gl names.
import { COORDINATE_SYSTEM } from "@deck.gl/core";
import { SolidPolygonLayer, PathLayer, ScatterplotLayer, BitmapLayer } from "@deck.gl/layers";
import { SimpleMeshLayer } from "@deck.gl/mesh-layers";
import { earcut } from "@math.gl/polygon";
import { geometryChunk, partsOf, numericColumn, listColumn, rgbaChunk } from "./arrow.js";
import { paletteStops, colorize } from "./palettes.js";

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
  const geomIdx = table.schema.fields.findIndex((f) => f.name === ref.geometry.column);
  if (geomIdx < 0) throw new Error(`layer ${L.id}: geometry column "${ref.geometry.column}" not found`);
  const origin = ref.origin_subtracted ? ctx.scene.view.local_origin : null;
  const fields = table.schema.fields;
  const out = [];
  let count = 0;
  table.batches.forEach((b, bi) => {
    const g = geometryChunk(b.data.children[geomIdx], enc);
    g.coords = absolute(g.coords, g.size, origin);
    bounds(g.coords, g.size, ctx.bounds);
    const base = { coordinateSystem: ctx.coordinateSystem, pickable: false };
    const id = `${L.id}--${bi}`;
    const what = `layer ${L.id}`;
    const widthPx = L.stroke_width_px === undefined ? 1 : L.stroke_width_px;
    if (L.kind === "path") {
      const multi = enc === "geoarrow.multilinestring";
      const parts = partsOf(g, multi ? 1 : 0);
      const starts = g.levels[multi ? 1 : 0];
      count += g.rows;
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
  });
  const noun = { path: "line", polygon: "polygon", point: "point" }[L.kind];
  return { layers: out, summary: `${count.toLocaleString("en-US")} ${noun}${count === 1 ? "" : "s"}` };
}

function buildRaster(L, ctx) {
  const { scene, tables } = ctx;
  const what = `layer ${L.id}`;
  const vals = numericColumn(tables[L.values], L.values_column || "value", what);
  const stops = paletteStops(L.palette.name, ctx.warn);
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
        material: false,
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
