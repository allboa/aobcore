// Named palettes for raster values. Scene spec 0.1 fixes no registry of
// names; these are the names this renderer knows. Per the spec README, an
// unknown name is an error for that layer: the layer is not drawn and the
// error is reported, never silently replaced by another palette.
const STOPS = {
  // The polar probe's ramp (YlGnBu, reversed).
  ocean: [[8, 29, 88], [37, 52, 148], [34, 94, 168], [29, 145, 192], [65, 182, 196], [127, 205, 187], [199, 233, 180], [237, 248, 177]],
  viridis: [[68, 1, 84], [70, 50, 127], [54, 92, 141], [39, 127, 142], [31, 161, 135], [74, 194, 109], [159, 218, 58], [253, 231, 37]],
  ice: [[4, 18, 40], [20, 55, 95], [45, 100, 150], [90, 150, 195], [150, 195, 225], [210, 232, 245], [255, 255, 255]],
  gray: [[0, 0, 0], [255, 255, 255]],
  grey: [[0, 0, 0], [255, 255, 255]],
};

export const PALETTE_NAMES = Object.keys(STOPS);

export class UnknownPaletteError extends Error {}

export function paletteStops(name) {
  if (Object.prototype.hasOwnProperty.call(STOPS, name)) return STOPS[name];
  throw new UnknownPaletteError(`unknown palette "${name}" (known: ${PALETTE_NAMES.join(", ")})`);
}

export function ramp(stops, t) {
  t = Math.max(0, Math.min(1, t)) * (stops.length - 1);
  const i = Math.min(stops.length - 2, Math.floor(t));
  const f = t - i;
  const a = stops[i];
  const b = stops[i + 1];
  return [a[0] + (b[0] - a[0]) * f, a[1] + (b[1] - a[1]) * f, a[2] + (b[2] - a[2]) * f];
}

export function cssGradient(stops) {
  const parts = [];
  for (let s = 0; s <= 10; s++) {
    const c = ramp(stops, s / 10).map(Math.round);
    parts.push(`rgb(${c.join(",")}) ${s * 10}%`);
  }
  return `linear-gradient(90deg, ${parts.join(", ")})`;
}

// Color ncol * nrow values into RGBA pixels. Arrow nulls, NaN and the
// grid's nodata value are transparent.
export function colorize(values, valid, grid, palette, stops) {
  const [nx, ny] = grid.dim;
  const n = nx * ny;
  if (values.length !== n) {
    throw new Error(`raster has ${values.length} values; grid dim ${nx} x ${ny} needs ${n}`);
  }
  const lo = palette.range[0];
  const hi = palette.range[1];
  const span = hi - lo || 1;
  const hasNodata = typeof grid.nodata === "number";
  // float32 values hold nodata rounded to float32, so compare in that type.
  const nodata = hasNodata && values instanceof Float32Array ? Math.fround(grid.nodata) : grid.nodata;
  // 256-entry lookup keeps the per-pixel work to one multiply.
  const lut = new Uint8Array(256 * 3);
  for (let k = 0; k < 256; k++) {
    const c = ramp(stops, k / 255);
    lut[k * 3] = c[0];
    lut[k * 3 + 1] = c[1];
    lut[k * 3 + 2] = c[2];
  }
  const px = new Uint8ClampedArray(n * 4);
  for (let p = 0; p < n; p++) {
    const v = values[p];
    if ((valid && !valid[p]) || v !== v || (hasNodata && v === nodata)) continue;
    let k = Math.round(((v - lo) / span) * 255);
    k = k < 0 ? 0 : k > 255 ? 255 : k;
    px[p * 4] = lut[k * 3];
    px[p * 4 + 1] = lut[k * 3 + 1];
    px[p * 4 + 2] = lut[k * 3 + 2];
    px[p * 4 + 3] = 255;
  }
  return px;
}

// Material for textured meshes: the texture's color as is. deck.gl 9.4
// lights a SimpleMeshLayer even with material: false (the default phong
// material and lights apply), which adds a camera-dependent specular
// highlight to rasters; full ambient and no diffuse or specular light give
// the plain texture.
export const UNLIT = { ambient: 1, diffuse: 0, shininess: 0, specularColor: [0, 0, 0] };
