// Scene spec 0.3 jpeg tiles, decoded by the browser's own image decoder.
// A tile is joined to its level's shared JPEG tables (jpegStream), handed
// to createImageBitmap as an image/jpeg Blob, drawn onto a canvas and read
// back as RGBA; the decoder does the YCbCr to RGB conversion (and chroma
// upsampling) itself. The samples come back interleaved like any other
// codec's (1 for greyscale, 3 for red, green, blue), so the palette and rgb
// paths treat them alike.
import { decodeBase64 } from "./arrow.js";
import { jpegStream, samplesFromRGBA, decodeSamples } from "./decode.js";

const tablesCache = new WeakMap();

function tablesOf(enc) {
  if (!enc.jpeg_tables) return null;
  if (!tablesCache.has(enc)) tablesCache.set(enc, decodeBase64(enc.jpeg_tables));
  return tablesCache.get(enc);
}

function canvas(w, h) {
  if (typeof OffscreenCanvas !== "undefined") return new OffscreenCanvas(w, h);
  const c = document.createElement("canvas");
  c.width = w;
  c.height = h;
  return c;
}

export async function decodeJpegTile(raw, enc, w, h) {
  const stream = jpegStream(tablesOf(enc), raw);
  let bitmap;
  try {
    bitmap = await createImageBitmap(new Blob([stream], { type: "image/jpeg" }), {
      colorSpaceConversion: "none",
      premultiplyAlpha: "none",
    });
  } catch (err) {
    throw new Error(`the browser could not decode a jpeg tile (${err && err.message ? err.message : err})`);
  }
  try {
    if (bitmap.width !== w || bitmap.height !== h) {
      throw new Error(`jpeg tile is ${bitmap.width} x ${bitmap.height}; the plan says ${w} x ${h}`);
    }
    const c = canvas(w, h);
    const g = c.getContext("2d", { willReadFrequently: true });
    g.drawImage(bitmap, 0, 0);
    const spp = enc.samples_per_pixel || 1;
    return { samples: samplesFromRGBA(g.getImageData(0, 0, w, h).data, spp), spp };
  } finally {
    if (bitmap.close) bitmap.close();
  }
}

// Any tile to {samples, spp}: jpeg through the browser, the rest in JS.
export async function decodeTileSamples(raw, enc, w, h) {
  if (enc.codec === "jpeg") return decodeJpegTile(raw, enc, w, h);
  return decodeSamples(raw, enc, w, h);
}
