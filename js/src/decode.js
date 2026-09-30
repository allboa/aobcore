// Tile bytes to sample values, per scene spec 0.2 tileEncoding: decompress
// (codec), undo the TIFF predictor, read samples in the stated byte order,
// and pick the band. Pure functions with no DOM, so they run in Node tests.
//
// Codecs: none, deflate (fflate), zstd (fzstd), lzw and packbits (the
// decoders from geotiff.js, imported by file because the package exports
// only its whole reader). lerc, lerc_deflate, lerc_zstd and webp are not
// supported: decodeTile throws UnsupportedCodecError, which the layer
// reports as a layer error. Nothing falls back silently.
import { unzlibSync } from "fflate";
import { decompress as zstdDecompress } from "fzstd";
// These two paths are geotiff.js internals, not its public exports: when
// the geotiff version in package.json changes, check they still exist and
// still export a default decoder class with a synchronous decodeBlock(buffer)
// (npm test decodes lzw and packbits tiles and fails if they do not).
import LZWDecoder from "../node_modules/geotiff/dist-module/compression/lzw.js";
import PackbitsDecoder from "../node_modules/geotiff/dist-module/compression/packbits.js";

export class UnsupportedCodecError extends Error {}

export const SUPPORTED_CODECS = ["none", "deflate", "lzw", "zstd", "packbits"];

const DTYPES = {
  uint8: [Uint8Array, 1, "getUint8"],
  int8: [Int8Array, 1, "getInt8"],
  uint16: [Uint16Array, 2, "getUint16"],
  int16: [Int16Array, 2, "getInt16"],
  uint32: [Uint32Array, 4, "getUint32"],
  int32: [Int32Array, 4, "getInt32"],
  float32: [Float32Array, 4, "getFloat32"],
  float64: [Float64Array, 8, "getFloat64"],
};

// Why an encoding cannot be decoded here, or null when it can.
export function encodingProblem(enc) {
  if (!SUPPORTED_CODECS.includes(enc.codec)) {
    return `codec ${enc.codec} is not supported by this renderer (supported: ${SUPPORTED_CODECS.join(", ")})`;
  }
  if (!DTYPES[enc.dtype]) return `dtype ${enc.dtype} is not supported`;
  const pred = enc.predictor || "none";
  if (pred === "floating_point" && !/^float/.test(enc.dtype)) return "the floating point predictor needs a float dtype";
  if (pred === "horizontal" && /^float/.test(enc.dtype)) return "the horizontal predictor needs an integer dtype";
  if (!["none", "horizontal", "floating_point"].includes(pred)) return `predictor ${pred} is not supported`;
  return null;
}

function toArrayBuffer(u8) {
  return u8.byteOffset === 0 && u8.byteLength === u8.buffer.byteLength ? u8.buffer : u8.slice().buffer;
}

export function decompress(codec, bytes) {
  switch (codec) {
    case "none":
      return bytes;
    case "deflate":
      return unzlibSync(bytes);
    case "zstd":
      return zstdDecompress(bytes);
    case "lzw":
      return new Uint8Array(new LZWDecoder({}).decodeBlock(toArrayBuffer(bytes)));
    case "packbits":
      return new Uint8Array(new PackbitsDecoder({}).decodeBlock(toArrayBuffer(bytes)));
    default:
      throw new UnsupportedCodecError(`codec ${codec} is not supported by this renderer (supported: ${SUPPORTED_CODECS.join(", ")})`);
  }
}

// Decode one tile of width w and height h (padding included). Returns the
// raw samples of the chosen band as a typed array of w * h values, row 0 at
// the top.
export function decodeTile(raw, enc, w, h) {
  const problem = encodingProblem(enc);
  if (problem) throw new UnsupportedCodecError(problem);
  const bytes = decompress(enc.codec, raw);
  const [Ctor, size, getter] = DTYPES[enc.dtype];
  const planar = enc.planar || "interleaved";
  const spp = planar === "separate" ? 1 : enc.samples_per_pixel || 1;
  const n = w * h * spp;
  if (bytes.length < n * size) {
    throw new Error(`tile decompressed to ${bytes.length} bytes; ${w} x ${h} x ${spp} ${enc.dtype} needs ${n * size}`);
  }
  const little = (enc.byte_order || "little") === "little";
  const pred = enc.predictor || "none";
  let out;
  if (pred === "floating_point") {
    out = undoFloatingPoint(bytes, Ctor, size, w * spp, h, spp);
  } else {
    out = new Ctor(n);
    const dv = new DataView(bytes.buffer, bytes.byteOffset, n * size);
    for (let i = 0; i < n; i++) out[i] = dv[getter](i * size, little);
    if (pred === "horizontal") {
      // Horizontal differencing: each sample is stored as the difference
      // from the same band's sample to its left. Typed array assignment
      // wraps modulo 2^bits, as the TIFF predictor does.
      const rowLen = w * spp;
      for (let r = 0; r < h; r++) {
        const o = r * rowLen;
        for (let i = spp; i < rowLen; i++) out[o + i] = out[o + i] + out[o + i - spp];
      }
    }
  }
  if (spp === 1) return out;
  const band = (enc.band || 1) - 1;
  const one = new Ctor(w * h);
  for (let i = 0; i < w * h; i++) one[i] = out[i * spp + band];
  return one;
}

// TIFF predictor 3 (Adobe tech note 3): per row, the bytes of the samples
// are split into planes, most significant byte first, and each byte is
// stored as a difference from the byte spp positions before it. The result
// is in this machine's byte order, which is little endian for every
// platform a browser runs on.
function undoFloatingPoint(bytes, Ctor, size, wc, h, spp) {
  const rowBytes = wc * size;
  const out = new Ctor(wc * h);
  const ob = new Uint8Array(out.buffer);
  const tmp = new Uint8Array(rowBytes);
  for (let r = 0; r < h; r++) {
    tmp.set(bytes.subarray(r * rowBytes, (r + 1) * rowBytes));
    for (let i = spp; i < rowBytes; i++) tmp[i] = (tmp[i] + tmp[i - spp]) & 0xff;
    const o = r * rowBytes;
    for (let c = 0; c < wc; c++) {
      for (let b = 0; b < size; b++) ob[o + c * size + b] = tmp[(size - b - 1) * wc + c];
    }
  }
  return out;
}

// Color one tile's raw samples into RGBA pixels (w x h, padding
// transparent). value = raw * scale + offset; nodata is compared with the
// raw value; NaN is transparent.
export function colorizeTile(values, w, h, win, enc, nodata, range, lut) {
  const px = new Uint8ClampedArray(w * h * 4);
  const scale = enc.scale === undefined ? 1 : enc.scale;
  const offset = enc.offset === undefined ? 0 : enc.offset;
  const lo = range[0];
  const span = range[1] - range[0] || 1;
  const hasNodata = typeof nodata === "number";
  const nd = hasNodata && values instanceof Float32Array ? Math.fround(nodata) : nodata;
  const x0 = win ? win.x : 0;
  const y0 = win ? win.y : 0;
  const x1 = win ? win.x + win.width : w;
  const y1 = win ? win.y + win.height : h;
  for (let r = y0; r < y1; r++) {
    for (let c = x0; c < x1; c++) {
      const p = r * w + c;
      const raw = values[p];
      if (raw !== raw || (hasNodata && raw === nd)) continue;
      const v = raw * scale + offset;
      let k = Math.round(((v - lo) / span) * 255);
      k = k < 0 ? 0 : k > 255 ? 255 : k;
      px[p * 4] = lut[k * 3];
      px[p * 4 + 1] = lut[k * 3 + 1];
      px[p * 4 + 2] = lut[k * 3 + 2];
      px[p * 4 + 3] = 255;
    }
  }
  return px;
}
