// Renderer tests.
//
//   node test/renderer.test.mjs
//
// 1. palettes.js in Node: known names resolve, an unknown name throws, and
//    float32 nodata is matched.
// 2. decode.js in Node: tiles written by GDAL (test/codec-tiles.json, from
//    tools/make-codec-tiles.R) decode to the values GDAL reads, for every
//    supported codec and predictor, both byte orders, and multi-band tiles;
//    an unsupported codec throws UnsupportedCodecError. Level selection.
// 3. The built bundle in headless Chromium: a scene whose raster names an
//    unknown palette still draws its other layers, the raster layer is not
//    drawn, and the status line reports "error: unknown palette".
// 4. A 0.2 tiled raster read by HTTP range requests from a local server
//    draws, one from a server that ignores Range downloads the file once,
//    and one with an unsupported codec is a layer error.
// 5. Scene spec 0.3 colour images: in Node, joining jpeg tables to a tile,
//    colorizeRGB (bands, alpha, nodata, range) and the RGBA LZW tile of
//    test/rgb-tiles.json against GDAL; in the browser, the YCbCr JPEG tile
//    decoded by the browser against GDAL's read, and a 0.3 scene of the
//    JPEG COG over HTTP range requests.
// 6. Scene spec 0.5 in the browser: legends drawn from the scene (palette
//    and stops ramps, classes, a no-data entry, hidden with their layer),
//    and in an EPSG:3031 view a click on a point selects it and its popup
//    shows the named columns (closed by Escape or the keyboard), a point
//    popup follows the pointer over a polygon, and a missing popup column
//    is a layer error. A click still selects when its press is held 1.5 s
//    or every pick takes 1.5 s, and a drag or a double click (which zooms)
//    does not select. In Node, cell text and legend helpers.
// 7. Served pages (decision 0006) in the browser: a page whose element
//    names a blob base fetches a blob it does not carry from the base plus
//    encodeURIComponent(key) (a key with "/", "@" and "+" round-trips),
//    a tiled raster takes listed tile blobs from the base and nothing by
//    range, an unlisted tile is read by range as before, and a missing blob
//    with no base is still an error (with a base, one that names the 404,
//    and a network failure names the key and URL); rendering again into
//    the same element aborts the first render's blob fetches. A listed tile
//    blob that answers 404 or has the wrong length is a layer error.
// Also in Node: the range reader keeps a whole-file (200) response, and the
// tile cache evicts least recently used idle tiles.
// Set CHROMIUM_PATH to pick a browser; SKIP_BROWSER=1 runs only part 1.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { tableToIPC, tableFromArrays, Table, Schema, Field, RecordBatch, Struct, List, FixedSizeList,
  Float64, Utf8, Int32, DateDay, makeData, vectorFromArray } from "apache-arrow";
import { paletteStops, colorize, UnknownPaletteError } from "../src/palettes.js";
import { decodeTile, decodeSamples, jpegStream, samplesFromRGBA, colorizeRGB, encodingProblem, UnsupportedCodecError } from "../src/decode.js";
import { selectLevel, rangeReader, tilesToEvict } from "../src/tiles.js";
import { cellText } from "../src/popup.js";
import { stopsGradient, rangeLabel, rgbaCss } from "../src/legend.js";

const here = dirname(fileURLToPath(import.meta.url));

// ---- 1. palettes ---------------------------------------------------------
assert.ok(Array.isArray(paletteStops("ocean")));
assert.throws(() => paletteStops("no-such-palette"), UnknownPaletteError);
assert.throws(() => paletteStops("toString"), UnknownPaletteError);

const nodata = 0.1; // not exact in float32
const vals = new Float32Array([0.1, 0.5]);
const px = colorize(vals, null, { dim: [2, 1], nodata }, { range: [0, 1] }, paletteStops("gray"));
assert.equal(px[3], 0, "float32 nodata cell is transparent");
assert.equal(px[7], 255, "other cell is opaque");
console.log("ok   palettes");

// ---- 2. decoders against GDAL ----------------------------------------------
const cases = JSON.parse(readFileSync(join(here, "codec-tiles.json"), "utf8"));
for (const c of cases) {
  const [w, h] = c.size;
  const v = decodeTile(new Uint8Array(Buffer.from(c.bytes, "base64")), c.encoding, w, h);
  assert.equal(v.length, w * h, c.name);
  const win = c.window;
  const row = (r) => Array.from(v.subarray(r * w, r * w + win.width), Number);
  const close = (a, b) => a.length === b.length && a.every((x, i) => Math.abs(x - b[i]) <= 1e-6 * Math.max(1, Math.abs(b[i])));
  assert.ok(close(row(0), c.first_row), `${c.name}: first row`);
  assert.ok(close(row(win.height - 1), c.last_row), `${c.name}: last row`);
  let sum = 0;
  for (let r = 0; r < win.height; r++) for (let k = 0; k < win.width; k++) sum += Number(v[r * w + k]);
  assert.ok(Math.abs(sum - c.sum) <= 1e-6 * Math.max(1, Math.abs(c.sum)), `${c.name}: sum ${sum} vs ${c.sum}`);
  console.log(`ok   decode ${c.name}`);
}
for (const codec of ["lerc", "lerc_deflate", "lerc_zstd", "webp"]) {
  assert.throws(() => decodeTile(new Uint8Array(4), { codec, dtype: "uint8" }, 1, 1), UnsupportedCodecError, codec);
}
console.log("ok   unsupported codecs throw");

// ---- 5a. colour images in Node ---------------------------------------------
{
  const u8 = (a) => Uint8Array.from(a);
  // tables: SOI, a DQT stub, EOI; tile: SOI, SOF stub, EOI.
  const joined = jpegStream(u8([0xff, 0xd8, 0xff, 0xdb, 1, 0xff, 0xd9]), u8([0xff, 0xd8, 0xff, 0xc0, 2, 0xff, 0xd9]));
  assert.deepEqual([...joined], [0xff, 0xd8, 0xff, 0xdb, 1, 0xff, 0xc0, 2, 0xff, 0xd9]);
  assert.deepEqual([...jpegStream(null, u8([0xff, 0xd8, 9]))], [0xff, 0xd8, 9], "no tables: the tile as is");
  assert.throws(() => jpegStream(u8([0xff, 0xd8, 0xff, 0xd9]), u8([1, 2, 3])), /SOI/);
  assert.deepEqual([...samplesFromRGBA(u8([1, 2, 3, 255, 4, 5, 6, 255]), 3)], [1, 2, 3, 4, 5, 6]);
  assert.deepEqual([...samplesFromRGBA(u8([7, 7, 7, 255]), 1)], [7]);
  const jpg = { codec: "jpeg", dtype: "uint8", samples_per_pixel: 3, planar: "interleaved" };
  assert.equal(encodingProblem(jpg), null);
  assert.match(encodingProblem({ ...jpg, samples_per_pixel: 4 }), /1 or 3 samples/);
  assert.match(encodingProblem({ ...jpg, dtype: "uint16" }), /uint8/);
  assert.match(encodingProblem({ ...jpg, predictor: "horizontal" }), /predictor/);
  assert.throws(() => decodeSamples(new Uint8Array(4), jpg, 1, 1), /browser/);

  // Two pixels of 4 bands: bands name red, green, blue and alpha.
  const enc = { dtype: "uint8" };
  const px = colorizeRGB(u8([10, 20, 30, 255, 1, 2, 3, 128]), 4, 2, 1, null, enc, undefined, { bands: [1, 2, 3], alpha: 4 });
  assert.deepEqual([...px], [10, 20, 30, 255, 1, 2, 3, 128]);
  const bgr = colorizeRGB(u8([10, 20, 30, 0, 1, 2, 3, 9]), 4, 2, 1, null, enc, undefined, { bands: [3, 2, 1], alpha: 4 });
  assert.deepEqual([...bgr], [0, 0, 0, 0, 3, 2, 1, 9], "alpha 0 is transparent; bands are reordered");
  const nd = colorizeRGB(u8([0, 0, 0, 0, 5, 0]), 3, 2, 1, null, enc, 0, { bands: [1, 2, 3] });
  assert.deepEqual([...nd], [0, 0, 0, 0, 0, 5, 0, 255], "all colour bands at nodata is transparent, one is not");
  const grey = colorizeRGB(u8([200]), 1, 1, 1, null, enc, undefined, { bands: [1, 1, 1] });
  assert.deepEqual([...grey], [200, 200, 200, 255]);
  // uint8 ignores scale and offset unless a range is given; a range stretches.
  const scaled = { dtype: "uint8", scale: 2, offset: 1 };
  assert.deepEqual([...colorizeRGB(u8([100, 100, 100]), 3, 1, 1, null, scaled, undefined, { bands: [1, 2, 3] })], [100, 100, 100, 255]);
  assert.deepEqual([...colorizeRGB(u8([0, 50, 100]), 3, 1, 1, null, enc, undefined, { bands: [1, 2, 3], range: [0, 100] })], [0, 128, 255, 255]);
  // uint16 with a range in scaled values, clamped; the window leaves padding transparent.
  const u16 = colorizeRGB(Uint16Array.from([0, 2000, 4000, 9, 9, 9]), 3, 2, 1, { x: 0, y: 0, width: 1, height: 1 },
    { dtype: "uint16", scale: 0.001 }, undefined, { bands: [1, 2, 3], range: [0, 2] });
  assert.deepEqual([...u16], [0, 255, 255, 255, 0, 0, 0, 0]);
  console.log("ok   colour images: jpeg table join, rgb bands, alpha, nodata and range");

  for (const c of JSON.parse(readFileSync(join(here, "rgb-tiles.json"), "utf8")).filter((c) => c.encoding.codec !== "jpeg")) {
    const [w, h] = c.size;
    const { samples, spp } = decodeSamples(new Uint8Array(Buffer.from(c.bytes, "base64")), c.encoding, w, h);
    const want = new Uint8Array(Buffer.from(c.samples, "base64"));
    assert.equal(spp, c.encoding.samples_per_pixel);
    const win = c.window;
    for (let r = 0; r < win.height; r++) {
      for (let k = 0; k < win.width * spp; k++) {
        if (samples[r * w * spp + k] !== want[r * win.width * spp + k]) assert.fail(`${c.name}: row ${r} sample ${k}`);
      }
    }
    console.log(`ok   decode ${c.name}`);
  }
}
const lv = [{ pixel_size: 256 }, { pixel_size: 32 }, { pixel_size: 64 }, { pixel_size: 128 }];
assert.equal(selectLevel(lv, 100, "coarsest_sufficient"), 2);
assert.equal(selectLevel(lv, 10, "coarsest_sufficient"), 1, "finest when none suffices");
assert.equal(selectLevel(lv, 1000, "coarsest_sufficient"), 0);
assert.equal(selectLevel(lv, 100, "nearest_pixel_size"), 3);
assert.equal(selectLevel(lv, 96, "nearest_pixel_size"), 2, "ties go to the finer level");
console.log("ok   level selection");

// Range reader: 206 answers are used as they are; a 200 (Range ignored) is
// read once and later tiles are cut from it, even when requested together.
{
  const file = Uint8Array.from({ length: 100 }, (_, i) => i);
  let bodies = 0;
  let cancelled = 0;
  let fetches = 0;
  const fake = (ignore) => async (url, opts) => {
    fetches++;
    const [a, b] = /bytes=(\d+)-(\d+)/.exec(opts.headers.Range).slice(1).map(Number);
    await new Promise((r) => setTimeout(r, 5));
    const part = ignore ? file : file.slice(a, b + 1);
    const sig = opts.signal;
    return {
      status: ignore ? 200 : 206, ok: true,
      body: { cancel: async () => { cancelled++; } },
      arrayBuffer: () => new Promise((res, rej) => {
        bodies++;
        const t = setTimeout(() => res(part.slice().buffer), 20);
        if (sig) sig.addEventListener("abort", () => { clearTimeout(t); rej(sig.reason); });
      }),
    };
  };
  const partial = rangeReader(fake(false));
  assert.deepEqual(await Promise.all([partial("u", 10, 3), partial("u", 0, 1)]).then((r) => r.map((x) => [...x])), [[10, 11, 12], [0]]);
  bodies = 0;
  fetches = 0;
  const whole = rangeReader(fake(true));
  const got = await Promise.all([whole("u", 0, 2), whole("u", 50, 2), whole("u", 98, 2)]);
  assert.deepEqual(got.map((x) => [...x]), [[0, 1], [50, 51], [98, 99]]);
  assert.deepEqual([...await whole("u", 20, 1)], [20]);
  assert.equal(fetches, 1, "requests sent together wait for the first answer");
  assert.equal(bodies, 1, "the whole file is read once");
  assert.equal(cancelled, 0);
  await assert.rejects(whole("u", 99, 5), /has 100 bytes/);
  // The whole file is read under the first tile's signal. When that tile
  // is aborted, a tile still wanted fetches again instead of failing.
  const race = rangeReader(fake(true));
  const a = new AbortController();
  const b = new AbortController();
  const pa = race("v", 0, 2, a.signal);
  const pb = race("v", 50, 2, b.signal);
  setTimeout(() => a.abort(), 12);
  const [ra, rb] = await Promise.allSettled([pa, pb]);
  assert.equal(ra.status, "rejected");
  assert.equal(rb.status, "fulfilled", String(rb.reason));
  assert.deepEqual([...rb.value], [50, 51]);
  console.log("ok   range reader keeps a whole-file response");
}
{
  const t = (used) => ({ used });
  const cached = [t(5), t(1), t(9), t(3), t(9)];
  assert.deepEqual(tilesToEvict(cached, 9, 3), []);
  assert.deepEqual(tilesToEvict(cached, 9, 1).map((x) => x.used), [1, 3]);
  assert.deepEqual(tilesToEvict(cached, 9, 0).map((x) => x.used), [1, 3, 5]);
  console.log("ok   tile cache evicts least recently used idle tiles");
}

// ---- 6a. popup text and legend helpers in Node -------------------------------
{
  assert.equal(cellText("1957-01-13", new Utf8()), "1957-01-13", "ISO text passes through");
  assert.equal(cellText(null, new Utf8()), "NA");
  assert.equal(cellText(15, new Float64()), "15");
  assert.equal(cellText(0.1 + 0.2, new Float64()), "0.3");
  assert.equal(cellText(Date.UTC(1957, 0, 13), new DateDay()), "1957-01-13");
  assert.equal(cellText(true), "true");
  assert.equal(cellText(12n), "12");
  assert.equal(rangeLabel(-2), "-2");
  assert.equal(rangeLabel(1 / 3), "0.333333");
  assert.equal(rgbaCss([1, 2, 3, 255]), "rgba(1, 2, 3, 1)");
  assert.equal(rgbaCss(["url(x)", "1", 300, 255]), "rgba(0, 1, 255, 1)", "only numbers reach the CSS");
  assert.equal(stopsGradient([{ at: 0, color: [0, 0, 0, 0] }, { at: 1, color: [255, 0, 0, 255] }]),
    "linear-gradient(90deg, rgba(0, 0, 0, 0) 0%, rgba(255, 0, 0, 1) 100%)");
  console.log("ok   popup cell text and legend helpers");
}

if (process.env.SKIP_BROWSER) process.exit(0);

// ---- 3. unknown palette in a page -----------------------------------------
const { chromium } = await import("playwright-core");
const bundle = readFileSync(join(here, "..", "..", "inst", "renderer", "aob-renderer.min.js"), "utf8");
const b64 = (t) => Buffer.from(tableToIPC(t, "stream")).toString("base64");
const blobs = { values: b64(tableFromArrays({ value: new Float64Array([0, 1, 2, 3]) })) };
const scene = {
  version: "0.1",
  view: { type: "cartesian", extent: [0, 2, 0, 2] },
  data: { values: { format: "arrow-ipc-stream", blob: "values" } },
  layers: [
    { id: "bad", kind: "raster", grid: { crs: "EPSG:3031", extent: [0, 1, 0, 1], dim: [2, 2] },
      values: "values", palette: { name: "no-such-palette", range: [0, 3] } },
    { id: "good", kind: "raster", grid: { crs: "EPSG:3031", extent: [1, 2, 1, 2], dim: [2, 2] },
      values: "values", palette: { name: "viridis", range: [0, 3] } },
  ],
};
const html = `<!DOCTYPE html><html><head><meta charset="utf-8"></head><body style="margin:0;height:100vh">
<div data-aob-scene="s" style="height:100%"></div>
<script type="application/json" id="s">${JSON.stringify(scene)}</script>
<script type="application/octet-stream" data-aob-blob="values" data-aob-scene="s">${blobs.values}</script>
<script>${bundle}</script></body></html>`;

const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH || undefined,
  args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
});
try {
  const page = await browser.newPage();
  await page.setContent(html);
  await page.waitForFunction(() => {
    const s = document.querySelector("div[data-aob-scene]").dataset.aobStatus;
    return s === "ready" || s === "error";
  }, null, { timeout: 60000 });
  const r = await page.evaluate(() => {
    const c = document.querySelector("div[data-aob-scene]");
    return {
      status: c.dataset.aobStatus,
      errors: c.dataset.aobErrors,
      line: c.querySelector(".aob-status").textContent,
      isError: c.querySelector(".aob-status").classList.contains("aob-error"),
      counts: [...c.querySelectorAll(".aob-count")].map((e) => e.textContent),
    };
  });
  assert.equal(r.status, "ready", "the scene still draws");
  assert.equal(r.errors, "1");
  assert.ok(r.isError, "status line shows an error");
  assert.match(r.line, /error: layer bad: unknown palette "no-such-palette"/);
  assert.deepEqual(r.counts, ["2 x 2 cells", "error: unknown palette"]);
  console.log("ok   unknown palette is a layer error");
} finally {
  await browser.close();
}

// ---- 4. tiled raster over HTTP range requests -------------------------------
const { createServer } = await import("node:http");
const tiff = readFileSync(join(here, "..", "..", "inst", "extdata", "polar_3031.tif"));
const ycbcr = readFileSync(join(here, "..", "..", "inst", "extdata", "polar_ycbcr.tif"));
const rgbScene = JSON.parse(readFileSync(join(here, "rgb-scene.json"), "utf8"));
const rgbTiles = JSON.parse(readFileSync(join(here, "rgb-tiles.json"), "utf8"));
const tiled = JSON.parse(readFileSync(join(here, "tiled-scene.json"), "utf8"));
const fine = JSON.parse(readFileSync(join(here, "tiled-scene-fine.json"), "utf8"));
const pageFor = (sc, bl) => `<!DOCTYPE html><html><head><meta charset="utf-8"></head><body style="margin:0;height:100vh">
<div data-aob-scene="s" style="height:100%"></div>
<script type="application/json" id="s">${JSON.stringify(sc)}</script>
${Object.entries(bl).map(([k, v]) => `<script type="application/octet-stream" data-aob-blob="${k}" data-aob-scene="s">${v}</script>`).join("\n")}
<script>${bundle}</script></body></html>`;
const bad = JSON.parse(JSON.stringify(tiled.scene));
bad.layers[0].plan.levels[0].encoding.codec = "lerc";
bad.layers.push({ id: "values", kind: "raster", grid: { crs: "EPSG:3031", extent: [0, 1e6, 0, 1e6], dim: [2, 2] },
  values: "values", palette: { name: "viridis", range: [0, 3] } });
bad.data.values = { format: "arrow-ipc-stream", blob: "values" };
const pages = {
  "/tiled.html": pageFor(tiled.scene, tiled.blobs),
  "/norange/tiled.html": pageFor(fine.scene, fine.blobs),
  "/bad.html": pageFor(bad, { ...tiled.blobs, values: blobs.values }),
  "/rgb.html": pageFor(rgbScene.scene, rgbScene.blobs),
};
const rgbRanges = [];
const ranges = [];
let wholeFile = 0;
const server = createServer((req, res) => {
  if (pages[req.url]) {
    res.writeHead(200, { "content-type": "text/html" });
    return res.end(pages[req.url]);
  }
  if (req.url === "/norange/polar_3031.tif") {
    // A server that ignores Range.
    wholeFile++;
    res.writeHead(200, { "content-type": "image/tiff" });
    return res.end(tiff);
  }
  if (req.url === "/polar_ycbcr.tif") {
    const m = /^bytes=(\d+)-(\d+)$/.exec(req.headers.range || "");
    if (!m) {
      res.writeHead(200, { "content-type": "image/tiff" });
      return res.end(ycbcr);
    }
    rgbRanges.push(req.headers.range);
    const a = Number(m[1]);
    const b = Number(m[2]);
    res.writeHead(206, { "content-type": "image/tiff", "content-range": `bytes ${a}-${b}/${ycbcr.length}` });
    return res.end(ycbcr.subarray(a, b + 1));
  }
  if (req.url === "/polar_3031.tif") {
    const m = /^bytes=(\d+)-(\d+)$/.exec(req.headers.range || "");
    if (!m) {
      res.writeHead(200, { "content-type": "image/tiff" });
      return res.end(tiff);
    }
    ranges.push(req.headers.range);
    const a = Number(m[1]);
    const b = Number(m[2]);
    res.writeHead(206, { "content-type": "image/tiff", "content-range": `bytes ${a}-${b}/${tiff.length}` });
    return res.end(tiff.subarray(a, b + 1));
  }
  res.writeHead(404);
  res.end();
});
await new Promise((r) => server.listen(0, "127.0.0.1", r));
const base = `http://127.0.0.1:${server.address().port}`;
const browser2 = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH || undefined,
  args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
});
try {
  const state = async (path) => {
    const page = await browser2.newPage({ viewport: { width: 900, height: 700 } });
    await page.goto(base + path);
    await page.waitForFunction(() => {
      const s = document.querySelector("div[data-aob-scene]").dataset.aobStatus;
      return s === "ready" || s === "error";
    }, null, { timeout: 60000 });
    const r = await page.evaluate(() => {
      const c = document.querySelector("div[data-aob-scene]");
      return { status: c.dataset.aobStatus, errors: c.dataset.aobErrors, tiles: c.dataset.aobTiles,
        pending: c.dataset.aobPending, line: c.querySelector(".aob-status").textContent,
        counts: [...c.querySelectorAll(".aob-count")].map((e) => e.textContent) };
    });
    await page.close();
    return r;
  };
  const ok = await state("/tiled.html");
  assert.equal(ok.status, "ready");
  assert.equal(ok.errors, undefined, ok.line);
  assert.equal(ok.pending, "0");
  assert.match(ok.tiles, /^sst: level [23], \d+ tiles?$/);
  const lv = tiled.scene.layers[0].plan.levels;
  const want = new Set(lv.flatMap((l) => l.tiles.map((t) => `bytes=${t.byte_offset}-${t.byte_offset + t.byte_length - 1}`)));
  assert.ok(ranges.length > 0, "tiles were fetched with range requests");
  for (const r of ranges) assert.ok(want.has(r), `range ${r} is a planned tile`);
  console.log(`ok   tiled raster over HTTP range requests (${ok.tiles}; ${ranges.length} ranges)`);

  const nr = await state("/norange/tiled.html");
  assert.equal(nr.status, "ready");
  assert.equal(nr.errors, undefined, nr.line);
  assert.equal(nr.tiles, "sst: level 0, 16 tiles");
  // Requests sent together would each get a 200, so the renderer's first
  // tiles must share one download: the server sends the whole file once.
  assert.equal(wholeFile, 1, `whole file sent ${wholeFile} times for 16 tiles`);
  console.log("ok   a server that ignores Range sends the file once for 16 tiles");

  const b = await state("/bad.html");
  assert.equal(b.status, "ready", "the rest of the scene still draws");
  assert.equal(b.errors, "1");
  assert.match(b.line, /error: layer sst: level \d: codec lerc is not supported by this renderer/);
  assert.deepEqual(b.counts, ["2 x 2 cells", "error: codec lerc is not supported by this renderer"]);
  console.log("ok   unsupported codec is a layer error");

  // ---- 5b. colour images in the browser -----------------------------------
  const rgb = await state("/rgb.html");
  assert.equal(rgb.status, "ready");
  assert.equal(rgb.errors, undefined, rgb.line);
  assert.equal(rgb.pending, "0");
  assert.match(rgb.tiles, /^cog: level \d, \d+ tiles?$/);
  assert.ok(rgbRanges.length > 0, "jpeg tiles were fetched with range requests");
  console.log(`ok   0.3 colour image of a YCbCr JPEG COG over HTTP range requests (${rgb.tiles})`);

  // The browser's JPEG decode of a tile joined to its tables, against GDAL.
  const page = await browser2.newPage();
  await page.goto(base + "/rgb.html");
  await page.waitForFunction(() => document.querySelector("div[data-aob-scene]").dataset.aobStatus === "ready", null, { timeout: 60000 });
  for (const c of rgbTiles) {
    const got = await page.evaluate(async (c) => {
      const bytes = Uint8Array.from(atob(c.bytes), (ch) => ch.charCodeAt(0));
      const want = Uint8Array.from(atob(c.samples), (ch) => ch.charCodeAt(0));
      const [w, h] = c.size;
      const { samples, spp } = await aob._decodeTileSamples(bytes, c.encoding, w, h);
      let sum = 0;
      let max = 0;
      const n = c.window.width * c.window.height * spp;
      for (let r = 0; r < c.window.height; r++) {
        for (let k = 0; k < c.window.width * spp; k++) {
          const d = Math.abs(samples[r * w * spp + k] - want[r * c.window.width * spp + k]);
          sum += d;
          if (d > max) max = d;
        }
      }
      return { spp, mean: sum / n, max };
    }, c);
    assert.equal(got.spp, c.encoding.samples_per_pixel, c.name);
    if (c.encoding.codec === "jpeg") {
      // Decoders upsample chroma differently, so sharp edges differ a little.
      assert.ok(got.mean < 1.5, `${c.name}: mean difference ${got.mean}`);
    } else {
      assert.equal(got.max, 0, c.name);
    }
    console.log(`ok   browser decode ${c.name} (mean difference ${got.mean.toFixed(3)}, max ${got.max})`);
  }
  await page.close();
} finally {
  await browser2.close();
  server.close();
}

// ---- view.bounds keep the camera in (scene spec 0.4) -------------------------
{
  const scene = { version: "0.4", view: { type: "projected", crs: "EPSG:3031", bounds: [-1e7, 1e7, -1e7, 1e7] },
                  data: {}, layers: [] };
  const browser3 = await chromium.launch({
    executablePath: process.env.CHROMIUM_PATH || undefined,
    args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  });
  try {
    const page = await browser3.newPage({ viewport: { width: 800, height: 600 } });
    await page.setContent(`<!DOCTYPE html><html><head><meta charset="utf-8"></head><body style="margin:0;height:100vh">
<div id="c" style="height:100%;width:100%"></div><script>${bundle}</script></body></html>`);
    const r = await page.evaluate(async (sc) => {
      const c = document.getElementById("c");
      const h = await aob.render(c, sc);
      const w = c.querySelector("canvas").parentElement.clientWidth;
      const hh = c.querySelector("canvas").parentElement.clientHeight;
      const first = h.view();
      const out = h.setView({ ...first, zoom: -40, target: [5e7, -5e7, 0] });
      const near = h.setView({ ...first, zoom: first.zoom + 6, target: [2e7, 0, 0] });
      return { w, hh, first, out, near };
    }, scene);
    // Padded bounds: 1.5e7 each side, so 3e7 across.
    const minZoom = Math.log2(Math.min(r.w / 3e7, r.hh / 3e7));
    assert.ok(Math.abs(r.out.zoom - minZoom) < 1e-9, `zoom out stops at ${minZoom} (got ${r.out.zoom})`);
    assert.ok(r.first.zoom >= minZoom - 1e-9, "the initial view is within the limit");
    // Zoomed out as far as it goes, the (square) padded bounds fill the canvas's
    // short side and are centred on the long one.
    assert.deepEqual(r.out.target.slice(0, 2).map((v) => Math.round(v)), [0, 0]);
    // Zoomed in, a target past the edge is pulled back so the view stays inside.
    const half = (r.w / 2) * Math.pow(2, -r.near.zoom);
    assert.ok(Math.abs(r.near.target[0] - (1.5e7 - half)) < 1, `x held at the edge (got ${r.near.target[0]})`);
    console.log("ok   view.bounds clamp zoom and pan");
  } finally {
    await browser3.close();
  }
}

// ---- 6b. legends and popups in an EPSG:3031 view (scene spec 0.5) -------------
{
  const geoField = (name, type, ext) => new Field(name, type, false, new Map([["ARROW:extension:name", ext]]));
  const xyType = () => new FixedSizeList(2, new Field("xy", new Float64(), false));
  const xyData = (coords) => makeData({ type: xyType(), length: coords.length / 2,
    child: makeData({ type: new Float64(), length: coords.length, data: Float64Array.from(coords) }) });
  // A table of a geometry column and attribute vectors, as Arrow IPC base64.
  const geoBlob = (geomField, geomData, attrs) => {
    const vecs = Object.entries(attrs).map(([k, v]) => [k, v]);
    const fields = [geomField, ...vecs.map(([k, v]) => new Field(k, v.type, true))];
    const data = makeData({ type: new Struct(fields), length: geomData.length,
      children: [geomData, ...vecs.map(([, v]) => v.data[0])] });
    return b64(new Table([new RecordBatch(new Schema(fields), data)]));
  };
  const pts = [[1e6, 1e6], [-1.5e6, 5e5], [5e5, -1.8e6]];
  const stationsBlob = geoBlob(geoField("geometry", xyType(), "geoarrow.point"), xyData(pts.flat()), {
    name: vectorFromArray(["Davis", "Mawson", "Casey"], new Utf8()),
    opened: vectorFromArray(["1957-01-13", "1954-02-13", null], new Utf8()),
    depth_m: vectorFromArray([120.5, 450, 30], new Float64()),
  });
  // One polygon: a square from (-2.4e6, -2.4e6) to (-1e6, -1e6).
  const ring = [-2.4e6, -2.4e6, -1e6, -2.4e6, -1e6, -1e6, -2.4e6, -1e6, -2.4e6, -2.4e6];
  const ringType = new List(new Field("vertices", xyType(), false));
  const polyType = new List(new Field("rings", ringType, false));
  const rings = makeData({ type: ringType, length: 1, valueOffsets: Int32Array.from([0, 5]), child: xyData(ring) });
  const polys = makeData({ type: polyType, length: 1, valueOffsets: Int32Array.from([0, 1]), child: rings });
  const zoneBlob = geoBlob(geoField("geom", polyType, "geoarrow.polygon"), polys, {
    zone: vectorFromArray(["Protected"], new Utf8()),
    area: vectorFromArray([1960000], new Int32()),
  });
  const values = b64(tableFromArrays({ value: new Float64Array([-2, 3, 8, 13]) }));
  const scene = {
    version: "0.5",
    view: { type: "projected", crs: "EPSG:3031", extent: [-3e6, 3e6, -3e6, 3e6] },
    data: {
      values: { format: "arrow-ipc-stream", blob: "values" },
      zones: { format: "arrow-ipc-stream", blob: "zones", geometry: { column: "geom", encoding: "geoarrow.polygon" } },
      stations: { format: "arrow-ipc-stream", blob: "stations", geometry: { column: "geometry", encoding: "geoarrow.point" } },
    },
    layers: [
      { id: "sst", kind: "raster", label: "SST", grid: { crs: "EPSG:3031", extent: [2e6, 3e6, 2e6, 3e6], dim: [2, 2] },
        values: "values", palette: { name: "ocean", range: [-2, 13] } },
      { id: "zones", kind: "polygon", label: "Zones", data: "zones", fill: [27, 158, 119, 160],
        popup: { columns: ["zone", "area"], trigger: "point" } },
      { id: "stations", kind: "point", label: "Stations", data: "stations", radius_px: 8,
        popup: { columns: ["name", "opened", "depth_m"] } },
      { id: "broken", kind: "point", label: "Broken popup", data: "stations", radius_px: 2,
        popup: { columns: ["name", "nope"] } },
    ],
    legends: [
      { layer: "sst", title: "SST (degrees C)", ramp: { palette: "ocean", range: [-2, 13] },
        na: { label: "no data", color: [0, 0, 0, 0] } },
      { layer: "zones", classes: [{ label: "Protected", color: [27, 158, 119, 160] }] },
      { layer: "stations", title: "Depth (m)", ramp: { range: [0, 4000],
        stops: [{ at: 0, color: [255, 255, 204, 255] }, { at: 1, color: [8, 29, 88, 255] }] } },
    ],
  };
  const blobs6 = { values, zones: zoneBlob, stations: stationsBlob };
  const browser4 = await chromium.launch({
    executablePath: process.env.CHROMIUM_PATH || undefined,
    args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  });
  try {
    const page = await browser4.newPage({ viewport: { width: 900, height: 700 } });
    await page.setContent(`<!DOCTYPE html><html><head><meta charset="utf-8"></head><body style="margin:0;height:100vh">
<div id="c" style="height:100%;width:100%"></div><script>${bundle}</script></body></html>`);
    await page.evaluate(async ([sc, bl]) => {
      const c = document.getElementById("c");
      window.h = await new Promise((resolve, reject) => {
        aob.render(c, sc, { blobs: bl, onReady: resolve }).catch(reject);
      });
    }, [scene, blobs6]);
    // Screen position of a point in view CRS units.
    const screen = (x, y) => page.evaluate(([x, y]) => {
      const r = document.querySelector(".aob-canvas").getBoundingClientRect();
      const v = window.h.view();
      const k = Math.pow(2, v.zoom);
      return [r.left + r.width / 2 + (x - v.target[0]) * k, r.top + r.height / 2 - (y - v.target[1]) * k];
    }, [x, y]);
    const popupState = () => page.evaluate(() => {
      const p = document.querySelector(".aob-popup");
      return { hidden: p.hidden, layer: p.dataset.aobLayer, trigger: p.dataset.aobTrigger,
        title: p.querySelector(".aob-popup-title").textContent,
        rows: [...p.querySelectorAll("dt")].map((dt) => [dt.textContent, dt.nextElementSibling.textContent]),
        focus: p.contains(document.activeElement), selected: document.getElementById("c").dataset.aobSelected,
        role: p.getAttribute("role") };
    });

    // Click once and wait for the popup to reach a state. Clicks are
    // spaced so two at one spot are never taken as a double click (zoom).
    const clickFor = async (x, y, test, arg) => {
      await page.waitForTimeout(350);
      await page.mouse.click(x, y);
      await page.waitForFunction(test, arg, { timeout: 5000 });
    };
    const openPopup = () => !document.querySelector(".aob-popup").hidden;
    const popupRow = (r) => !document.querySelector(".aob-popup").hidden && document.querySelector(".aob-popup").dataset.aobRow === String(r);

    // Legends, from the scene's array, in its order.
    const lg = await page.evaluate(() => [...document.querySelectorAll(".aob-legend")].map((e) => ({
      title: e.querySelector(".aob-legend-title").textContent, hidden: e.hidden, label: e.getAttribute("aria-label"),
      bar: e.querySelector(".aob-legend-bar") && e.querySelector(".aob-legend-bar").getAttribute("aria-label"),
      scale: [...e.querySelectorAll(".aob-legend-scale span")].map((s) => s.textContent),
      classes: [...e.querySelectorAll(".aob-legend-class")].map((li) => li.textContent),
      na: [...e.querySelectorAll(".aob-legend-na")].map((li) => li.textContent) })));
    assert.deepEqual(lg.map((l) => l.title), ["SST (degrees C)", "Zones", "Depth (m)"], "title falls back to the layer label");
    assert.equal(lg[0].bar, "Colour ramp from -2 to 13");
    assert.deepEqual(lg[0].scale, ["-2", "13"]);
    assert.deepEqual(lg[0].na, ["no data"]);
    assert.deepEqual(lg[1].classes, ["Protected"]);
    assert.deepEqual(lg[2].scale, ["0", "4000"]);
    assert.equal(lg[2].label, "Legend: Depth (m)");
    const bg = await page.evaluate(() => document.querySelectorAll(".aob-legend-bar")[1].style.background);
    assert.match(bg, /rgb\(255, 255, 204\) 0%.*rgb\(8, 29, 88\) 100%/);
    console.log("ok   0.5 legends: palette and stops ramps, classes, no-data entry");

    // A missing popup column is an error for that layer; the layer draws.
    const st = await page.evaluate(() => ({ errors: document.getElementById("c").dataset.aobErrors,
      line: document.querySelector(".aob-status").textContent,
      counts: [...document.querySelectorAll(".aob-count")].map((e) => e.textContent) }));
    assert.equal(st.errors, "1");
    assert.match(st.line, /error: layer broken: popup column "nope" not found in data stations; popup not shown/);
    assert.deepEqual(st.counts, ["3 points", "3 points", "1 polygon", "2 x 2 cells"]);
    console.log("ok   a missing popup column is a layer error");

    // Select a station in the EPSG:3031 view: its popup shows its attributes.
    const [x0, y0] = await screen(...pts[0]);
    await clickFor(x0, y0, popupRow, 0);
    let p = await popupState();
    assert.equal(p.layer, "stations");
    assert.equal(p.title, "Stations");
    assert.equal(p.role, "dialog");
    assert.deepEqual(p.rows, [["name", "Davis"], ["opened", "1957-01-13"], ["depth_m", "120.5"]]);
    assert.equal(p.selected, "stations:0");
    assert.ok(p.focus, "the popup takes focus");
    assert.deepEqual(await page.evaluate(() => window.h.selected()), { layer: "stations", row: 0 });
    // Another selection replaces it; a missing value reads NA.
    const [x2, y2] = await screen(...pts[2]);
    await clickFor(x2, y2, popupRow, 2);
    p = await popupState();
    assert.deepEqual(p.rows, [["name", "Casey"], ["opened", "NA"], ["depth_m", "30"]]);
    // Escape closes it.
    await page.keyboard.press("Escape");
    p = await popupState();
    assert.ok(p.hidden, "Escape closes the popup");
    assert.equal(p.selected, undefined);
    // The close button works from the keyboard.
    await clickFor(x0, y0, popupRow, 0);
    await page.keyboard.press("Tab");
    assert.equal(await page.evaluate(() => document.activeElement.getAttribute("aria-label")), "Close");
    await page.keyboard.press("Enter");
    assert.ok((await popupState()).hidden, "the close button closes the popup");
    // A click on empty map closes a popup too.
    await clickFor(x0, y0, popupRow, 0);
    const [xe, ye] = await screen(0, 2.5e6);
    await clickFor(xe, ye, () => document.querySelector(".aob-popup").hidden);
    console.log("ok   0.5 popup: select a feature in an EPSG:3031 view and see its attributes; keyboard close");

    // trigger "point": shown while the pointer is over the polygon.
    const hover = await page.evaluate(() => matchMedia("(hover: hover)").matches);
    const [xz, yz] = await screen(-1.7e6, -1.7e6);
    if (hover) {
      await page.mouse.move(xz, yz);
      await page.waitForFunction(() => !document.querySelector(".aob-popup").hidden, null, { timeout: 5000 });
      p = await popupState();
      assert.equal(p.trigger, "point");
      assert.equal(p.role, "tooltip", "a pointed-at feature's popup is a tooltip");
      assert.deepEqual(p.rows, [["zone", "Protected"], ["area", "1960000"]]);
      assert.ok(!p.focus, "a point popup does not take focus");
      await page.mouse.move(xe, ye);
      await page.waitForFunction(() => document.querySelector(".aob-popup").hidden, null, { timeout: 5000 });
      console.log("ok   0.5 popup: trigger point follows the pointer");
    } else {
      await clickFor(xz, yz, openPopup);
      assert.equal((await popupState()).trigger, "select");
      console.log("ok   0.5 popup: trigger point acts as select with no hover");
    }

    // A click selects whatever the length of the press or the pick (#26):
    // deck.gl's tap is dropped when its press lasts over its time limit,
    // and its pointerdown pick can take seconds with software rendering.
    const closed = () => document.querySelector(".aob-popup").hidden && !document.getElementById("c").dataset.aobSelected;
    await page.mouse.move(xe, ye);
    await page.waitForTimeout(350);
    await page.mouse.move(x0, y0);
    await page.mouse.down();
    await page.waitForTimeout(1500);
    await page.mouse.up();
    await page.waitForFunction(popupRow, 0, { timeout: 5000 });
    await page.keyboard.press("Escape");
    // Every pick (its draw and read-back) now takes 1.5 s. The press
    // itself does not pick.
    await page.evaluate(() => {
      const picker = window.h.deck.deckPicker;
      window.slowPicks = 0;
      window.fastPick = picker._drawAndSample;
      picker._drawAndSample = function (...args) {
        window.slowPicks++;
        const t = performance.now();
        while (performance.now() - t < 1500);
        return window.fastPick.apply(this, args);
      };
    });
    await page.waitForTimeout(350);
    await page.mouse.move(x2, y2);
    await page.waitForTimeout(2000); // let a hover pick finish
    const before = await page.evaluate(() => window.slowPicks);
    const t0 = Date.now();
    await page.mouse.down();
    const downMs = Date.now() - t0;
    assert.equal(await page.evaluate(() => window.slowPicks), before, "a press does not pick");
    assert.ok(downMs < 1000, `a press is not blocked by a pick (${downMs} ms)`);
    await page.waitForTimeout(1200);
    await page.mouse.up();
    await page.waitForFunction(popupRow, 2, { timeout: 10000 });
    assert.equal((await popupState()).trigger, "select");
    await page.keyboard.press("Escape");
    await page.evaluate(() => { window.h.deck.deckPicker._drawAndSample = window.fastPick; });
    // A drag that starts on a feature pans and selects nothing, even one
    // that comes back to where it started.
    const v0 = await page.evaluate(() => window.h.view());
    // Put the camera back. handle.setView() only moves deck's camera when
    // the view has bounds, so set deck's initial view state as well.
    const resetView = (v) => page.evaluate((v) => {
      window.h.deck.setProps({ initialViewState: { ...v, resetKey: Math.random() } });
      window.h.setView(v);
    }, v);
    await page.waitForTimeout(350);
    await page.mouse.move(x0, y0);
    await page.mouse.down();
    await page.mouse.move(x0 + 60, y0 + 30, { steps: 6 });
    await page.mouse.up();
    await page.waitForTimeout(800);
    assert.ok(await page.evaluate(closed), "a drag does not select");
    const v1 = await page.evaluate(() => window.h.view());
    assert.ok(Math.abs(v1.target[0] - v0.target[0]) > 1, "a drag pans");
    await resetView(v0);
    await page.waitForTimeout(350);
    await page.mouse.move(x0, y0);
    await page.mouse.down();
    await page.mouse.move(x0 + 40, y0, { steps: 4 });
    await page.mouse.move(x0, y0, { steps: 4 });
    await page.mouse.up();
    await page.waitForTimeout(800);
    assert.ok(await page.evaluate(closed), "a drag back to its start does not select");
    await resetView(v0);
    // A double click on a feature zooms in and does not select it.
    await page.waitForTimeout(350);
    await page.mouse.dblclick(x0, y0);
    await page.waitForFunction((z) => window.h.view().zoom > z + 0.5, v0.zoom, { timeout: 5000 });
    await page.waitForTimeout(800);
    assert.ok(await page.evaluate(closed), "a double click does not select");
    await resetView(v0);
    console.log("ok   0.5 popup: a long press or a slow pick still selects; a drag or double click does not");

    // Hiding a layer hides its legend and closes its popup.
    await page.mouse.move(xe, ye);
    await clickFor(x0, y0, popupRow, 0);
    await page.evaluate(() => {
      const lab = [...document.querySelectorAll(".aob-layer")].find((l) => l.textContent.startsWith("Stations"));
      lab.querySelector("input").click();
    });
    const after = await page.evaluate(() => ({ popup: document.querySelector(".aob-popup").hidden,
      legend: document.querySelector('[data-aob-legend="stations"]').hidden }));
    assert.deepEqual(after, { popup: true, legend: true });
    console.log("ok   0.5 hiding a layer hides its legend and popup");
  } finally {
    await browser4.close();
  }
}

// ---- 7. served pages: blobs fetched from a blob base --------------------------
{
  const { createServer } = await import("node:http");
  const tiff = readFileSync(join(here, "..", "..", "inst", "extdata", "polar_3031.tif"));
  const tiled = JSON.parse(readFileSync(join(here, "tiled-scene.json"), "utf8"));
  const bytesOf = (b64) => Buffer.from(b64, "base64");
  const odd = "v/a@b+c d";
  const values = tableToIPC(tableFromArrays({ value: new Float64Array([0, 1, 2, 3]) }), "stream");
  const rasterScene = {
    version: "0.1",
    view: { type: "cartesian", extent: [0, 1, 0, 1] },
    data: { values: { format: "arrow-ipc-stream", blob: odd } },
    layers: [{ id: "good", kind: "raster", grid: { crs: "EPSG:3031", extent: [0, 1, 0, 1], dim: [2, 2] },
      values: "values", palette: { name: "viridis", range: [0, 3] } }],
  };
  // Every planned tile of the tiled fixture, as the server's blobs.
  const tileBlobs = {};
  for (const lv of tiled.scene.layers[0].plan.levels) {
    for (const t of lv.tiles) {
      tileBlobs[`sst@${t.byte_offset}+${t.byte_length}`] = tiff.subarray(t.byte_offset, t.byte_offset + t.byte_length);
    }
  }
  const meshBlobs = Object.fromEntries(Object.entries(tiled.blobs).map(([k, v]) => [k, bytesOf(v)]));
  // A linked page as aobcore's page builder writes it: no blob scripts, a
  // blob base on the element and the served keys in a JSON script.
  const linked = (sc, keys, base = "blob/") => `<!DOCTYPE html><html><head><meta charset="utf-8"></head><body style="margin:0;height:100vh">
<div class="aob-page" data-aob-scene="aob-scene"${base === null ? "" : ` data-aob-blob-base="${base}"`} style="height:100%"></div>
<script type="application/json" id="aob-scene">${JSON.stringify(sc)}</script>
${keys === null ? "" : `<script type="application/json" data-aob-blob-keys data-aob-scene="aob-scene">${JSON.stringify(keys)}</script>`}
<script src="aob-renderer.min.js"></script></body></html>`;
  const sites = {
    // name: [page, blobs served under <name>/blob/]
    raster: [linked(rasterScene, [odd]), { [odd]: values }],
    tiles: [linked(tiled.scene, [...Object.keys(meshBlobs), ...Object.keys(tileBlobs)]), { ...meshBlobs, ...tileBlobs }],
    ranged: [linked(tiled.scene, Object.keys(meshBlobs)), meshBlobs],
    nobase: [linked(rasterScene, null, null), {}],
    gone: [linked(rasterScene, []), {}],
    // Listed tile blobs the server does not have, or has with the wrong length.
    tilegone: [linked(tiled.scene, [...Object.keys(meshBlobs), ...Object.keys(tileBlobs)]), meshBlobs],
    tileshort: [linked(tiled.scene, [...Object.keys(meshBlobs), ...Object.keys(tileBlobs)]),
      { ...meshBlobs, ...Object.fromEntries(Object.entries(tileBlobs).map(([k, v]) => [k, v.subarray(0, v.length - 1)])) }],
    // A blob base no server answers: a network failure, not a status.
    netfail: [linked(rasterScene, [odd], "http://127.0.0.1:1/blob/"), {}],
  };
  let slowAborted = null;
  const seen = { blob: [], range: [], whole: 0, other: [] };
  const server = createServer((req, res) => {
    const m = /^\/([a-z]+)\/(.*)$/.exec(req.url);
    const site = m && (sites[m[1]] || (m[1] === "slow" ? [] : null));
    if (!site) {
      res.writeHead(404);
      return res.end();
    }
    const rest = m && m[2];
    if (m && m[1] === "slow") {
      // Answers after 3 s, unless the request is aborted first.
      slowAborted = false;
      const timer = setTimeout(() => {
        res.writeHead(200, { "content-type": "application/vnd.apache.arrow.stream" });
        res.end(values);
      }, 3000);
      res.on("close", () => {
        if (!res.writableEnded) {
          slowAborted = true;
          clearTimeout(timer);
        }
      });
      return;
    }
    if (rest === "") {
      res.writeHead(200, { "content-type": "text/html" });
      return res.end(site[0]);
    }
    if (rest === "aob-renderer.min.js") {
      res.writeHead(200, { "content-type": "text/javascript" });
      return res.end(bundle);
    }
    if (rest.startsWith("blob/")) {
      // One path segment, decoded once, matched exactly.
      const seg = rest.slice(5);
      const key = seg.includes("/") ? null : decodeURIComponent(seg);
      seen.blob.push(`${m[1]}:${seg}`);
      if (key === null || !(key in site[1])) {
        res.writeHead(404);
        return res.end();
      }
      res.writeHead(200, { "content-type": "application/vnd.apache.arrow.stream" });
      return res.end(site[1][key]);
    }
    if (rest === "polar_3031.tif") {
      const r = /^bytes=(\d+)-(\d+)$/.exec(req.headers.range || "");
      if (!r) {
        seen.whole++;
        res.writeHead(200, { "content-type": "image/tiff" });
        return res.end(tiff);
      }
      seen.range.push(`${m[1]}:${req.headers.range}`);
      const a = Number(r[1]);
      const b = Number(r[2]);
      res.writeHead(206, { "content-type": "image/tiff", "content-range": `bytes ${a}-${b}/${tiff.length}` });
      return res.end(tiff.subarray(a, b + 1));
    }
    seen.other.push(req.url);
    res.writeHead(404);
    res.end();
  });
  await new Promise((r) => server.listen(0, "127.0.0.1", r));
  const base = `http://127.0.0.1:${server.address().port}`;
  const browser7 = await chromium.launch({
    executablePath: process.env.CHROMIUM_PATH || undefined,
    args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  });
  try {
    const state = async (path) => {
      const page = await browser7.newPage({ viewport: { width: 900, height: 700 } });
      await page.goto(base + path);
      await page.waitForFunction(() => {
        const s = document.querySelector("div[data-aob-scene]").dataset.aobStatus;
        return s === "ready" || s === "error";
      }, null, { timeout: 60000 });
      const r = await page.evaluate(() => {
        const c = document.querySelector("div[data-aob-scene]");
        const p = aob.fromPage("aob-scene");
        return { status: c.dataset.aobStatus, errors: c.dataset.aobErrors, tiles: c.dataset.aobTiles,
          pending: c.dataset.aobPending, line: c.querySelector(".aob-status").textContent,
          blobBase: p.blobBase, blobKeys: p.blobKeys, nBlobs: Object.keys(p.blobs).length };
      });
      await page.close();
      return r;
    };

    const enc = encodeURIComponent(odd);
    assert.equal(enc, "v%2Fa%40b%2Bc%20d");
    const r = await state("/raster/");
    assert.equal(r.status, "ready");
    assert.equal(r.errors, undefined, r.line);
    assert.equal(r.blobBase, "blob/");
    assert.deepEqual(r.blobKeys, [odd]);
    assert.equal(r.nBlobs, 0);
    assert.deepEqual(seen.blob, [`raster:${enc}`], "the key is one encoded path segment");
    console.log(`ok   served page: a blob is fetched from the blob base (key ${JSON.stringify(odd)} as ${enc})`);

    const t = await state("/tiles/");
    assert.equal(t.status, "ready");
    assert.equal(t.errors, undefined, t.line);
    assert.equal(t.pending, "0");
    assert.match(t.tiles, /^sst: level [23], \d+ tiles?$/);
    const tileFetches = seen.blob.filter((b) => b.startsWith("tiles:sst%40"));
    assert.ok(tileFetches.length > 0, "tiles were fetched from the blob base");
    for (const b of tileFetches) assert.ok(decodeURIComponent(b.slice(6)) in tileBlobs, `${b} is a planned tile`);
    assert.equal(seen.range.filter((x) => x.startsWith("tiles:")).length, 0, "no range requests");
    assert.equal(seen.whole, 0);
    console.log(`ok   served page: listed tile blobs come from the blob base (${t.tiles}; ${tileFetches.length} tiles)`);

    const g = await state("/ranged/");
    assert.equal(g.status, "ready");
    assert.equal(g.errors, undefined, g.line);
    assert.equal(g.pending, "0");
    assert.ok(seen.range.filter((x) => x.startsWith("ranged:")).length > 0, "unlisted tiles are read by range");
    assert.equal(seen.blob.filter((b) => b.startsWith("ranged:sst%40")).length, 0, "no tile blob fetched");
    assert.equal(seen.whole, 0);
    console.log("ok   served page: unlisted tiles are read by range requests as before");

    const n = await state("/nobase/");
    assert.equal(n.status, "error");
    assert.equal(n.blobBase, undefined);
    assert.equal(n.blobKeys, undefined);
    assert.match(n.line, /data values: blob "v\/a@b\+c d" was not delivered$/);
    assert.equal(seen.blob.filter((b) => b.startsWith("nobase:")).length, 0, "nothing fetched without a base");
    console.log("ok   a missing blob with no blob base is an error as before");

    const x = await state("/gone/");
    assert.equal(x.status, "error");
    assert.match(x.line, /data values: blob "v\/a@b\+c d" was not delivered \(blob\/v%2Fa%40b%2Bc%20d returned 404\)/);
    console.log("ok   a blob the server does not have is an error naming the 404");

    for (const [site, msg] of [["tilegone", /blob "sst@\d+\+\d+" was not delivered \(blob\/sst%40\d+%2B\d+ returned 404\)/],
      ["tileshort", /got (\d+) bytes, expected \d+/]]) {
      const tg = await state(`/${site}/`);
      assert.equal(tg.status, "ready", `${site}: the rest of the scene still draws`);
      assert.equal(tg.errors, "1", tg.line);
      assert.match(tg.line, new RegExp(`error: layer sst: tile \\d+/\\d+/\\d+: .*${msg.source}`));
      assert.equal(seen.range.filter((x) => x.startsWith(`${site}:`)).length, 0, `${site}: no range fallback`);
    }
    console.log("ok   a listed tile blob that is missing (404) or the wrong length is a layer error");

    const nf = await state("/netfail/");
    assert.equal(nf.status, "error");
    assert.match(nf.line, /data values: blob "v\/a@b\+c d" could not be fetched from http:\/\/127\.0\.0\.1:1\/blob\/v%2Fa%40b%2Bc%20d: /);
    console.log("ok   a network failure fetching a blob is an error naming the key and URL");

    // Rendering again into the container aborts the first render's fetches,
    // and the first render leaves the container to the second.
    const page = await browser7.newPage({ viewport: { width: 900, height: 700 } });
    await page.goto(base + "/raster/");
    await page.waitForFunction(() => document.querySelector("div[data-aob-scene]").dataset.aobStatus === "ready", null, { timeout: 60000 });
    const ab = await page.evaluate(async (sc) => {
      const c = document.querySelector("div[data-aob-scene]");
      const first = aob.render(c, sc, { blobBase: "/slow/blob/" }).then(() => "resolved", (e) => e.name);
      await new Promise((r) => setTimeout(r, 300));
      const second = await aob.render(c, sc, { blobBase: "blob/" });
      const r1 = await first;
      await new Promise((r) => setTimeout(r, 300));
      return { r1, status: c.dataset.aobStatus, errors: second.errors.length, line: c.querySelector(".aob-status").textContent };
    }, rasterScene);
    await page.close();
    assert.equal(ab.r1, "AbortError");
    assert.equal(ab.errors, 0);
    assert.equal(ab.line, "");
    assert.equal(slowAborted, true, "the slow blob request was aborted");
    console.log("ok   rendering again aborts the first render's blob fetches");
    assert.deepEqual(seen.other, []);
  } finally {
    await browser7.close();
    server.close();
  }
}
