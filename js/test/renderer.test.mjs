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
// Also in Node: the range reader keeps a whole-file (200) response, and the
// tile cache evicts least recently used idle tiles.
// Set CHROMIUM_PATH to pick a browser; SKIP_BROWSER=1 runs only part 1.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { tableToIPC, tableFromArrays } from "apache-arrow";
import { paletteStops, colorize, UnknownPaletteError } from "../src/palettes.js";
import { decodeTile, decodeSamples, jpegStream, samplesFromRGBA, colorizeRGB, encodingProblem, UnsupportedCodecError } from "../src/decode.js";
import { selectLevel, rangeReader, tilesToEvict } from "../src/tiles.js";

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
