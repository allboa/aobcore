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
// Also in Node: the range reader keeps a whole-file (200) response, and the
// tile cache evicts least recently used idle tiles.
// Set CHROMIUM_PATH to pick a browser; SKIP_BROWSER=1 runs only part 1.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { tableToIPC, tableFromArrays } from "apache-arrow";
import { paletteStops, colorize, UnknownPaletteError } from "../src/palettes.js";
import { decodeTile, UnsupportedCodecError } from "../src/decode.js";
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
for (const codec of ["lerc", "lerc_deflate", "lerc_zstd", "webp", "jpeg"]) {
  assert.throws(() => decodeTile(new Uint8Array(4), { codec, dtype: "uint8" }, 1, 1), UnsupportedCodecError, codec);
}
console.log("ok   unsupported codecs throw");
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
  const fake = (ignore) => async (url, opts) => {
    const [a, b] = /bytes=(\d+)-(\d+)/.exec(opts.headers.Range).slice(1).map(Number);
    await new Promise((r) => setTimeout(r, 5));
    const part = ignore ? file : file.slice(a, b + 1);
    return {
      status: ignore ? 200 : 206, ok: true,
      body: { cancel: async () => { cancelled++; } },
      arrayBuffer: async () => { bodies++; return part.slice().buffer; },
    };
  };
  const partial = rangeReader(fake(false));
  assert.deepEqual([...await partial("u", 10, 3)], [10, 11, 12]);
  bodies = 0;
  const whole = rangeReader(fake(true));
  const got = await Promise.all([whole("u", 0, 2), whole("u", 50, 2), whole("u", 98, 2)]);
  assert.deepEqual(got.map((x) => [...x]), [[0, 1], [50, 51], [98, 99]]);
  assert.deepEqual([...await whole("u", 20, 1)], [20]);
  assert.equal(bodies, 1, "the whole file is read once");
  assert.equal(cancelled, 2, "the other whole-file bodies are cancelled");
  await assert.rejects(whole("u", 99, 5), /has 100 bytes/);
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
const tiled = JSON.parse(readFileSync(join(here, "tiled-scene.json"), "utf8"));
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
  "/norange/tiled.html": pageFor(tiled.scene, tiled.blobs),
  "/bad.html": pageFor(bad, { ...tiled.blobs, values: blobs.values }),
};
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
  assert.match(nr.tiles, /^sst: level [23], \d+ tiles?$/);
  assert.ok(wholeFile >= 1, "the file was fetched");
  const nTiles = Number(/(\d+) tiles?$/.exec(nr.tiles)[1]);
  // Requests sent together each get a 200; the reader keeps one body and
  // cancels the rest (the Node test above checks it reads one body).
  assert.ok(wholeFile <= nTiles, `whole file sent ${wholeFile} times for ${nTiles} tiles`);
  console.log(`ok   a server that ignores Range sends the file ${wholeFile} time(s) for ${nTiles} tile(s)`);

  const b = await state("/bad.html");
  assert.equal(b.status, "ready", "the rest of the scene still draws");
  assert.equal(b.errors, "1");
  assert.match(b.line, /error: layer sst: level \d: codec lerc is not supported by this renderer/);
  assert.deepEqual(b.counts, ["2 x 2 cells", "error: codec lerc is not supported by this renderer"]);
  console.log("ok   unsupported codec is a layer error");
} finally {
  await browser2.close();
  server.close();
}
