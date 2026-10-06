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
//    does not select. In Node, cell text and legend helpers. The same
//    scene with view.bounds: leaving the canvas clears the hover highlight,
//    a double click zooms in one level about the pointer, a slow double
//    click is two clicks and the third click of a triple click selects, and
//    a press just before finalize() leaves the finalized deck alone.
// 7. Served pages (decision 0006) in the browser: a page whose element
//    names a blob base fetches a blob it does not carry from the base plus
//    encodeURIComponent(key) (a key with "/", "@" and "+" round-trips),
//    a tiled raster takes listed tile blobs from the base and nothing by
//    range, an unlisted tile is read by range as before, and a missing blob
//    with no base is still an error (with a base, one that names the 404,
//    and a network failure names the key and URL); rendering again into
//    the same element aborts the first render's blob fetches. A listed tile
//    blob that answers 404 or has the wrong length is a layer error.
// 8. Selections over a websocket (decision 0007): in Node, the socket
//    channel's backoff and final close codes, protocol 1 from the page's
//    side (hello first, whole selections, settled views, the size cap,
//    the not-connected note, reload) and the click rules; in the browser,
//    a served page with data-aob-socket against an R stand-in
//    (test/ws-server.mjs) in an EPSG:3031 view: click, Shift and Cmd
//    toggles, clearing, a pan's one view, reload keeping the camera, the
//    note and reconnecting, another origin refused, and a page without
//    data-aob-socket opening no socket.
// 9. Fragments (decision 0009): several scenes in one host document with
//    one renderer, each fragment's theme its own.
// 10. A host's channel (decision 0009): protocol 1 over a channel the page
//    makes (as a Shiny binding does), with no socket; the channel gets
//    message objects, and a reload over it is ignored.
// 11. Escape in a host page with two views clears only the focused one.
// 12. Scene spec's explicit-data contract (scenespec#11), with scenespec's
//    fixtures (test/fixtures/scenespec, see its README): in Node, the CRS
//    match rule and the geometry checks on every fixture; in the browser,
//    the six valid streams (interleaved and separated coordinates, a
//    PROJJSON and an authority-code CRS) draw with their feature counts,
//    and each scene of fixtures/data/invalid is a layer error.
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
import { socketChannel, socketUrl, RETRY_MAX_MS, STABLE_MS } from "../src/channel.js";
import { linkToR, utf8Length, NOT_CONNECTED, TOO_MANY } from "../src/link.js";
import { selectionState, clickSelection, selectionText } from "../src/selection.js";
import { rStandIn } from "./ws-server.mjs";
import { geometryProblem, sameCrs, readTable } from "../src/arrow.js";
import { readdirSync } from "node:fs";

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

// ---- 12a. explicit-data contract in Node -----------------------------------
// Scenespec's fixtures as scenes whose url references are blobs (the
// renderer is given blobs, as a page carries them), drawn as 0.5: they use
// nothing from 0.6 (chunk references), which this renderer does not read.
const specDir = join(here, "fixtures", "scenespec");
function specScene(file) {
  const scene = JSON.parse(readFileSync(file, "utf8"));
  const blobs = {};
  for (const [id, ref] of Object.entries(scene.data)) {
    if (ref.url === undefined) continue;
    blobs[id] = readFileSync(join(dirname(file), ref.url)).toString("base64");
    delete ref.url;
    ref.blob = id;
  }
  scene.version = "0.5";
  return { scene, blobs };
}
const validSpec = specScene(join(specDir, "valid", "explicit-data-0.6.json"));
const invalidDir = join(specDir, "data", "invalid");
const invalidSpec = readdirSync(invalidDir).filter((f) => f.endsWith(".json")).sort()
  .map((f) => ({ name: f.replace(/\.json$/, ""), ...specScene(join(invalidDir, f)) }));
{
  const epsg3031 = { type: "ProjectedCRS", name: "WGS 84 / Antarctic Polar Stereographic", id: { authority: "EPSG", code: 3031 } };
  assert.ok(sameCrs("EPSG:3031", "epsg:3031"), "authority codes compare case-insensitively");
  assert.ok(sameCrs(epsg3031, "EPSG:3031") && sameCrs("EPSG:3031", epsg3031), "a PROJJSON id names its CRS");
  assert.ok(sameCrs({ ...epsg3031, ids: [{ authority: "ESRI", code: 1 }, { authority: "epsg", code: "3031" }] }, "EPSG:3031"));
  assert.ok(sameCrs({ $schema: "a", name: "x", type: "ProjectedCRS" }, { type: "ProjectedCRS", name: "x" }), "key order and $schema aside");
  assert.ok(!sameCrs("EPSG:3031", "EPSG:4326"));
  assert.ok(!sameCrs({ name: "x" }, { name: "y" }));
  assert.ok(!sameCrs("+proj=stere", "EPSG:3031"));
  const geomOf = (s, id) => {
    const ref = s.scene.data[id];
    const t = readTable(Buffer.from(s.blobs[id], "base64"));
    return [t.schema.fields.find((f) => f.name === ref.geometry.column), ref, s.scene.view];
  };
  for (const id of Object.keys(validSpec.scene.data)) assert.equal(geometryProblem(...geomOf(validSpec, id)), null, id);
  // A cartesian view without a CRS needs none in the data; one with a CRS does.
  const [f, ref] = geomOf(invalidSpec.find((s) => s.name === "no-crs"), "layer");
  assert.equal(geometryProblem(f, ref, { type: "cartesian" }), null);
  assert.match(geometryProblem(f, ref, { type: "cartesian", crs: "EPSG:3031" }), /no crs/);
  const geometryFaults = invalidSpec.filter((s) => geometryProblem(...geomOf(s, "layer")) !== null).map((s) => s.name);
  assert.deepEqual(geometryFaults, ["crs-not-view", "crs-type-mismatch", "encoding-not-declared", "geometrycollection",
    "no-crs", "storage-mismatch", "wkb", "xym"], "the geometry faults; the rest are faults of named columns");
  console.log("ok   explicit-data contract: CRS match rule, and the geometry of every scenespec fixture");
}

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

// ---- 8a. the link to R (decision 0007) in Node -------------------------------
{
  // Timers that run only when told to.
  const fakeTimers = () => {
    let t = 0;
    let id = 0;
    const q = new Map();
    return {
      setTimeout: (f, ms) => {
        q.set(++id, { f, at: t + ms, ms });
        return id;
      },
      clearTimeout: (i) => q.delete(i),
      now: () => t,
      delays: () => [...q.values()].map((x) => x.ms),
      advance(ms) {
        const end = t + ms;
        for (;;) {
          const due = [...q.entries()].filter(([, x]) => x.at <= end).sort((a, b) => a[1].at - b[1].at)[0];
          if (!due) break;
          q.delete(due[0]);
          t = due[1].at;
          due[1].f();
        }
        t = end;
      },
    };
  };

  assert.equal(socketUrl("ws", "http://127.0.0.1:8123/abc/"), "ws://127.0.0.1:8123/abc/ws");
  assert.equal(socketUrl("ws", "https://proxy.example/p/8123/abc/index.html"), "wss://proxy.example/p/8123/abc/ws");

  // The channel retries with backoff, 1 s doubling to 30 s, reset by an
  // open; a close that says retrying cannot help is final.
  {
    const made = [];
    class FakeWS {
      constructor(url) {
        this.url = url;
        this.sent = [];
        made.push(this);
      }
      send(t) {
        this.sent.push(t);
      }
      close() {}
    }
    const timers = fakeTimers();
    const states = [];
    const ch = socketChannel("ws://h/t/ws", { WebSocket: FakeWS, timers, onState: (s, i) => states.push([s, i.delay || i.code || null]) });
    assert.equal(made.length, 1);
    assert.equal(ch.send({ type: "x" }), false, "nothing is sent before the socket opens");
    const delays = [];
    for (let i = 0; i < 7; i++) {
      made[made.length - 1].onclose({ code: 1006 });
      delays.push(timers.delays()[0]);
      timers.advance(delays[delays.length - 1]);
    }
    assert.deepEqual(delays, [1000, 2000, 4000, 8000, 16000, RETRY_MAX_MS, RETRY_MAX_MS]);
    assert.equal(made.length, 8);
    const got = [];
    ch.onMessage((m) => got.push(m));
    const w = made[made.length - 1];
    w.onopen();
    assert.ok(ch.connected);
    assert.equal(ch.send({ type: "hello" }), true);
    assert.deepEqual(JSON.parse(w.sent[0]), { type: "hello" });
    w.onmessage({ data: "not json" });
    w.onmessage({ data: "[1]" });
    w.onmessage({ data: JSON.stringify({ type: "reload", scene: 2 }) });
    assert.deepEqual(got, [{ type: "reload", scene: 2 }], "only objects with a type reach listeners");
    // An open alone does not shorten the delay: R may accept and close at
    // once (1013 too many pages, 1011 an error after hello).
    w.onclose({ code: 1001 });
    assert.deepEqual(timers.delays(), [RETRY_MAX_MS], "an open alone does not reset the backoff");
    ch.close();
    assert.deepEqual(timers.delays(), []);
    const fresh = () => {
      const m0 = made.length;
      const c = socketChannel("ws://h/t/ws", { WebSocket: FakeWS, timers, onState: () => {} });
      return [c, () => made[made.length - 1], m0];
    };
    {
      const [c, cur] = fresh();
      for (const code of [1013, 1011]) {
        const seen = [];
        for (let i = 0; i < 7; i++) {
          cur().onopen();
          if (code === 1011) {
            c.resetBackoff(); // R said hello, then closed within STABLE_MS
            timers.advance(STABLE_MS - 1);
          }
          cur().onclose({ code });
          seen.push(timers.delays().filter((d) => d !== STABLE_MS)[0]);
          timers.advance(seen[seen.length - 1]);
        }
        if (code === 1013) assert.deepEqual(seen, [1000, 2000, 4000, 8000, 16000, RETRY_MAX_MS, RETRY_MAX_MS], "1013 backs off");
        else assert.deepEqual(seen, [RETRY_MAX_MS, RETRY_MAX_MS, RETRY_MAX_MS, RETRY_MAX_MS, RETRY_MAX_MS, RETRY_MAX_MS, RETRY_MAX_MS], "1011 after hello keeps backing off");
      }
      // Open, R's hello, and STABLE_MS open: the next retry is 1 s again.
      cur().onopen();
      c.resetBackoff();
      timers.advance(STABLE_MS);
      cur().onclose({ code: 1001 });
      assert.deepEqual(timers.delays(), [1000]);
      c.close();
    }
    {
      const [c, cur] = fresh();
      const seen = [];
      for (let i = 0; i < 4; i++) {
        cur().onopen();
        c.resetBackoff();
        timers.advance(100);
        cur().onclose({ code: 1011 });
        seen.push(timers.delays()[0]);
        timers.advance(seen[seen.length - 1]);
      }
      assert.deepEqual(seen, [1000, 2000, 4000, 8000], "1011 right after hello backs off");
      c.close();
    }
    socketChannel("ws://h/t/ws", { WebSocket: FakeWS, timers, onState: (s, i) => states.push([s, i.delay || i.code || null]) });
    for (const code of [1003, 1007, 1008, 4000]) {
      const before = made.length;
      made[made.length - 1].onclose({ code, reason: "r" });
      assert.deepEqual(timers.delays(), [], `no retry after ${code}`);
      assert.deepEqual(states[states.length - 1], ["refused", code]);
      // Start again for the next code.
      if (code !== 4000) {
        socketChannel("ws://h/t/ws", { WebSocket: FakeWS, timers, onState: (s, i) => states.push([s, i.delay || i.code || null]) });
        assert.equal(made.length, before + 1);
      }
    }
    console.log("ok   socket channel: backoff 1 s doubling to 30 s, reset only after R's hello and 5 s open; 1003, 1007, 1008 and 4000 are final");
  }

  // Protocol 1 from the page's side.
  {
    const timers = fakeTimers();
    const sent = [];
    let listener = null;
    // The channel is always handed message objects, never JSON text.
    const channel = { send: (m) => { assert.equal(typeof m, "object", "channel.send() gets an object"); sent.push(JSON.parse(JSON.stringify(m))); return true; },
      onMessage: (f) => { listener = f; return () => { listener = null; }; }, close() {} };
    const sel = selectionState();
    const page = { serial: 3, renderer: "0.0.5", specs: ["0.1", "0.5"], selectable: null, reloaded: null, notes: [], states: [],
      selection: () => sel.items(), view: () => ({ extent: [0, 1, 0, 1], zoom: 0, units_per_pixel: 1, size_px: [10, 10] }),
      setSelectable: (ids) => { page.selectable = ids; }, reload: (n) => { page.reloaded = n; },
      note: (t) => page.notes.push(t), state: (s) => page.states.push(s) };
    const link = linkToR(channel, page, { timers, now: timers.now });
    link.open();
    assert.deepEqual(sent.shift(), { type: "hello", protocol: 1, renderer: "0.0.5", specs: ["0.1", "0.5"], scene: 3 });
    // Nothing but hello before R's hello.
    link.viewChanged();
    assert.equal(link.selected("click", [1, 2]), false);
    timers.advance(1000);
    assert.deepEqual(sent, []);
    listener({ type: "hello", protocol: 1, connection: 1, scene: 3, spec: "0.5", select: ["a", 7, "b"], max_message: 300 });
    assert.deepEqual(page.selectable, ["a", "b"]);
    assert.equal(page.states[page.states.length - 1], "ready");
    // R's hello is answered with the camera, and no select: nothing selected.
    assert.deepEqual(sent.map((m) => m.type), ["view"]);
    assert.deepEqual(sent.shift(), { type: "view", scene: 3, seq: 1, extent: [0, 1, 0, 1], zoom: 0, units_per_pixel: 1, size_px: [10, 10] });
    clickSelection(sel, { layer: "a", row: 4 }, false);
    link.selected("click", [5, 6]);
    assert.deepEqual(sent.shift(), { type: "select", scene: 3, seq: 2, trigger: "click", items: [{ layer: "a", rows: [4] }], at: [5, 6] });
    // The camera settles: one view 250 ms after the last change.
    for (let i = 0; i < 5; i++) {
      link.viewChanged();
      timers.advance(100);
    }
    assert.deepEqual(sent, []);
    timers.advance(150);
    assert.deepEqual(sent.map((m) => [m.type, m.seq]), [["view", 3]]);
    sent.length = 0;
    // A selection larger than R takes stays in the page, with a note.
    for (let r = 0; r < 100; r++) sel.add("b", r);
    assert.equal(link.selected("toggle"), false);
    assert.deepEqual(sent, []);
    assert.match(page.notes[page.notes.length - 1], /This selection \(101 features\) is too large to send to R; it stays in this page/);
    sel.clear();
    clickSelection(sel, { layer: "b", row: 1 }, false);
    link.selected("click");
    assert.equal(page.notes[page.notes.length - 1], null, "the note goes with the next selection sent");
    assert.deepEqual(sent.shift().items, [{ layer: "b", rows: [1] }]);
    // Lost: the note; back: hello, then the whole selection again.
    link.lost("closed", { code: 1006, delay: 1000 });
    assert.equal(page.notes[page.notes.length - 1], NOT_CONNECTED);
    assert.equal(NOT_CONNECTED, "Not connected to R: selections stay in this page");
    clickSelection(sel, { layer: "b", row: 9 }, true);
    assert.equal(link.selected("toggle"), false, "nothing is sent while not connected");
    link.open();
    assert.equal(page.notes[page.notes.length - 1], null);
    assert.equal(sent.shift().type, "hello");
    listener({ type: "hello", protocol: 1, connection: 2, scene: 3, spec: "0.5", select: ["b"], max_message: 1048576 });
    const again = sent.shift();
    assert.deepEqual([again.type, again.trigger, again.items], ["select", "toggle", [{ layer: "b", rows: [1, 9] }]]);
    // The camera follows, no sooner than 250 ms after the last view.
    assert.deepEqual(sent, []);
    timers.advance(250);
    assert.equal(sent.shift().type, "view");
    link.lost("refused", { code: 4000, reason: "protocol" });
    assert.equal(page.notes[page.notes.length - 1], "Not connected to R (this page needs a newer aobcore): selections stay in this page");
    listener({ type: "reload", scene: 3 });
    assert.equal(page.reloaded, null, "no reload at the page's own serial");
    listener({ type: "reload", scene: 4 });
    assert.equal(page.reloaded, 4);
    link.lost("closed", { code: 1013, delay: 1000 });
    assert.equal(page.notes[page.notes.length - 1], TOO_MANY);
    assert.equal(utf8Length("a\u00e9\u20ac\ud83d\ude00"), 1 + 2 + 3 + 4);
    console.log("ok   protocol 1: hello first, whole selections, settled views, the size cap, notes and reload");
  }

  // Selection rules: a click selects one, Shift or Cmd toggles, a click on
  // nothing clears.
  {
    const sel = selectionState();
    assert.equal(clickSelection(sel, { layer: "a", row: 3 }, false), "click");
    assert.equal(clickSelection(sel, { layer: "a", row: 1 }, true), "toggle");
    assert.equal(clickSelection(sel, { layer: "b", row: 0 }, true), "toggle");
    assert.deepEqual(sel.items(), [{ layer: "a", rows: [1, 3] }, { layer: "b", rows: [0] }]);
    assert.equal(selectionText(sel.items()), "a:1,3;b:0");
    clickSelection(sel, { layer: "a", row: 3 }, true);
    clickSelection(sel, { layer: "a", row: 1 }, true);
    assert.deepEqual(sel.items(), [{ layer: "b", rows: [0] }]);
    assert.equal(clickSelection(sel, { layer: "a", row: 2 }, false), "click");
    assert.deepEqual(sel.items(), [{ layer: "a", rows: [2] }]);
    sel.add("b", 5);
    sel.keepLayers(new Set(["b"]));
    assert.deepEqual(sel.items(), [{ layer: "b", rows: [5] }]);
    assert.equal(clickSelection(sel, null, true), "toggle", "Shift or Cmd on nothing keeps the selection");
    assert.deepEqual(sel.items(), [{ layer: "b", rows: [5] }]);
    assert.equal(clickSelection(sel, null, false), "click");
    assert.equal(sel.size, 0);
    assert.equal(sel.clear(), false);
    console.log("ok   selection: click selects one, Shift or Cmd adds and removes, a click on nothing clears");
  }
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
let part6 = null; // the scene and blobs, again in part 8
{
  // GeoArrow geometry with the view's CRS, as the explicit-data contract asks.
  const geoField = (name, type, ext) => new Field(name, type, false, new Map([["ARROW:extension:name", ext],
    ["ARROW:extension:metadata", JSON.stringify({ crs: "EPSG:3031", crs_type: "authority_code" })]]));
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
  part6 = { scene, blobs: blobs6, pts };
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

// ---- 6c. double-click zoom and hover in a view with bounds (#28, #45) ---------
// The 0.5 scene again, now with view.bounds (as a CCAMLR areas scene has):
// moving the pointer off the canvas clears the hover highlight (and a point
// popup), and a double click zooms in one level about the pointer, which
// stays over the same point.
{
  const { scene, blobs } = part6;
  const bounded = { ...scene, view: { ...scene.view, bounds: [-4e6, 4e6, -4e6, 4e6] } };
  const browser6 = await chromium.launch({
    executablePath: process.env.CHROMIUM_PATH || undefined,
    args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  });
  try {
    const page = await browser6.newPage({ viewport: { width: 900, height: 700 } });
    await page.setContent(`<!DOCTYPE html><html><head><meta charset="utf-8"></head><body style="margin:0;height:100vh">
<div id="c" style="height:100%;width:100%"></div><script>${bundle}</script></body></html>`);
    await page.evaluate(async ([sc, bl]) => {
      window.h = await new Promise((resolve, reject) => {
        aob.render(document.getElementById("c"), sc, { blobs: bl, onReady: resolve }).catch(reject);
      });
    }, [bounded, blobs]);
    // The view CRS point under a page position, from deck's viewport.
    const under = (x, y) => page.evaluate(([x, y]) => {
      const r = window.h.deck.getCanvas().getBoundingClientRect();
      return window.h.deck.getViewports()[0].unproject([x - r.left, y - r.top]);
    }, [x, y]);
    const screen = (x, y) => page.evaluate(([x, y]) => {
      const r = window.h.deck.getCanvas().getBoundingClientRect();
      const p = window.h.deck.getViewports()[0].project([x, y]);
      return [r.left + p[0], r.top + p[1]];
    }, [x, y]);
    // Hover the zone (trigger point: a popup and the highlight), then leave
    // the canvas for the side panel.
    const highlighted = () => page.evaluate(() => {
      const l = window.h.deck.layerManager.getLayers().find((l) => l.id.startsWith("zones") && l.props.autoHighlight);
      const m = l && l.getModels()[0];
      const c = m && m.shaderInputs.getUniformValues().picking;
      return c ? !!c.isHighlightActive : null;
    });
    const [xz, yz] = await screen(-1.7e6, -1.7e6);
    await page.waitForTimeout(350);
    await page.mouse.move(xz, yz, { steps: 4 });
    await page.waitForFunction(() => !document.querySelector(".aob-popup").hidden, null, { timeout: 5000 });
    assert.equal(await highlighted(), true, "hovering the zone highlights it");
    const panel = await page.evaluate(() => {
      const r = document.querySelector(".aob-panel").getBoundingClientRect();
      return [r.left + r.width / 2, r.bottom - 20];
    });
    await page.mouse.move(panel[0], panel[1]); // one jump: no move over the canvas between
    await page.waitForFunction(() => document.querySelector(".aob-popup").hidden, null, { timeout: 5000 });
    await page.waitForTimeout(300);
    assert.equal(await highlighted(), false, "leaving the canvas clears the hover highlight");
    console.log("ok   leaving the canvas clears the hover highlight and the point popup");

    // Off the stations and the zone, within the bounds.
    const [xd, yd] = await screen(1.5e6, -1e6);
    const vBefore = await page.evaluate(() => window.h.view());
    const z0 = vBefore.zoom;
    const at0 = await under(xd, yd);
    await page.mouse.move(xd, yd);
    await page.waitForTimeout(350);
    await page.mouse.dblclick(xd, yd);
    await page.waitForFunction((z) => Math.abs(window.h.view().zoom - (z + 1)) < 1e-6, z0, { timeout: 5000 });
    await page.waitForTimeout(500); // the transition is over: nothing moves on
    const v1 = await page.evaluate(() => window.h.view());
    assert.ok(Math.abs(v1.zoom - (z0 + 1)) < 1e-6, `a double click zooms in one level (${z0} to ${v1.zoom})`);
    const deckZoom = await page.evaluate(() => window.h.deck.getViewports()[0].zoom);
    assert.ok(Math.abs(deckZoom - (z0 + 1)) < 1e-6, `deck draws the new zoom (${deckZoom})`);
    const at1 = await under(xd, yd);
    const px = Math.pow(2, -v1.zoom); // view units per CSS pixel
    assert.ok(Math.hypot(at1[0] - at0[0], at1[1] - at0[1]) < 2 * px,
      `the point under the pointer stays (${at0} then ${at1})`);
    assert.ok(await page.evaluate(() => document.querySelector(".aob-popup").hidden), "a double click does not select");
    // After the transition handle.view() has the shape it had before: the
    // clamped view with minZoom and maxZoom and a 3-element target, not
    // deck's last frame.
    assert.deepEqual(Object.keys(v1).sort(), Object.keys(vBefore).sort(),
      `handle.view() keeps its fields after a transition (${Object.keys(v1)})`);
    assert.equal(v1.target.length, vBefore.target.length, "the target keeps its length");
    assert.ok(Number.isFinite(v1.minZoom) && Number.isFinite(v1.maxZoom), "minZoom and maxZoom are kept");
    assert.equal(v1.maxZoom, vBefore.maxZoom, "maxZoom is unchanged");
    console.log("ok   view.bounds: a double click zooms in one level about the pointer");

    // Double clicks are decided by deck's rules (#29): a slow double click
    // (the second press 250 ms after the first release, held 120 ms) is two
    // clicks, so it selects and does not zoom; the third click of a triple
    // click selects. (Its zoom is deck's: the third press stops the zoom
    // transition the double click started.)
    const { pts } = part6;
    const [xs, ys] = await screen(...pts[2]);
    const popupRow = (r) => !document.querySelector(".aob-popup").hidden &&
      document.querySelector(".aob-popup").dataset.aobRow === String(r);
    await page.mouse.move(xs, ys);
    await page.waitForTimeout(350);
    await page.mouse.down();
    await page.mouse.up();
    await page.waitForTimeout(250);
    await page.mouse.down();
    await page.waitForTimeout(120);
    await page.mouse.up();
    await page.waitForFunction(popupRow, 2, { timeout: 5000 });
    await page.waitForTimeout(400);
    const v2 = await page.evaluate(() => window.h.view());
    assert.equal(v2.zoom, v1.zoom, "a slow double click does not zoom");
    await page.keyboard.press("Escape");
    await page.waitForTimeout(350);
    const [xt, yt] = await screen(...pts[2]);
    await page.mouse.click(xt, yt, { clickCount: 3 });
    await page.waitForFunction(popupRow, 2, { timeout: 5000 });
    await page.keyboard.press("Escape");
    console.log("ok   a slow double click selects and does not zoom; a triple click selects");

    // A press on something in the canvas's container other than the canvas
    // (a stand-in for a deck widget) does not select what is under it (#29).
    await page.waitForTimeout(350);
    const [xw, yw] = await screen(...pts[2]);
    await page.evaluate(([x, y]) => {
      const host = document.querySelector(".aob-canvas");
      const r = host.getBoundingClientRect();
      const w = document.createElement("button");
      w.id = "widget";
      w.style.cssText = `position:absolute;z-index:5;left:${x - r.left - 15}px;top:${y - r.top - 15}px;width:30px;height:30px`;
      host.append(w);
    }, [xw, yw]);
    assert.equal(await page.evaluate(([x, y]) => document.elementFromPoint(x, y).id, [xw, yw]), "widget");
    await page.mouse.click(xw, yw);
    await page.waitForTimeout(600);
    assert.ok(await page.evaluate(() => document.querySelector(".aob-popup").hidden && window.h.selected() === null),
      "a press on a widget over a feature does not select it");
    await page.evaluate(() => document.getElementById("widget").remove());
    await page.mouse.click(xw, yw);
    await page.waitForFunction(popupRow, 2, { timeout: 5000 });
    await page.keyboard.press("Escape");
    console.log("ok   a press on a widget over the canvas does not select; on the canvas it does");

    // A pointer press just before teardown: its delayed restore does not
    // touch the finalized deck (#29).
    const torn = await page.evaluate(async () => {
      const errs = [];
      const onErr = (e) => errs.push(String(e.message || e));
      window.addEventListener("error", onErr);
      const canvas = window.h.deck.getCanvas();
      const r = canvas.getBoundingClientRect();
      const at = { clientX: r.left + 20, clientY: r.top + 20, isPrimary: true, button: 0, pointerId: 7, bubbles: true };
      const setProps = window.h.deck.setProps.bind(window.h.deck);
      let after = 0;
      let done = false;
      window.h.deck.setProps = (p) => { if (done) after++; return setProps(p); };
      // A press that never bubbles back to the canvas's parent, so only the
      // timer restores picking.
      const stop = (e) => e.stopPropagation();
      canvas.addEventListener("pointerdown", stop);
      canvas.dispatchEvent(new PointerEvent("pointerdown", at));
      canvas.removeEventListener("pointerdown", stop);
      window.h.finalize();
      done = true;
      await new Promise((r) => setTimeout(r, 50));
      window.removeEventListener("error", onErr);
      return { after, errs };
    });
    assert.deepEqual(torn, { after: 0, errs: [] }, "no setProps after finalize");
    console.log("ok   a press just before finalize() does not touch the finalized deck");
  } finally {
    await browser6.close();
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

// ---- 8b. selections over a websocket (decision 0007) in the browser ----------
// A served page with data-aob-socket talks protocol 1 to an R stand-in
// (ws-server.mjs) on the same server: in an EPSG:3031 view a click selects,
// Shift and Cmd click add and remove, a click on nothing and Escape clear,
// a layer without a popup is selectable when R names it, a pan sends one
// view once it settles, reload keeps the camera, a lost socket shows the
// note and reconnects with the whole selection, and a page on another
// origin cannot connect. A page without data-aob-socket opens no socket.
{
  const { createServer } = await import("node:http");
  const sc = JSON.parse(JSON.stringify(part6.scene));
  // Zones lose their popup (selectable all the same); "broken" goes.
  delete sc.layers.find((L) => L.id === "zones").popup;
  sc.layers = sc.layers.filter((L) => L.id !== "broken");
  const pts = part6.pts;
  const blobBytes = Object.fromEntries(Object.entries(part6.blobs).map(([k, v]) => [k, Buffer.from(v, "base64")]));
  let serial = 1;
  const page8 = (socket) => `<!DOCTYPE html><html><head><meta charset="utf-8"></head><body style="margin:0;height:100vh">
<div class="aob-page" data-aob-scene="aob-scene" data-aob-blob-base="blob/"${socket ? ` data-aob-socket="ws" data-aob-scene-serial="${serial}"` : ""} style="height:100%"></div>
<script type="application/json" id="aob-scene">${JSON.stringify(sc)}</script>
<script type="application/json" data-aob-blob-keys data-aob-scene="aob-scene">${JSON.stringify(Object.keys(blobBytes))}</script>
<script src="aob-renderer.min.js"></script></body></html>`;
  const server = createServer((req, res) => {
    const u = new URL(req.url, "http://x");
    const m = /^\/(tok|embedded)\/(.*)$/.exec(u.pathname);
    if (!m) {
      res.writeHead(404);
      return res.end();
    }
    if (m[2] === "") {
      res.writeHead(200, { "content-type": "text/html; charset=utf-8" });
      return res.end(page8(m[1] === "tok"));
    }
    if (m[2] === "aob-renderer.min.js") {
      res.writeHead(200, { "content-type": "text/javascript" });
      return res.end(bundle);
    }
    const b = m[2].startsWith("blob/") && blobBytes[decodeURIComponent(m[2].slice(5))];
    if (b) {
      res.writeHead(200, { "content-type": "application/vnd.apache.arrow.stream" });
      return res.end(b);
    }
    res.writeHead(404);
    res.end();
  });
  await new Promise((r) => server.listen(0, "127.0.0.1", r));
  const port = server.address().port;
  const base = `http://127.0.0.1:${port}`;
  const r = rStandIn({ serial: 1, select: ["zones", "stations", "sst", "nope"], origins: [base], path: "/tok/ws" });
  server.on("upgrade", (req, sock) => r.upgrade(req, sock));
  const browser8 = await chromium.launch({
    executablePath: process.env.CHROMIUM_PATH || undefined,
    args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  });
  try {
    const ctx = await browser8.newContext({ viewport: { width: 900, height: 700 } });
    // Count the sockets every page opens.
    await ctx.addInitScript(() => {
      window.socketsOpened = [];
      const WS = window.WebSocket;
      window.WebSocket = function (url, p) {
        window.socketsOpened.push(String(url));
        return new WS(url, p);
      };
      window.WebSocket.prototype = WS.prototype;
    });
    const page = await ctx.newPage();
    const cdiv = () => document.querySelector("div[data-aob-scene]");
    await page.goto(`${base}/tok/`);
    await page.waitForFunction(() => document.querySelector("div[data-aob-scene]").dataset.aobLink === "ready", null, { timeout: 60000 });
    const hello = r.of("hello")[0];
    assert.deepEqual(hello, { type: "hello", protocol: 1, renderer: "0.0.5", specs: ["0.1", "0.2", "0.3", "0.4", "0.5"], scene: 1 });
    assert.deepEqual(await page.evaluate(() => window.socketsOpened), [`ws://127.0.0.1:${port}/tok/ws`]);
    assert.equal(await page.evaluate(() => document.querySelector("div[data-aob-scene]").dataset.aobSelectable), "zones,stations",
      "only vector layers in the scene are selectable");
    await r.until(() => r.of("view").length >= 1);
    console.log("ok   served page: the socket opens once drawn, hello both ways, the camera follows hello");

    const screen = (x, y) => page.evaluate(([x, y]) => {
      const rc = document.querySelector(".aob-canvas").getBoundingClientRect();
      const v = document.querySelector("div[data-aob-scene]").aob.view();
      const k = Math.pow(2, v.zoom);
      return [rc.left + rc.width / 2 + (x - v.target[0]) * k, rc.top + rc.height / 2 - (y - v.target[1]) * k];
    }, [x, y]);
    const nSel = () => r.of("select").length;
    const lastSel = () => r.of("select").at(-1);
    const click = async (x, y, mod) => {
      const n = nSel();
      await page.waitForTimeout(350);
      if (mod) await page.keyboard.down(mod);
      await page.mouse.click(x, y);
      if (mod) await page.keyboard.up(mod);
      await r.until(() => nSel() > n);
      return lastSel();
    };
    const [x0, y0] = await screen(...pts[0]);
    const [x2, y2] = await screen(...pts[2]);
    const [xz, yz] = await screen(-1.7e6, -1.7e6);
    const [xe, ye] = await screen(0, 2.5e6);
    let s = await click(x0, y0);
    assert.equal(s.trigger, "click");
    assert.equal(s.scene, 1);
    assert.deepEqual(s.items, [{ layer: "stations", rows: [0] }]);
    assert.ok(Math.abs(s.at[0] - pts[0][0]) < 2e4 && Math.abs(s.at[1] - pts[0][1]) < 2e4, `at is the pressed point (${s.at})`);
    const pop = await page.evaluate(() => document.querySelector(".aob-popup").dataset.aobRow);
    assert.equal(pop, "0", "the row sent is the row the popup shows");
    s = await click(x2, y2, "Shift");
    assert.deepEqual([s.trigger, s.items], ["toggle", [{ layer: "stations", rows: [0, 2] }]]);
    s = await click(xz, yz, "Meta");
    assert.deepEqual([s.trigger, s.items], ["toggle", [{ layer: "stations", rows: [0, 2] }, { layer: "zones", rows: [0] }]],
      "Cmd adds; a layer with no popup is selectable");
    assert.equal(await page.evaluate(() => document.querySelector("div[data-aob-scene]").dataset.aobSelection), "stations:0,2;zones:0");
    const hlIds = await page.evaluate(() => document.querySelector("div[data-aob-scene]").aob.deck.props.layers.map((l) => l.id).filter((id) => id.includes("-sel")));
    assert.ok(hlIds.some((id) => id.startsWith("stations--0-sel")) && hlIds.some((id) => id.startsWith("zones--0-sel")), `highlight layers: ${hlIds}`);
    s = await click(x0, y0, "Shift");
    assert.deepEqual([s.trigger, s.items], ["toggle", [{ layer: "stations", rows: [2] }, { layer: "zones", rows: [0] }]], "Shift removes");
    assert.ok(await page.evaluate(() => document.querySelector(".aob-popup").hidden), "a Shift click opens no popup");
    s = await click(xe, ye, "Shift");
    assert.deepEqual([s.trigger, s.items], ["toggle", [{ layer: "stations", rows: [2] }, { layer: "zones", rows: [0] }]],
      "a Shift click on nothing keeps the selection");
    assert.ok(Array.isArray(s.at), "and says where");
    s = await click(xe, ye);
    assert.deepEqual([s.trigger, s.items], ["click", []], "a click on nothing clears");
    assert.ok(Array.isArray(s.at), "a click on nothing still says where");
    await click(xz, yz);
    const n = nSel();
    await page.keyboard.press("Escape");
    await r.until(() => nSel() > n);
    s = lastSel();
    assert.deepEqual([s.trigger, s.items, s.at], ["clear", [], undefined], "Escape clears");
    assert.equal(await page.evaluate(() => document.querySelector("div[data-aob-scene]").dataset.aobSelection), undefined);
    const seqs = r.messages.map((m) => m.msg.seq).filter((q) => q !== undefined);
    assert.deepEqual(seqs, [...seqs].sort((a, b) => a - b), "seq counts up");
    console.log("ok   selection mode in EPSG:3031: click, Shift and Cmd toggle (no popup), a click on nothing and Escape clear, Shift on nothing keeps");

    // A pan sends one view, after it settles.
    await page.waitForTimeout(600);
    const nv = r.of("view").length;
    await page.mouse.move(450, 350);
    await page.mouse.down();
    for (let i = 1; i <= 10; i++) {
      await page.mouse.move(450 + i * 12, 350 + i * 6);
      await page.waitForTimeout(40);
    }
    await page.mouse.up();
    await page.waitForTimeout(1200);
    const views = r.of("view").slice(nv);
    assert.equal(views.length, 1, `one view per settled pan (got ${views.length})`);
    const vs = await page.evaluate(() => document.querySelector("div[data-aob-scene]").aob.view());
    const v = views[0];
    const k = Math.pow(2, -vs.zoom);
    assert.ok(Math.abs((v.extent[0] + v.extent[1]) / 2 - vs.target[0]) < k && Math.abs((v.extent[2] + v.extent[3]) / 2 - vs.target[1]) < k);
    assert.equal(v.zoom, vs.zoom);
    assert.ok(v.units_per_pixel > 0 && v.size_px.length === 2);
    console.log("ok   a pan sends one view message once the camera settles");

    // R replaces the scene: the page reloads with its camera.
    const before = await page.evaluate(() => {
      const h = document.querySelector("div[data-aob-scene]").aob;
      return h.setView({ ...h.view(), zoom: h.view().zoom + 1.5, target: [-1.2e6, 4e5, 0] });
    });
    await page.evaluate(() => document.querySelector("div[data-aob-scene]").aob.deck.setProps({ initialViewState: document.querySelector("div[data-aob-scene]").aob.view() }));
    serial = 2;
    const hellos = r.of("hello").length;
    r.reload(2);
    await r.until(() => r.of("hello").length > hellos, 60000);
    await page.waitForFunction(() => document.querySelector("div[data-aob-scene]").dataset.aobLink === "ready", null, { timeout: 60000 });
    assert.equal(r.of("hello").at(-1).scene, 2);
    const after = await page.evaluate(() => ({ v: document.querySelector("div[data-aob-scene]").aob.view(),
      kept: document.querySelector("div[data-aob-scene]").dataset.aobCameraKept, opened: window.socketsOpened.length }));
    assert.equal(after.kept, "true");
    assert.ok(Math.abs(after.v.zoom - before.zoom) < 1e-9 && Math.abs(after.v.target[0] + 1.2e6) < 1 && Math.abs(after.v.target[1] - 4e5) < 1,
      `camera kept across reload (${JSON.stringify(after.v.target)} at ${after.v.zoom})`);
    assert.equal(after.opened, 1, "the reloaded page opens one socket");
    console.log("ok   reload from R reloads the page with its camera kept");

    // R stops: the note shows, the selection stays, and the page reconnects
    // with its whole selection.
    const [ax, ay] = await screen(...pts[1]);
    await click(ax, ay);
    const closes = r.closes.length;
    r.stop();
    await page.waitForFunction(() => {
      const n = document.querySelector(".aob-link");
      return n && !n.hidden && n.textContent === "Not connected to R: selections stay in this page";
    }, null, { timeout: 5000 });
    assert.ok(r.closes.length > closes);
    assert.equal(await page.evaluate(() => document.querySelector("div[data-aob-scene]").dataset.aobSelection), "stations:1", "the selection stays");
    const nh = r.of("hello").length;
    const ns = nSel();
    await r.until(() => r.of("hello").length > nh && nSel() > ns, 10000);
    assert.deepEqual(lastSel().items, [{ layer: "stations", rows: [1] }], "the whole selection again on reconnecting");
    await page.waitForFunction(() => document.querySelector(".aob-link").hidden, null, { timeout: 5000 });
    console.log("ok   a lost socket shows the note, keeps the selection, and reconnects with it");

    // Another origin (localhost, not 127.0.0.1) cannot connect.
    const other = await ctx.newPage();
    const refused = r.refused.length;
    const nmsg = r.messages.length;
    await other.goto(`http://localhost:${port}/tok/`);
    await other.waitForFunction(() => {
      const n = document.querySelector(".aob-link");
      return n && !n.hidden;
    }, null, { timeout: 60000 });
    assert.ok(r.refused.length > refused);
    assert.equal(r.refused.at(-1).origin, `http://localhost:${port}`);
    assert.equal(r.messages.length, nmsg, "nothing from the refused page");
    assert.equal(await other.evaluate(() => document.querySelector("div[data-aob-scene]").dataset.aobLink), "closed");
    await other.close();
    console.log("ok   a page on another origin cannot connect, and says it is not connected");

    // No data-aob-socket: no socket, no selection mode.
    const emb = await ctx.newPage();
    await emb.goto(`${base}/embedded/`);
    await emb.waitForFunction(() => document.querySelector("div[data-aob-scene]").dataset.aobStatus === "ready", null, { timeout: 60000 });
    const ex = await emb.evaluate(() => {
      const c = document.querySelector("div[data-aob-scene]");
      return { sockets: window.socketsOpened.length, state: c.dataset.aobLink, note: !!c.querySelector(".aob-link") };
    });
    await emb.waitForTimeout(350);
    const [ex0, ey0] = await emb.evaluate(([x, y]) => {
      const rc = document.querySelector(".aob-canvas").getBoundingClientRect();
      const v = document.querySelector("div[data-aob-scene]").aob.view();
      const k = Math.pow(2, v.zoom);
      return [rc.left + rc.width / 2 + (x - v.target[0]) * k, rc.top + rc.height / 2 - (y - v.target[1]) * k];
    }, pts[0]);
    await emb.mouse.click(ex0, ey0);
    await emb.waitForFunction(() => !document.querySelector(".aob-popup").hidden, null, { timeout: 5000 });
    const [zx, zy] = await emb.evaluate(([x, y]) => {
      const rc = document.querySelector(".aob-canvas").getBoundingClientRect();
      const v = document.querySelector("div[data-aob-scene]").aob.view();
      const k = Math.pow(2, v.zoom);
      return [rc.left + rc.width / 2 + (x - v.target[0]) * k, rc.top + rc.height / 2 - (y - v.target[1]) * k];
    }, [-1.7e6, -1.7e6]);
    await emb.waitForTimeout(350);
    await emb.mouse.click(zx, zy);
    await emb.waitForTimeout(800);
    const ey = await emb.evaluate(() => {
      const c = document.querySelector("div[data-aob-scene]");
      return { selection: c.dataset.aobSelection, items: c.aob.selection(), popup: c.dataset.aobSelected,
        pickable: c.aob.deck.props.layers.filter((l) => l.props.pickable).map((l) => l.id) };
    });
    assert.deepEqual(ex, { sockets: 0, state: undefined, note: false });
    assert.equal(ey.selection, undefined);
    assert.deepEqual(ey.items, []);
    assert.equal(ey.popup, undefined, "a layer without a popup is not picked");
    assert.deepEqual(ey.pickable, ["stations--0"], "only the popup layer is pickable, as before");
    await emb.close();
    console.log("ok   a page without data-aob-socket opens no socket and has no selection mode");
    await ctx.close();
  } finally {
    await browser8.close();
    server.close();
  }
}

// ---- 9. fragments in a host document (decision 0009) --------------------------
// Two scenes as aobcore scene_tag() writes them, in one document with one
// copy of the renderer in the head: both draw, each fragment's data-theme
// fixes its own colours whatever the browser prefers, a fragment without one
// follows prefers-color-scheme, and the theme button changes its own
// fragment and not the host's root element.
{
  const scene = { version: "0.4", view: { type: "projected", crs: "EPSG:3031", extent: [-1e6, 1e6, -1e6, 1e6] },
                  data: {}, layers: [] };
  const frag = (id, theme) => `<div class="aob-fragment" data-aob-scene="${id}"${theme ? ` data-theme="${theme}"` : ""} style="width:100%;height:360px"></div>
<script type="application/json" id="${id}">${JSON.stringify(scene)}</script>`;
  const browser9 = await chromium.launch({
    executablePath: process.env.CHROMIUM_PATH || undefined,
    args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  });
  try {
    const page = await browser9.newPage({ viewport: { width: 900, height: 1200 }, colorScheme: "dark" });
    await page.setContent(`<!DOCTYPE html><html><head><meta charset="utf-8"><script>${bundle}</script></head>
<body><p>Text</p>${frag("a", "light")}<p>More text</p>${frag("b", "dark")}${frag("c", null)}</body></html>`);
    await page.waitForFunction(() => [...document.querySelectorAll(".aob-fragment")].every((e) =>
      e.dataset.aobStatus === "ready" || e.dataset.aobStatus === "error"), null, { timeout: 60000 });
    const bg = () => page.evaluate(() => [...document.querySelectorAll(".aob-fragment")].map((e) =>
      getComputedStyle(e).getPropertyValue("--aob-bg").trim()));
    const r = await page.evaluate(() => [...document.querySelectorAll(".aob-fragment")].map((e) => e.dataset.aobStatus));
    assert.deepEqual(r, ["ready", "ready", "ready"], "every fragment draws");
    assert.deepEqual(await bg(), ["#eef2f4", "#0f171c", "#0f171c"], "light, dark, and auto in a dark browser");
    await page.click('.aob-fragment[data-aob-scene="a"] .aob-theme');
    const after = await page.evaluate(() => ({
      root: document.documentElement.dataset.theme,
      a: document.querySelector('.aob-fragment[data-aob-scene="a"]').dataset.theme,
      label: document.querySelector('.aob-fragment[data-aob-scene="a"] .aob-theme').textContent,
    }));
    assert.deepEqual(after, { root: undefined, a: "dark", label: "Theme: dark" });
    assert.deepEqual(await bg(), ["#0f171c", "#0f171c", "#0f171c"]);
    // The host's own colours are left alone in a dark browser: no
    // color-scheme on its root, its text and inputs as the browser draws them.
    const host = await page.evaluate(() => {
      const input = document.createElement("input");
      document.body.append(input);
      return { root: getComputedStyle(document.documentElement).colorScheme,
               text: getComputedStyle(document.querySelector("p")).color,
               input: getComputedStyle(input).backgroundColor,
               fragment: getComputedStyle(document.querySelector(".aob-fragment")).colorScheme };
    });
    assert.deepEqual(host, { root: "normal", text: "rgb(0, 0, 0)", input: "rgb(255, 255, 255)", fragment: "dark" });
    // A fragment inserted later (as Shiny's renderUI() inserts one) draws
    // through its trailing boot script.
    await page.evaluate((sc) => {
      const d = document.createElement("div");
      d.className = "aob-fragment";
      d.dataset.aobScene = "late";
      d.style.height = "300px";
      const j = document.createElement("script");
      j.type = "application/json";
      j.id = "late";
      j.textContent = JSON.stringify(sc);
      const b = document.createElement("script");
      b.textContent = "if (window.aob && window.aob.boot) window.aob.boot();";
      document.body.append(d, j, b);
    }, scene);
    await page.waitForFunction(() => document.querySelector('.aob-fragment[data-aob-scene="late"]').dataset.aobStatus === "ready",
      null, { timeout: 60000 });
    console.log("ok   fragments in one document draw, each with its own theme, and the host's colours are its own");
  } finally {
    await browser9.close();
  }
}

// ---- 10. a host's channel instead of a socket (decision 0009) ----------------
// aob.render(el, scene, {channel}) with a channel made in the page, as a
// Shiny output binding makes one: no socket is opened, the page's hello
// goes to the channel, the host's hello makes layers selectable, and a
// click sends protocol 1's select through it (serial 0 when none is given).
{
  const sc = JSON.parse(JSON.stringify(part6.scene));
  sc.layers = sc.layers.filter((L) => L.id !== "broken");
  const browser10 = await chromium.launch({
    executablePath: process.env.CHROMIUM_PATH || undefined,
    args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  });
  try {
    const page = await browser10.newPage({ viewport: { width: 900, height: 700 } });
    await page.setContent(`<!DOCTYPE html><html><head><meta charset="utf-8"></head><body style="margin:0;height:100vh">
<div id="c" class="aob-fragment" style="height:100%;width:100%"></div><script>${bundle}</script></body></html>`);
    await page.evaluate(async ([sc, blobs]) => {
      window.socketsOpened = 0;
      const WS = window.WebSocket;
      window.WebSocket = function (url, p) {
        window.socketsOpened++;
        return new WS(url, p);
      };
      window.sent = [];
      const channel = (onState) => {
        const listeners = [];
        setTimeout(() => onState("open", {}), 0);
        return {
          get connected() { return true; },
          send(m) {
            if (typeof m !== "object") throw new Error("channel.send() was given " + typeof m);
            const msg = m;
            window.sent.push(msg);
            if (msg.type === "hello") {
              setTimeout(() => listeners.forEach((f) => f({ type: "hello", protocol: 1, select: ["stations"] })), 0);
            }
            return true;
          },
          onMessage(f) { listeners.push(f); window.deliver = (m) => listeners.forEach((g) => g(m)); return () => {}; },
          close() {},
        };
      };
      const c = document.getElementById("c");
      c.aob = await aob.render(c, sc, { blobs, channel });
    }, [sc, part6.blobs]);
    await page.waitForFunction(() => document.getElementById("c").dataset.aobLink === "ready", null, { timeout: 60000 });
    const hello = await page.evaluate(() => window.sent[0]);
    assert.deepEqual(hello, { type: "hello", protocol: 1, renderer: "0.0.5", specs: ["0.1", "0.2", "0.3", "0.4", "0.5"], scene: 0 });
    assert.equal(await page.evaluate(() => document.getElementById("c").dataset.aobSelectable), "stations");
    await page.waitForTimeout(400);
    const [x0, y0] = await page.evaluate(([x, y]) => {
      const rc = document.querySelector(".aob-canvas").getBoundingClientRect();
      const v = document.getElementById("c").aob.view();
      const k = Math.pow(2, v.zoom);
      return [rc.left + rc.width / 2 + (x - v.target[0]) * k, rc.top + rc.height / 2 - (y - v.target[1]) * k];
    }, part6.pts[0]);
    await page.mouse.click(x0, y0);
    await page.waitForFunction(() => window.sent.some((m) => m.type === "select"), null, { timeout: 5000 });
    const sel = await page.evaluate(() => window.sent.find((m) => m.type === "select"));
    assert.equal(sel.scene, 0);
    assert.equal(sel.trigger, "click");
    assert.deepEqual(sel.items, [{ layer: "stations", rows: [0] }]);
    assert.equal(await page.evaluate(() => window.socketsOpened), 0, "no socket");
    console.log("ok   a host's channel carries protocol 1 instead of a socket");
    // A reload over a host channel does not reload the host page.
    await page.evaluate(() => { window.notReloaded = true; });
    await page.evaluate(() => window.deliver && window.deliver({ type: "reload", scene: 99 }));
    await page.waitForTimeout(300);
    assert.equal(await page.evaluate(() => window.notReloaded), true, "a reload over a channel is ignored");
    console.log("ok   a reload over a host channel is ignored");
  } finally {
    await browser10.close();
  }
}

// ---- 11. Escape in a host page with two views (decision 0009) ----------------
// Two fragments with host channels, a feature selected in each: Escape
// closes the focused view's popup, then clears only that view's selection
// (the view clicked last, whose map the press focused).
{
  const sc = JSON.parse(JSON.stringify(part6.scene));
  sc.layers = sc.layers.filter((L) => L.id !== "broken");
  const browser11 = await chromium.launch({
    executablePath: process.env.CHROMIUM_PATH || undefined,
    args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  });
  try {
    const page = await browser11.newPage({ viewport: { width: 900, height: 1000 } });
    await page.setContent(`<!DOCTYPE html><html><head><meta charset="utf-8"></head><body style="margin:0">
<div id="a" class="aob-fragment" style="height:480px;width:100%"></div>
<div id="b" class="aob-fragment" style="height:480px;width:100%"></div><script>${bundle}</script></body></html>`);
    await page.evaluate(async ([sc, blobs]) => {
      const channel = (onState) => {
        const listeners = [];
        setTimeout(() => onState("open", {}), 0);
        return {
          send(m) {
            if (m.type === "hello") setTimeout(() => listeners.forEach((f) => f({ type: "hello", protocol: 1, select: ["stations"] })), 0);
            return true;
          },
          onMessage(f) { listeners.push(f); return () => {}; },
          close() {},
        };
      };
      for (const id of ["a", "b"]) {
        const c = document.getElementById(id);
        c.aob = await aob.render(c, sc, { blobs, channel });
      }
    }, [sc, part6.blobs]);
    await page.waitForFunction(() => ["a", "b"].every((id) => document.getElementById(id).dataset.aobLink === "ready"),
      null, { timeout: 60000 });
    await page.waitForTimeout(400);
    const screen = (id, [x, y]) => page.evaluate(([id, x, y]) => {
      const c = document.getElementById(id);
      const rc = c.querySelector(".aob-canvas").getBoundingClientRect();
      const v = c.aob.view();
      const k = Math.pow(2, v.zoom);
      return [rc.left + rc.width / 2 + (x - v.target[0]) * k, rc.top + rc.height / 2 - (y - v.target[1]) * k];
    }, [id, x, y]);
    const sel = () => page.evaluate(() => ["a", "b"].map((id) => document.getElementById(id).dataset.aobSelection || ""));
    const [ax, ay] = await screen("a", part6.pts[0]);
    await page.mouse.click(ax, ay);
    await page.waitForTimeout(400);
    const [bx, by] = await screen("b", part6.pts[0]);
    await page.mouse.click(bx, by);
    await page.waitForFunction(() => ["a", "b"].every((id) => document.getElementById(id).dataset.aobSelection), null, { timeout: 5000 });
    const before = await sel();
    // The first Escape closes b's popup (which has the focus), as in a page.
    await page.keyboard.press("Escape");
    await page.waitForTimeout(300);
    assert.deepEqual(await sel(), before, "the first Escape only closes the popup");
    assert.equal(await page.evaluate(() => document.querySelector("#b .aob-popup").hidden), true);
    assert.equal(await page.evaluate(() => document.getElementById("b").contains(document.activeElement)), true,
      "the focus goes back to b's map");
    await page.keyboard.press("Escape");
    await page.waitForTimeout(300);
    const after = await sel();
    assert.deepEqual(after, [before[0], ""], `Escape clears only the focused view (${JSON.stringify(before)} -> ${JSON.stringify(after)})`);
    console.log("ok   Escape in a host page clears only the view with the focus");
  } finally {
    await browser11.close();
  }
}

// ---- 12. explicit-data contract in the browser ------------------------------
{
  // Rows per table, and parts (points, lines or polygons drawn) of the
  // multi kinds, read here with apache-arrow alone.
  const rows = {};
  const parts = {};
  for (const [id, b] of Object.entries(validSpec.blobs)) {
    const t = readTable(Buffer.from(b, "base64"));
    rows[id] = t.numRows;
    const g = t.getChild("geometry");
    parts[id] = id.startsWith("multi") ? [...g].reduce((n, v) => n + v.length, 0) : t.numRows;
  }
  assert.deepEqual(rows, { point: 3, linestring: 2, polygon: 2, multipoint: 2, multilinestring: 2, multipolygon: 2 });
  assert.ok(parts.multipoint > 2 && parts.multilinestring > 2 && parts.multipolygon > 2, JSON.stringify(parts));

  // What each invalid scene reports: every one is a layer error. The
  // geometry faults and a colour column that is not RGBA stop the layer
  // being drawn; a popup column that is missing or not an attribute stops
  // its popup (the layer draws).
  const expected = {
    "attribute-binary": [/layer layer: popup column "payload" \(Binary\) in data layer is not an attribute type; popup not shown/, "2 points"],
    "colour-column-not-rgba": [/layer layer: data layer: fill colour column "name" is Utf8, not FixedSizeList<Uint8, 4>; not drawn/, "error: data not drawn"],
    "crs-not-view": [/geometry CRS OGC:CRS84 is not the view CRS EPSG:3031; not drawn/, "error: data not drawn"],
    "crs-type-mismatch": [/crs_type is projjson but crs is not a JSON object; not drawn/, "error: data not drawn"],
    "encoding-not-declared": [/geometry column "geometry" is geoarrow.multilinestring but the scene declares geoarrow.linestring; not drawn/, "error: data not drawn"],
    "geometrycollection": [/geometry column "geometry" is geoarrow.geometrycollection: .*; not drawn/, "error: data not drawn"],
    "no-crs": [/geometry column "geometry" has no crs in its extension metadata \(view CRS EPSG:3031\); not drawn/, "error: data not drawn"],
    "popup-column-missing": [/layer layer: popup column "elevation_m" not found in data layer; popup not shown/, "3 points"],
    "storage-mismatch": [/geoarrow.linestring storage must be 1 nested List level\(s\) above the coordinates; found FixedSizeList<Float64, 2> at level 1; not drawn/, "error: data not drawn"],
    "wkb": [/geometry column "geometry" is geoarrow.wkb: serialised geometry \(WKB or WKT\) is not drawn.*; not drawn/, "error: data not drawn"],
    "xym": [/coordinates are xym: M coordinates are not drawn; not drawn/, "error: data not drawn"],
  };
  assert.deepEqual(invalidSpec.map((s) => s.name), Object.keys(expected).sort(), "every invalid fixture has an expectation");

  const browser12 = await chromium.launch({
    executablePath: process.env.CHROMIUM_PATH || undefined,
    args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  });
  try {
    const page = await browser12.newPage({ viewport: { width: 800, height: 600 } });
    await page.setContent(`<!DOCTYPE html><html><head><meta charset="utf-8"></head><body style="margin:0;height:100vh">
<script>${bundle}</script></body></html>`);
    // Render a scene into a new element and report its status, notes, the
    // layer list's counts (scene order) and the drawn parts per layer.
    const draw = (sc, bl) => page.evaluate(async ([sc, bl]) => {
      document.querySelectorAll(".aob-root").forEach((e) => {
        if (e.aob) e.aob.finalize();
        e.remove();
      });
      const c = document.createElement("div");
      c.style.cssText = "height:100%;width:100%";
      document.body.append(c);
      c.aob = await new Promise((resolve, reject) => {
        aob.render(c, sc, { blobs: bl, onReady: resolve }).catch(reject);
      });
      const drawn = {};
      for (const l of c.aob.deck.props.layers.flat(Infinity)) {
        if (!l) continue;
        const id = l.id.split("--")[0];
        if (l.id.endsWith("-stroke")) continue; // a polygon's outlines are its rings
        drawn[id] = (drawn[id] || 0) + l.props.data.length;
      }
      return { status: c.dataset.aobStatus, errors: c.dataset.aobErrors, line: c.querySelector(".aob-status").textContent,
        counts: [...c.querySelectorAll(".aob-count")].map((e) => e.textContent).reverse(), drawn };
    }, [sc, bl]);

    const v = await draw(validSpec.scene, validSpec.blobs);
    assert.equal(v.status, "ready", v.line);
    assert.equal(v.errors, undefined, v.line);
    assert.deepEqual(v.counts, ["3 points", "2 lines", "2 polygons", "2 points", "2 lines", "2 polygons"],
      "features per kind: point, linestring, polygon (interleaved), multipoint, multilinestring, multipolygon (separated)");
    assert.deepEqual(v.drawn, parts, "parts drawn per layer");
    console.log(`ok   explicit data: the six scenespec streams draw (parts ${JSON.stringify(parts)}), separated and authority-code CRS included`);

    for (const s of invalidSpec) {
      const r = await draw(s.scene, s.blobs);
      const [line, count] = expected[s.name];
      assert.equal(r.status, "ready", `${s.name}: the scene draws (${r.line})`);
      assert.equal(r.errors, "1", `${s.name}: one layer error (${r.line})`);
      assert.match(r.line, line, s.name);
      assert.deepEqual(r.counts, [count], s.name);
    }
    console.log(`ok   explicit data: each of the ${invalidSpec.length} scenespec invalid-data scenes is a layer error`);
  } finally {
    await browser12.close();
  }
}
