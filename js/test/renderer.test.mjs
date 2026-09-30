// Renderer tests.
//
//   node test/renderer.test.mjs
//
// 1. palettes.js in Node: known names resolve, an unknown name throws, and
//    float32 nodata is matched.
// 2. The built bundle in headless Chromium: a scene whose raster names an
//    unknown palette still draws its other layers, the raster layer is not
//    drawn, and the status line reports "error: unknown palette".
// Set CHROMIUM_PATH to pick a browser; SKIP_BROWSER=1 runs only part 1.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { tableToIPC, tableFromArrays } from "apache-arrow";
import { paletteStops, colorize, UnknownPaletteError } from "../src/palettes.js";

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

if (process.env.SKIP_BROWSER) process.exit(0);

// ---- 2. unknown palette in a page -----------------------------------------
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
