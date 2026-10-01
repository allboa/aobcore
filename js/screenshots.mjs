// Headless screenshots of rendered scene pages, light and dark.
//
//   node screenshots.mjs <dir-with-html> [outdir]
//
// Opens every .html file in <dir> in headless Chromium (WebGL through
// SwiftShader), waits for the renderer to report ready, and writes
// <name>-light.png and <name>-dark.png to outdir (default
// ../tools/screenshots). A scene with a scene spec 0.5 popup on a point
// layer (trigger select) is also taken with the popup of that layer's first
// feature open, as <name>-popup-light.png and <name>-popup-dark.png. The browser is playwright-core's Chromium; set
// CHROMIUM_PATH to use a specific executable.
import { chromium } from "playwright-core";
import { readdirSync, mkdirSync } from "node:fs";
import { join, resolve, basename, dirname } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const [inDir, outArg] = process.argv.slice(2);
if (!inDir) {
  console.error("usage: node screenshots.mjs <dir-with-html> [outdir]");
  process.exit(2);
}
const outDir = resolve(outArg || join(here, "..", "tools", "screenshots"));
mkdirSync(outDir, { recursive: true });

const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH || undefined,
  args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
});
let failed = 0;
const pages = readdirSync(inDir).filter((f) => f.endsWith(".html")).sort();
for (const f of pages) {
  for (const scheme of ["light", "dark"]) {
    const ctx = await browser.newContext({ viewport: { width: 1100, height: 760 }, colorScheme: scheme, deviceScaleFactor: 1 });
    const page = await ctx.newPage();
    const logs = [];
    page.on("console", (m) => logs.push(`${m.type()}: ${m.text()}`));
    page.on("pageerror", (e) => logs.push(`pageerror: ${e.message}`));
    await page.goto(pathToFileURL(resolve(inDir, f)).href);
    const status = await page
      .waitForFunction(() => {
        const s = document.querySelector("[data-aob-scene]:not(script)");
        return s && (s.dataset.aobStatus === "ready" || s.dataset.aobStatus === "error") && s.dataset.aobStatus;
      }, null, { timeout: 60000 })
      .then((h) => h.jsonValue())
      .catch(() => "timeout");
    await page.waitForTimeout(300);
    const info = await page.evaluate(() => {
      const s = document.querySelector("[data-aob-scene]:not(script)");
      return s ? [s.dataset.aobInfo, s.dataset.aobTiles].filter(Boolean).join("; ") : "";
    });
    const out = join(outDir, `${basename(f, ".html")}-${scheme}.png`);
    await page.screenshot({ path: out });
    let popupOk = true;
    if (status === "ready") {
      const at = await page.evaluate(firstSelectPoint);
      if (at) {
        await page.mouse.click(at.x, at.y);
        popupOk = await page.waitForFunction(() => {
          const p = document.querySelector(".aob-popup");
          return p && !p.hidden;
        }, null, { timeout: 5000 }).then(() => true).catch(() => false);
        await page.waitForTimeout(200);
        const pout = join(outDir, `${basename(f, ".html")}-popup-${scheme}.png`);
        await page.screenshot({ path: pout });
        console.log(`${popupOk ? "ok  " : "FAIL"} ${pout} (popup ${at.layer} row 0)`);
      }
    }
    const errors = logs.filter((l) => /^(error|pageerror)/.test(l));
    if (!popupOk) failed++;
    console.log(`${status === "ready" ? "ok  " : "FAIL"} ${out} (${status}; ${info})`);
    logs.forEach((l) => console.log(`     ${l}`));
    if (status !== "ready" || errors.length) failed++;
    await ctx.close();
  }
}
await browser.close();
process.exit(failed ? 1 : 0);

// In the page: the screen position of the first feature of the first point
// layer with a select popup, or null. Orthographic views only.
function firstSelectPoint() {
  const c = document.querySelector("[data-aob-scene]:not(script)");
  const h = c && c.aob;
  if (!h || h.scene.view.type === "globe") return null;
  const L = h.scene.layers.find((l) => l.popup && (l.popup.trigger || "select") === "select" &&
    /point$/.test(h.scene.data[l.data].geometry.encoding));
  if (!L) return null;
  const ref = h.scene.data[L.data];
  let g = h.tables[L.data].getChild(ref.geometry.column).get(0);
  while (g && typeof g.get === "function" && typeof g.get(0) !== "number") g = g.get(0);
  let [x, y] = [g.get(0), g.get(1)];
  if (ref.origin_subtracted) [x, y] = [x + h.scene.view.local_origin[0], y + h.scene.view.local_origin[1]];
  const canvas = c.querySelector(".aob-canvas");
  const r = canvas.getBoundingClientRect();
  const v = h.view();
  const k = Math.pow(2, v.zoom);
  return { layer: L.id, x: r.left + r.width / 2 + (x - v.target[0]) * k, y: r.top + r.height / 2 - (y - v.target[1]) * k };
}
