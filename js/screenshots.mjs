// Headless screenshots of rendered scene pages, light and dark.
//
//   node screenshots.mjs <dir-with-html> [outdir]
//
// Opens every .html file in <dir> in headless Chromium (WebGL through
// SwiftShader), waits for the renderer to report ready, and writes
// <name>-light.png and <name>-dark.png to outdir (default
// ../tools/screenshots). The browser is playwright-core's Chromium; set
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
      return s ? s.dataset.aobInfo || "" : "";
    });
    const out = join(outDir, `${basename(f, ".html")}-${scheme}.png`);
    await page.screenshot({ path: out });
    const errors = logs.filter((l) => /^(error|pageerror)/.test(l));
    console.log(`${status === "ready" ? "ok  " : "FAIL"} ${out} (${status}; ${info})`);
    logs.forEach((l) => console.log(`     ${l}`));
    if (status !== "ready" || errors.length) failed++;
    await ctx.close();
  }
}
await browser.close();
process.exit(failed ? 1 : 0);
