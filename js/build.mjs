// Bundle the renderer into one minified file for the R package.
//
//   node build.mjs          write ../inst/renderer/aob-renderer.min.js
//   node build.mjs --check  fail if the committed bundle differs from a fresh build
import { build } from "esbuild";
import { readFileSync, writeFileSync, mkdirSync, existsSync, readdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const outFile = join(here, "..", "inst", "renderer", "aob-renderer.min.js");
const copyrightsFile = join(here, "..", "inst", "COPYRIGHTS");
const pkg = JSON.parse(readFileSync(join(here, "package.json"), "utf8"));
const lock = JSON.parse(readFileSync(join(here, "package-lock.json"), "utf8"));
const ver = (name) => lock.packages[`node_modules/${name}`].version;

const banner =
  `/* aob-renderer ${pkg.version}: allonboard scene spec 0.1, 0.2 and 0.3 renderer. ` +
  `Built from js/ in allboa/aobcore with esbuild ${ver("esbuild")}; ` +
  `bundles deck.gl ${ver("@deck.gl/core")} (MIT) and apache-arrow ${ver("apache-arrow")} (Apache-2.0). ` +
  `Licenses: inst/COPYRIGHTS. */`;

const result = await build({
  entryPoints: [join(here, "src", "index.js")],
  bundle: true,
  minify: true,
  format: "iife",
  globalName: "aob",
  target: ["es2020"],
  platform: "browser",
  charset: "ascii",
  legalComments: "none",
  banner: { js: banner },
  define: { "process.env.NODE_ENV": '"production"' },
  write: false,
  metafile: true,
  logLevel: "warning",
});
const code = result.outputFiles[0].text;
const copyrights = copyrightNotice(result.metafile);

// The bundle is inlined into a <script> element, so it must not contain a
// closing script tag, and shipped files are ASCII only.
if (/<\/script/i.test(code)) throw new Error("bundle contains </script and cannot be inlined");
if (/[^\x00-\x7f]/.test(code)) throw new Error("bundle contains non-ASCII characters");

if (process.argv.includes("--check")) {
  let bad = 0;
  for (const [file, text] of [[outFile, code], [copyrightsFile, copyrights]]) {
    const committed = existsSync(file) ? readFileSync(file, "utf8") : "";
    if (committed !== text) {
      console.error(`${file} differs from a fresh build (${committed.length} vs ${text.length} bytes). Run npm run build in js/ and commit.`);
      bad++;
    }
  }
  if (bad) process.exit(1);
  console.log(`bundle up to date: ${code.length} bytes`);
} else {
  mkdirSync(dirname(outFile), { recursive: true });
  writeFileSync(outFile, code);
  writeFileSync(copyrightsFile, copyrights);
  console.log(`wrote ${outFile}: ${code.length} bytes (${(code.length / 1024).toFixed(0)} KiB)`);
  console.log(`wrote ${copyrightsFile}`);
}

// inst/COPYRIGHTS: every npm package with code in the bundle, its licence and
// copyright line, then each distinct licence text once.
function copyrightNotice(metafile) {
  const names = new Set();
  for (const input of Object.keys(Object.values(metafile.outputs)[0].inputs)) {
    const m = input.match(/node_modules\/((@[^/]+\/)?[^/]+)/);
    if (m) names.add(m[1]);
  }
  const texts = new Map();
  const notices = [];
  const rows = [...names].sort().map((name) => {
    const dir = join(here, "node_modules", name);
    const meta = JSON.parse(readFileSync(join(dir, "package.json"), "utf8"));
    const files = readdirSync(dir).filter((f) => /^(licen[cs]e|notice|copyrightnotice)/i.test(f)).sort();
    let holder = "";
    for (const f of files) {
      const line = readFileSync(join(dir, f), "utf8").split(/\r?\n/).find((l) => /^\s*Copyright\b/.test(l) && !/owner/.test(l));
      if (line && !holder) holder = line.trim();
    }
    if (!holder && meta.author) holder = `Copyright ${typeof meta.author === "string" ? meta.author : meta.author.name}`;
    for (const f of files.filter((f) => /^notice/i.test(f))) {
      notices.push([name, readFileSync(join(dir, f), "utf8").replace(/\r\n/g, "\n").trim()]);
    }
    const lic = files.find((f) => /^licen/i.test(f));
    if (lic) {
      const text = readFileSync(join(dir, lic), "utf8").replace(/\r\n/g, "\n").trim();
      if (!texts.has(meta.license)) texts.set(meta.license, text);
    }
    return `${name} ${meta.version} (${meta.license})${holder ? `\n    ${holder}` : ""}`;
  });
  const out = [
    "The file inst/renderer/aob-renderer.min.js bundles JavaScript from the",
    "npm packages below (built from js/ with esbuild; see js/package-lock.json).",
    "Their copyright holders and licences are listed here; the licence texts",
    "follow.",
    "",
    ...rows,
    "",
  ];
  for (const [lic, text] of texts) out.push(`---- ${lic} ----`, "", text, "");
  // Apache-2.0 section 4(d): NOTICE files travel with the work, verbatim.
  for (const [name, text] of notices) out.push(`---- NOTICE file of ${name} ----`, "", text, "");
  const s = out.join("\n");
  if (/[^\x00-\x7f]/.test(s)) throw new Error("COPYRIGHTS would contain non-ASCII characters");
  return s;
}
