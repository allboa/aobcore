// Scene spec 0.5 legends as page elements. A legend is data written by the
// producer: this draws it as given and works nothing out from the layer.
// Plain DOM; no renderer library names here.
import { paletteStops, UnknownPaletteError } from "./palettes.js";

function el(tag, cls, text) {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text !== undefined) e.textContent = text;
  return e;
}

export function rgbaCss(c) {
  const a = c.length > 3 ? c[3] / 255 : 1;
  return `rgba(${c[0]}, ${c[1]}, ${c[2]}, ${Number(a.toFixed(3))})`;
}

// A CSS gradient through 0.5 ramp stops ({at, color}); positions as given.
export function stopsGradient(stops) {
  return `linear-gradient(90deg, ${stops.map((s) => `${rgbaCss(s.color)} ${Number((s.at * 100).toFixed(3))}%`).join(", ")})`;
}

// A palette's colours as evenly spaced 0.5 stops.
export function paletteRampStops(name) {
  const cols = paletteStops(name);
  return cols.map((c, i) => ({ at: i / (cols.length - 1), color: [c[0], c[1], c[2], 255] }));
}

// Range end labels: numbers as written, trimmed to a readable precision.
export function rangeLabel(v) {
  if (Number.isInteger(v)) return String(v);
  return String(Number(v.toPrecision(6)));
}

function swatch(color) {
  const s = el("span", "aob-swatch");
  s.setAttribute("aria-hidden", "true");
  const fill = el("span", "aob-swatch-fill");
  fill.style.background = rgbaCss(color);
  s.append(fill);
  return s;
}

function classRow(c, extra) {
  const li = el("li", `aob-legend-class${extra ? ` ${extra}` : ""}`);
  li.append(swatch(c.color), el("span", "aob-legend-label", c.label));
  return li;
}

// One legend's element. `layer` is the layer it keys (for a fallback title);
// `error(msg)` reports a problem with the legend (an unknown palette).
export function legendElement(lg, layer, error) {
  const title = lg.title || (layer && (layer.label || layer.id)) || lg.layer;
  const box = el("section", "aob-legend");
  box.dataset.aobLegend = lg.layer;
  box.setAttribute("aria-label", `Legend: ${title}`);
  box.append(el("div", "aob-legend-title", title));
  if (lg.ramp) {
    const r = lg.ramp;
    let stops = r.stops;
    if (!stops) {
      try {
        stops = paletteRampStops(r.palette);
      } catch (err) {
        if (!(err instanceof UnknownPaletteError)) throw err;
        error(`legend for layer ${lg.layer}: ${err.message}`);
        box.append(el("div", "aob-legend-note", `unknown palette "${r.palette}"`));
        stops = null;
      }
    }
    if (stops) {
      const lo = rangeLabel(r.range[0]);
      const hi = rangeLabel(r.range[1]);
      const bar = el("div", "aob-legend-bar");
      bar.style.background = stopsGradient(stops);
      bar.setAttribute("role", "img");
      bar.setAttribute("aria-label", `Colour ramp from ${lo} to ${hi}`);
      const scale = el("div", "aob-legend-scale");
      scale.setAttribute("aria-hidden", "true");
      scale.append(el("span", "", lo), el("span", "", hi));
      box.append(bar, scale);
    }
  }
  const rows = (lg.classes || []).map((c) => classRow(c));
  if (lg.na) rows.push(classRow(lg.na, "aob-legend-na"));
  if (rows.length) {
    const list = el("ul", "aob-legend-classes");
    list.append(...rows);
    box.append(list);
  }
  return box;
}
