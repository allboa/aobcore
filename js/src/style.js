// Page chrome for the renderer: light and dark tokens, the map, a layer
// panel with legends, feature popups, a status line, and (served pages
// linked to R) the selection highlight colours and the link note. Dark follows prefers-color-scheme
// unless the root element has data-theme="light" or data-theme="dark".
// A fragment in a host document (aobcore scene_tag(), decision 0009) is an
// element with class aob-fragment: its own data-theme="light" or "dark"
// fixes its theme without touching the host's root element. The tokens
// are custom properties only (--aob-*), so a host page's own colours are
// never changed: color-scheme follows --aob-scheme on the renderer's own
// element (.aob-root), not on the root. A whole page sets its root's
// color-scheme itself (aobcore's page CSS).
const LIGHT = `--aob-bg: #eef2f4; --aob-panel: #f8fafb; --aob-ink: #1d2a33; --aob-muted: #5b6b76;
  --aob-rule: #d3dce1; --aob-accent: #1f6f8b; --aob-map: #dfe7eb; --aob-error: #a3322b;
  --aob-sel: #c2185b; --aob-sel-halo: #ffffff; --aob-sel-fill: #c2185b4d;
  --aob-scheme: light;`;
const DARK = `--aob-bg: #0f171c; --aob-panel: #152029; --aob-ink: #e3ebef; --aob-muted: #93a4ae;
  --aob-rule: #26343e; --aob-accent: #6cc0dd; --aob-map: #0b1216; --aob-error: #f08a80;
  --aob-sel: #ff7ab8; --aob-sel-halo: #000000; --aob-sel-fill: #ff7ab84d;
  --aob-scheme: dark;`;

export const CSS = `
:root {
  ${LIGHT}
  --aob-font: system-ui, -apple-system, "Segoe UI", sans-serif;
  --aob-mono: ui-monospace, "SFMono-Regular", Menlo, Consolas, monospace;
}
@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) {
    ${DARK}
  }
}
:root[data-theme="dark"] {
  ${DARK}
}
.aob-fragment[data-theme="light"] {
  ${LIGHT}
}
.aob-fragment[data-theme="dark"] {
  ${DARK}
}
.aob-root {
  color-scheme: var(--aob-scheme, light);
  position: relative; display: grid; grid-template-columns: minmax(0, 1fr) 260px;
  background: var(--aob-bg); color: var(--aob-ink); font: 14px/1.45 var(--aob-font);
  min-height: 320px; overflow: hidden;
}
.aob-map { position: relative; background: var(--aob-map); min-width: 0; min-height: 0; }
.aob-canvas { position: absolute; inset: 0; }
.aob-tag, .aob-readout {
  position: absolute; left: 12px; font: 12px var(--aob-mono); background: var(--aob-panel);
  color: var(--aob-ink); border: 1px solid var(--aob-rule); border-radius: 4px; padding: 3px 8px;
  pointer-events: none; font-variant-numeric: tabular-nums; white-space: pre;
}
.aob-tag { top: 12px; }
.aob-readout { bottom: 12px; }
.aob-readout:empty { display: none; }
.aob-panel {
  background: var(--aob-panel); border-left: 1px solid var(--aob-rule); padding: 12px 14px;
  overflow: auto; display: flex; flex-direction: column; gap: 12px;
}
.aob-h { font-size: 12px; font-weight: 600; letter-spacing: 0.04em; text-transform: uppercase;
  color: var(--aob-muted); margin: 0; }
.aob-layers { display: flex; flex-direction: column; gap: 6px; }
.aob-layer { display: grid; grid-template-columns: auto 1fr; column-gap: 8px; align-items: baseline; cursor: pointer; }
.aob-layer input { accent-color: var(--aob-accent); margin: 0; }
.aob-count { grid-column: 2; color: var(--aob-muted); font: 11px var(--aob-mono); }
.aob-legend-title { font-size: 12px; margin-bottom: 4px; }
.aob-legend-bar { height: 10px; border-radius: 2px; border: 1px solid var(--aob-rule); }
.aob-legend-scale { display: flex; justify-content: space-between; font: 11px var(--aob-mono);
  color: var(--aob-muted); margin-top: 2px; }
.aob-legends { display: flex; flex-direction: column; gap: 12px; }
.aob-legend[hidden] { display: none; }
.aob-legend-note { font-size: 12px; color: var(--aob-error); }
.aob-legend-classes { list-style: none; margin: 4px 0 0; padding: 0; display: flex; flex-direction: column; gap: 3px; }
.aob-legend-class { display: flex; align-items: center; gap: 8px; font-size: 12px; }
.aob-legend-na { margin-top: 4px; color: var(--aob-muted); }
.aob-legend-bar + .aob-legend-scale + .aob-legend-classes { margin-top: 6px; }
.aob-swatch {
  flex: none; width: 16px; height: 12px; border: 1px solid var(--aob-rule); border-radius: 2px; overflow: hidden;
  background: repeating-conic-gradient(var(--aob-rule) 0 25%, var(--aob-panel) 0 50%) 0 0 / 6px 6px;
}
.aob-swatch-fill { display: block; width: 100%; height: 100%; }
.aob-popup {
  position: absolute; z-index: 2; min-width: 160px; max-width: min(320px, 70%); max-height: 60%; overflow: auto;
  background: var(--aob-panel); color: var(--aob-ink); border: 1px solid var(--aob-rule); border-radius: 6px;
  box-shadow: 0 4px 14px rgba(0, 0, 0, 0.18); font-size: 12px;
}
.aob-popup[hidden] { display: none; }
.aob-popup:focus { outline: none; }
.aob-popup:focus-visible { outline: 2px solid var(--aob-accent); outline-offset: 2px; }
.aob-popup-head { display: flex; align-items: center; justify-content: space-between; gap: 8px;
  padding: 6px 6px 6px 10px; border-bottom: 1px solid var(--aob-rule); }
.aob-popup-title { font-weight: 600; overflow-wrap: anywhere; }
.aob-popup-close {
  font: inherit; font-size: 16px; line-height: 1; width: 24px; height: 24px; padding: 0; flex: none;
  color: var(--aob-ink); background: transparent; border: 1px solid transparent; border-radius: 4px; cursor: pointer;
}
.aob-popup-close:hover { border-color: var(--aob-rule); }
.aob-popup-close:focus-visible { outline: 2px solid var(--aob-accent); outline-offset: 1px; }
.aob-popup-close[hidden] { display: none; }
.aob-popup-body { display: grid; grid-template-columns: auto minmax(0, 1fr); column-gap: 10px; row-gap: 2px;
  margin: 0; padding: 6px 10px 8px; }
.aob-popup-body dt { color: var(--aob-muted); font-family: var(--aob-mono); font-size: 11px; padding-top: 1px; }
.aob-popup-body dd { margin: 0; overflow-wrap: anywhere; font-variant-numeric: tabular-nums; }
.aob-theme {
  margin-top: auto; align-self: flex-start; font: inherit; font-size: 12px; color: var(--aob-ink);
  background: transparent; border: 1px solid var(--aob-rule); border-radius: 4px; padding: 4px 10px; cursor: pointer;
}
.aob-theme:focus-visible, .aob-layer input:focus-visible { outline: 2px solid var(--aob-accent); outline-offset: 2px; }
.aob-status {
  position: absolute; left: 50%; top: 12px; transform: translateX(-50%); max-width: 70%;
  background: var(--aob-panel); border: 1px solid var(--aob-rule); border-radius: 4px;
  padding: 4px 10px; font-size: 12px; color: var(--aob-muted);
}
.aob-status[hidden] { display: none; }
.aob-link {
  position: absolute; right: 12px; bottom: 12px; max-width: min(360px, 70%); z-index: 1;
  background: var(--aob-panel); border: 1px solid var(--aob-rule); border-left: 3px solid var(--aob-error);
  border-radius: 4px; padding: 4px 10px; font-size: 12px; color: var(--aob-ink);
}
.aob-link[hidden] { display: none; }
.aob-status.aob-error { color: var(--aob-error); border-color: var(--aob-error); }
@media (max-width: 640px) {
  .aob-root { grid-template-columns: 1fr; grid-template-rows: minmax(0, 1fr) auto; }
  .aob-panel { border-left: 0; border-top: 1px solid var(--aob-rule); max-height: 40%; }
}
`;
