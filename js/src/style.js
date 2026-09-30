// Page chrome for the renderer: light and dark tokens, the map, a layer
// panel with legends, and a status line. Dark follows prefers-color-scheme
// unless the root element has data-theme="light" or data-theme="dark".
export const CSS = `
:root {
  --aob-bg: #eef2f4; --aob-panel: #f8fafb; --aob-ink: #1d2a33; --aob-muted: #5b6b76;
  --aob-rule: #d3dce1; --aob-accent: #1f6f8b; --aob-map: #dfe7eb; --aob-error: #a3322b;
  --aob-font: system-ui, -apple-system, "Segoe UI", sans-serif;
  --aob-mono: ui-monospace, "SFMono-Regular", Menlo, Consolas, monospace;
  color-scheme: light;
}
@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) {
    --aob-bg: #0f171c; --aob-panel: #152029; --aob-ink: #e3ebef; --aob-muted: #93a4ae;
    --aob-rule: #26343e; --aob-accent: #6cc0dd; --aob-map: #0b1216; --aob-error: #f08a80;
    color-scheme: dark;
  }
}
:root[data-theme="dark"] {
  --aob-bg: #0f171c; --aob-panel: #152029; --aob-ink: #e3ebef; --aob-muted: #93a4ae;
  --aob-rule: #26343e; --aob-accent: #6cc0dd; --aob-map: #0b1216; --aob-error: #f08a80;
  color-scheme: dark;
}
.aob-root {
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
.aob-status.aob-error { color: var(--aob-error); border-color: var(--aob-error); }
@media (max-width: 640px) {
  .aob-root { grid-template-columns: 1fr; grid-template-rows: minmax(0, 1fr) auto; }
  .aob-panel { border-left: 0; border-top: 1px solid var(--aob-rule); max-height: 40%; }
}
`;
