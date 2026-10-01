// The page's selection in selection mode (decision 0007, item 6): a set of
// feature rows per layer id, rows being 0-based rows of the layer's Arrow
// data (every record batch in stream order), as popups use. Plain data; no
// renderer library names here.

export function selectionState() {
  const byLayer = new Map(); // layer id -> Set of rows
  const order = []; // layer ids in the order first selected
  const api = {
    has(layer, row) {
      const s = byLayer.get(layer);
      return !!s && s.has(row);
    },
    rowsOf(layer) {
      return byLayer.get(layer) || null;
    },
    get size() {
      let n = 0;
      for (const s of byLayer.values()) n += s.size;
      return n;
    },
    clear() {
      const had = api.size > 0;
      byLayer.clear();
      order.length = 0;
      return had;
    },
    add(layer, row) {
      let s = byLayer.get(layer);
      if (!s) {
        s = new Set();
        byLayer.set(layer, s);
        order.push(layer);
      }
      s.add(row);
    },
    remove(layer, row) {
      const s = byLayer.get(layer);
      if (!s) return;
      s.delete(row);
      if (!s.size) {
        byLayer.delete(layer);
        order.splice(order.indexOf(layer), 1);
      }
    },
    // Drop layers that can no longer be selected.
    keepLayers(ids) {
      for (const id of [...byLayer.keys()]) {
        if (ids.has(id)) continue;
        byLayer.delete(id);
        order.splice(order.indexOf(id), 1);
      }
    },
    // [{layer, rows}]: rows distinct and ascending, layers in the order
    // first selected.
    items() {
      return order.map((layer) => ({ layer, rows: [...byLayer.get(layer)].sort((a, b) => a - b) }));
    },
  };
  return api;
}

// A click in selection mode; returns the trigger of the select message it
// sends (every click sends one, so R also hears where a click on nothing
// was). `hit` is {layer, row} or null; `multi`
// is Shift or Cmd held. A click on a feature selects it and only it; with
// Shift or Cmd it adds or removes it; a click on nothing clears.
export function clickSelection(sel, hit, multi) {
  if (!hit) {
    sel.clear();
    return "click";
  }
  if (multi) {
    if (sel.has(hit.layer, hit.row)) sel.remove(hit.layer, hit.row);
    else sel.add(hit.layer, hit.row);
    return "toggle";
  }
  sel.clear();
  sel.add(hit.layer, hit.row);
  return "click";
}

// The selection as text for a data attribute: "layer:row,row;layer:row".
export function selectionText(items) {
  return items.map((it) => `${it.layer}:${it.rows.join(",")}`).join(";");
}
