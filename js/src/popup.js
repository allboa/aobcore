// Scene spec 0.5 popups: one feature's attribute values as text, labelled
// by column name. Plain DOM and Arrow; no renderer library names here.
import { Type } from "apache-arrow";

function el(tag, cls, text) {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text !== undefined) e.textContent = text;
  return e;
}

const pad = (n, w = 2) => String(n).padStart(w, "0");

// A cell as text. Strings (ISO dates and times written as text included)
// pass through unchanged; Arrow dates and timestamps are written as ISO
// 8601 in UTC; missing values are "NA".
export function cellText(value, type) {
  if (value === null || value === undefined) return "NA";
  const id = type ? type.typeId : undefined;
  if (id === Type.Date || id === Type.DateDay || id === Type.DateMillisecond) {
    const d = value instanceof Date ? value : new Date(Number(value));
    return `${d.getUTCFullYear()}-${pad(d.getUTCMonth() + 1)}-${pad(d.getUTCDate())}`;
  }
  if (id === Type.Timestamp || id === Type.TimestampSecond || id === Type.TimestampMillisecond ||
      id === Type.TimestampMicrosecond || id === Type.TimestampNanosecond) {
    const d = value instanceof Date ? value : new Date(Number(value));
    return isNaN(d.getTime()) ? String(value) : d.toISOString().replace(".000Z", "Z");
  }
  if (typeof value === "string") return value;
  if (typeof value === "bigint" || typeof value === "boolean") return String(value);
  if (typeof value === "number") {
    if (Number.isNaN(value)) return "NaN";
    if (Number.isInteger(value) || !isFinite(value)) return String(value);
    return String(Number(value.toPrecision(10)));
  }
  if (value instanceof Date) return value.toISOString();
  if (value && typeof value.toArray === "function") {
    return Array.from(value.toArray(), (v) => cellText(v)).join(", ");
  }
  if (ArrayBuffer.isView(value)) return Array.from(value, (v) => cellText(v)).join(", ");
  return String(value);
}

// The named columns of one row as [name, text] pairs.
export function popupRows(table, columns, row) {
  return columns.map((name) => {
    const col = table.getChild(name);
    const field = table.schema.fields.find((f) => f.name === name);
    return [name, cellText(col.get(row), field && field.type)];
  });
}

// The popup element inside the map. show() fills and places it; hide()
// removes it. Escape and the close button dismiss it.
export function popupBox(map, onClose) {
  const box = el("div", "aob-popup");
  box.setAttribute("role", "dialog");
  box.tabIndex = -1;
  box.hidden = true;
  const head = el("div", "aob-popup-head");
  const title = el("div", "aob-popup-title");
  title.id = `aob-popup-title-${Math.random().toString(36).slice(2, 9)}`;
  box.setAttribute("aria-labelledby", title.id);
  const close = el("button", "aob-popup-close", "\u00d7");
  close.type = "button";
  close.setAttribute("aria-label", "Close");
  const body = el("dl", "aob-popup-body");
  head.append(title, close);
  box.append(head, body);
  map.append(box);
  let current = null;
  const hide = (restoreFocus) => {
    if (box.hidden) return;
    const hadFocus = box.contains(document.activeElement);
    box.hidden = true;
    current = null;
    if (restoreFocus || hadFocus) map.focus({ preventScroll: true });
    if (onClose) onClose();
  };
  close.addEventListener("click", () => hide(true));
  map.addEventListener("keydown", (e) => {
    if (e.key === "Escape" && !box.hidden) hide(true);
  });
  box.addEventListener("keydown", (e) => {
    if (e.key === "Escape") {
      e.stopPropagation();
      hide(true);
    }
  });
  return {
    element: box,
    get current() {
      return current;
    },
    // sel: {layer, row, rows: [[name, text]], x, y, trigger}
    show(sel, focus) {
      current = sel;
      title.textContent = sel.layer.label || sel.layer.id;
      body.textContent = "";
      for (const [k, v] of sel.rows) body.append(el("dt", "", k), el("dd", "", v));
      box.dataset.aobLayer = sel.layer.id;
      box.dataset.aobRow = String(sel.row);
      box.dataset.aobTrigger = sel.trigger;
      close.hidden = sel.trigger === "point";
      box.hidden = false;
      // Place beside the point, kept inside the map.
      const w = map.clientWidth;
      const h = map.clientHeight;
      const bw = box.offsetWidth;
      const bh = box.offsetHeight;
      let x = sel.x + 12;
      let y = sel.y + 12;
      if (x + bw > w - 8) x = Math.max(8, sel.x - bw - 12);
      if (y + bh > h - 8) y = Math.max(8, sel.y - bh - 12);
      box.style.left = `${x}px`;
      box.style.top = `${y}px`;
      if (focus) box.focus({ preventScroll: true });
    },
    hide,
  };
}
