// Protocol 1 between a served page and R (decision 0007, item 2), over a
// channel (channel.js). The page sends hello, select and view; R sends
// hello and reload. Plain objects; no renderer library names here.
//
// linkToR(channel, page, opts) speaks for one rendered scene. `page` is the
// renderer's side:
//   serial            the scene serial the page was served with
//   renderer, specs   this renderer's version and the spec versions it draws
//   selection()       the whole selection, [{layer, rows}] with rows
//                     distinct, ascending and 0-based
//   view()            the settled camera for a view message, without
//                     type, scene and seq
//   setSelectable(ids) the layer ids R lets the viewer select
//   reload(serial)    reload the page, keeping the camera
//   note(text)        show (or, with null, clear) a note about the link
//   state(s)          the link's state, for tools and tests: "connecting",
//                     "open", "ready" (R said hello), "closed" or "refused"
// It returns {selected(trigger, at), viewChanged(), close()}.

export const PROTOCOL = 1;
export const VIEW_SETTLE_MS = 250;
export const VIEW_MIN_GAP_MS = 250; // at most four a second
export const DEFAULT_MAX_MESSAGE = 1048576;

export const NOT_CONNECTED = "Not connected to R: selections stay in this page";
export const TOO_MANY = "Not connected to R (too many pages are connected to R; retrying): selections stay in this page";

// Why a close that will not be retried happened, for the note.
const REFUSED = {
  1003: "the page sent a message R does not take",
  1007: "R could not read a message from the page",
  1008: "R refused this page",
  4000: "this page needs a newer aobcore",
};

export function refusedNote(code, reason) {
  const why = REFUSED[code] || (reason ? `closed: ${reason}` : `closed (${code})`);
  return `Not connected to R (${why}): selections stay in this page`;
}

// Bytes of a string as UTF-8.
export function utf8Length(s) {
  let n = 0;
  for (let i = 0; i < s.length; i++) {
    const c = s.charCodeAt(i);
    if (c < 0x80) n += 1;
    else if (c < 0x800) n += 2;
    else if (c >= 0xd800 && c <= 0xdbff) {
      n += 4;
      i++;
    } else n += 3;
  }
  return n;
}

export function linkToR(channel, page, opts = {}) {
  const timers = opts.timers || globalThis;
  const now = opts.now || (() => Date.now());
  let seq = 0;
  let ready = false;
  let helloed = 0; // R hellos seen: more than one is a reconnect
  let maxMessage = DEFAULT_MAX_MESSAGE;
  let lastTrigger = "click";
  let viewTimer = null;
  let lastViewAt = -Infinity;
  let linkNote = null;
  let sizeNote = null;
  const showNote = () => page.note([linkNote, sizeNote].filter(Boolean).join("; ") || null);

  // The channel is always given a message object; a websocket channel
  // writes it as JSON text (the size check above stringifies it itself).
  const send = (msg) => channel.send(msg);
  const next = (type, body) => ({ type, scene: page.serial, seq: ++seq, ...body });

  // The whole selection; not sent when larger than R takes.
  const sendSelect = (trigger, at) => {
    lastTrigger = trigger;
    if (!ready) return false;
    const msg = next("select", { trigger, items: page.selection() });
    if (at) msg.at = at;
    const text = JSON.stringify(msg);
    if (utf8Length(text) > maxMessage) {
      const n = msg.items.reduce((s, it) => s + it.rows.length, 0);
      sizeNote = `This selection (${n.toLocaleString("en-US")} features) is too large to send to R; it stays in this page`;
      showNote();
      return false;
    }
    if (sizeNote) {
      sizeNote = null;
      showNote();
    }
    return send(msg);
  };

  const sendView = () => {
    viewTimer = null;
    if (!ready) return;
    const gap = now() - lastViewAt;
    if (gap < VIEW_MIN_GAP_MS) {
      viewTimer = timers.setTimeout(sendView, VIEW_MIN_GAP_MS - gap);
      return;
    }
    lastViewAt = now();
    send(next("view", page.view()));
  };

  const offMessage = channel.onMessage((msg) => {
    if (msg.type === "hello") {
      if (msg.protocol !== PROTOCOL) return; // R closes with 4000
      ready = true;
      helloed++;
      if (typeof msg.max_message === "number" && msg.max_message > 0) maxMessage = msg.max_message;
      page.setSelectable(Array.isArray(msg.select) ? msg.select.filter((s) => typeof s === "string") : []);
      page.state("ready");
      if (channel.resetBackoff) channel.resetBackoff();
      // On reconnecting, the whole selection again (R may have lost it);
      // on the first hello only when the viewer selected before it came.
      const items = page.selection();
      if (helloed > 1 || items.length) sendSelect(lastTrigger);
      // And the camera as it is now.
      if (viewTimer !== null) timers.clearTimeout(viewTimer);
      sendView();
    } else if (msg.type === "reload") {
      // Only for a newer scene: a reload at this page's own serial would
      // reload it for ever.
      if (Number.isInteger(msg.scene) && msg.scene > page.serial) page.reload(msg.scene);
    }
  });

  return {
    open() {
      ready = false;
      linkNote = null;
      showNote();
      page.state("open");
      send({ type: "hello", protocol: PROTOCOL, renderer: page.renderer, specs: page.specs, scene: page.serial });
    },
    lost(state, info) {
      ready = false;
      if (viewTimer !== null) timers.clearTimeout(viewTimer);
      viewTimer = null;
      linkNote = state === "refused" ? refusedNote(info.code, info.reason) : info && info.code === 1013 ? TOO_MANY : NOT_CONNECTED;
      showNote();
      page.state(state);
    },
    selected: sendSelect,
    viewChanged() {
      if (!ready) return;
      if (viewTimer !== null) timers.clearTimeout(viewTimer);
      viewTimer = timers.setTimeout(sendView, VIEW_SETTLE_MS);
    },
    close() {
      offMessage();
      if (viewTimer !== null) timers.clearTimeout(viewTimer);
      viewTimer = null;
      ready = false;
    },
  };
}

// The channel's states drive the link: open sends hello, a close shows the
// note. Returns the link.
export function connectLink(makeChannel, page, opts) {
  let link = null;
  let early = null; // a refusal before the link exists (a URL that cannot open)
  const channel = makeChannel((state, info) => {
    if (state === "connecting") page.state("connecting");
    else if (!link) early = [state, info];
    else if (state === "open") link.open();
    else link.lost(state, info);
  });
  link = linkToR(channel, page, opts);
  if (early && early[0] !== "open") link.lost(early[0], early[1]);
  const close = link.close;
  link.close = () => {
    close();
    channel.close();
  };
  link.channel = channel;
  return link;
}
