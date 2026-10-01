// A channel to R (decision 0007, item 9): send(message), onMessage(f) and
// a connected state. This one is a websocket; a Shiny or webR channel would
// offer the same three things. Messages are JSON objects; frames are text.
//
// socketChannel(url, opts) opens the socket and keeps it open: when it
// closes it retries after a delay that starts at 1 s and doubles to 30 s,
// for as long as the page is open, except after a close code that says
// retrying cannot help (FINAL_CODES). opts.onState(state, info) hears
// "connecting", "open", "closed" (will retry; info.delay in ms) and
// "refused" (will not; info.code and info.reason). opts.WebSocket and
// opts.timers ({setTimeout, clearTimeout}) are for tests.

// 1003 a binary frame, 1007 not UTF-8 or not JSON, 1008 refused or out of
// order, 4000 a protocol R does not speak.
export const FINAL_CODES = [1003, 1007, 1008, 4000];
export const RETRY_FIRST_MS = 1000;
export const RETRY_MAX_MS = 30000;

// The websocket URL of `rel` (for example "ws") relative to a page URL:
// ws: for http:, wss: for https:.
export function socketUrl(rel, base) {
  const u = new URL(rel, base);
  if (u.protocol === "http:") u.protocol = "ws:";
  else if (u.protocol === "https:") u.protocol = "wss:";
  return u.href;
}

export function socketChannel(url, opts = {}) {
  const WS = opts.WebSocket || globalThis.WebSocket;
  const timers = opts.timers || globalThis;
  const onState = opts.onState || (() => {});
  const listeners = [];
  let ws = null;
  let open = false;
  let stopped = false;
  let delay = RETRY_FIRST_MS;
  let retry = null;

  const connect = () => {
    retry = null;
    if (stopped) return;
    onState("connecting", {});
    let sock;
    try {
      sock = new WS(url);
    } catch (err) {
      // A URL the browser will not open is not going to work later.
      onState("refused", { code: 0, reason: String(err && err.message ? err.message : err) });
      return;
    }
    ws = sock;
    sock.onopen = () => {
      if (ws !== sock) return;
      open = true;
      delay = RETRY_FIRST_MS;
      onState("open", {});
    };
    sock.onmessage = (e) => {
      if (ws !== sock || typeof e.data !== "string") return;
      let msg;
      try {
        msg = JSON.parse(e.data);
      } catch (err) {
        return;
      }
      if (!msg || typeof msg !== "object" || typeof msg.type !== "string") return;
      for (const f of listeners.slice()) f(msg);
    };
    sock.onclose = (e) => {
      if (ws !== sock) return;
      ws = null;
      open = false;
      if (stopped) return;
      const code = e && e.code;
      const reason = (e && e.reason) || "";
      if (FINAL_CODES.includes(code)) {
        onState("refused", { code, reason });
        return;
      }
      const wait = delay;
      delay = Math.min(delay * 2, RETRY_MAX_MS);
      onState("closed", { code, reason, delay: wait });
      retry = timers.setTimeout(connect, wait);
    };
  };
  connect();

  return {
    get connected() {
      return open;
    },
    // Sends one message; false when the socket is not open (nothing is
    // queued: the page sends its whole state again on reconnecting).
    send(msg) {
      if (!open || !ws) return false;
      ws.send(typeof msg === "string" ? msg : JSON.stringify(msg));
      return true;
    },
    // Calls f(message) for each message; returns a function that removes it.
    onMessage(f) {
      listeners.push(f);
      return () => {
        const i = listeners.indexOf(f);
        if (i >= 0) listeners.splice(i, 1);
      };
    },
    close() {
      stopped = true;
      if (retry !== null) timers.clearTimeout(retry);
      retry = null;
      if (ws) {
        const s = ws;
        ws = null;
        open = false;
        try {
          s.close(1000);
        } catch (err) {
          // already closing
        }
      }
    },
  };
}
