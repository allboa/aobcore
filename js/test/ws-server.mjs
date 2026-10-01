// A minimal websocket server side (RFC 6455) on Node's http module, and an
// R stand-in that speaks protocol 1 as decision 0007 defines it, for the
// renderer's browser tests. Not shipped. Text frames only; no extensions.
//
//   const r = rStandIn({ serial: 1, select: ["zones"], origins: [base] });
//   server.on("upgrade", (req, sock, head) => r.upgrade(req, sock, head));
//
// r.messages holds every message from pages ({conn, msg}); r.reload(n)
// sends reload to every open page; r.stop() closes them with 1001.
import { createHash } from "node:crypto";

const GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

function frame(opcode, payload) {
  const n = payload.length;
  let head;
  if (n < 126) head = Buffer.from([0x80 | opcode, n]);
  else if (n < 65536) {
    head = Buffer.alloc(4);
    head[0] = 0x80 | opcode;
    head[1] = 126;
    head.writeUInt16BE(n, 2);
  } else {
    head = Buffer.alloc(10);
    head[0] = 0x80 | opcode;
    head[1] = 127;
    head.writeBigUInt64BE(BigInt(n), 2);
  }
  return Buffer.concat([head, payload]);
}

// Completes the handshake on `socket`; returns a connection.
export function acceptSocket(req, socket) {
  const key = req.headers["sec-websocket-key"];
  const accept = createHash("sha1").update(key + GUID).digest("base64");
  socket.write("HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n" +
    `Sec-WebSocket-Accept: ${accept}\r\n\r\n`);
  let buf = Buffer.alloc(0);
  let closed = false;
  const conn = {
    onText: () => {},
    onBinary: () => {},
    onClose: () => {},
    send(text) {
      if (!closed) socket.write(frame(0x1, Buffer.from(text, "utf8")));
    },
    close(code = 1000, reason = "") {
      if (closed) return;
      closed = true;
      const r = Buffer.from(reason, "utf8");
      const p = Buffer.alloc(2 + r.length);
      p.writeUInt16BE(code, 0);
      r.copy(p, 2);
      socket.write(frame(0x8, p));
      socket.end();
      conn.onClose(code);
    },
    get closed() {
      return closed;
    },
  };
  socket.on("data", (d) => {
    buf = Buffer.concat([buf, d]);
    for (;;) {
      if (buf.length < 2) return;
      const op = buf[0] & 0x0f;
      let n = buf[1] & 0x7f;
      let o = 2;
      if (n === 126) {
        if (buf.length < 4) return;
        n = buf.readUInt16BE(2);
        o = 4;
      } else if (n === 127) {
        if (buf.length < 10) return;
        n = Number(buf.readBigUInt64BE(2));
        o = 10;
      }
      const masked = (buf[1] & 0x80) !== 0;
      if (buf.length < o + (masked ? 4 : 0) + n) return;
      const mask = masked ? buf.subarray(o, o + 4) : null;
      o += masked ? 4 : 0;
      const payload = Buffer.from(buf.subarray(o, o + n));
      if (mask) for (let i = 0; i < n; i++) payload[i] ^= mask[i & 3];
      buf = buf.subarray(o + n);
      if (op === 0x1) conn.onText(payload.toString("utf8"));
      else if (op === 0x2) conn.onBinary(payload);
      else if (op === 0x8) {
        if (!closed) {
          closed = true;
          socket.write(frame(0x8, payload.subarray(0, 2)));
          socket.end();
          conn.onClose(payload.length >= 2 ? payload.readUInt16BE(0) : 1005);
        }
        return;
      } else if (op === 0x9) socket.write(frame(0xa, payload));
    }
  });
  socket.on("close", () => {
    if (!closed) {
      closed = true;
      conn.onClose(1006);
    }
  });
  socket.on("error", () => {});
  return conn;
}

// An R stand-in for protocol 1: checks the path and Origin, answers hello,
// records select and view, and sends reload for a stale scene.
export function rStandIn(opts) {
  const r = {
    serial: opts.serial,
    select: opts.select,
    spec: opts.spec || "0.5",
    maxMessage: opts.maxMessage || 1048576,
    path: opts.path || "/ws",
    origins: opts.origins,
    conns: new Set(),
    messages: [],
    closes: [],
    refused: [],
    n: 0,
    waiters: [],
  };
  const notify = () => {
    for (const w of r.waiters.slice()) {
      if (w.test()) {
        r.waiters.splice(r.waiters.indexOf(w), 1);
        w.resolve();
      }
    }
  };
  // Resolves when test() holds (checked after each message and close).
  r.until = (test, ms = 10000) => new Promise((resolve, reject) => {
    if (test()) return resolve();
    const w = { test, resolve };
    r.waiters.push(w);
    setTimeout(() => {
      const i = r.waiters.indexOf(w);
      if (i >= 0) {
        r.waiters.splice(i, 1);
        reject(new Error("timed out waiting on the R stand-in"));
      }
    }, ms);
  });
  r.of = (type) => r.messages.filter((m) => m.msg.type === type).map((m) => m.msg);
  r.upgrade = (req, socket) => {
    const path = new URL(req.url, "http://x").pathname;
    const origin = req.headers.origin;
    if (path !== r.path || !origin || !r.origins.includes(origin)) {
      r.refused.push({ path, origin });
      socket.end(`HTTP/1.1 ${path !== r.path ? 404 : 403} Refused\r\nContent-Length: 0\r\n\r\n`);
      notify();
      return;
    }
    const conn = acceptSocket(req, socket);
    const id = ++r.n;
    let helloed = false;
    r.conns.add(conn);
    conn.onBinary = () => conn.close(1003, "binary");
    conn.onClose = (code) => {
      r.conns.delete(conn);
      r.closes.push({ conn: id, code });
      notify();
    };
    conn.onText = (text) => {
      if (opts.limitText !== false && Buffer.byteLength(text) > r.maxMessage) return conn.close(1009, "too large");
      let msg;
      try {
        msg = JSON.parse(text);
      } catch (err) {
        return conn.close(1007, "not JSON");
      }
      if (!msg || typeof msg.type !== "string") return conn.close(1007, "no type");
      r.messages.push({ conn: id, msg });
      if (!helloed) {
        if (msg.type !== "hello") return conn.close(1008, "hello first");
        if (msg.protocol !== 1) return conn.close(4000, "protocol");
        helloed = true;
        conn.send(JSON.stringify({ type: "hello", protocol: 1, connection: id, scene: r.serial, spec: r.spec,
          select: r.select, max_message: r.maxMessage }));
      } else if ((msg.type === "select" || msg.type === "view") && msg.scene !== r.serial) {
        conn.send(JSON.stringify({ type: "reload", scene: r.serial }));
      }
      notify();
    };
  };
  r.reload = (serial) => {
    r.serial = serial;
    for (const c of r.conns) c.send(JSON.stringify({ type: "reload", scene: serial }));
  };
  r.stop = () => {
    for (const c of [...r.conns]) c.close(1001, "stopping");
  };
  return r;
}
