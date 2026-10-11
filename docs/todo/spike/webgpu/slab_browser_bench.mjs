// The slab bench as the BROWSER sees it: fetch() from inside a page on the server's origin, so
// Chromium's own HTTP stack, flow-control windows and 6-per-origin limit apply (curl has none of them).
//
//   node slab_browser_bench.mjs <cdpPort> <origin> <tokenFile> <scrub|flat> <conc> <label> [probePath]
//
// Start Chromium first, pointed at the origin (any path that answers without the token):
//   chromium --headless=new --remote-debugging-port=9333 --ignore-certificate-errors \
//            --user-data-dir=/tmp/cdp [--disable-http2] https://127.0.0.1:8090/api/health
// Same request lists as slab_cache_bench.sh (MODE=scrub / MODE=flat). [probePath], e.g. /api/version:
// fetched every 25 ms during the run, reports small-request latency p50/p95/max.
// Env: PROJ, IMG, VN (valueName) — the store to read.
import { readFileSync } from 'node:fs';
const [cdp, origin, tokFile, mode, conc, label, probe = ''] = process.argv.slice(2);
const tok = readFileSync(tokFile, 'utf8').trim();
const targets = await (await fetch(`http://127.0.0.1:${cdp}/json/list`)).json();
const page = targets.find(t => t.type === 'page' && t.url.startsWith(origin));
const ws = new WebSocket(page.webSocketDebuggerUrl);
await new Promise(r => ws.addEventListener('open', r, { once: true }));
let id = 0; const pending = new Map();
ws.addEventListener('message', e => { const m = JSON.parse(e.data); if (pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); } });
const call = (method, params = {}) => new Promise(r => { const i = ++id; pending.set(i, r); ws.send(JSON.stringify({ id: i, method, params })); });
const urls = [];
const { PROJ = 'zolIMa', IMG = 'Dml3RG', VN = 'default' } = process.env;
const q = `projectUid=${PROJ}&imageUid=${IMG}&valueName=${VN}`;
if (mode === 'flat') { for (const t of [5,25,45,65,85,105,125,145]) for (let c = 0; c < 4; c++) urls.push(`/api/viewer/slab?${q}&t=${t}&c=${c}`); }
else for (const t of [10,60,110,160]) for (let by = 0; by < 8; by++) for (let bx = 0; bx < 8; bx++)
  urls.push(`/api/viewer/slab?${q}&t=${t}&c=0&cTo=3&z=0&zTo=30&x=${bx*128}&xTo=${bx*128+127}&y=${by*128}&yTo=${by*128+127}&level=0`);
const expr = `(async () => {
  const urls = ${JSON.stringify(urls)}; let next = 0, bytes = 0, bad = 0;
  const h = { Authorization: 'Bearer ${tok}' };
  const lat = []; let done = false;
  const prober = (async () => { if (!'${probe}') return;
    while (!done) { const s = performance.now(); await (await fetch('${probe}', { headers: h, cache: 'no-store' })).arrayBuffer();
      lat.push(performance.now() - s); await new Promise(r => setTimeout(r, 25)); } })();
  const t0 = performance.now();
  await Promise.all(Array.from({ length: ${Number(conc)} }, async () => {
    while (next < urls.length) { const u = urls[next++]; const r = await fetch(u, { headers: h, cache: 'no-store' });
      if (r.status !== 200) bad++; const n = (await r.arrayBuffer()).byteLength; bytes += n; } }));
  const wall = performance.now() - t0; done = true; await prober;
  lat.sort((a, b) => a - b); const pc = q => lat.length ? lat[Math.min(lat.length - 1, Math.floor(q * lat.length))].toFixed(0) : '-';
  const proto = performance.getEntriesByType('resource').slice(-1)[0]?.nextHopProtocol;
  performance.clearResourceTimings();
  return JSON.stringify({ wall, bytes, bad, proto, probe: lat.length ? 'n=' + lat.length + ' p50=' + pc(0.5) + ' p95=' + pc(0.95) + ' max=' + pc(1) + 'ms' : '' });
})()`;
const r = await call('Runtime.evaluate', { expression: expr, awaitPromise: true, returnByValue: true });
const o = JSON.parse(r.result.result.value);
console.log(`${label.padEnd(22)} wall=${(o.wall/1000).toFixed(2)}s proto=${o.proto} MB=${(o.bytes/1e6).toFixed(0)} bad=${o.bad} ${o.probe}`);
ws.close();
