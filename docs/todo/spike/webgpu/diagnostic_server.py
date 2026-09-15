"""Local HTTP server that pairs with `diagnostic.html` — measures the fetch → decode →
writeBrick side of the upload pipeline end-to-end on the same box the browser runs on.

The standalone half of the diagnostic (`diagnostic.html`) covers the GPU-side upload path;
this server closes the loop by serving realistic brick-shaped payloads over HTTP so the
browser can measure fetch + `res.arrayBuffer()` + writeBrick together, at brick-realistic
sizes, at a sweep of concurrency (the `MAX_INFLIGHT` question #893 left open with only one
data point).

Precedent: `docs/todo/spike/webgpu/chunk_server.py` from PR #649. Same discipline — bind
127.0.0.1 on a throwaway port (NOT any of the app's ports 8080/5173/7655/7656/7657/7660),
threading HTTP/1.1 with keep-alive, `X-Server-Ms` on every response so a slow response can
be attributed instead of guessed at.

Endpoints:
  /                    serves diagnostic.html (same-origin, so no CORS to worry about)
  /diagnostic.html     same
  /synthetic-brick     synthetic bricks of the shape the client asks for. Answers the
                       "network + decode + writeBrick end-to-end" question without needing
                       a real project. Query: bx=&by=&bz=&nc=&bpv=&enc=identity|zstd
                       Emits an `X-Slab-Shape: nc,nz,ny,nx` header matching the real API's
                       format so the client can parse it exactly as `brickLoader.ts` does.
  /real-brick          reads a brick out of a real zarr level and returns it in the same
                       (c,z,y,x) column-major layout the server-side slab route uses. Query:
                       project=&uid=&version=&level=&t=&x=&xTo=&y=&yTo=&z=&zTo=&c=&cTo=&enc=
                       Optional — synthetic covers the network-throughput question fine; use
                       real when you want to catch codec/IO cost on THIS storage.
  /meta                lists what's available on real-brick (project + uids).

Run:  pixi run python docs/todo/spike/webgpu/diagnostic_server.py --port 7789
      then open http://127.0.0.1:7789/ in Chrome/Chromium.

Read-only; nothing here writes back to the projects dir. Doesn't collide with `cecelia dev`
— separate port, separate process.
"""
import argparse, json, os, sys, time
import http.server, socketserver, urllib.parse

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
STATE = {}


def _load_zstd():
    """`numcodecs.Zstd` — the same codec cecelia writes with. Loaded once at startup so a
    missing dependency fails loud instead of on the first zstd request."""
    import numcodecs
    return numcodecs.Zstd(level=1)


def _build_real_registry(projects_dir, project, uids, version):
    """Discover real zarr levels for optional /real-brick. Silently drops uids that don't
    resolve so a stale config never blocks the network measurement — synthetic still works."""
    reg = {}
    for uid in [u.strip() for u in uids if u.strip()]:
        zpath = os.path.join(os.path.expanduser(projects_dir), project, '0', uid, version)
        if not os.path.isdir(zpath):
            print('SKIP %s: no %s' % (uid, zpath), flush=True)
            continue
        # discover level dirs (`0`, `1`, ...)
        levels = {}
        for name in sorted(os.listdir(zpath)):
            lvl = os.path.join(zpath, name)
            zarr_meta = os.path.join(lvl, '.zarray')
            if not os.path.isfile(zarr_meta):
                continue
            try:
                meta = json.load(open(zarr_meta, encoding='utf-8'))
            except Exception:
                continue
            nt, nc, nz, ny, nx = meta['shape']
            cy_n, cx_n = meta['chunks'][3], meta['chunks'][4]
            import numcodecs
            levels[name] = dict(
                path=lvl, meta=meta, nt=nt, nc=nc, nz=nz, ny=ny, nx=nx,
                cy=int(np.ceil(ny / cy_n)), cx=int(np.ceil(nx / cx_n)),
                cy_n=cy_n, cx_n=cx_n,
                codec=numcodecs.get_codec(meta['compressor']),
                sep=meta.get('dimension_separator', '.'),
            )
        if levels:
            reg[uid] = levels
            print('serving %s levels=%s' % (uid, sorted(levels)), flush=True)
    return reg


def _read_real_brick(lvl, t, c, cTo, x, xTo, y, yTo, z, zTo):
    """Read one brick × (c..cTo) from a zarr level and return raw bytes in (c,z,y,x)
    column-major layout — same shape the real /api/viewer/slab route emits."""
    nc_out = cTo - c + 1
    nz_out, ny_out, nx_out = zTo - z + 1, yTo - y + 1, xTo - x + 1
    out = np.zeros((nc_out, nz_out, ny_out, nx_out), dtype=np.uint16)
    cy_n, cx_n = lvl['cy_n'], lvl['cx_n']
    for ci, cc in enumerate(range(c, cTo + 1)):
        for zi, zz in enumerate(range(z, zTo + 1)):
            iy0, iy1 = y // cy_n, (yTo) // cy_n
            ix0, ix1 = x // cx_n, (xTo) // cx_n
            for iy in range(iy0, iy1 + 1):
                for ix in range(ix0, ix1 + 1):
                    k = os.path.join(lvl['path'],
                                     lvl['sep'].join(str(v) for v in (t, cc, zz, iy, ix)))
                    if not os.path.exists(k):
                        continue
                    buf = np.frombuffer(lvl['codec'].decode(open(k, 'rb').read()),
                                        dtype=np.uint16).reshape(cy_n, cx_n)
                    yy0, xx0 = iy * cy_n, ix * cx_n
                    dy0, dy1 = max(0, y - yy0), min(cy_n, yTo + 1 - yy0)
                    dx0, dx1 = max(0, x - xx0), min(cx_n, xTo + 1 - xx0)
                    oy = yy0 + dy0 - y
                    ox = xx0 + dx0 - x
                    out[ci, zi, oy:oy + (dy1 - dy0), ox:ox + (dx1 - dx0)] = \
                        buf[dy0:dy1, dx0:dx1]
    return out.tobytes(), (nc_out, nz_out, ny_out, nx_out)


class Handler(http.server.BaseHTTPRequestHandler):
    # HTTP/1.0 forces one-request-per-socket. Tried HTTP/1.1 + keep-alive first and
    # Chromium wedged at N>16 concurrent — Python's BaseHTTPRequestHandler is single-
    # threaded per connection and the pool starved. HTTP/1.0 with `Connection: close`
    # gives up keep-alive perf (irrelevant on localhost) for zero queueing surprises.
    protocol_version = 'HTTP/1.0'

    def log_message(self, *a):
        pass

    def _send(self, code, body, ctype, extra=None, encoding=None):
        self.send_response(code)
        self.send_header('Content-Type', ctype)
        self.send_header('Content-Length', str(len(body)))
        self.send_header('Cache-Control', 'no-store')
        # Explicit close so Chromium doesn't hold sockets open expecting keep-alive.
        self.send_header('Connection', 'close')
        # Same-origin: the diagnostic HTML is served from here too, so no CORS headers
        # needed. Adding permissive CORS anyway for the case where someone opens the file
        # via file:// and points its fetches at this server explicitly.
        self.send_header('Access-Control-Allow-Origin', '*')
        if encoding:
            self.send_header('Content-Encoding', encoding)
        for k, v in (extra or {}).items():
            self.send_header(k, str(v))
        self.end_headers()
        self.wfile.write(body)
        self.close_connection = True

    def _serve_static(self, path):
        p = os.path.join(HERE, path)
        if not os.path.isfile(p):
            return self._send(404, b'not found', 'text/plain')
        body = open(p, 'rb').read()
        ctype = 'text/html; charset=utf-8' if path.endswith('.html') else 'application/octet-stream'
        return self._send(200, body, ctype)

    def do_GET(self):
        t0 = time.perf_counter()
        u = urllib.parse.urlparse(self.path)
        q = {k: v[0] for k, v in urllib.parse.parse_qs(u.query).items()}
        try:
            if u.path in ('/', '/index.html', '/diagnostic.html'):
                return self._serve_static('diagnostic.html')

            if u.path == '/meta':
                real = STATE.get('real', {})
                return self._send(200, json.dumps({
                    'synthetic': True,
                    'real': {uid: sorted(lvls) for uid, lvls in real.items()},
                    'project': STATE['args'].project,
                    'version': STATE['args'].version,
                    'ports_to_avoid': [8080, 5173, 7655, 7656, 7657, 7660],
                }).encode(), 'application/json',
                    {'X-Server-Ms': round(1000 * (time.perf_counter() - t0), 2)})

            if u.path == '/synthetic-brick':
                bx = int(q.get('bx', 128)); by = int(q.get('by', 128)); bz = int(q.get('bz', 32))
                nc = int(q.get('nc', 4));   bpv = int(q.get('bpv', 2))
                enc = q.get('enc', 'identity')
                # `X-Slab-Shape: nc,nz,ny,nx` — same header brickLoader.parseBrickSlabShape reads.
                shape_header = f'{nc},{bz},{by},{bx}'
                size = bx * by * bz * nc * bpv
                # Cheap non-zero pattern so the driver can't optimise a zero page. Realloc
                # per request so we're honest about server-side allocation cost too.
                t_alloc = time.perf_counter()
                buf = bytearray(size)
                # Sparse fill: every 997 bytes gets a byte. Matches the client's fill.
                for i in range(0, size, 997):
                    buf[i] = i & 0xff
                srv_alloc_ms = round(1000 * (time.perf_counter() - t_alloc), 3)
                body = bytes(buf)
                encoding = None
                srv_compress_ms = 0.0
                if enc == 'zstd':
                    t_c = time.perf_counter()
                    body = STATE['zstd'].encode(body)
                    encoding = 'zstd'
                    srv_compress_ms = round(1000 * (time.perf_counter() - t_c), 2)
                headers = {
                    'X-Slab-Shape': shape_header,
                    'X-Server-Ms': round(1000 * (time.perf_counter() - t0), 2),
                    'X-Server-Alloc-Ms': srv_alloc_ms,
                    'X-Server-Compress-Ms': srv_compress_ms,
                    'X-Uncompressed-Bytes': size,
                }
                return self._send(200, body, 'application/octet-stream', headers, encoding=encoding)

            if u.path == '/real-brick':
                real = STATE.get('real', {})
                uid = q.get('uid') or (next(iter(real)) if real else None)
                if not uid or uid not in real:
                    return self._send(404, b'no real image; pass ?uid=<one of /meta>',
                                      'text/plain')
                level = q.get('level', '0')
                if level not in real[uid]:
                    return self._send(404, ('no level %s for %s' % (level, uid)).encode(),
                                      'text/plain')
                lvl = real[uid][level]
                t = int(q.get('t', 0)); c = int(q.get('c', 0))
                cTo = int(q.get('cTo', lvl['nc'] - 1))
                x = int(q.get('x', 0));    xTo = int(q.get('xTo', lvl['nx'] - 1))
                y = int(q.get('y', 0));    yTo = int(q.get('yTo', lvl['ny'] - 1))
                z = int(q.get('z', 0));    zTo = int(q.get('zTo', lvl['nz'] - 1))
                t_read = time.perf_counter()
                body, (nc_o, nz_o, ny_o, nx_o) = _read_real_brick(
                    lvl, t, c, cTo, x, xTo, y, yTo, z, zTo)
                srv_read_ms = round(1000 * (time.perf_counter() - t_read), 2)
                enc = q.get('enc', 'identity')
                encoding = None
                srv_compress_ms = 0.0
                if enc == 'zstd':
                    t_c = time.perf_counter()
                    body = STATE['zstd'].encode(body)
                    encoding = 'zstd'
                    srv_compress_ms = round(1000 * (time.perf_counter() - t_c), 2)
                headers = {
                    'X-Slab-Shape': f'{nc_o},{nz_o},{ny_o},{nx_o}',
                    'X-Server-Ms': round(1000 * (time.perf_counter() - t0), 2),
                    'X-Server-Read-Ms': srv_read_ms,
                    'X-Server-Compress-Ms': srv_compress_ms,
                    'X-Uncompressed-Bytes': nc_o * nz_o * ny_o * nx_o * 2,
                }
                return self._send(200, body, 'application/octet-stream', headers,
                                  encoding=encoding)

            return self._send(404, b'not found', 'text/plain')
        except Exception as e:
            import traceback
            traceback.print_exc()
            return self._send(500, ('%s' % e).encode(), 'text/plain')


class Server(socketserver.ThreadingMixIn, http.server.HTTPServer):
    daemon_threads = True
    allow_reuse_address = True


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--port', type=int, default=7789,
                    help='throwaway port; keep off 8080/5173/7655-60 (cecelia app ports)')
    ap.add_argument('--project', default='zolIMa')
    ap.add_argument('--uids', default='fXgbTl,VJy1Nx',
                    help='comma-separated image uids for /real-brick (optional)')
    ap.add_argument('--version', default='ccidSmoothed.ome.zarr')
    ap.add_argument('--projects-dir', default='~/cecelia-feijoa/projects')
    STATE['args'] = ap.parse_args()
    STATE['zstd'] = _load_zstd()
    STATE['real'] = _build_real_registry(
        STATE['args'].projects_dir, STATE['args'].project,
        STATE['args'].uids.split(','), STATE['args'].version)
    srv = Server(('127.0.0.1', STATE['args'].port), Handler)
    print('READY http://127.0.0.1:%d/' % STATE['args'].port, flush=True)
    srv.serve_forever()


if __name__ == '__main__':
    main()
