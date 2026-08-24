"""G3 - serve the REAL zarr store to a browser, three ways, so delivery designs can be compared.

The question G3 answers is not "is 38 MB a lot" (it is not) but "which delivery shape survives
1116 chunk files per timepoint". CLOUD_MIGRATION_ASSESSMENT section 3b already measured that the
20 KB chunk geometry is pathological for network backends - ~20x from chunking alone on CIFS - so
the same comparison over HTTP is the thing worth measuring, not raw bandwidth.

Three endpoints, three designs:

  /raw/<t>.<c>.<z>.<iy>.<ix>   the store as it is: one HTTP request per blosc-compressed chunk.
                               1116 requests per 4-channel timepoint. The browser would then have to
                               blosc-decode each one itself (WASM).
  /slab?t=&c=                  server reads + decodes one channel's whole (z,y,x) volume and sends
                               raw uint16, letting HTTP Content-Encoding do the compression. 4
                               requests per timepoint, and the browser decodes NOTHING - the network
                               stack does it natively, off the main thread.
  /meta                        geometry, so the page does not hardcode it.

`X-Server-Ms` on every response separates server work from transfer, so a slow number can be
attributed instead of guessed at.

Bind is 127.0.0.1 on a fixed port; this is a throwaway test server, deliberately NOT one of the
app's ports (8080/5173/7655/7656/7657/7660).

Run:  CCIA_REPO=<repo> pixi run python chunk_server.py --port 7788
"""
import argparse, json, os, sys, time
import http.server, socketserver, urllib.parse

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
STATE = {}


def build():
    """One entry per image uid. Multiple images so a phantom-vs-real comparison can hold DIMENSIONS
    fixed and vary only the data - fXgbTl (small, whole-plane chunks) and VJy1Nx (the real target)."""
    repo = os.environ.get('CCIA_REPO') or os.getcwd()
    sys.path.insert(0, os.path.join(repo, 'python'))
    import numcodecs
    a = STATE['args']
    STATE['imgs'] = {}
    STATE['zstd'] = numcodecs.Zstd(level=1)
    for uid in [u.strip() for u in a.uids.split(',') if u.strip()]:
        zpath = os.path.join(os.path.expanduser(a.projects_dir), a.project, '0', uid, a.version)
        lvl = os.path.join(zpath, '0')
        if not os.path.isdir(lvl):
            print('SKIP %s (no %s)' % (uid, lvl), flush=True)
            continue
        meta = json.load(open(os.path.join(lvl, '.zarray'), encoding='utf-8'))
        nt, nc, nz, ny, nx = meta['shape']
        cy_n, cx_n = meta['chunks'][3], meta['chunks'][4]
        STATE['imgs'][uid] = dict(
            uid=uid, zpath=zpath, lvl=lvl, meta=meta, nt=nt, nc=nc, nz=nz, ny=ny, nx=nx,
            cy=int(np.ceil(ny / cy_n)), cx=int(np.ceil(nx / cx_n)), cy_n=cy_n, cx_n=cx_n,
            codec=numcodecs.get_codec(meta['compressor']),
            # The two on-disk layouts are BOTH in use in the same project: VJy1Nx is flat
            # ('.', one file per chunk) and fXgbTl is nested ('/', a directory tree). Hardcoding
            # the flat form made every fXgbTl chunk look absent, so read_slab returned zeros and
            # the render would have been silently black. Read it, never assume it.
            sep=meta.get('dimension_separator', '.'))
        print('serving %s  shape=%s chunks=%s' % (uid, meta['shape'], meta['chunks']), flush=True)
    if not STATE['imgs']:
        raise SystemExit('no images found')


def img_of(q):
    uids = list(STATE['imgs'])
    return STATE['imgs'][q.get('uid', [uids[0]])[0]]


def chunk_keys(s):
    return [(z, iy, ix) for z in range(s['nz'])
            for iy in range(s['cy']) for ix in range(s['cx'])]


def chunk_path(s, t, c, z, iy, ix):
    return os.path.join(s['lvl'], s['sep'].join(str(v) for v in (t, c, z, iy, ix)))


def read_slab(s, t, c):
    """One channel's whole (z,y,x) volume as a contiguous uint16 array."""
    out = np.zeros((s['nz'], s['ny'], s['nx']), dtype=np.uint16)
    for z, iy, ix in chunk_keys(s):
        k = chunk_path(s, t, c, z, iy, ix)
        y0, x0 = iy * s['cy_n'], ix * s['cx_n']
        y1, x1 = min(y0 + s['cy_n'], s['ny']), min(x0 + s['cx_n'], s['nx'])
        if not os.path.exists(k):
            continue                                    # sparse store: absent chunk = fill_value 0
        buf = np.frombuffer(s['codec'].decode(open(k, 'rb').read()),
                            dtype=np.uint16).reshape(s['cy_n'], s['cx_n'])
        out[z, y0:y1, x0:x1] = buf[:y1 - y0, :x1 - x0]
    return out


def read_scale(s):
    """NGFF per-axis scale (t,c,z,y,x) from the multiscales .zattrs, so the browser gets the real
    voxel anisotropy instead of hardcoding it."""
    try:
        za = json.load(open(os.path.join(s['zpath'], '.zattrs'), encoding='utf-8'))
        ds = za['multiscales'][0]['datasets'][0]
        for ct in ds.get('coordinateTransformations', []):
            if ct.get('type') == 'scale':
                return ct['scale']
    except Exception:
        pass
    return None


class H(http.server.BaseHTTPRequestHandler):
    protocol_version = 'HTTP/1.1'                        # keep-alive: 1116 requests need it

    def log_message(self, *a):
        pass

    def _send(self, code, body, ctype, extra=None, encoding=None):
        self.send_response(code)
        self.send_header('Content-Type', ctype)
        self.send_header('Content-Length', str(len(body)))
        self.send_header('Cache-Control', 'no-store')
        if encoding:
            self.send_header('Content-Encoding', encoding)
        for k, v in (extra or {}).items():
            self.send_header(k, str(v))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        t0 = time.perf_counter()
        u = urllib.parse.urlparse(self.path)
        q = urllib.parse.parse_qs(u.query)
        s = STATE
        try:
            if u.path in ('/', '/index.html'):
                body = open(os.path.join(HERE, 'real_volume.html'), 'rb').read()
                return self._send(200, body, 'text/html; charset=utf-8')

            if u.path == '/timecourse':
                body = open(os.path.join(HERE, 'timecourse.html'), 'rb').read()
                return self._send(200, body, 'text/html; charset=utf-8')

            if u.path == '/chunk_bench':
                body = open(os.path.join(HERE, 'chunk_bench.html'), 'rb').read()
                return self._send(200, body, 'text/html; charset=utf-8')

            if u.path == '/images':
                return self._send(200, json.dumps(sorted(STATE['imgs'])).encode(), 'application/json')

            if u.path == '/meta':
                s = img_of(q)
                keys = chunk_keys(s)
                present = sum(1 for c in range(s['nc']) for (z, iy, ix) in keys
                              if os.path.exists(chunk_path(s, 0, c, z, iy, ix)))
                body = json.dumps({
                    'uid': s['uid'],
                    'shape': s['meta']['shape'], 'chunks': s['meta']['chunks'],
                    'compressor': s['meta']['compressor'], 'nz': s['nz'], 'ny': s['ny'],
                    'nx': s['nx'], 'nc': s['nc'], 'nt': s['nt'], 'cy': s['cy'], 'cx': s['cx'],
                    'chunks_per_timepoint_nominal': s['nz'] * s['cy'] * s['cx'] * s['nc'],
                    'chunks_per_timepoint_present': present,
                    'dimension_separator': s['sep'],
                    'bytes_per_channel_uncompressed': s['nz'] * s['ny'] * s['nx'] * 2,
                    'scale_um': read_scale(s),
                }).encode()
                return self._send(200, body, 'application/json',
                                  {'X-Server-Ms': round(1000 * (time.perf_counter() - t0), 1)})

            if u.path.startswith('/raw/'):
                # URLs always use dots; the store's own separator is applied here, so a client
                # never has to know which layout it is talking to.
                s = img_of(q)
                idx = u.path[5:].split('?')[0].split('.')
                p = os.path.join(s['lvl'], s['sep'].join(idx))
                if not os.path.exists(p):
                    return self._send(204, b'', 'application/octet-stream')   # sparse: absent chunk
                body = open(p, 'rb').read()
                return self._send(200, body, 'application/octet-stream',
                                  {'X-Server-Ms': round(1000 * (time.perf_counter() - t0), 2)})

            if u.path == '/slab':
                s = img_of(q)
                t = int(q.get('t', ['0'])[0]); c = int(q.get('c', ['0'])[0])
                arr = read_slab(s, t, c)
                raw = arr.tobytes()
                srv_read = round(1000 * (time.perf_counter() - t0), 1)
                acc = self.headers.get('Accept-Encoding', '')
                enc = None
                if 'zstd' in acc:
                    t1 = time.perf_counter()
                    raw = STATE['zstd'].encode(raw)
                    enc = 'zstd'
                    comp = round(1000 * (time.perf_counter() - t1), 1)
                else:
                    comp = 0.0
                return self._send(200, raw, 'application/octet-stream',
                                  {'X-Server-Ms': round(1000 * (time.perf_counter() - t0), 1),
                                   'X-Server-Read-Ms': srv_read, 'X-Server-Compress-Ms': comp,
                                   'X-Uncompressed-Bytes': arr.nbytes}, encoding=enc)

            return self._send(404, b'not found', 'text/plain')
        except Exception as e:
            return self._send(500, ('%s' % e).encode(), 'text/plain')


class Server(socketserver.ThreadingMixIn, http.server.HTTPServer):
    daemon_threads = True
    allow_reuse_address = True


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--project', default='zolIMa')
    ap.add_argument('--uids', default='fXgbTl,VJy1Nx')
    ap.add_argument('--version', default='ccidSmoothed.ome.zarr')
    ap.add_argument('--projects-dir', default='~/cecelia-feijoa/projects')
    ap.add_argument('--port', type=int, default=7788)
    STATE['args'] = ap.parse_args()
    build()
    srv = Server(('127.0.0.1', STATE['args'].port), H)
    print('READY http://127.0.0.1:%d/' % STATE['args'].port, flush=True)
    srv.serve_forever()


if __name__ == '__main__':
    main()
