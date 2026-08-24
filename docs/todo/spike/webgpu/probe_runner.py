#!/usr/bin/env python3
"""G0 harness: serve a WebGPU probe page over http://127.0.0.1 and collect its result.

Why a server rather than `firefox --screenshot`: the screenshot fires on load, before
`requestAdapter()` resolves, so it captures "probing…". The page POSTs its result back instead.
http://127.0.0.1 is also a secure context, which file:// is not — and WebGPU is
secure-context-only, so this removes that as a confound.

Usage: python3 probe_runner.py [--prefs KEY=VAL ...] [--page FILE]
"""
import argparse, http.server, json, os, shutil, socket, subprocess, sys, tempfile, threading, time

HERE = os.path.dirname(os.path.abspath(__file__))
result = {}
done = threading.Event()


def make_handler(page):
    class H(http.server.SimpleHTTPRequestHandler):
        def __init__(self, *a, **kw):
            super().__init__(*a, directory=HERE, **kw)

        def do_GET(self):
            if self.path.startswith('/result?'):
                # The page reports by NAVIGATING here, not by fetch(): a navigation is the one
                # channel that is trivially observable from the server side.
                import urllib.parse as up
                q = up.parse_qs(up.urlparse(self.path).query)
                try:
                    result.update(json.loads(q.get('data', ['{}'])[0]))
                except Exception:
                    result['raw'] = q.get('data', [''])[0]
                self.send_response(200)
                self.send_header('Content-Type', 'text/plain')
                self.end_headers()
                self.wfile.write(b'ok')
                done.set()
                return
            if self.path in ('/', '/index.html'):
                self.path = '/' + page
            return super().do_GET()

        def do_POST(self):
            n = int(self.headers.get('Content-Length', 0))
            body = self.rfile.read(n).decode('utf8', 'replace')
            try:
                result.update(json.loads(body))
            except Exception:
                result['raw'] = body
            self.send_response(204)
            self.end_headers()
            done.set()

        def log_message(self, fmt, *a):
            print('[srv] ' + (fmt % a), flush=True)
    return H


def free_port():
    s = socket.socket()
    s.bind(('127.0.0.1', 0))
    p = s.getsockname()[1]
    s.close()
    return p


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--prefs', nargs='*', default=[])
    ap.add_argument('--page', default='adapter_probe.html')
    ap.add_argument('--timeout', type=float, default=60)
    ap.add_argument('--shot', default=os.path.expanduser('~/Downloads/TMP/webgpu_probe.png'))
    a = ap.parse_args()

    port = free_port()
    srv = http.server.ThreadingHTTPServer(('127.0.0.1', port), make_handler(a.page))
    threading.Thread(target=srv.serve_forever, daemon=True).start()

    # firefox here is a SNAP: it cannot read /tmp, and its `home` interface denies TOP-LEVEL
    # dot-directories, so ~/.cache is refused too (it fails as "Firefox is already running", not as
    # a permission error). A dir under the repo works. Never his real profile.
    prof_root = os.environ.get('PROBE_PROFILE_ROOT') or os.path.join(HERE, '_profiles')
    os.makedirs(prof_root, exist_ok=True)
    prof = tempfile.mkdtemp(prefix='ffprof-', dir=prof_root)
    with open(os.path.join(prof, 'user.js'), 'w') as fh:
        for p in a.prefs:
            k, _, v = p.partition('=')
            fh.write('user_pref("%s", %s);\n' % (k, v))
    env = dict(os.environ, __NV_PRIME_RENDER_OFFLOAD='1', __GLX_VENDOR_LIBRARY_NAME='nvidia')
    proc = subprocess.Popen(
        ['firefox', '--headless', '--profile', prof, '--no-remote',
         'http://127.0.0.1:%d/?post=1' % port],
        env=env, stdout=None, stderr=None)
    try:
        done.wait(a.timeout)
    finally:
        proc.terminate()
        try:
            proc.wait(10)
        except Exception:
            proc.kill()
        srv.shutdown()
        shutil.rmtree(prof, ignore_errors=True)

    if not result:
        print('NO RESULT (timeout after %.0fs)' % a.timeout)
        sys.exit(1)
    print(json.dumps(result, indent=1))


if __name__ == '__main__':
    main()
