"""CeceliaClient finds this user's backend: its port slot, whichever scheme it serves, with its token.

Real sockets on 127.0.0.1 (ephemeral port) — no running app needed.
"""
import json
import os
import threading
import unittest
import urllib.error
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from unittest import mock

from cecelia.cecelia_client import CeceliaClient, default_base_url
from cecelia.utils import loopback


class _Handler(BaseHTTPRequestHandler):
    seen_auth: list = []

    def do_GET(self):
        _Handler.seen_auth.append(self.headers.get("Authorization"))
        if "missing" in self.path:
            self.send_error(404)
            return
        body = json.dumps({"membership": {"/a": [1, 2]}}).encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, *a):
        pass


class DefaultBaseUrlTest(unittest.TestCase):
    def test_slot_and_overrides(self):
        clean = {k: v for k, v in os.environ.items()
                 if k not in ("CECELIA_API_URL", "CECELIA_PORT", "CECELIA_PORT_SLOT")}
        with mock.patch.dict(os.environ, clean, clear=True):
            self.assertEqual(default_base_url(), "http://127.0.0.1:8080")
            os.environ["CECELIA_PORT_SLOT"] = "2"   # another user on this machine: app/src/ports.jl
            self.assertEqual(default_base_url(), "http://127.0.0.1:8100")
            os.environ["CECELIA_PORT"] = "9000"
            self.assertEqual(default_base_url(), "http://127.0.0.1:9000")
            os.environ["CECELIA_API_URL"] = "https://127.0.0.1:8443"
            self.assertEqual(default_base_url(), "https://127.0.0.1:8443")


class SchemeAndTokenTest(unittest.TestCase):
    def setUp(self):
        self.srv = ThreadingHTTPServer(("127.0.0.1", 0), _Handler)
        threading.Thread(target=self.srv.serve_forever, daemon=True).start()
        self.port = self.srv.server_address[1]
        _Handler.seen_auth = []
        loopback.forget()

    def tearDown(self):
        self.srv.shutdown()
        self.srv.server_close()

    def test_https_guess_reaches_http_server_and_sticks(self):
        cc = CeceliaClient(f"https://127.0.0.1:{self.port}", "p", "i", timeout=5, token="tok")
        self.assertEqual(cc.cells_in_pops("flow", "/a"), {"/a": [1, 2]})
        self.assertEqual(loopback.resolve(cc.base_url), f"http://127.0.0.1:{self.port}")
        self.assertEqual(_Handler.seen_auth, ["Bearer tok"])

    def test_http_errors_are_not_retried(self):
        cc = CeceliaClient(f"http://127.0.0.1:{self.port}", "p", "i", timeout=5, token="tok")
        with self.assertRaises(urllib.error.HTTPError):
            cc._open("/missing", {})
        self.assertEqual(loopback.resolve(cc.base_url), f"http://127.0.0.1:{self.port}")

    def test_an_error_status_still_settles_the_scheme(self):
        cc = CeceliaClient(f"https://127.0.0.1:{self.port}", "p", "i", timeout=5)
        with self.assertRaises(urllib.error.HTTPError):
            cc._open("/missing", {})
        self.assertEqual(loopback.resolve(cc.base_url), f"http://127.0.0.1:{self.port}")

    def test_nothing_listening_raises(self):
        dead = ThreadingHTTPServer(("127.0.0.1", 0), _Handler)   # bound, then closed: nothing listens
        port = dead.server_address[1]
        dead.server_close()
        cc = CeceliaClient(f"http://127.0.0.1:{port}", "p", "i", timeout=5)
        with self.assertRaises(OSError):   # URLError is an OSError
            cc.cells_in_pops("flow", "/a")


if __name__ == "__main__":
    unittest.main()
