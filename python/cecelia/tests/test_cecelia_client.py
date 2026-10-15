"""CeceliaClient finds this user's backend: its port slot, whichever scheme it serves, with its token.

Real sockets on 127.0.0.1 (ephemeral port) — no running app needed.
"""
import json
import os
import tempfile
import threading
import unittest
import urllib.error
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
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
    def setUp(self):
        self._cfg = tempfile.TemporaryDirectory()   # never this machine's real sticky slot
        self.addCleanup(self._cfg.cleanup)
        self.clean = {k: v for k, v in os.environ.items()
                      if k not in ("CECELIA_API_URL", "CECELIA_PORT", "CECELIA_PORT_SLOT")}
        self.clean["CECELIA_DEV_DIR"] = self._cfg.name  # DEV-DIR-OK: temp dir

    def test_slot_and_overrides(self):
        with mock.patch.dict(os.environ, self.clean, clear=True):
            self.assertEqual(default_base_url(), "http://127.0.0.1:8080")
            os.environ["CECELIA_PORT_SLOT"] = "2"   # another user on this machine: app/src/ports.jl
            self.assertEqual(default_base_url(), "http://127.0.0.1:8100")
            os.environ["CECELIA_PORT"] = "9000"
            self.assertEqual(default_base_url(), "http://127.0.0.1:9000")
            os.environ["CECELIA_API_URL"] = "https://127.0.0.1:8443"
            self.assertEqual(default_base_url(), "https://127.0.0.1:8443")

    def test_plain_shell_follows_the_sticky_slot(self):
        # `pixi run mcp` or a notebook started from a shell has no CECELIA_PORT_SLOT: the slot the
        # backend persisted (`<config_dir>/port-slot`) is what says which backend is this user's.
        with open(os.path.join(self._cfg.name, "port-slot"), "w", encoding="utf-8") as f:
            f.write("1\n")
        with mock.patch.dict(os.environ, self.clean, clear=True):
            self.assertEqual(default_base_url(), "http://127.0.0.1:8090")
            os.environ["CECELIA_PORT_SLOT"] = "3"   # an inherited slot still wins
            self.assertEqual(default_base_url(), "http://127.0.0.1:8110")


class ServicePortTest(unittest.TestCase):
    """`loopback.service_port` is the Python twin of `service_port` (app/src/ports.jl): a standalone
    worker (`pixi run preview`) binds this user's slot port, not slot 0's."""

    def test_slot_and_pin(self):
        with tempfile.TemporaryDirectory() as cfg:
            env = {k: v for k, v in os.environ.items()
                   if k not in ("CECELIA_PORT_SLOT", "CECELIA_PREVIEW_PORT", "CECELIA_PLUTO_PORT")}
            env["CECELIA_DEV_DIR"] = cfg  # DEV-DIR-OK: temp dir
            with mock.patch.dict(os.environ, env, clear=True):
                self.assertEqual(loopback.service_port("preview"), 7656)
                self.assertEqual(loopback.service_port("notebooks"), 7660)
                os.environ["CECELIA_PORT_SLOT"] = "1"
                self.assertEqual(loopback.service_port("preview"), 7666)
                self.assertEqual(loopback.service_port("runner"), 7667)
                os.environ["CECELIA_PREVIEW_PORT"] = "7999"   # pinned by the launching backend
                self.assertEqual(loopback.service_port("preview"), 7999)

    def test_matches_ports_jl(self):
        # Same table as SERVICE_PORT_BASE / SERVICE_PORT_ENV — a drift would put the worker on a port
        # its backend never probes.
        src = (Path(loopback.__file__).resolve().parents[3] / "app" / "src" / "ports.jl").read_text(encoding="utf-8")
        for svc, (base, env) in loopback._SERVICE_PORTS.items():
            self.assertRegex(src, rf"\b{svc} = {base}\b")
            self.assertRegex(src, rf'\b{svc} = "{env}"')
        self.assertIn(f"PORT_SLOT_STRIDE = {loopback._PORT_SLOT_STRIDE}", src)


class TokenLookupTest(unittest.TestCase):
    """Outside a backend-spawned task (a notebook, a script) there is no `CECELIA_API_TOKEN`: the client
    reads `<config_dir>/api-token` like every other client, instead of sending no token and getting 401s."""

    def test_token_file_when_env_has_none(self):
        with tempfile.TemporaryDirectory() as d:
            with open(os.path.join(d, "api-token"), "w", encoding="utf-8") as f:
                f.write("filetok\n")
            with mock.patch.dict(os.environ, {"CECELIA_DEV_DIR": d}, clear=True):  # DEV-DIR-OK: temp dir
                self.assertEqual(CeceliaClient().token, "filetok")
                os.environ["CECELIA_API_TOKEN"] = "envtok"   # a spawned task's env wins
                self.assertEqual(CeceliaClient().token, "envtok")
                self.assertEqual(CeceliaClient(token="given").token, "given")


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
