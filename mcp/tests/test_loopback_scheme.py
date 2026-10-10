"""The MCP reaches the app whichever scheme it was registered with (#1558).

Installed apps serve HTTPS with a self-signed cert, dev serves HTTP, and TLS can be toggled in
Settings — so the registered `CECELIA_API_URL` can name the wrong scheme. On loopback the client skips
verification (the cert is self-signed by design) and, on a connection-level failure, retries once with
the other scheme and remembers the one that worked. Off loopback nothing changes: certificates are
verified and the scheme is never swapped.

Real sockets on 127.0.0.1 (ephemeral ports), real TLS with a throwaway cert — no running app needed.
"""
import asyncio
import datetime
import json
import ssl
import tempfile
import threading
import unittest
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from unittest import mock

from cecelia.utils import loopback
from cecelia_mcp import client, wsclient
from cecelia_mcp.monitor import SessionMonitor


def _self_signed(dirpath: Path) -> tuple[str, str]:
    from cryptography import x509
    from cryptography.hazmat.primitives import hashes, serialization
    from cryptography.hazmat.primitives.asymmetric import ec
    from cryptography.x509.oid import NameOID

    key = ec.generate_private_key(ec.SECP256R1())
    name = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "localhost")])
    now = datetime.datetime.now(datetime.timezone.utc)
    cert = (x509.CertificateBuilder().subject_name(name).issuer_name(name)
            .public_key(key.public_key()).serial_number(x509.random_serial_number())
            .not_valid_before(now - datetime.timedelta(days=1))
            .not_valid_after(now + datetime.timedelta(days=1))
            .sign(key, hashes.SHA256()))
    crt, pem = dirpath / "c.crt", dirpath / "c.key"
    crt.write_bytes(cert.public_bytes(serialization.Encoding.PEM))
    pem.write_bytes(key.private_bytes(serialization.Encoding.PEM,
                                      serialization.PrivateFormat.TraditionalOpenSSL,
                                      serialization.NoEncryption()))
    return str(crt), str(pem)


class _Handler(BaseHTTPRequestHandler):
    def do_GET(self):  # noqa: N802 — http.server API
        body = json.dumps({"path": self.path}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, *a):
        pass


def _serve(tls: bool, certdir: Path | None = None) -> ThreadingHTTPServer:
    srv = ThreadingHTTPServer(("127.0.0.1", 0), _Handler)
    if tls:
        ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        ctx.load_cert_chain(*_self_signed(certdir))
        srv.socket = ctx.wrap_socket(srv.socket, server_side=True)
    threading.Thread(target=srv.serve_forever, daemon=True).start()
    return srv


class LoopbackSchemeTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls._tmp = tempfile.TemporaryDirectory()
        cls.https = _serve(True, Path(cls._tmp.name))
        cls.http = _serve(False)

    @classmethod
    def tearDownClass(cls):
        for s in (cls.https, cls.http):
            s.shutdown()
            s.server_close()
        cls._tmp.cleanup()

    def setUp(self):
        loopback.forget()

    def _port(self, srv):
        return srv.server_address[1]

    def test_https_self_signed_on_loopback(self):
        url = f"https://127.0.0.1:{self._port(self.https)}"
        self.assertEqual(client.http_json(url, "GET", "/api/health")["path"], "/api/health")

    def test_http_registration_reaches_https_server(self):
        # the reported failure: registered http://, app serves HTTPS → "Connection reset by peer"
        url = f"http://127.0.0.1:{self._port(self.https)}"
        self.assertEqual(client.http_json(url, "GET", "/api/x")["path"], "/api/x")
        self.assertEqual(loopback.resolve(url), f"https://127.0.0.1:{self._port(self.https)}")

    def test_https_registration_reaches_http_server(self):
        # the TLS toggle the other way
        url = f"https://localhost:{self._port(self.http)}"
        self.assertEqual(client.http_json(url, "GET", "/api/x")["path"], "/api/x")
        self.assertEqual(loopback.resolve(url), f"http://localhost:{self._port(self.http)}")

    def test_the_working_scheme_is_remembered(self):
        url = f"http://127.0.0.1:{self._port(self.https)}"
        client.http_json(url, "GET", "/api/x")
        with mock.patch.object(client.urllib.request, "urlopen", wraps=client.urllib.request.urlopen) as u:
            client.http_json(url, "GET", "/api/y")
        self.assertEqual(u.call_count, 1)  # straight to https, no failed http attempt first
        self.assertTrue(u.call_args[0][0].full_url.startswith("https://"))

    def test_both_schemes_failing_names_both(self):
        srv = ThreadingHTTPServer(("127.0.0.1", 0), _Handler)  # bound, then closed: nothing listens
        port = srv.server_address[1]
        srv.server_close()
        with self.assertRaises(client.ApiError) as cm:
            client.http_json(f"http://127.0.0.1:{port}", "GET", "/api/x")
        self.assertEqual(cm.exception.status, 0)
        self.assertIn("https://", cm.exception.message)

    def test_open_url_names_every_url_tried(self):
        # the transport the agent_eval scripts share with http_json: nothing listening → ConnectionError
        srv = ThreadingHTTPServer(("127.0.0.1", 0), _Handler)
        port = srv.server_address[1]
        srv.server_close()
        with self.assertRaises(ConnectionError) as cm:
            loopback.open_url(f"http://127.0.0.1:{port}", "/api/x", timeout=5)
        self.assertIn(f"http://127.0.0.1:{port}", str(cm.exception))
        self.assertIn(f"https://127.0.0.1:{port}", str(cm.exception))

    def test_http_errors_are_not_retried(self):
        # an API answer (404 here) is not a scheme problem
        url = f"http://127.0.0.1:{self._port(self.http)}"
        with mock.patch.object(_Handler, "do_GET", lambda self: self.send_error(404)):
            with self.assertRaises(client.ApiError) as cm:
                client.http_json(url, "GET", "/api/x")
        self.assertEqual(cm.exception.status, 404)
        self.assertEqual(loopback.resolve(url), url)

    def test_ws_listener_finds_the_other_scheme(self):
        # the observer's event stream: registered ws://, server only speaks wss://
        import websockets.asyncio.server as wss

        frames = [json.dumps({"type": "task:completed", "taskId": "t1"})]

        async def handler(ws):
            for f in frames:
                await ws.send(f)
            await asyncio.sleep(0.2)

        async def run():
            ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
            ctx.load_cert_chain(*_self_signed(Path(self._tmp.name)))
            async with wss.serve(handler, "127.0.0.1", 0, ssl=ctx) as server:
                port = server.sockets[0].getsockname()[1]
                seen = []
                monitor = SessionMonitor()
                stop = asyncio.Event()
                with mock.patch.object(wsclient, "feed_raw",
                                       lambda m, raw: (seen.append(raw), stop.set())):
                    await asyncio.wait_for(
                        wsclient.observe(monitor, f"ws://127.0.0.1:{port}/ws", stop=stop), timeout=10)
                return seen

        self.assertEqual(asyncio.run(run()), frames)


class OffLoopbackTest(unittest.TestCase):
    def test_remote_hosts_are_verified_and_never_swapped(self):
        for url in ("https://example.org:8080", "https://10.0.0.5:8080", "https://127.0.0.1.evil.com"):
            self.assertFalse(loopback.is_loopback(url), url)
            self.assertIsNone(loopback.ssl_context(url), url)  # None = urllib's verifying default
            self.assertEqual(loopback.candidates(url), [url])

    def test_loopback_hosts(self):
        for url in ("http://127.0.0.1:8080", "https://localhost:1", "http://[::1]:8080", "wss://localhost/ws"):
            self.assertTrue(loopback.is_loopback(url), url)
        self.assertEqual(loopback.candidates("ws://localhost:8080/ws"),
                         ["ws://localhost:8080/ws", "wss://localhost:8080/ws"])
        self.assertIsNone(loopback.ssl_context("http://127.0.0.1:8080"))  # plain HTTP needs none
        self.assertIsNotNone(loopback.ssl_context("wss://127.0.0.1:8080/ws"))


if __name__ == "__main__":
    unittest.main()
