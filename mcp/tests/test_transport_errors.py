"""A broken reply reaches the agent with a reason, never as a bare "Error executing tool …" (run-95af9a73ad).

The MCP tool wrappers (`_tool` in server.py / autonomous_server.py) pass only `ApiError` (and the
guard errors) to the model as text; mcp 2.x reduces anything else to `Error executing tool <name>`.
So every way the transport can fail must surface as `ApiError` with a message: a body cut short, a
garbled status line, a 200 that isn't JSON, a WebSocket upgrade the server refuses.

Real sockets on 127.0.0.1 (ephemeral ports) that send canned broken bytes — no running app needed.
"""
import socket
import socketserver
import threading
import unittest

from cecelia.utils import loopback
from cecelia_mcp import client
from cecelia_mcp.autonomous import AutonomousClient

_JSON_HEAD = b"HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n"


class _Canned(socketserver.BaseRequestHandler):
    reply = b""

    def handle(self):
        self.request.recv(65536)  # the request; its content doesn't matter
        self.request.sendall(self.server.reply)
        self.request.shutdown(socket.SHUT_WR)
        # Drain until the client hangs up. Closing with request bytes still unread (a body that came
        # in a second segment) makes the kernel send RST, and macOS then hands the client
        # "Connection reset" in place of the canned reply.
        self.request.settimeout(5)
        try:
            while self.request.recv(65536):
                pass
        except OSError:
            pass


def _serve(reply: bytes) -> socketserver.ThreadingTCPServer:
    srv = socketserver.ThreadingTCPServer(("127.0.0.1", 0), _Canned)
    srv.daemon_threads = True
    srv.reply = reply
    threading.Thread(target=srv.serve_forever, daemon=True).start()
    return srv


class BrokenReplyTest(unittest.TestCase):
    def setUp(self):
        loopback.forget()

    def _call(self, reply: bytes) -> client.ApiError:
        srv = _serve(reply)
        self.addCleanup(srv.server_close)
        self.addCleanup(srv.shutdown)
        with self.assertRaises(client.ApiError) as cm:
            client.http_json(f"http://127.0.0.1:{srv.server_address[1]}", "POST", "/api/gating/set",
                             body={"x": 1}, timeout=5)
        self.assertTrue(cm.exception.message.strip())
        return cm.exception

    def test_body_cut_short(self):
        # Content-Length promises 100 bytes, the server closes after 9 → http.client.IncompleteRead
        e = self._call(_JSON_HEAD + b"Content-Length: 100\r\n\r\n" + b'{"ok": tr')
        self.assertIn("IncompleteRead", e.message)
        self.assertIn("/api/gating/set", e.message)

    def test_garbled_status_line(self):
        # not HTTP at all → http.client.BadStatusLine, which is neither OSError nor URLError
        e = self._call(b"SSH-2.0-OpenSSH_9.6\r\n")
        self.assertIn("BadStatusLine", e.message)

    def test_non_json_body(self):
        body = b"<html>502 Bad Gateway</html>"
        e = self._call(b"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n"
                       + f"Content-Length: {len(body)}\r\n\r\n".encode() + body)
        self.assertIn("non-JSON", e.message)
        self.assertIn("502 Bad Gateway", e.message)

    def test_raw_skips_json_parsing(self):
        srv = _serve(b"HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\n\x89PNG")
        self.addCleanup(srv.server_close)
        self.addCleanup(srv.shutdown)
        out = client.http_json(f"http://127.0.0.1:{srv.server_address[1]}", "GET", "/api/x",
                               timeout=5, raw=True)
        self.assertEqual(out, b"\x89PNG")

    def test_websocket_upgrade_refused(self):
        # a plain HTTP answer to the /ws upgrade → websockets.InvalidStatus (not an OSError)
        body = b'{"error": "no"}'
        srv = _serve(_JSON_HEAD + f"Content-Length: {len(body)}\r\n\r\n".encode() + body)
        self.addCleanup(srv.server_close)
        self.addCleanup(srv.shutdown)
        c = AutonomousClient(base_url=f"http://127.0.0.1:{srv.server_address[1]}")
        with self.assertRaises(client.ApiError) as cm:
            c._ws_send({"type": "ping"}, lambda f: True, wait_s=1)
        self.assertIn("InvalidStatus", cm.exception.message)


if __name__ == "__main__":
    unittest.main()
