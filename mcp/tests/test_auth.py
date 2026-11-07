"""The MCP sends the API token on every request (cecelia_mcp/auth.py ↔ app/src/api_token.jl)."""
import os
import pathlib
import tempfile
import unittest
from unittest import mock

from cecelia.utils import loopback
from cecelia_mcp import auth


class AuthTest(unittest.TestCase):
    def test_token_file_from_the_observer_registration(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "api-token")
            with open(path, "w", encoding="utf-8") as f:
                f.write("abc123\n")
            with mock.patch.dict(os.environ, {"CECELIA_API_TOKEN_FILE": path}, clear=True):
                self.assertEqual(auth.auth_headers(), {"Authorization": "Bearer abc123"})

    def test_env_value_wins(self):
        with mock.patch.dict(os.environ, {"CECELIA_API_TOKEN": "fromenv",
                                          "CECELIA_API_TOKEN_FILE": "/nonexistent"}, clear=True):
            self.assertEqual(auth.api_token(), "fromenv")

    def test_default_config_dir(self):
        with tempfile.TemporaryDirectory() as d:
            with open(os.path.join(d, "api-token"), "w", encoding="utf-8") as f:
                f.write("devtok")
            with mock.patch.dict(os.environ, {"CECELIA_DEV_DIR": d}, clear=True):  # DEV-DIR-OK: temp dir
                self.assertEqual(auth.api_token(), "devtok")

    def test_dotenv_dev_dir(self):
        # a dev checkout's MCP has no CECELIA_DEV_DIR in its env — config_dir() reads .env, so must we
        with tempfile.TemporaryDirectory() as root, tempfile.TemporaryDirectory() as cfg:
            with open(os.path.join(root, ".env"), "w", encoding="utf-8") as f:
                f.write(f"CECELIA_DEV_DIR={cfg}\n")  # DEV-DIR-OK: temp dir
            with open(os.path.join(cfg, "api-token"), "w", encoding="utf-8") as f:
                f.write("dotenvtok")
            with mock.patch.object(loopback, "_REPO_ROOT", pathlib.Path(root)), \
                 mock.patch.dict(os.environ, {}, clear=True):
                self.assertEqual(auth.api_token(), "dotenvtok")

    def test_no_token_sends_no_header(self):
        with tempfile.TemporaryDirectory() as d:
            with mock.patch.dict(os.environ, {"CECELIA_DEV_DIR": d}, clear=True):  # DEV-DIR-OK: temp dir
                self.assertEqual(auth.auth_headers(), {})

    def test_request_headers_mark_claude(self):
        with mock.patch.dict(os.environ, {"CECELIA_API_TOKEN": "t"}, clear=True):
            h = auth.request_headers()
        self.assertEqual(h["X-Cecelia-Client"], "claude")
        self.assertEqual(h["Authorization"], "Bearer t")

    def test_every_request_path_sends_the_client_header(self):
        # AI_TOGGLE_PLAN D1: with AI off the server refuses requests marked `X-Cecelia-Client: claude`.
        # A path that sends only the token (the autonomous WebSocket once did) slips past the switch,
        # so outside auth.py nothing may build headers from auth_headers() — only request_headers().
        pkg = pathlib.Path(auth.__file__).parent
        offenders = [f"{p.name}:{i}" for p in sorted(pkg.glob("*.py")) if p.name != "auth.py"
                     for i, line in enumerate(p.read_text(encoding="utf-8").splitlines(), 1)
                     if "auth_headers" in line or "additional_headers=" in line and "request_headers()" not in line]
        self.assertEqual(offenders, [])
        users = [p.name for p in pkg.glob("*.py") if "request_headers()" in p.read_text(encoding="utf-8")]
        self.assertTrue({"client.py", "autonomous.py", "wsclient.py"} <= set(users), users)


if __name__ == "__main__":
    unittest.main()
