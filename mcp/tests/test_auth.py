"""The MCP sends the API token on every request (cecelia_mcp/auth.py ↔ app/src/api_token.jl)."""
import os
import pathlib
import tempfile
import unittest
from unittest import mock

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
            with mock.patch.object(auth, "_REPO_ROOT", pathlib.Path(root)), \
                 mock.patch.dict(os.environ, {}, clear=True):
                self.assertEqual(auth.api_token(), "dotenvtok")

    def test_no_token_sends_no_header(self):
        with tempfile.TemporaryDirectory() as d:
            with mock.patch.dict(os.environ, {"CECELIA_DEV_DIR": d}, clear=True):  # DEV-DIR-OK: temp dir
                self.assertEqual(auth.auth_headers(), {})


if __name__ == "__main__":
    unittest.main()
