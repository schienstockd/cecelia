"""The laptop launcher (`scripts/remote-launcher/cecelia-connect.sh`) must tunnel to THIS user's
backend on the VM, not to a fixed 8080.

The connection profile is written at install (`scripts/print-connection.sh`), before the VM's
Cecelia has picked a port slot (app/src/ports.jl), so its `remotePort` is always slot 0's 8080. On a
VM shared by several OS users, a later user's backend is on slot N, and 8080 is another user's
Cecelia. The launcher reads `api_port` from the VM's `cecelia.lock` at connect time instead.

Driven end to end with fake `ssh` / `nc` / `curl` / `xdg-open` on PATH: the fake ssh answers the
lock query and records the tunnel's `-L` spec.
"""
import json
import os
import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest

REPO = pathlib.Path(__file__).resolve().parents[3]
CONNECT = REPO / "scripts" / "remote-launcher" / "cecelia-connect.sh"

# ssh: a call with a remote command answers the lock query; the tunnel call (-f -N -L) records its
# forward spec and marks its local port open for the fake nc (`nc -z 127.0.0.1 PORT`).
FAKE_SSH = """#!/bin/sh
case " $* " in
  *" -L "*)
    while [ $# -gt 0 ]; do
      if [ "$1" = "-L" ]; then
        printf '%s\\n' "$2" >> "$FAKE_DIR/tunnel"; : > "$FAKE_DIR/open-${2%%:*}"
      fi
      shift
    done
    exit 0 ;;
  *cecelia.lock*)
    [ -f "$FAKE_DIR/remote-lock" ] && cat "$FAKE_DIR/remote-lock"; exit 0 ;;
esac
exit 0
"""
FAKE_NC = '#!/bin/sh\n[ -f "$FAKE_DIR/open-$3" ]\n'
FAKE_OK = "#!/bin/sh\nexit 0\n"
# The name the launcher's warning and INSTALL.md must mention; this test never reads the variable.
MOVED_DIR_VAR = "CECELIA_DEV_DIR"  # DEV-DIR-OK: asserted as text in a message/doc, never read


@unittest.skipIf(sys.platform == "win32" or shutil.which("sh") is None, "POSIX launcher script")
class RemoteConnectPortTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self._tmp.name)
        self.fake = self.root / "fake"
        self.bin = self.root / "bin"
        self.home = self.root / "launcher"
        for d in (self.fake, self.bin, self.home):
            d.mkdir()
        for name, body in (("ssh", FAKE_SSH), ("nc", FAKE_NC), ("curl", FAKE_OK), ("xdg-open", FAKE_OK)):
            p = self.bin / name
            p.write_text(body, encoding="utf-8")
            p.chmod(0o755)
        (self.home / "connection.json").write_text(json.dumps(
            {"host": "203.0.113.5", "user": "alice", "localPort": 8080, "remotePort": 8080}),
            encoding="utf-8")

    def tearDown(self):
        self._tmp.cleanup()

    def _connect(self) -> str:
        return self._run() and (self.fake / "tunnel").read_text(encoding="utf-8").strip()

    def _run(self) -> bool:
        env = {**os.environ, "PATH": f"{self.bin}{os.pathsep}{os.environ.get('PATH', '')}",
               "FAKE_DIR": str(self.fake), "CECELIA_LAUNCHER_HOME": str(self.home)}
        r = subprocess.run(["sh", str(CONNECT)], env=env, capture_output=True, text=True,
                           encoding="utf-8", timeout=60)
        self.assertEqual(r.returncode, 0, r.stderr)
        self.stderr = r.stderr
        return True

    def test_tunnels_to_the_port_in_the_users_remote_lock(self):
        # Second OS user on the VM: their backend took slot 1 (8090); slot 0's 8080 is someone else's.
        (self.fake / "remote-lock").write_text(
            '{"pid":4242,"startedAt":"2026-11-05T10:00:00","api_port":8090}', encoding="utf-8")
        self.assertEqual(self._connect(), "8080:localhost:8090")

    def _existing_tunnel(self, local_port: int, remote_port):
        # A live process stands in for the detached ssh of an earlier connect.
        proc = subprocess.Popen(["sleep", "60"])
        self.addCleanup(lambda: (proc.poll() is None and proc.kill(), proc.wait()))
        (self.home / "tunnel.pid").write_text(str(proc.pid), encoding="utf-8")
        (self.home / "tunnel.port").write_text(str(local_port), encoding="utf-8")
        if remote_port is not None:
            (self.home / "tunnel.remote").write_text(str(remote_port), encoding="utf-8")
        (self.fake / f"open-{local_port}").write_text("", encoding="utf-8")
        return proc

    def test_stale_tunnel_to_another_port_is_reopened(self):
        # Tunnel opened before this user's Cecelia started (no lock, so 8080 = slot 0); now the lock
        # says 8090. The live tunnel must not be reused: it reaches another user's backend.
        old = self._existing_tunnel(8085, 8080)
        (self.fake / "remote-lock").write_text('{"pid":1,"api_port":8090}', encoding="utf-8")
        self.assertEqual(self._connect(), "8080:localhost:8090")
        self.assertIsNotNone(old.wait(timeout=10))
        self.assertEqual((self.home / "tunnel.remote").read_text(encoding="utf-8"), "8090")

    def test_tunnel_from_older_launcher_without_remote_record_is_reopened(self):
        self._existing_tunnel(8085, None)
        (self.fake / "remote-lock").write_text('{"pid":1,"api_port":8090}', encoding="utf-8")
        self.assertEqual(self._connect(), "8080:localhost:8090")

    def test_tunnel_to_the_right_port_is_reused(self):
        old = self._existing_tunnel(8085, 8090)
        (self.fake / "remote-lock").write_text('{"pid":1,"api_port":8090}', encoding="utf-8")
        self._run()
        self.assertFalse((self.fake / "tunnel").exists())
        self.assertIsNone(old.poll())

    def test_no_remote_lock_keeps_the_profile_port(self):
        self.assertEqual(self._connect(), "8080:localhost:8080")

    def test_no_remote_lock_warns_that_the_config_dir_may_be_moved(self):
        # A VM running Cecelia with a moved config dir (env or a clone's .env) writes its lock outside
        # ~/.cecelia, so the launcher finds none and falls back to the profile's port, which may be
        # another user's slot. That fallback must not be silent.
        self._connect()
        self.assertIn(MOVED_DIR_VAR, self.stderr)
        self.assertIn("8080", self.stderr)

    def test_lock_found_prints_no_missing_lock_warning(self):
        (self.fake / "remote-lock").write_text('{"pid":1,"api_port":8080}', encoding="utf-8")
        self._connect()
        self.assertNotIn(MOVED_DIR_VAR, self.stderr)

    def test_manual_tunnel_docs_name_the_moved_config_dir(self):
        # docs/INSTALL.md's manual tunnel reads cecelia.lock and api-token from ~/.cecelia; on a VM
        # with a moved config dir both live elsewhere, and the section must say so.
        text = (REPO / "docs" / "INSTALL.md").read_text(encoding="utf-8")
        start = text.index("**Tunnel from the laptop.**")
        section = text[start:text.index("**Viewer.**", start)]
        self.assertIn("cecelia.lock", section)
        self.assertIn(MOVED_DIR_VAR, section)


if __name__ == "__main__":
    unittest.main()
