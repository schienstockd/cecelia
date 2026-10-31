"""`agent_sandbox.node_bin_dir`: a sandboxed agent gets the repo's pinned Node major on PATH.

The shell's `node` may be fnm's per-shell symlink under `/run/user`, which the sandbox can't read, and
systemd's PATH has only the system node, too old for the frontend's vitest. Fake `node` scripts stand in.
"""
from __future__ import annotations

import os
import pathlib
import sys
import tempfile
import unittest
import unittest.mock

from cecelia.effectiveness import agent_sandbox


def _node(dir_: pathlib.Path, version: str) -> pathlib.Path:
    dir_.mkdir(parents=True, exist_ok=True)
    node = dir_ / "node"
    node.write_text(f"#!/bin/sh\necho {version}\n", encoding="utf-8")
    node.chmod(0o755)
    return node


@unittest.skipIf(sys.platform.startswith("win"), "fake node is a shell script")
class NodeBinDirTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = pathlib.Path(tmp.name)
        self.pin = self.tmp / ".node-version"
        self.pin.write_text("24\n", encoding="utf-8")
        self.fnm = self.tmp / "fnm"
        self.system = _node(self.tmp / "usr" / "bin", "v18.19.1").parent

    def env(self, *path: pathlib.Path) -> dict[str, str]:
        return {"PATH": os.pathsep.join(map(str, path)), "FNM_DIR": str(self.fnm)}

    def test_a_too_old_system_node_falls_back_to_fnm_s_newest_matching_install(self):
        for v in ("v24.1.0", "v24.16.0", "v22.3.0"):
            _node(self.fnm / "node-versions" / v / "installation" / "bin", v)
        got = agent_sandbox.node_bin_dir(self.env(self.system), self.pin)
        self.assertEqual(got, str(self.fnm / "node-versions" / "v24.16.0" / "installation" / "bin"))

    def test_a_symlinked_node_is_followed_to_its_real_installation(self):
        real = _node(self.fnm / "node-versions" / "v24.16.0" / "installation" / "bin", "v24.16.0")
        shell = self.tmp / "run" / "user" / "fnm_multishells" / "bin"
        shell.mkdir(parents=True)
        (shell / "node").symlink_to(real)
        got = agent_sandbox.node_bin_dir(self.env(shell, self.system), self.pin)
        self.assertEqual(got, str(real.parent.resolve()))

    def test_no_matching_node_anywhere_leaves_path_alone(self):
        self.assertIsNone(agent_sandbox.node_bin_dir(self.env(self.system), self.pin))
        self.assertIsNone(agent_sandbox.node_bin_dir(self.env(self.system), self.tmp / "missing"))

    def test_agent_env_puts_the_pinned_node_first_once(self):
        bin_dir = _node(self.fnm / "node-versions" / "v24.16.0" / "installation" / "bin", "v24.16.0").parent
        with unittest.mock.patch.dict(os.environ, self.env(self.system, bin_dir), clear=True), \
                unittest.mock.patch.object(agent_sandbox, "_NODE_VERSION_FILE", self.pin):
            path = agent_sandbox.agent_env()["PATH"].split(os.pathsep)
        self.assertEqual(path, [str(bin_dir), str(self.system)])



if __name__ == "__main__":
    unittest.main()
