"""Tests for `scripts/docshots/adopt_tracks.py`: a doc fixture's rebuilt gates take over their source
gates' tracks (the viewer draws a pop's ribbons only from cells whose `track_source` is its uid)."""
from __future__ import annotations

import importlib.util
import json
import pathlib
import tempfile
import unittest

_REPO = pathlib.Path(__file__).resolve().parents[3]


def _load():
    spec = importlib.util.spec_from_file_location("docshots_adopt_tracks", _REPO / "scripts" / "docshots" / "adopt_tracks.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


at = _load()


def _sidecar(d: pathlib.Path, tree: list) -> pathlib.Path:
    f = d / "Tcells.json"
    f.write_text(json.dumps({"populations": tree}), encoding="utf-8")
    return f


class PopUids(unittest.TestCase):
    def test_every_level_of_the_tree_by_path(self):
        with tempfile.TemporaryDirectory() as t:
            f = _sidecar(pathlib.Path(t), [{"name": "qc", "uid": "Q", "children": [
                {"name": "OTI", "uid": "O"}, {"name": "non-OTI", "uid": "N", "children": [{"name": "gBT", "uid": "G"}]}]}])
            self.assertEqual(at.pop_uids(f), {"/qc": "Q", "/qc/OTI": "O", "/qc/non-OTI": "N", "/qc/non-OTI/gBT": "G"})


class UidMap(unittest.TestCase):
    def test_source_uid_to_the_copy_uid_at_the_same_path(self):
        src = {"/qc": "Q", "/qc/OTI": "O", "/qc/gone": "X"}
        copy = {"/qc": "q2", "/qc/OTI": "o2", "/qc/new": "n2"}
        # a path only one side has is left out: its cells keep their marker
        self.assertEqual(at.uid_map(src, copy), {"Q": "q2", "O": "o2"})

    def test_a_uid_the_copy_kept_is_not_rewritten(self):
        self.assertEqual(at.uid_map({"/qc": "Q"}, {"/qc": "Q"}), {})


class Adopt(unittest.TestCase):
    def test_no_sidecar_or_no_cell_table_changes_nothing(self):
        with tempfile.TemporaryDirectory() as t:
            src, copy = pathlib.Path(t, "s"), pathlib.Path(t, "c")
            self.assertEqual(at.adopt(src, copy, "Tcells"), 0)
            for d, uid in ((src, "Q"), (copy, "q2")):
                (d / "gating").mkdir(parents=True)
                _sidecar(d / "gating", [{"name": "qc", "uid": uid}])
            self.assertEqual(at.adopt(src, copy, "Tcells"), 0)


if __name__ == "__main__":
    unittest.main()
