"""Atomic durable writes (`cecelia.utils.atomic_io`).

The failure being prevented: writing straight to the destination truncates it first, so a kill in that
window leaves a half-written file. `task:cancel` kills the Python process by design, so this is a
routine path, not an edge case — and for `.h5ad` (the cell table) a truncated HDF5 is unreadable and
the previous content is gone. Julia's side of this is #420; these are the Python counterparts.
"""

import ast
import json
import os
import pathlib
import tempfile
import unittest

import anndata as ad
import numpy as np

from cecelia.utils.atomic_io import (atomic_path, write_atomic, write_h5ad_atomic,
                                     write_json_atomic)


def _adata(n=3):
    a = ad.AnnData(np.arange(n * 2, dtype="float32").reshape(n, 2))
    a.obs["label"] = list(range(1, n + 1))
    return a


class AtomicWriteTest(unittest.TestCase):
    def setUp(self):
        self.td = tempfile.mkdtemp()

    def _leftovers(self, d=None):
        return [f for f in os.listdir(d or self.td) if ".tmp." in f]

    def test_json_round_trips(self):
        p = os.path.join(self.td, "qc.json")
        write_json_atomic(p, {"nCells": 42})
        with open(p, encoding="utf-8") as f:
            self.assertEqual(json.load(f)["nCells"], 42)
        self.assertEqual(self._leftovers(), [])

    def test_a_failed_write_leaves_the_previous_content(self):
        """The whole point: an interrupted write must not destroy what was there."""
        p = os.path.join(self.td, "state.json")
        write_json_atomic(p, {"v": 1})
        with self.assertRaises(RuntimeError):
            with write_atomic(p) as f:
                f.write('{"partial":')
                raise RuntimeError("killed mid-write")
        with open(p, encoding="utf-8") as f:
            self.assertEqual(json.load(f)["v"], 1)      # untouched, NOT truncated
        self.assertEqual(self._leftovers(), [])         # and no temp left behind

    def test_a_failed_write_creates_nothing_when_there_was_no_file(self):
        p = os.path.join(self.td, "absent.json")
        with self.assertRaises(RuntimeError):
            with write_atomic(p) as f:
                f.write("x")
                raise RuntimeError("boom")
        self.assertFalse(os.path.exists(p))
        self.assertEqual(self._leftovers(), [])

    def test_missing_parent_dirs_are_created(self):
        p = os.path.join(self.td, "a", "b", "c.json")
        write_json_atomic(p, {"ok": True})
        self.assertTrue(os.path.isfile(p))

    def test_temp_name_cannot_be_mistaken_for_a_real_output(self):
        """Discovery is a directory listing filtered by extension — `endswith(".h5ad")` in
        `img_spatial_graph_suffixes`, `glob("labelProps/*.h5ad")`, `*.mp4` for movies. A leftover temp
        from a killed process must not match, so the suffix goes AFTER the real extension."""
        seen = {}

        def capture(tmp):
            seen["tmp"] = os.path.basename(tmp)

        p = os.path.join(self.td, "base.h5ad")
        with atomic_path(p) as tmp:
            capture(tmp)
            pathlib.Path(tmp).write_text("x", encoding="utf-8")
        self.assertFalse(seen["tmp"].endswith(".h5ad"))
        self.assertTrue(seen["tmp"].startswith("base.h5ad.tmp."))

    def test_temp_is_a_sibling_so_the_replace_is_atomic(self):
        """Same directory ⇒ same filesystem ⇒ `os.replace` is a rename, not a cross-device copy."""
        p = os.path.join(self.td, "sub", "x.json")
        with atomic_path(p) as tmp:
            self.assertEqual(os.path.dirname(tmp), os.path.dirname(p))
            pathlib.Path(tmp).write_text("{}", encoding="utf-8")

    def test_text_mode_writes_utf8_regardless_of_locale(self):
        """`open()` defaults to the LOCALE encoding — cp1252 on Windows, where `µm` in an OME-XML
        or a QC message is a `UnicodeEncodeError`. The helper pins UTF-8 so callers can't inherit
        the platform's."""
        p = os.path.join(self.td, "qc.txt")
        with write_atomic(p) as f:
            f.write("resolution ≥ 0.5 µm")
        self.assertEqual(pathlib.Path(p).read_bytes().decode("utf-8"), "resolution ≥ 0.5 µm")

    def test_binary_mode_is_untouched(self):
        p = os.path.join(self.td, "raw.bin")
        with write_atomic(p, "wb") as f:
            f.write(b"\x00\x9d\xff")
        self.assertEqual(pathlib.Path(p).read_bytes(), b"\x00\x9d\xff")

    def test_two_writers_of_one_path_get_distinct_temps(self):
        p = os.path.join(self.td, "x.json")
        with atomic_path(p) as a, atomic_path(p) as b:
            self.assertNotEqual(a, b)
            pathlib.Path(a).write_text("{}", encoding="utf-8")
            pathlib.Path(b).write_text("{}", encoding="utf-8")


class AtomicH5adTest(unittest.TestCase):
    def setUp(self):
        self.td = tempfile.mkdtemp()

    def test_writes_a_readable_h5ad(self):
        p = os.path.join(self.td, "base.h5ad")
        write_h5ad_atomic(_adata(), p)
        self.assertEqual(ad.read_h5ad(p).n_obs, 3)
        self.assertEqual([f for f in os.listdir(self.td) if ".tmp." in f], [])

    def test_a_failed_h5ad_write_keeps_the_existing_table(self):
        """The severe case: the cell table IS the measurement data, and a truncated HDF5 is not
        partially readable the way a truncated JSON is."""
        p = os.path.join(self.td, "base.h5ad")
        write_h5ad_atomic(_adata(n=5), p)

        class Boom(Exception):
            pass

        class Exploding:
            def write_h5ad(self, path, **kw):
                pathlib.Path(path).write_bytes(b"\x89HDF\r\n\x1a\n truncated")   # partial file
                raise Boom("cancelled mid-save")

        with self.assertRaises(Boom):
            write_h5ad_atomic(Exploding(), p)
        self.assertEqual(ad.read_h5ad(p).n_obs, 5)                               # original intact
        self.assertEqual([f for f in os.listdir(self.td) if ".tmp." in f], [])


class NoHandRolledWritesTest(unittest.TestCase):
    """Detector, not advisory: a NEW bare `write_h5ad` fails here. This is how the truncating form
    spread on the Julia side — the correct pattern existed in one place and nothing stopped the next
    writer hand-rolling the unsafe one. Allow-listed by exact call, not by whole file."""

    ROOTS = ("python/cecelia", "app/src", "napari", "api")
    # `atomic_io` IS the implementation; `label_props_utils` calls it as a method on the temp path
    ALLOWED_FILES = {"atomic_io.py"}

    def _repo(self):
        return pathlib.Path(__file__).resolve().parents[3]

    def test_no_bare_write_h5ad(self):
        repo = self._repo()
        offenders = []
        for root in self.ROOTS:
            for f in (repo / root).rglob("*.py"):
                if f.name in self.ALLOWED_FILES or "/tests/" in f.as_posix():
                    continue
                tree = ast.parse(f.read_text(encoding="utf-8"))
                for node in ast.walk(tree):
                    # `<x>.write_h5ad(...)` — the unsafe form. `write_h5ad_atomic(...)` is a plain
                    # Call with a Name func, so it never matches here.
                    if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute) \
                            and node.func.attr == "write_h5ad":
                        offenders.append(f"{f.relative_to(repo)}:{node.lineno}")
        self.assertEqual(offenders, [], "use write_h5ad_atomic (cecelia.utils.atomic_io): "
                                        f"{offenders}")


class TextIoDeclaresEncodingTest(unittest.TestCase):
    """Detector: text-mode file I/O must name its encoding.

    Python's default is the *locale* encoding, which is UTF-8 on Linux/macOS and **cp1252 on
    Windows** — so a bare `open(p)` / `read_text()` is a platform-dependent bug that only ever
    shows up on the Windows runner. It has bitten twice over: reading a source file with a `∝`
    in it (`0x9D` is undefined in cp1252 → `UnicodeDecodeError`), and writing an OME-XML with
    `µm` in it would fail the same way on the encode side. Everything we read and write is UTF-8.
    """

    ROOTS = ("python/cecelia", "app/src", "napari", "api", "mcp", "scripts")
    # `atomic_io` IS the implementation — it supplies the utf-8 default via **open_kwargs
    ALLOWED_FILES = {"atomic_io.py"}

    def _repo(self):
        return pathlib.Path(__file__).resolve().parents[3]

    @staticmethod
    def _mode(call):
        """The literal mode of an `open(...)` call, or `None` if absent/not a literal."""
        if len(call.args) > 1 and isinstance(call.args[1], ast.Constant):
            return call.args[1].value
        for kw in call.keywords:
            if kw.arg == "mode" and isinstance(kw.value, ast.Constant):
                return kw.value.value
        return None

    def test_no_text_io_without_an_explicit_encoding(self):
        repo = self._repo()
        offenders = []
        for root in self.ROOTS:
            for f in sorted((repo / root).rglob("*.py")):
                if f.name in self.ALLOWED_FILES:
                    continue
                tree = ast.parse(f.read_text(encoding="utf-8"))
                for node in ast.walk(tree):
                    if not isinstance(node, ast.Call):
                        continue
                    if {kw.arg for kw in node.keywords} & {"encoding", None}:
                        continue          # explicit, or forwarded via **kwargs
                    # bare `open(...)` — an attribute call (`zarr.open`, `gzip.open`) is not a
                    # builtin text open and is left alone
                    if isinstance(node.func, ast.Name) and node.func.id == "open":
                        mode = self._mode(node)
                        if mode is None or "b" not in mode:
                            offenders.append(f"{f.relative_to(repo)}:{node.lineno} open()")
                    elif isinstance(node.func, ast.Attribute) \
                            and node.func.attr in ("read_text", "write_text"):
                        offenders.append(
                            f"{f.relative_to(repo)}:{node.lineno} {node.func.attr}()")
        self.assertEqual(
            offenders, [],
            "pass encoding='utf-8' (the locale default is cp1252 on Windows):\n  "
            + "\n  ".join(offenders))


if __name__ == "__main__":
    unittest.main()
