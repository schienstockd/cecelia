"""Task runners write their QC/result JSON through `write_json_atomic`.

Julia reads these files back after the process exits (`qcOutPath` → the qc/ sidecar, `resultPath` →
the import handler), so a half-written one is consumed as real data. Twelve runners hand-rolled
`open(p, "w") + json.dump` while two already used the helper; this pins the migration and stops the
next runner re-growing the unsafe form — the same detector shape as `no_bare_write_h5ad`.

The `write_json_atomic` NAME is the specific risk here: it resolves at *call* time, so a runner that
calls it without importing it raises `NameError` only during a real task run, long after CI is green.
`test_the_helper_resolves_in_every_runner_that_calls_it` imports each runner and checks the binding.
"""

import ast
import contextlib
import importlib.util
import io
import json
import os
import pathlib
import tempfile
import unittest

import numpy as np
import tifffile


def _repo():
    return pathlib.Path(__file__).resolve().parents[3]


def _runners():
    """Every task runner — `(path, source)`. Runners live beside their `.jl` under app/src/tasks."""
    return [(f, f.read_text(encoding="utf-8"))
            for f in sorted((_repo() / "app" / "src" / "tasks").rglob("*_run.py"))]


def _load(path):
    """Import a runner BY PATH, the way `run_py` runs it (they are not an importable package)."""
    spec = importlib.util.spec_from_file_location(path.stem, path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class TaskRunnerJsonWriteTest(unittest.TestCase):
    def test_no_hand_rolled_json_dump_to_a_file(self):
        """`json.dump(payload, f)` writes straight into the destination — the truncating form."""
        offenders = []
        for path, src in _runners():
            for node in ast.walk(ast.parse(src, filename=str(path))):
                if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute) \
                        and node.func.attr == "dump" and isinstance(node.func.value, ast.Name) \
                        and node.func.value.id == "json":
                    offenders.append(f"{path.relative_to(_repo())}:{node.lineno}")
        self.assertEqual(
            offenders, [],
            "write result/QC JSON with write_json_atomic (cecelia.utils.atomic_io) — a cancelled "
            f"task must not leave a truncated file Julia then reads:\n  {offenders}")

    def test_the_helper_resolves_in_every_runner_that_calls_it(self):
        """A call without the import is a `NameError` that only fires during a real task run."""
        missing = []
        for path, src in _runners():
            if "write_json_atomic" not in src:
                continue
            mod = _load(path)
            if not hasattr(mod, "write_json_atomic"):
                missing.append(str(path.relative_to(_repo())))
        self.assertEqual(missing, [], f"calls write_json_atomic without importing it: {missing}")

    def test_the_scan_actually_reaches_the_runners(self):
        """Guard against the scan silently covering nothing (a moved tasks dir, a bad glob)."""
        callers = [p for p, src in _runners() if "write_json_atomic" in src]
        self.assertGreaterEqual(
            len(callers), 14, f"expected the known result/QC writers to be in scope, found {callers}")


class ReadImagejPhysicalSizeRunTest(unittest.TestCase):
    """Executes a migrated runner end to end — the only one whose whole `run()` is reachable without
    an image pipeline. Covers the write itself, not just the call's shape."""

    def setUp(self):
        self.td = tempfile.mkdtemp()
        self.mod = _load(_repo() / "app" / "src" / "tasks" / "importImages"
                         / "read_imagej_physical_size_run.py")

    def _run(self, im_path):
        out = os.path.join(self.td, "result.json")
        with contextlib.redirect_stdout(io.StringIO()):       # runners log to stdout
            self.mod.run({"imPath": im_path, "resultPath": out})
        with open(out, encoding="utf-8") as f:
            return out, json.load(f)

    def _imagej_tiff(self, unit, spacing):
        p = os.path.join(self.td, f"ij_{unit}.tif")
        tifffile.imwrite(p, np.zeros((2, 4, 4), dtype="uint8"), imagej=True,
                         metadata={"unit": unit, "spacing": spacing})
        return p

    def test_writes_the_converted_z_spacing(self):
        _, result = self._run(self._imagej_tiff("nm", 500.0))
        self.assertAlmostEqual(result["PhysicalSizeZ"], 0.5)   # 500 nm → 0.5 µm
        self.assertEqual(result["sourceUnit"], "nm")

    def test_writes_an_empty_result_when_there_is_nothing_to_correct(self):
        """Julia reads this file unconditionally, so the no-op case must still produce valid JSON."""
        plain = os.path.join(self.td, "not-a.tiff")
        pathlib.Path(plain).write_text("not a tiff", encoding="utf-8")
        out, result = self._run(plain)
        self.assertEqual(result, {})
        self.assertTrue(os.path.isfile(out))

    def test_leaves_no_temp_behind(self):
        self._run(self._imagej_tiff("mm", 0.002))
        self.assertEqual([f for f in os.listdir(self.td) if ".tmp." in f], [])


class CellNeighboursDumpTest(unittest.TestCase):
    """`_dump` is the QC write for cellNeighbours, and it is reachable directly."""

    def setUp(self):
        self.td = tempfile.mkdtemp()
        self.mod = _load(_repo() / "app" / "src" / "tasks" / "spatialAnalysis"
                         / "cell_neighbours_run.py")

    def test_writes_the_payload(self):
        p = os.path.join(self.td, "qc.json")
        self.mod._dump(p, {"nCells": 7, "meanDegree": 2.5})
        with open(p, encoding="utf-8") as f:
            self.assertEqual(json.load(f)["nCells"], 7)

    def test_no_path_is_a_no_op(self):
        self.mod._dump(None, {"nCells": 7})
        self.assertEqual(os.listdir(self.td), [])


if __name__ == "__main__":
    unittest.main()
