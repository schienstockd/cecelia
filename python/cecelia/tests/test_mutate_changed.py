"""`scripts/mutate_changed.py` picks the mutants that edit a changed line (TEST_VALUE_PLAN.md → P4).

The mutants file is mutmut 3.8.1's own output for `dim_utils._store_axis_order`: its `__mutmut_orig`
copy and mutant 3 (`!=` → `==`), copied as mutmut wrote them, blank lines before the mutant's `def`
included. In the source the function's `def` is line 24 and the `!=` comparison line 27.
"""
from __future__ import annotations

import importlib.util
import json
import pathlib
import tempfile
import unittest
from unittest import mock

_REPO = pathlib.Path(__file__).resolve().parents[3]
_spec = importlib.util.spec_from_file_location("mutate_changed", _REPO / "scripts" / "mutate_changed.py")
mc = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(mc)

_ORIG = '''def x__store_axis_order__mutmut_orig(axes, ndim):
  """Upper-case axis letters from NGFF `axes` when they name each of `ndim` dimensions once, all of
  them T/Z/C/Y/X; else None (the caller falls back to the OME-XML)."""
  if not axes or len(axes) != ndim:
    return None
  order = [str(a).upper() for a in axes]
  if len(set(order)) != ndim or not set(order) <= set("TZCYX"):
    return None
  return order'''
_MUT3 = '''


def x__store_axis_order__mutmut_3(axes, ndim):
  """Upper-case axis letters from NGFF `axes` when they name each of `ndim` dimensions once, all of
  them T/Z/C/Y/X; else None (the caller falls back to the OME-XML)."""
  if not axes or len(axes) == ndim:
    return None
  order = [str(a).upper() for a in axes]
  if len(set(order)) != ndim or not set(order) <= set("TZCYX"):
    return None
  return order'''


class ChangedMutantsTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.mutfile = pathlib.Path(tmp.name) / "dim_utils.py"
        orig, mut = _ORIG.split("\n"), _MUT3.split("\n")
        self.mutfile.write_text("\n".join(orig + mut) + "\n", encoding="utf-8")
        spans = {"x__store_axis_order__mutmut_orig": [1, len(orig)],
                 "x__store_axis_order__mutmut_3": [len(orig) + 1, len(orig) + len(mut)]}
        pathlib.Path(str(self.mutfile) + ".spans").write_text(
            json.dumps({"version": 1, "spans": spans}), encoding="utf-8")

    def pick(self, lines):
        return mc.py_changed_mutants(self.mutfile, {"x__store_axis_order": 24}, set(lines))

    def test_a_mutant_on_a_changed_line_is_picked(self):
        self.assertEqual(self.pick({27}), {"x__store_axis_order__mutmut_3": {
            "line": 27, "orig": "if not axes or len(axes) != ndim:", "new": "if not axes or len(axes) == ndim:"}})

    def test_a_mutant_off_the_changed_lines_is_not(self):
        # the lines around it; 25 (the docstring) is where the blank lines mutmut wrote before the
        # mutant's `def` would land if the two copies were lined up from the top
        self.assertEqual(self.pick({21, 22, 23, 25, 26, 28, 30}), {})


class FunctionsAndRangesTest(unittest.TestCase):
    def test_the_function_or_method_holding_a_changed_line(self):
        with tempfile.TemporaryDirectory() as d:
            src = pathlib.Path(d) / "m.py"
            src.write_text("X = 1\n\ndef f():\n    return 1\n\nclass C:\n    @staticmethod\n"
                           "    def g():\n        return 2\n", encoding="utf-8")
            self.assertEqual(mc.py_funcs(src, {4}), {"x_f": 3})
            self.assertEqual(mc.py_funcs(src, {7}), {"xǁCǁg": 8})   # a decorator line counts
            self.assertEqual(mc.py_funcs(src, {1}), {})             # module level: no function

    def test_julia_definition_owning_a_line(self):
        with tempfile.TemporaryDirectory() as d:
            src = pathlib.Path(d) / "m.jl"
            src.write_text("const K = (1, 2)\n\nfunction f!(x)\n    x + 1\nend\n\ng(x) = x * 2\n",
                           encoding="utf-8")
            self.assertEqual(mc.jl_names(src, {4}), {"f!"})
            self.assertEqual(mc.jl_names(src, {1, 7}), {"K", "g"})

    def test_tests_found_by_name_and_the_names_no_test_has(self):
        with tempfile.TemporaryDirectory() as d, mock.patch.object(mc, "ROOT", pathlib.Path(d)):
            t = pathlib.Path(d, "python/cecelia/tests")
            t.mkdir(parents=True)
            (t / "test_a.py").write_text("from cecelia.utils import czi_meta\nczi_meta.parse_settings(x)\n",
                                         encoding="utf-8")
            (t / "test_b.py").write_text("parse_settings(x)  # another module's\n", encoding="utf-8")
            (t / "test_c.py").write_text("from cecelia.utils import czi_meta\nczi_meta.Reader().read()\n",
                                         encoding="utf-8")
            hits, unnamed = mc.py_tests("cecelia.utils.czi_meta", ["x_parse_settings", "x__band", "xǁReaderǁread"], set())
            self.assertEqual([h.name for h in hits], ["test_a.py", "test_c.py"])
            self.assertEqual(mc.test_paths(hits), ["python/cecelia/tests/test_a.py", "python/cecelia/tests/test_c.py"])
            self.assertEqual(unnamed, ["x__band"])
            # a test file the change itself touched is run whatever it names
            hits, _ = mc.py_tests("cecelia.utils.czi_meta", ["x__band"], {"python/cecelia/tests/test_b.py"})
            self.assertEqual([h.name for h in hits], ["test_b.py"])

    def test_a_private_helper_maps_through_its_callers_in_the_file(self):
        with tempfile.TemporaryDirectory() as d:
            src = pathlib.Path(d) / "m.py"
            src.write_text("def _band(ch):\n    return 1\n\ndef _channels(x):\n    return _band(x)\n\n"
                           "def parse_settings(x):\n    return _channels(x)\n\ndef other():\n    return 2\n",
                           encoding="utf-8")
            self.assertEqual(mc.py_with_callers(src, {"x__band"}), {"x__band", "x__channels", "x_parse_settings"})
            jl = pathlib.Path(d) / "m.jl"
            jl.write_text("_band(ch) = ch + 1\n\nfunction parse(x)\n    _band(x)  # _unused\nend\n\nother() = 2\n",
                          encoding="utf-8")
            self.assertEqual(mc.with_callers({"_band"}, mc.jl_refs(jl)), {"_band", "parse"})

    def test_line_ranges(self):
        self.assertEqual(mc.ranges({3, 4, 5, 9, 11, 12}), "3-5,9,11-12")

    def test_at_names_lines_without_a_diff(self):
        self.assertEqual(mc.at_lines(["python/cecelia/x.py:3-5,9", "app/src/a.jl:2"]),
                         {"python/cecelia/x.py": {3, 4, 5, 9}, "app/src/a.jl": {2}})
        with self.assertRaises(ValueError):
            mc.at_lines(["python/cecelia/x.py"])


if __name__ == "__main__":
    unittest.main()
