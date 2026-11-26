"""Ratchet: the coherence gaps in `docs/architecture/gaps.md` may only go down.

`scripts/architecture/gaps_baseline.json` holds the count of each gap kind. This test regenerates the
gaps the Python-side extractors find (boundaries, Python imports, Vue imports — ~7 s) into a temp
dir, from the code as it is now, and fails when:

* a kind rises above its baseline — the change added a gap. The failure names the new items. Close
  them (the gap's "to close" line in gaps.md says how), or, for a false alarm, make the name visible
  to the extractor (a literal);
* a kind falls below its baseline — a gap was closed. Lower the ceiling so it stays closed:
  `pixi run arch-diagrams && pixi run arch-gaps-baseline`.

The Julia kinds (`julia-upward`) need the package loaded, so they are not regenerated here: they are
read from the committed `gaps/julia.json`, which `pixi run arch-diagrams` refreshes. Their live check is
the "julia-upward ratchet" testset in `app/test/suite/ratchets.jl` (`pixi run test-pkg`).

Run with `pixi run test-py`.
"""
import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

_REPO = Path(__file__).resolve().parents[3]
_SCRIPTS = _REPO / "scripts" / "architecture"
_LIVE = ["boundaries_extract.py", "python_extract.py", "vue_extract.py"]


def _gaps_module():
    spec = importlib.util.spec_from_file_location("arch_gaps", _SCRIPTS / "gaps.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class TestGapRatchet(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.g = _gaps_module()
        cls._tmp = tempfile.TemporaryDirectory()
        out = Path(cls._tmp.name)
        env = dict(os.environ, CECELIA_ARCH_OUT=str(out))
        for script in _LIVE:
            subprocess.run([sys.executable, str(_SCRIPTS / script)], env=env, check=True,
                           capture_output=True, text=True, encoding="utf-8")
        committed = _REPO / "docs" / "architecture" / "gaps"
        cls.live = cls.g.load(out / "gaps") + json.loads((committed / "julia.json").read_text(encoding="utf-8"))
        cls.committed = cls.g.load(committed)
        cls.baseline = json.loads(cls.g.BASELINE.read_text(encoding="utf-8"))

    @classmethod
    def tearDownClass(cls):
        cls._tmp.cleanup()

    def test_every_kind_has_a_baseline(self):
        self.assertEqual(set(self.baseline), set(self.g.KINDS),
                         "gaps_baseline.json and gaps.KINDS disagree — run `pixi run arch-gaps-baseline`")

    def test_no_gap_kind_rises(self):
        now = self.g.counts(self.live)
        known = {(it["kind"], it["subject"]) for it in self.committed}
        risen = []
        for kind, n in now.items():
            if n > self.baseline.get(kind, 0):
                new = [it["subject"] for it in self.live if it["kind"] == kind and (kind, it["subject"]) not in known]
                risen.append(f"{kind}: {self.baseline.get(kind, 0)} → {n}; new: {', '.join(new) or '(regenerate to see)'}"
                             f" — {self.g.KINDS[kind][1]}; to close: {self.g.KINDS[kind][2]}")
        self.assertEqual(risen, [], "this change adds coherence gaps (docs/architecture/gaps.md):\n" + "\n".join(risen))

    def test_baseline_follows_closed_gaps(self):
        now = self.g.counts(self.live)
        loose = [f"{k}: {self.baseline[k]} → {now[k]}" for k in self.baseline if now.get(k, 0) < self.baseline[k]]
        self.assertEqual(loose, [], "gaps were closed — lock that in: "
                                    "`pixi run arch-diagrams && pixi run arch-gaps-baseline`\n" + "\n".join(loose))



class TestBareFetchScan(unittest.TestCase):
    """`fetch-direct` counts direct calls of the global `fetch` — the scan must see a call however it is
    written, and must not count a method of that name, a comment, or an exempt line."""

    @classmethod
    def setUpClass(cls):
        spec = importlib.util.spec_from_file_location("arch_vue", _SCRIPTS / "vue_extract.py")
        cls.v = importlib.util.module_from_spec(spec)
        sys.path.insert(0, str(_SCRIPTS))   # vue_extract imports its sibling `gaps`
        try:
            spec.loader.exec_module(cls.v)
        finally:
            sys.path.remove(str(_SCRIPTS))

    def test_a_call_is_seen(self):
        for ln in ["const r = await fetch('/api/x')", "fetch(url, { method: 'POST' })",
                   "  return (await fetch (`/api/a`)).json()", "x = fetch(u)"]:
            self.assertTrue(self.v.BARE_FETCH.search(ln) and not self.v.METHOD_SIG.match(ln), ln)

    def test_a_method_named_fetch_is_not(self):
        self.assertIsNone(self.v.BARE_FETCH.search("await io.fetch(t)"))
        self.assertIsNone(self.v.BARE_FETCH.search("prefetch(t)"))
        self.assertIsNotNone(self.v.METHOD_SIG.match("  fetch(t: number): Promise<boolean>"))

    def test_the_exempt_marker_is_the_one_routes_use(self):
        self.assertEqual(self.v.EXEMPT, "ARCH-EXEMPT:")


if __name__ == "__main__":
    unittest.main()
