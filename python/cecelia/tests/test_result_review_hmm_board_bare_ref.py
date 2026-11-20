# RESULT-REVIEW: result-0da30934
"""An HMM run's bare pop ref (`"/qc"`, an older run) is on the image's ACTIVE segmentation, which the
eval board cannot know. It must not be resolved onto a segmentation called `default` — that draws
another segmentation's cells with no error. Left unresolved, the board expander rejects it loudly."""
from __future__ import annotations

import importlib.util
import pathlib
import sys
import unittest

_REPO = pathlib.Path(__file__).resolve().parents[3]


def _load(name):
    key = f"agent_eval_{name}"
    spec = importlib.util.spec_from_file_location(key, _REPO / "scripts" / "agent_eval" / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    sys.modules[key] = mod
    sys.path.insert(0, str(_REPO / "scripts" / "agent_eval"))   # stage_boards imports its siblings by name
    spec.loader.exec_module(mod)
    return mod


stage_boards = _load("stage_boards")


class TestHmmBoardBareRef(unittest.TestCase):
    def test_bare_ref_is_not_put_on_default(self):
        p = stage_boards._plots("hmm", [{"params": {"pops": ["/qc"], "colName": "m"}}])
        plots = [x for cand in p[0][1] for x in cand]
        refs = [r for x in plots for r in x.get("pops", [])]
        self.assertTrue(refs)
        self.assertFalse(any(r.startswith("default/") for r in refs), refs)
        self.assertFalse(any(x.get("valueName") == "default" for x in plots))

    def test_prefixed_refs_keep_their_segmentation(self):
        p = stage_boards._plots("hmm", [{"params": {"pops": ["T/qc", "/qc"], "colName": "m"}}])
        self.assertEqual([x["valueName"] for x in p[0][1][0] if x["plot"] == "hmmStateCards"], ["T"])


if __name__ == "__main__":
    unittest.main()
