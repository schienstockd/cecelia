"""Regenerate `test-data/projects/testpr/1/KDIeEm/labelProps/skel__branch.h5ad`.

A tiny branch table written by the REAL branching writer (`_skeletonise` → `_summarise_paths` →
`_write_branch_h5ad`, `app/src/tasks/segment/branching_run.py`), so its `uns/paths` layout is the
task's own — not a hand-built copy. `pixi run python python/cecelia/tests/make_skeleton_fixture.py`
after any change to that layout.

Not a TestCase (the discovery pattern is `test_*.py`) — a generator for the fixture the Julia suite
reads (`skeleton_segments`, `app/src/skeleton_lines.jl`).

One 2D T-junction (a horizontal bar, row 10, cols 2–38, crossed by a vertical stub, col 20, rows
2–10) at timepoints 0 and 2: three straight branches per frame, so each simplifies to its two
endpoints. Asymmetric in x and y, so an axis swap in a reader FAILS.
"""
import os

import numpy as np
import pandas as pd

from cecelia.tests.test_branching_anisotropy import _load_runner

_ROOT = os.environ.get("CECELIA_TEST_DATA") or os.path.normpath(
    os.path.join(os.path.dirname(__file__), "..", "..", "..", "test-data", "projects"))
OUT = os.path.join(_ROOT, "testpr", "1", "KDIeEm", "labelProps", "skel__branch.h5ad")


def main():
    mod = _load_runner()
    im = np.zeros((21, 41), dtype=bool)
    im[10, 2:39] = True
    im[2:11, 20] = True
    thin = mod._skeletonise(im, pre=0, is_3d=False)
    tables, lines, offset = [], [], 0
    for t in (0, 2):
        df, arr, polylines = mod._summarise_paths(thin, t_index=t, path_tolerance=0.5)
        df, _, offset = mod._globalise_labels(df, arr, offset)
        tables.append(df)
        lines.extend(polylines)
    mod._write_branch_h5ad(pd.concat(tables, ignore_index=True), is_3d=False, has_time=True,
                           out_path=OUT, polylines=lines)
    print(f"wrote {OUT}")


if __name__ == "__main__":
    main()
