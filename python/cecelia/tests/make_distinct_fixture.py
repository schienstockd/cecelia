"""Regenerate `test-data/projects/testpr/1/KDIeEx/` — a second image derived from KDIeEm.

`ccid.json` (KDIeEm's, uid `KDIeEx`), `labelProps/B.h5ad` and `labelProps/B__tracks.h5ad`: KDIeEm's real
`B` with a known part removed and known measures scaled, so a second image or segmentation built from
it disagrees with `B` on numbers a test can compute by hand. Against
a verbatim copy of `B`, a read that takes the wrong image, or pools one image twice, returns the same
medians and passes. Run with `pixi run python python/cecelia/tests/make_distinct_fixture.py`.

The definition, which tests re-derive from `B` itself (never from this file's output):

  * cells: drop every cell whose track id is divisible by 3 (whole tracks go), and every untracked
    cell whose label is divisible by 3.
  * tracks: drop every track whose id is divisible by 3 (the same tracks).
  * `area` (cells) and `live.track.speed` (tracks) are multiplied by `SCALE` (2.0).

Everything else — labels, track ids, centroids, obs, uns — is B's, so joins still line up.
Not a TestCase: the discovery pattern is `test_*.py`.
"""
import os

import numpy as np
import anndata as ad   # H5AD-OK: creating a new fixture table (the sanctioned creation exception)

from cecelia.utils.atomic_io import write_h5ad_atomic

SCALE = 2.0
_ROOT = os.environ.get("CECELIA_TEST_DATA") or os.path.normpath(
    os.path.join(os.path.dirname(__file__), "..", "..", "..", "test-data", "projects"))
SRC = os.path.join(_ROOT, "testpr", "1", "KDIeEm")
DST = os.path.join(_ROOT, "testpr", "1", "KDIeEx")
LP = os.path.join(SRC, "labelProps")
OUT = os.path.join(DST, "labelProps")


def keep_cell(label: int, track_id: float) -> bool:
    if np.isnan(track_id):
        return label % 3 != 0
    return int(track_id) % 3 != 0


def main() -> None:
    import json
    os.makedirs(OUT, exist_ok=True)
    with open(os.path.join(SRC, "ccid.json"), encoding="utf-8") as f:
        ccid = json.load(f)
    ccid.update(uid="KDIeEx", name="test-fixture-kdieex")
    ccid.pop("labels", None)   # no label store here either; the cell table is the fixture
    with open(os.path.join(DST, "ccid.json"), "w", encoding="utf-8") as f:
        json.dump(ccid, f, indent=2)
    cells = ad.read_h5ad(os.path.join(LP, "B.h5ad"))
    labels = cells.obs.index.astype(np.int64).to_numpy()
    tids = cells.obs["track_id"].to_numpy(dtype=float)
    mask = np.array([keep_cell(int(lb), t) for lb, t in zip(labels, tids)])
    bx = cells[mask].copy()
    ai = list(bx.var_names).index("area")
    bx.X[:, ai] = bx.X[:, ai] * SCALE
    write_h5ad_atomic(bx, os.path.join(OUT, "B.h5ad"))

    tracks = ad.read_h5ad(os.path.join(LP, "B__tracks.h5ad"))
    tmask = np.array([int(t) % 3 != 0 for t in tracks.obs.index.astype(np.int64)])
    tx = tracks[tmask].copy()
    si = list(tx.var_names).index("live.track.speed")
    tx.X[:, si] = tx.X[:, si] * SCALE
    write_h5ad_atomic(tx, os.path.join(OUT, "B__tracks.h5ad"))
    print(f"KDIeEx: {bx.n_obs}/{cells.n_obs} cells, {tx.n_obs}/{tracks.n_obs} tracks")


if __name__ == "__main__":
    main()
