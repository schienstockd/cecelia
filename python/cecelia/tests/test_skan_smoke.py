"""Phase 0 smoke test for the branching port (docs/todo/BRANCHING_PLAN.md).

Pins the skan API we depend on: `Skeleton` + `summarize` emit the columns
`branching_run.py` will consume, with `separator='-'` locked in against skan's
scheduled default-separator flip.
"""
import unittest

import numpy as np
import skan
from skimage.morphology import skeletonize


REQUIRED_COLUMNS = {
    "skeleton-id",
    "node-id-src",
    "node-id-dst",
    "branch-distance",
    "branch-type",
    "euclidean-distance",
}


class SkanSmokeTest(unittest.TestCase):
    def test_summarize_columns_present_with_hyphen_separator(self):
        # A tiny 2D "H" — two vertical bars joined by a horizontal crossbar. Skeletonises to a
        # graph with a junction node in the middle of the crossbar, so skan sees a real branch
        # network (not a single unbranched path).
        img = np.zeros((21, 21), dtype=bool)
        img[3:18, 5] = True
        img[3:18, 15] = True
        img[10, 5:16] = True

        sk = skan.Skeleton(skeletonize(img))
        df = skan.summarize(sk, separator="-")

        missing = REQUIRED_COLUMNS - set(df.columns)
        self.assertFalse(
            missing,
            f"skan.summarize missing expected columns: {missing}. Got: {sorted(df.columns)}",
        )
        # coordinate columns are per-dimension — `image-coord-src-0`, `image-coord-src-1`, etc.
        coord_src = [c for c in df.columns if c.startswith("image-coord-src-")]
        coord_dst = [c for c in df.columns if c.startswith("image-coord-dst-")]
        self.assertEqual(
            len(coord_src), 2, f"expected 2 image-coord-src-N columns, got {coord_src}"
        )
        self.assertEqual(
            len(coord_dst), 2, f"expected 2 image-coord-dst-N columns, got {coord_dst}"
        )
        # branch-type is skan's integer classification (0..3 in 2D). Just assert it's an integer
        # dtype so the downstream `ensure_filter_pop!` per branch-type is safe (Decision 3).
        self.assertTrue(
            np.issubdtype(df["branch-type"].dtype, np.integer),
            f"branch-type dtype is not integer: {df['branch-type'].dtype}",
        )


if __name__ == "__main__":
    unittest.main()
