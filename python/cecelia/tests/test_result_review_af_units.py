"""RESULT-REVIEW: result-3caf6917

Result-review repro for the per-cell AF correction (`cecelia.utils.cell_af_utils`): each pins a
failure the first version had, on a panel built from the definition of the problem — autofluorescence
ADDS the same photons to every marker on a dye, a true positive adds a fixed amount on top."""
import unittest

import numpy as np

from cecelia.utils.cell_af_utils import correct_cell_af


def _af_panel(n, markers, seed):
    rng = np.random.default_rng(seed)
    af = rng.lognormal(2.0, 0.8, n)
    X = np.stack([af + rng.normal(0, 0.5, n).clip(0) for _ in range(markers)], axis=1)
    return rng, af, X


class ResultReview(unittest.TestCase):
    def test_a_positive_keeps_its_signal_whatever_its_autofluorescence(self):
        # subtracting in arcsinh space divided at the bright end: the same
        # +60 came out 47.5 on low-AF cells and 22.3 on high-AF cells (Spearman with AF −1.0).
        rng, af, X = _af_panel(20000, 8, 0)
        pos = rng.random(len(af)) < 0.3
        X[pos, 0] += 60.0
        C, info = correct_cell_af(X, ["a"] * 8)
        v, a = C[pos, info["channels"].index(0)], af[pos]
        lo, hi = np.percentile(a, [20, 80])
        low_af, high_af = np.median(v[a < lo]), np.median(v[a > hi])
        self.assertLess(abs(high_af - low_af), 0.1 * 60, (low_af, high_af))
        self.assertLess(abs(np.median(v) - 60.0), 0.15 * 60)


if __name__ == "__main__":
    unittest.main()
