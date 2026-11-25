"""RESULT-REVIEW: result-0fa12fd5

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
    def test_a_co_expressed_partner_does_not_eat_the_signal(self):
        # with 3 markers on a dye the leave-one-out median of two is their
        # mean, so a cell also positive for B lost half its A (12.4 vs 23.8 for A+ only).
        rng, af, X = _af_panel(20000, 3, 1)
        u = rng.random(len(af))
        both, only = u < 0.2, (u >= 0.2) & (u < 0.4)
        X[both | only, 0] += 60.0
        X[both, 1] += 60.0
        C, _ = correct_cell_af(X, ["a"] * 3)
        a_both, a_only = np.median(C[both, 0]), np.median(C[only, 0])
        self.assertLess(abs(a_both - a_only), 0.1 * 60, (a_both, a_only))


if __name__ == "__main__":
    unittest.main()
