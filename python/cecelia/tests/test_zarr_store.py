"""Regression: create_multiscales must write pixel-exact data even when the source dask array's
chunking does NOT match the per-plane destination grid.

The per-plane-chunking change made the destination chunks (1 along T/C/Z, 512-tiled X/Y) differ from
the source's own (auto) chunks, but kept `da.store(..., lock=False)`. When two source blocks map into
one destination chunk, lock-free stores race on the zarr chunk file (read-modify-write) → scrambled
planes, worst on EXPANDED outputs like drift correction. The fix rechunks the source to the dest grid
first (one writer per chunk). This writes a deliberately misaligned, non-512-aligned (expanded-canvas-
like) source and asserts an exact round-trip; the buggy pattern fails it (corrupts ~5/5 trials).

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import os
import sys
import shutil
import tempfile
import unittest

import numpy as np
import dask.array as da
import zarr

# `cecelia.*` resolves via the editable install in the pixi env — no sys.path needed.
import cecelia.utils.zarr_utils as zu


class CreateMultiscalesStoreTest(unittest.TestCase):
    def test_misaligned_source_roundtrips_exact(self):
        rng = np.random.default_rng(0)
        shape = (6, 2, 3, 730, 620)                 # [T,C,Z,Y,X] — Y/X NOT multiples of 512 (expanded case)
        base = rng.integers(0, 65535, size=shape, dtype=np.uint16)
        # source chunks span T/C with odd spatial blocks → misaligned vs the (1,1,1,512,512) dest grid
        src = da.from_array(base, chunks=(3, 2, 3, 300, 400))

        d = tempfile.mkdtemp()
        try:
            path = os.path.join(d, "out.ome.zarr")
            zu.create_multiscales(src, path, nscales=1)
            back = zarr.open_group(path, mode="r")["0"][:]
            self.assertTrue(np.array_equal(back, base),
                            "create_multiscales corrupted data on a misaligned source")
        finally:
            shutil.rmtree(d, ignore_errors=True)


if __name__ == "__main__":
    unittest.main()
