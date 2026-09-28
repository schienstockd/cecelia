Add a helper `python/cecelia/analysis_scratch/peek_first_frame.py` with a function

    def peek_first_frame(path: str) -> np.ndarray: ...

that opens an OME-ZARR store at `path` and returns timepoint 0 of channel 0 (the first XY
plane at the highest-resolution level) as a numpy array. Handles both bioformats2raw-wrapped
stores (`0/` subgroup) and flat-root stores.

Ship the file with the function + imports. No tests, don't commit.
