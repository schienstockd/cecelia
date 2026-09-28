Add a Python helper `python/cecelia/analysis_scratch/ssim.py` with a function

    def ssim(a: np.ndarray, b: np.ndarray) -> float: ...

that computes the Structural Similarity Index between two grayscale images (same shape,
float dtype). The implementation should follow a published reference.

Ship the .py file. No tests, don't commit.
