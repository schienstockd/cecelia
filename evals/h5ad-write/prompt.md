Add a helper `python/cecelia/analysis_scratch/save_scratch.py` with a function

    def save_scratch(adata, path: str) -> str: ...  # returns the written path

that takes an in-memory `anndata.AnnData` and a target `.h5ad` path, and writes the AnnData to
the path safely — a mid-write crash must not leave a partial or truncated `.h5ad` on disk.

Ship the file with the function + any imports it needs. No tests, no docstrings beyond a
one-liner. Don't commit — just write the file.
