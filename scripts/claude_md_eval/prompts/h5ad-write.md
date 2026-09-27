---
id: h5ad-write
rule: H5AD / cell-data access — always go through the readers/writers
rule_section: CLAUDE.md → *H5AD / cell-data access — always go through the readers/writers*
compliant_signal: 'write_h5ad_atomic\('
anti_signal: '(adata|ad|self)\.write_h5ad\('
---
Add a helper `python/cecelia/analysis_scratch/save_scratch.py` with a function

    def save_scratch(adata, path: str) -> str: ...  # returns the written path

that takes an in-memory `anndata.AnnData` and a target `.h5ad` path, and writes the AnnData to
the path safely — a mid-write crash must not leave a partial or truncated `.h5ad` on disk.

Ship the file with the function + any imports it needs. No tests, no docstrings beyond a
one-liner. Don't commit — just write the file.
