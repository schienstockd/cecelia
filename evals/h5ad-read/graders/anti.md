---
type: regex
target:
  source: file
  path: python/cecelia/analysis_scratch/read_track_speed.py
pattern: '(anndata|ad|adata)\.read_h5ad\(|from anndata import.*read_h5ad|import anndata\b(?!.*# H5AD-OK)'
match: not_contains
weight: 1.0
---

Raw `anndata.read_h5ad` / `import anndata` bypasses the sanctioned wrapper family
(`LabelPropsView` for reads; `write_h5ad_atomic` for writes). Anti-signal — hitting this
inverts the run's verdict.
