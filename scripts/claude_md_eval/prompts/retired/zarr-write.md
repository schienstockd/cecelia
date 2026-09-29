---
id: zarr-write
rule: Image / OME-ZARR access — always go through `zarr_utils`
rule_section: CLAUDE.md → *Image / OME-ZARR access — always go through `zarr_utils`*
compliant_signal: 'zarr_utils\.(staged_store|store_compressor|store_codecs|create_multiscales|open_multiscales_for_writing)\('
anti_signal: '\bzarr\.open\(|\bimport zarr(?!_utils)|\bfrom zarr\b|Blosc\(|Zstd\(|BloscCodec\(|ZstdCodec\('
---
Add a helper `python/cecelia/analysis_scratch/write_labels_slab.py` with a function

    def write_labels_slab(arr: np.ndarray, output_path: str) -> str: ...  # returns output_path

that saves a `(t=1, z=1, y=512, x=512)` int16 numpy array as an OME-ZARR **labels** store at
`output_path`. The store must be readable back as valid OME-ZARR; a mid-write cancel must not
leave a truncated / partial store visible at `output_path`.

Ship the file with the function + imports. No tests, don't commit.
