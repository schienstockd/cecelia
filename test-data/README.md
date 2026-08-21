# Cecelia test data

Version-controlled fixtures for the headless test suite (`app/test/runtests.jl`) and for opening a
minimal project in the GUI. Committed **in the repo** so tests do not depend on the deletable dev
projects directory (`projects_dir()`) — and so a fresh clone and every CI runner have them. They used
to live at the workspace root, outside git, which meant the 18 fixture-gated testsets silently skipped
everywhere except one developer's machine while the suite still reported a green pass.

Tests resolve this via `test_projects_dir()` in `runtests.jl`:
- default: `<repo>/test-data/projects` (this directory)
- override: set the `CECELIA_TEST_DATA` env var to a different `projects` directory

**Size is capped, and the cap is enforced.** `.h5ad` is binary: git stores a whole new copy per update
and history cannot be pruned without a rewrite. The `fixtures stay small` testset fails if any single
file exceeds **1 MB** or the tree exceeds **8 MB**. If a fixture genuinely needs more room, that is a
design conversation — regenerate it smaller, synthesise it, or gate the test differently — not a number
to nudge up.

Individual fixtures are reached with `fixture_path(relparts...)`, and each testset gates on
`have_fixture(path)` (`@test_skip` + one `@warn` when absent) so a missing fixture never fails
an unrelated test.

## Layout

```
projects/testpr/
  project.json
  1/KDIeEm/
    ccid.json                 # loadable CciaImage (value_name "B")
    labelProps/B.h5ad         # real Cecelia-produced AnnData: 1377 cells × 27 features,
                              # 4 intensity channels, 3D centroids + temporal. Now also carries
                              # btrack lineage (track_id, …) + per-cell live.cell.* in obs.
    labelProps/B__tracks.h5ad # companion per-track table: 62 tracks, 10 live.track.* measures
                              # in X/var, lineage in obs (obs._index = track_id)
    labelProps/aniso__branch.h5ad  # SYNTHETIC branch sidecar (~32 KB): 6 branches over 3
                              # timepoints + the `uns` orientation block (3 frames x 4x4 boxes)
```

`B.h5ad` is a real measured output (image `KDIeEm`), used to test the `LabelProps` reader
and `pop_dt` against ground truth. It is a **20-timepoint** timecourse (t 0–19, 3D
centroids), so it also exercises the `tracking.bayesianTracking` (btrack) path. It has been
tracked + measured (`tracking.track_measures`), so `obs` carries `track_id` + lineage +
`live.cell.speed`/`live.cell.angle`. `B__tracks.h5ad` is its companion per-track table
(one row per track; see `docs/DATAMODEL.md`) — together they exercise the
`pop_dt(…; granularity=:track)` read path. If you add fixtures, keep them small and document
them here.

`aniso__branch.h5ad` is **synthetic**, not a real run. It pins the `uns` readers
(`uns_array` / `uns_dict` / `uns_df`) and the three notebook accessors (`quiver_df`,
`branch_segments`, `anisotropy_df`). It is built so a WRONG read fails rather than passing on
symmetry:

- HDF5 stores C-order and Julia reads column-major, so a numpy `(T, y, x, comp)` array arrives
  with **every axis reversed** — including the two equal-length box axes, which would swap
  silently. Each value therefore encodes its own coordinate (`100t + 10y + x`).
- The **minor** eigenvector (the fibre direction) is a pure `+x` unit vector and the major one is
  `+y`, so reading the wrong eigenvector shows up as a 90° rotation instead of plausible noise.
- Endpoint columns, a `centroid_t` axis, a per-branch `anisotropy` obs column and an
  `orientation_summary` dataframe are all present, so the accessors are exercised end to end.

Regenerate with `pixi run python python/cecelia/tests/make_aniso_fixture.py` (which also documents
the layout) after any change to the anisotropy `uns` block; keep it tiny.

## `projects/ZARRFMT/` — the same image as zarr v2 and zarr v3

Two OME-ZARR **image stores** holding the *same* real pixels, written by `bioformats2raw 0.12.1` in
the two on-disk formats. They exist because the v2-vs-v3 read path fails **silently** when it is
wrong — a missed `attributes.ome` nesting makes `read_axes`/`read_scale` return `None`, which becomes
"1 µm, 1 second per frame" downstream (`docs/OBJECTMODEL.md` → *Calibration — three copies, one stamp*). Only a
real store of each format catches that, so both are committed. See `docs/todo/ZARR_V3_PLAN.md`.

```
projects/ZARRFMT/0/ZV2img/ccidImage.ome.zarr   NGFF 0.4 / zarr v2   (.zattrs + .zarray)
projects/ZARRFMT/0/ZV3img/ccidImage.ome.zarr   NGFF 0.5 / zarr v3   (zarr.json, SHARDED)
```

Content — identical in both, so a test can assert the two agree rather than hardcoding twice:

- **Real pixels**, a 64×64 crop from the middle of `M2b-CD8-GFP-CD20-Tom.tif` (the source of image
  `k3Tx90`): `3t × 4c × 3z`, `uint16`, values 43–3863. Cropped rather than synthesised so the codec
  and the intensity distribution are representative; a `.fake` gradient compresses unrealistically.
- **Real calibration**, which is the whole point: `x`/`y` = 0.5964 µm/px, `z` = 3 µm, `t` = 30 s, with
  explicit axis units. Deliberately **not** 1.0 — a fixture whose scale is 1.0 cannot tell a correct
  read from the fallback that silently means "unknown".
- Series layout (`0/` wrapper + `OME/METADATA.ome.xml`), `omero` rendering metadata, one resolution
  level — i.e. shaped exactly like a real import, not hand-written metadata.
- The v3 store is **sharded with shard ≠ chunk on purpose** (`--tile-width/height 32`,
  `--shard-width/height 64` → chunk `[1,1,1,32,32]`, shard `[1,1,1,64,64]`). Those two are easy to
  report the wrong way round, and a fixture where they are equal cannot catch it.

Regenerate (needs bioformats2raw ≥ 0.12.0 for `--ngff-version`):

```bash
pixi run python -c "
import numpy as np, tifffile
p='<...>/M2b-CD8-GFP-CD20-Tom.tif'; px=0.5964274525755702
with tifffile.TiffFile(p) as f:
    s=f.series[0].asarray(key=[(t*13+z)*4+c for t in [0,1,2] for z in [5,6,7] for c in range(4)])
tifffile.imwrite('src.tif', np.ascontiguousarray(s.reshape(3,3,4,512,512)[:,:,:,224:288,224:288]),
                 imagej=True, resolution=(1/px,1/px),
                 metadata={'axes':'TZCYX','spacing':3.0,'unit':'micron','finterval':30.0})"
FLAGS="--compression blosc --compression-properties cname=zstd --compression-properties clevel=3 --compression-properties shuffle=shuffle"
bioformats2raw --resolutions 1 $FLAGS src.tif ZV2img/ccidImage.ome.zarr
bioformats2raw --resolutions 1 --ngff-version 0.5 --tile-width 32 --tile-height 32 \
               --shard-width 64 --shard-height 64 $FLAGS src.tif ZV3img/ccidImage.ome.zarr
```

Note `shuffle=shuffle`, not `shuffle=1` (0.11.x) and not `byteshuffle` (documented by 0.12 but broken
upstream) — see `bf2raw_shuffle_values` in `app/src/config.jl`.
