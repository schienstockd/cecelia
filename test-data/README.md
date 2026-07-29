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
