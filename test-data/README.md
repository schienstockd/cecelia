# Cecelia test data

Version-controlled fixtures for the headless test suite (`cecelia-pineapple/app/test/runtests.jl`)
and for opening a minimal project in the GUI. Kept at the workspace root so tests do **not**
depend on the deletable dev projects directory (`projects_dir()`).

Tests resolve this via `fixture_path(...)` / `have_fixture(...)` in `runtests.jl`
(root = `<workspace-root>/test-data/projects`, override with `CECELIA_TEST_DATA`).

## Layout

A complete, loadable project (openable in the GUI to exercise the gating page, not just the
headless tests):

```
projects/testpr/
  project.json                # CciaProject, set_uids = ["testst"]
  1/testst/ccid.json          # CciaSet, image_uids = ["KDIeEm"]
  1/KDIeEm/
    ccid.json                 # CciaImage — labelProps "B", 4 channels (ch0–ch3)
    labelProps/B.h5ad         # real Cecelia-produced AnnData: 1377 cells × 27 features,
                              # 4 channels, 3D centroids + temporal; + btrack lineage &
                              # per-cell live.cell.* in obs
    labelProps/B__tracks.h5ad # companion per-track table: 62 tracks, 10 live.track.* in X/var
```

`B.h5ad` is a real measured output (image `KDIeEm`). Keep fixtures small and document any
additions here.

`B.h5ad` has 20 timepoints and can be used for end-to-end btrack tests (cell linking across time).
It is tracked + measured, so it pairs with `B__tracks.h5ad` to exercise the
`pop_dt(…; granularity=:track)` per-track read path (see `docs/DATAMODEL.md`).
