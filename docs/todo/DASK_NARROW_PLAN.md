# Narrow dask to where it earns its keep

**Status:** parked (2026-10-10) on `docs/dask-narrow-plan`. Nothing built. The `zarr_utils.py`
overlap with chunk-inherit has cleared: that work merged as #1576, and this branch is rebased on it.

## Goal

Importing a `cecelia.utils` module must not import `dask.array`. dask stays available to the few code
paths that actually use its graph, and those paths import it inside the function.

This continues [`ZARR_STREAMING_PLAN.md`](ZARR_STREAMING_PLAN.md) Decision 2: reads use plain
`zarr.Array`, and `as_dask=True` is the exception. That plan turned down a blanket sweep because it
was "churn without functional gain". The gain below is measured, so this plan does the sweep, but only
up to that gain.

## Why (measured 2026-10-10, `python -X importtime`, this machine)

| import | warm | `dask.array` loaded |
|---|---|---|
| `cecelia.utils.zarr_utils` | 0.69–0.75 s (0.83–1.06 s on the next runs; 2.5–4.6 s cold) | yes |
| `cecelia.utils.segmentation_utils` | 0.72 s | yes |
| `zarr` alone | 0.21–0.26 s | no |

`zarr_utils` itself takes 0.5 ms. Its top-level `import dask.array` (`zarr_utils.py:20`) accounts for
about 0.49 s:

- xarray → pandas → pyarrow, about 0.16 s: `dask/array/core.py:33` imports xarray whenever it is
  installed.
- `scipy.fft`, about 0.09 s: `dask.array.fft` is always imported.
- `scipy.sparse`, about 0.07 s: `dask.array.chunk_types`.

Every Python process that touches a store pays this. For a task that runs for minutes it doesn't
matter. For an interactive one-shot spawn it does: the `chunk-inherit` session found that importing
`zarr_utils` made the import wizard's peek script too slow, and worked around it by passing the chunk
size in from Julia instead.

## Inventory (origin/main @ db420f40, 2026-10-10; full grep, not truncated)

**Real dask use, in live task paths**
- `segmentation_utils.py:829`: the normalisation histogram for a single-level store, via
  `da.from_array` and then `intensity_utils.channel_histograms` (`da.bincount`).
- `app/src/tasks/importImages/saturation_run.py:44`: `open_as_zarr(as_dask=True)`. It passes the
  whole level 0 to `channel_histograms`.
- `app/src/tasks/editImages/register_run.py:101`: `open_as_zarr(as_dask=True)`. It only slices planes
  and calls `fortify`, so it doesn't need dask.

**Real dask use, outside the app**
- `rechunk_zarr.py:124`: a manual CLI that streams level to level with
  `da.store(da.from_array(...), lock=False)`.
- `scripts/agent_eval/crop.py:66`, and about 20 `docs/todo/flow-seg-experiments/*.py` scripts:
  `as_dask=True` followed by `.compute()`.

**Dask branches nothing in the app reaches** (no producer of a dask array was found)
- In `zarr_utils`: `open_zarr(as_dask=True)` and `zarr_data_to_dask`, plus the dask branches of
  `fortify`, `chunks`, `create_multiscales` (1305–1321) and `write_multiscale_pyramid` (1378).
- `block_transfer.place_block_lazy`: no callers. It was built for the napari bridge, which has been
  retired.
- `correction_utils.py:16`: the import is unused, and the module docstring ("returned as a dask
  array") is stale.
- `intensity_utils.py:15–19`: a module-level `try: import dask.array`, used only by `_is_dask` and
  the `da.bincount` branch.

**Test-only:** `app/test/suite/ome_qc.jl:292` feeds `da.from_array` into `create_multiscales`.

## Decisions (2026-10-10)

1. **Narrow, don't retire.** dask stays a dependency. squidpy (which `spatial_utils` uses),
   spatialdata, ome-zarr and distributed all require it, so it can't leave the pixi env. Removing it
   from `pyproject.toml` would buy nothing.
2. **No module-level `import dask` anywhere under `python/cecelia/utils/`.** A function that needs
   dask imports it locally. A function that only has to *recognise* a dask array must not import
   dask to do it. The check already exists as `intensity_utils._is_dask`. Move it to `zarr_utils` as
   the one helper, have `intensity_utils` import it, and make the body
   `'dask.array' in sys.modules and isinstance(a, sys.modules['dask.array'].Array)`. If dask was
   never imported, the input cannot be a dask array.
3. **The guard is an import-isolation test, not a grep.** In a subprocess: import each
   `cecelia.utils` module, then assert `'dask.array' not in sys.modules`. Write it first and confirm
   it fails on main. A grep for "top-level import" misses dask arriving through another module.
4. **Task runners read with plain zarr.** `saturation_run` and `register_run` switch to
   `as_dask=False`. `ZARR_STREAMING_PLAN.md` deferred this "until the file is touched"; this plan
   touches them.
5. **`as_dask=` stays as an opt-in.** The eval script and the experiments use it. It costs nothing
   once the import is lazy (inside `zarr_data_to_dask`).
6. **The `create_multiscales` dask branch stays.** It carries the `da.store` race fix, which
   `test_zarr_store` regression-tests. `ZARR_STREAMING_PLAN.md` 3.3 stays parked. Only its type check
   changes, to the Decision 2 helper.
7. **`channel_histograms` changes only if a measurement supports it.** Today its numpy branch does
   `np.asarray(whole channel)`, so handing it a zarr level would cause an out-of-memory error. It
   needs a per-timepoint `np.bincount` accumulation, the same as `correction_utils.py:1888`
   (`af_weight_stats`). That is single-threaded where `da.bincount` is threaded. Time both on a large
   single-level movie before switching. `ZARR_STREAMING_PLAN.md` Decision 3 (no Python-side pool)
   favours the loop, unless it is materially slower.

## Phases (each can ship on its own)

### P0: guard (red on main)
The import-isolation test from Decision 3. Its first run lists every module that currently pulls in
dask.

### P1: lazy imports (the latency win; behaviour unchanged)
- `zarr_utils`: delete lines 20–21. Add the `_is_dask` helper (Decision 2). Move the import into
  `zarr_data_to_dask` and the dask branch of `create_multiscales`.
- `segmentation_utils`, `rechunk_zarr`, `intensity_utils`: move the import into the function that
  uses it. `intensity_utils._is_dask` moves to `zarr_utils` (Decision 2).
- `correction_utils`: delete the import and fix the docstring.
- Check: P0 turns green. Re-measure `import cecelia.utils.zarr_utils` with a target of about 0.25 s
  warm, the cost of `zarr` alone. The full suite passes, including `test_zarr_store`.
- Follow-up: tell the `chunk-inherit` author that their workaround can go back to importing
  `zarr_utils`, if they want that.

### P2: task paths off dask (needs the measurement from Decision 7)
- Add a per-timepoint accumulation path to `channel_histograms` for zarr and numpy input.
- `segmentation_utils._compute_norm_params`: pass the zarr level through and drop `da.from_array`.
  `_subsample_time` is plain slicing, so it carries over.
- `saturation_run`, `register_run`: `as_dask=False`.
- Measure: the `channel_histograms` wall-clock and peak RSS, old against new, on one big single-level
  store (a drift-corrected movie). The norm-params values must not change, since a histogram is exact.

### P3: dead code + docs
- Delete `block_transfer.place_block_lazy` and its test. Drop it from the `docs/inventory/DATA_ACCESS.md`
  row.
- `CLAUDE.md` 206–210: replace the "`dask.array` is opt-in inside a task runner" rule with Decision 2
  plus a pointer to the P0 test. Delete `test_zarr_access_convention.py`'s `DASK-OK` check if P0
  covers it, or narrow the check to app runners if it doesn't. Don't keep two rules for one invariant.

## Not doing
- Removing dask from the environment (Decision 1).
- Retiring the `da.store` + rechunk branch (`ZARR_STREAMING_PLAN.md` 3.3).
- Migrating the `docs/todo/flow-seg-experiments/` scripts. `as_dask=True` keeps working for them.
