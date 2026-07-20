# Import: 16→8-bit rescale on import (large-image workflow)

**Status:** building · **Branch:** `feat/import-8bit-rescale`

## Problem

Lab OIR files (`smb://…/Mertk-Kat-memTom-nucGFP/…`, each `.oir` + `_000nn` companions ≈ 44 GB,
16-bit) are unwieldy. The manual workflow was: copy to a local workstation → open in Fiji →
rescale histogram → save as 8-bit. The old framework's "transfer to local before import" tickbox
moved the bytes but never shrank them (it still imported a 44 GB 16-bit image). The size win in the
Fiji workflow is the **16→8-bit + contrast rescale**, not the transfer — that's the lever to
automate.

Today `ImportOmezarr` (`app/src/tasks/importImages/omezarr.jl`) shells out to
`bioformats2raw --resolutions N <src> <dst>` and nothing else. bioformats2raw is a faithful
converter — it preserves pixel type; it has no bit-depth-reduction or intensity-rescale flag. So
the reduction must be a **second pass** over the converted zarr.

## Locked decisions (from Dominik)

1. **Only the 8-bit survives.** No 16-bit kept. Re-quantifying at 16-bit later means re-importing
   from SMB — accepted (same as the Fiji workflow).
2. **Rescale window: per-channel `[min, max]` computed globally over the whole stack** (all T/Z),
   *not* a percentile at the top. Top of window = the channel's **true max** so no bright signal is
   ever clipped — the right call for these low-dynamic-range images (a blind cast from the full
   `[0, 65535]` range would collapse the signal into the first few 8-bit codes; windowing on the
   *measured* min/max maps the actual signal across the full 0–255).
   - Bottom defaults to the true **min** (0th percentile).
   - Both exposed as low/high percentile params (defaults `0.0` / `100.0`) so a hot/dead pixel that
     pins the max can be trimmed (e.g. high `= 99.9`) without code changes.
3. **Never keep 44 GB locally.** Source stays on SMB; the 16-bit is a transient scratch zarr,
   deleted after conversion. Permanent footprint = the 8-bit zarr (~half, less compressed).

## Design

Fold the rescale into `ImportOmezarr` as an opt-in post-step (default off → today's behaviour
unchanged for other users/images):

1. bioformats2raw → **transient nested 16-bit zarr** at a temp path in the image's `0/` dir
   (`--resolutions 1`: single level, no pyramid — we discard this).
2. `read_ome_metadata(transient)` **before** deleting it — the nested bioformats2raw layout the
   reader understands. This is where SizeC/T/Z, channel names and physical sizes come from
   (see *Metadata trap* below).
3. `run_py` rescale runner reads the transient via `zarr_utils`, computes an **exact per-channel
   histogram over the whole stack** (streaming — sampling is noisy at the sparse bright tail of
   low-DR data), windows each channel to `[lo, hi]`, linear-maps + clips to `uint8`, and writes the
   final **flat 8-bit pyramid** (`create_multiscales`, `nscales = pyramidScale`) +
   `save_meta_in_zarr` (copies OME-XML calibration from the transient). Returns per-channel window
   + clip stats as a result JSON for QC.
4. Delete the transient 16-bit zarr.
5. Merge `read_ome_metadata` result into `ccid.json` as today; record the per-channel window in
   `meta` (reproducible; reusable to force the identical window on sibling images later).

Orthogonal, separate param — **stage source to scratch** (`stageLocal`, built): copy the source +
its companion set to a local scratch dir (`0/<uid>/_stage_src`) before conversion, point
bioformats2raw at the local copy, delete after the source read finishes. Independent of bit depth
(useful for a plain 16-bit import off SMB too).

**Why it's needed (not a nicety):** a multi-file format like Olympus OIR is read with many small
random seeks; over SMB each seek is a network round-trip, so the read is latency-bound and glacial.
Copying the whole set locally first is one sequential, throughput-bound transfer — the copy-to-tmp
trick, automated. No bioformats flag changes its access pattern; staging is the fix.

`_companion_files` matches the main file + its companions by LITERAL stem prefix (never interpolate
the stem into a regex — `basal+NECA` would break it). **Real Olympus naming** (fixed 2026-07 after it
shipped broken): the registered file already ends in `_NNNN.oir` and the companions are
EXTENSIONLESS — `M1a-…-res_0001.oir` (main) + `M1a-…-res_0001_00001`, `_00002`, … . So the match is
`<main-stem>_<digits>` (optional extension), NOT a fixed `_<5 digits><same-ext>`; the first version
matched none of the extensionless parts, so only the main staged and bioformats saw ~4 of 181
timepoints. The literal-stem prefix still excludes a sibling acquisition (`…-res_0002.oir`).

**Non-blocking copy:** the copy is chunked with an explicit `yield()` per block (`_copy_file_yielding`),
NOT Julia's `cp`. `cp` is a single non-yielding blocking call (`jl_fs_sendfile`); when the pool worker
running it is scheduled onto the event-loop thread, a multi-GB copy froze the whole GUI until done.
Chunking + yielding keeps the WS server responsive and reports staging progress via `on_progress`.

**Peak disk:** with staging + 8-bit, the stage copy and the 16-bit transient briefly coexist during
conversion (≈ 2× the source), plus the 8-bit output. The stage copy is deleted the moment
bioformats2raw finishes reading, before the rescale pass. Needs local scratch on the projects disk.

### Metadata trap (documented in CLAUDE.md → *OME-ZARR dual-format*)

`read_ome_metadata` hardcodes the **nested** bioformats2raw layout (`zarr/0/.zattrs`);
`create_multiscales` writes the **flat** layout (root `.zattrs`). So metadata MUST be read from the
transient nested zarr, never from the flat 8-bit output. The flat 8-bit output still renders
correctly in napari — `create_multiscales` writes the `.zattrs` scale and `save_meta_in_zarr` writes
the `OME/METADATA.ome.xml` sidecar — exactly like the existing flat outputs of drift/AF/cellpose
correction. Spatial calibration + ccid `meta` are correct.

**napari on the flat 8-bit output (code-traced, not yet run live):** the timestamp overlay is
expected to work. napari's `read_time_increment` reads `OME/METADATA.ome.xml` `Pixels.TimeIncrement`,
and although the output is the flat layout, that sidecar IS written (`save_meta_in_zarr`) and
`sync_zarr_calibration!`'s `update_ome_xml_pixels!` patches it **layout-agnostically** (it's keyed on
the OME-XML file path, not `0/.zattrs`). So the derived `DeltaT` frame interval lands where napari
reads it. Spatial x/y/z scale is also correct (`create_multiscales` writes it from `dim_utils`).

The only writer that no-ops on the flat layout is `update_ome_scale!` (targets `0/.zattrs`), so the
NGFF **axis units** are omitted and the `.zattrs` **t-axis scale** isn't corrected — the SAME residual
every existing flat output (drift/AF/cellpose correct) already has. Impact is cosmetic (napari's
"units" label / time-slider step), not the voxel size or the visible timestamp.

**Verify on the first real import** (the one thing worth eyeballing, since the above is code-trace not
a live run): the timestamp overlay reads seconds (not a frame index) and the voxel size looks right.

## Files

- `app/src/tasks/importImages/omezarr.json` — params: `convertTo8bit` (bool), advanced
  `rescaleLowPercentile` / `rescaleHighPercentile`.
- `app/src/tasks/importImages/omezarr.jl` — transient-path + `run_py` rescale + delete + QC.
- `python/cecelia/utils/intensity_utils.py` — `stack_channel_ranges` (streaming per-channel
  min/max or percentiles) + `rescale_to_uint8`. Pure, unit-tested.
- `python/cecelia/tasks/importImages/rescale_to_8bit_run.py` — runner (mirrors `af_correct_run.py`).
- QC in `omezarr.jl`: per-channel clip fraction (blown-out/dead channel → warn), flat-channel warn;
  `COHORT_METRICS` entry.
- Tests: `python/cecelia/tests/test_intensity_utils.py` (golden rescale), `app/test/runtests.jl`
  (param validation + fun_name dispatch already covered for import; add the new params).

## QC

All banked in the one `importImages.omezarr` doc via `write_metadata_qc!` (recomputed from persisted
meta on import/resync/edit, so findings + metrics never diverge). Pure helpers in `qc.jl`,
unit-tested in `app/test/runtests.jl`.

**Metrics** (`COHORT_METRICS["importImages.omezarr"]`):
- `import_metrics` (EVERY import) — `nChannels`/`nZ`/`nT` from `SizeC`/`SizeZ`/`SizeT`. This also
  closes a pre-existing gap: the base import task banked findings but no objective metric, so it never
  fully met the "metric for every result-producing task" convention. An odd channel count /
  dimensionality vs cohort peers = wrong file or acquisition misconfig.
- `rescale_metrics` (converted images only) — `nChannelsClipped`, `nChannelsFlat`.

**Findings** (`rescale_qc_findings`, one per channel, first applicable):
- **warn** `rescale.channel_flat` — a channel has (near-)zero range → 8-bit output is blank.
- **warn** `rescale.channel_clipped` — a channel clips > 1% of pixels (high percentile too low).
- **warn** `rescale.hot_pixel` — true max ≫ 99.9th percentile → a hot pixel pins the window and
  squashes the signal; lower the high percentile.

Note: import has no "no output" finding — a failed conversion returns before QC is written, and the
calibration findings already cover "output looks wrong".
