# Side-by-side version comparison in movies

**Status:** P1–P6 built, then GENERALISED (2026-08-08, see *Risks* item 6): versions and segmentation
masks are the two dimensions of a **grid**, `_record_columns!` became `_record_grid!` and the compose
became nested. Then (2026-08-14) `compareLayout` gained a third option, `grid`, which **wraps** one
list of N cells into a near-square rectangle (4 → 2×2) — `_wrap_grid` reshapes the rows before
`_record_grid!` sees them, so it reuses the nested compose rather than adding a compositor.
**The names below are the ones this plan was written against and are kept as the build
record — read `docs/NAPARI.md` → *Side-by-side comparison* and `INVENTORY.md` for what exists now.**
All four suites green. **Still not run against a live napari** — the recording loop needs the viewer,
so its passes, the shared-view apply and the composed output have not been exercised end to end.
Everything either side of it is unit-tested.
**Branch/worktree:** `work/movie-compare`, then `work/movie-seg-audit`
(`~/cc-workspace/cecelia/movie-seg-audit`).
**Related:** `docs/todo/ANIMATION_PLAN.md` (F1 batch / F2 animation / H title card), `docs/NAPARI.md`
→ *Movie output size*, `docs/UI.md` → *UX primitive catalog*.

## Goal

An image can carry several **versions** (`filepaths` value names: `default`, AF-corrected,
drift-corrected, temporally smoothed, cropped, denoised…). Today every movie surface records exactly
**one** of them, and comparing two means recording two files and playing them next to each other by
hand. This adds a **comparison movie**: the selected versions rendered as columns of one mp4, in a
user-chosen order, from both the single-shot recorder and the batch.

Secondary goal, in the same change: the single-shot movie form is crowded and gets worse with a
version picker in it — so it is reorganised, not just extended.

## Scope

| In | Out |
|---|---|
| Single-shot record (ViewerPanel → Movie) | Animation page (keyframe timeline — a comparison there is ill-defined; the mechanism is available later if asked) |
| Batch movies (`BatchMoviesPanel` → `movie:batch`) | Comparing different **images** side by side (the batch's unit is one movie per image) |
| N ≥ 2 versions of ONE image, ordered, row or column layout | Live side-by-side *in* the napari window (see D2) |
| Per-column caption (the version name) | Per-column independent contrast/colour (see D4) |

---

## Audit — what already exists (build on these, add nothing parallel)

Everything below is canonical; the feature is mostly wiring, plus one genuinely new primitive
(the frame stitcher).

### Frontend

| Need | Existing | Note |
|---|---|---|
| Toggle + drag-to-reorder chips | `components/ChipSelect.vue` (`multiple` + `reorderable`) + `utils/chipSelect.ts` | **This is the component the feature is built on.** Already used reorderable in `BatchMoviesPanel` for filename attrs |
| fps / size / filename suffix | `components/MovieOutputControls.vue` (+ `utils/movieSize.ts`) | Shared by all three movie surfaces already |
| Title card on/off + duration + note | `components/TitleCardControls.vue` (+ `TitleCardCfg` in `utils/batchMovie.ts`) | ditto |
| Collapse a section | `components/CollapsibleSection.vue` (has `storageKey` persistence) | |
| Popover that escapes a clipping ancestor | `components/TeleportPopover.vue` | ViewerPanel is a `FloatingPanel`, so an inline popup would clip — this exists for exactly that |
| Version list for an image | `Object.keys(image.filepaths)` — `valueNames` in `ViewerPanel.vue:91`, `versionNames` in `BatchMoviesPanel.vue:57` | Same derivation in both; extract once |
| Batch config build/persist | `utils/batchMovie.ts` (`buildBatchMovieConfig`, `movieFilename`, `seedConfigFromViewState`) + `stores/settings.ts` (`get/setBatchMovieConfig`, `get/setMovieConfig`) | Per-set persisted — the "persist every option" rule already satisfied |
| Canvas size for the size placeholder | `composables/useNapariStatus.ts` | one shared poll, don't add a second |

### Julia (`api/`, `app/`)

| Need | Existing |
|---|---|
| Open + dress ONE version, then record | `api/src/napari_api.jl` → `_apply_movie_config!(project_uid, image_uid, img, config; do_open)` — takes `config.valueName` and opens that version |
| Batch loop with progress/cancel/logging | `run_batch_movies` (same file) — already `open → apply → record` per image |
| Single record on the task rail | `run_single_movie` (same file) |
| Output naming | `_movie_named_path` / `_movie_out_path` / `_movie_basename` / `_movie_suffix` |
| Title-card content | `_title_card_content(img, config)` |
| Bridge record calls | `app/src/napari.jl` → `record_timelapse!` / `record_keyframes!` |
| WS entry points | `api/src/sockets.jl` → `handle_movie_record` / `handle_movie_batch` |
| Single-flight viewer + cancel flag | `_with_viewer`, `_batch_cancelled` / `request_batch_cancel!` |

### Python

| Need | Existing |
|---|---|
| The ONE mp4 writer + size policy | `python/cecelia/utils/movie_io.py` — `movie_writer`, `coerce_movie_size`, `crop_to_even`, `size_from_xy` |
| Frame loop, staged `.tmp.mp4` → promote, per-frame progress + cancel | `python/cecelia/utils/napari_utils.py` → `_render_animation` / `record_timelapse` / `record_keyframes` |
| Text on frames (fonts, wrapping, fitting) | `python/cecelia/utils/title_card.py` — `_font`, `_wrap_lines`, `_fit_prefix`, `render_card_frame`, `prepend_title_to_movie` |
| Bridge command surface | `napari/napari_bridge.py` → `execute_command`, `_record_hooks`, `PROTOCOL` (mirrored by `NAPARI_PROTOCOL` in `app/src/napari.jl`, asserted equal by a test) |

### The one real gap

**Nothing in the repo composites video.** One new primitive is needed: a frame-level stitcher. It
belongs in `movie_io.py`, which is already "the one writer and one size policy for every mp4".

---

## Decisions (locked)

**D1 — Record N passes and stitch the frames; do NOT composite inside napari.**
For each selected version: open it, apply the movie config, record to a temp mp4; then stitch the
temp files frame-by-frame into the final movie. Every existing invariant survives untouched — one
writer, one size policy, staged-then-promoted output, per-frame progress, working cancel, and full
overlay support per column (tracks/pops/labels/colour-by all work, because each pass IS today's
recording).

**D2 — Rejected: N versions as translated layers in one napari canvas.**
It would be one render pass and would also give a live side-by-side in the window, but
`NapariState` binds `_im_data` / `_axes` / `_channel_axis` / `_im_scale` to a **single** store, and
every overlay, the timestamp, the colour-by cache, the props autosave and the layer-name namespace
read that state. It is a rewrite of the bridge's core model to save render time. Revisit only if
somebody actually asks to *interact* with a side-by-side view.

**D3 — Cost is honest and stated: an N-version comparison is N full renders.**
The UI says so (a hint next to the chips, e.g. `2 versions → 2 render passes`), and the task log
says `[1/2] recording <version>` per pass, as the batch does.

**D4 — Contrast is a VISIBLE two-way toggle, not a hardcoded rule.**
Correction shifts the intensity range, sometimes only slightly, so neither answer is right for
every comparison:

| Mode | What happens | When it's right |
|---|---|---|
| **First version as reference** (default) | The look is captured after column 1 is dressed and applied to every later pass — contrast limits, colormaps, gamma, visibility (`capture_view_state` / `apply_view_state`, matched by layer name) | The usual case: one intensity scale, so a raw / AF-corrected / denoised / smoothed pair is judged on the same ruler. Per-version rescaling would hide the very difference being compared |
| **Each version's saved napari settings** | Nothing extra is applied — each pass keeps what `autoLoadProps` loads for that version | A version whose range genuinely moved, or one you have already tuned by hand in the viewer and want recorded as tuned |

The second mode costs nothing to support: layer props are keyed on the **zarr filename**
(`_props_path` → `basename(zarr_path)` in `api/src/napari_api.jl`) and every version is its own
store, so each version already has its own saved settings, and `_apply_movie_config!` already opens
with `autoLoadProps = true`. So the toggle is one line at the call site — pass the shared snapshot
**with** its `layers` key (reference mode) or **without** it (per-version mode);
`napari_utils.apply_view_state` already tolerates a snapshot with no `layers`.

Camera + dims are shared in **both** modes — same field of view, same timepoint, or it is not a
comparison. Only the intensity mapping follows the toggle. Either way the per-column contrast
limits are written to the task log, so a figure's scaling is recoverable after the fact.

For a single-shot record, column 1 is the version currently open, so reference mode records what is
on screen.

**D5 — The size field means the size of ONE column.**
`coerce_movie_size`'s 4096 clamp is a GL-canvas constraint, so it stays a per-pass rule. The
stitched file is `N × width` (row) or `N × height` (column) and may legitimately exceed it; the
final dimensions are logged. Each column is even by construction, so the sum is even.

**D6 — One title card, on the stitched movie, not per column.**
Passes record with `title_card=None`; the card is composited onto the finished stitch by the
bridge's existing `napari_utils._maybe_prepend_title(viewer, path, card)` — so `stitch_movies` stays
viewer-free and card-free, and the Channels legend still comes from the live viewer (same channels
in every column).

**D7 — Columns are captioned with the version name.**
A caption band drawn per column with the existing `title_card` font helpers — a comparison without
labels is unreadable in a figure. (`default` renders as `default`; no renaming.)

**D8 — The version picker becomes ONE control on both surfaces: a multi-select, reorderable
`ChipSelect`.** 0 selected = the active version (today's default). 1 = record that version (replaces
`BatchMoviesPanel`'s "Image version" `<select>` outright). ≥2 = side-by-side, in chip order. There is
no separate "compare" mode switch — the selection *is* the mode.

**D9 — The Julia helper takes column SPECS, not version names.**
`_record_columns!(…, columns::Vector{<:NamedTuple})` where each column is `(label, config)`. The UI
only ever produces version columns, but comparing two colour-by measures or two segmentations is
then a config change, not a rewrite. One helper, used by both `run_single_movie` and
`run_batch_movies` — no second copy of the pass loop.

**D10 — Frame-count mismatch holds the last frame and logs it.**
Versions of one image normally share T, but a temporally-resampled version might not. Padding with
the last frame is the readable failure; silently truncating is not. Frame *size* mismatch cannot
normally happen (every pass screenshots the same canvas/requested size), so it is a padded, logged
safety net only.

---

## Architecture

```
Frontend                     Julia (api/)                     Bridge (napari/)          Python lib
────────                     ────────────                     ────────────────          ──────────
MovieCompareControls   ──►   handle_movie_record  ──►  _record_columns!
  (ChipSelect chips)         handle_movie_batch          │
  versions[] + layout                                    ├─ per column:
                                                         │    api_napari_open(valueName)
                                                         │    _apply_movie_config!
                                                         │    capture/apply_view_state   (D4)
                                                         │    record_timelapse!  ──►  record_timelapse ──► napari_utils
                                                         │      → {final}.col{i}.tmp.mp4                   ._render_animation
                                                         │                                                  (unchanged)
                                                         └─ stitch_movies!      ──►  stitch_movies    ──► movie_io
                                                              → {final}.mp4                                .stitch_movies  ← NEW
                                                                                                           (+ movie_writer,
                                                                                                            title_card fonts)
```

### New / changed files

| File | Change |
|---|---|
| `python/cecelia/utils/movie_io.py` | **NEW** `stitch_movies(paths, out_path, *, fps, labels=None, layout='row', title_card=None, on_progress=None, should_cancel=None)` — read each input with `imageio.v2`, pad short/small inputs (D10), draw the caption band (D7), `np.hstack`/`vstack`, write through `movie_writer`, stage as `{out}.tmp.mp4` and promote. Reuses `title_card._font` (promoted to a public `font()`) |
| `python/cecelia/tests/test_movie_io.py` | stitch tests: geometry (row/column), unequal lengths, unequal frame sizes, caption band, staging/promotion, cancel mid-stitch |
| `napari/napari_bridge.py` | new `stitch_movies` command (viewer-independent, forwards to `movie_io` + reports progress via `_record_hooks`); `_record_hooks` gains a frame offset/total so a multi-pass record shows ONE continuous bar; bump `PROTOCOL` to 3 |
| `app/src/napari.jl` | `stitch_movies!(v, out, paths; …)` wrapper; bump `NAPARI_PROTOCOL` to 3 (a test asserts the pair agree); `record_timelapse!` gains the frame offset/total passthrough |
| `api/src/napari_api.jl` | **NEW** `_record_columns!` (D9) — the pass loop + stitch + title card, used by both runners; `run_single_movie` and `run_batch_movies` delegate when >1 column; comparison suffix default |
| `api/src/sockets.jl` | `movie:record` accepts `valueNames` + `compareLayout`; `movie:batch` reads `config.valueNames` |
| `frontend/src/components/MovieCompareControls.vue` | **NEW** shared control — the reorderable `ChipSelect` over versions + the row/column segmented picker + the "N render passes" hint |
| `frontend/src/utils/movieCompare.ts` (+ `.test.ts`) | pure logic: normalise the selection against the versions an image actually has, the default filename suffix (`a-vs-b`), the pass-count label, the persisted-config migration `valueName` → `valueNames` |
| `frontend/src/components/ViewerPanel.vue` | Movie section redesign (below) |
| `frontend/src/modules/batchmovies/BatchMoviesPanel.vue` | "Image version" `<select>` → `MovieCompareControls` |
| `frontend/src/stores/settings.ts` | `movie.compareVersions` / `movie.compareLayout` / `movie.compareContrast` (per set); `BatchMovieCfg.valueNames` + `compareContrast` |
| `docs/NAPARI.md`, `docs/UI.md`, `INVENTORY.md`, `docs/todo/ANIMATION_PLAN.md` | movie section, catalog row, inventory lines, plan pointer |

### Temp file naming

Per-pass output is `{final}.col{i}.tmp.mp4`. That ends in `.tmp.mp4`, so it is already hidden by
`/api/movies` (`routes.jl` filters `.tmp.`) and already swept by
`napari_utils._clear_stale_staging` after a killed bridge. No new cleanup path.

---

## UI redesign

### Single-shot (ViewerPanel → Movie)

Today the section is: `MovieOutputControls` (wraps to two lines in the floating panel) + a record
button + `TitleCardControls`. Adding version chips inline makes it four dense rows in a narrow
panel.

```
Movie
  [default] [af_corrected] [+drift]   [⚙] [●]      ← ChipSelect: click to compare, drag to order
```
One row. An image with a single version — the common case — shows just the two buttons, so the
section gets *smaller* than it was. The chips wrap inside their own flex child so the buttons keep
their place; a settings summary line was tried here and dropped (it is the explanatory text the house
style says not to add, and the gear's tooltip already names what is behind it).

- The **compare chips stay visible** — they are the feature, and they also read as "which version am
  I about to record".
- Everything else (fps, size, filename suffix, layout, contrast mode (D4), title card) moves into a `TeleportPopover`
  behind the ⚙, which is the canonical primitive for exactly this and escapes the `FloatingPanel`
  clipping. Nothing is removed, and the resting state drops from ~4 rows to 2.
- The chip row is hidden entirely when the image has only one version (most images), so the common
  case gets *smaller*, not bigger.
- Alternative considered: wrap the body in `CollapsibleSection`. Rejected — it hides the settings
  behind an equally invisible chevron *and* keeps them in the narrow column when open.

### Batch (`BatchMoviesPanel` → "Image version")

Straight replacement of the `<select>` with the same `MovieCompareControls`, so the two surfaces
offer one control with one behaviour. The section stays where it is; the batch panel is not
crowded (it has the `PaneExpandBar` two-half layout already).

### Filename

The suffix currently defaults to the version name (so a corrected recording does not overwrite a raw
one). For a comparison it defaults to the versions joined — `default-vs-af_corrected` — still
user-overridable in `MovieOutputControls`.

---

## Phases — all built (kept as the build record; see *Status*)

**P1 — the stitcher (Python only).** `movie_io.stitch_movies` + tests. Nothing user-visible; fully
testable with `pixi run test-py` against generated mp4s.

**P2 — the bridge command + Julia wrapper.** `stitch_movies` command, protocol bump on both sides,
frame offset/total for a continuous progress bar. Verifiable from the REPL against two existing
movies.

**P3 — `_record_columns!` + the single-shot path.** WS `valueNames`, the pass loop, D4 view
sharing, the title card on the stitch, the comparison suffix. Ship with the ViewerPanel UI (P5)
or behind it via the REPL first.

**P4 — the batch path.** `config.valueNames` through `run_batch_movies` → `_record_columns!`. No new
mechanism, just the second caller.

**P5 — the UI.** `movieCompare.ts` + tests, `MovieCompareControls.vue`, the ViewerPanel Movie-section
redesign, the BatchMoviesPanel select replacement, settings persistence + migration.

**P6 — docs.** `docs/NAPARI.md` (how a comparison is produced, the per-column size rule),
`docs/UI.md` catalog row, `INVENTORY.md` entries for the new control + `stitch_movies`,
`ANIMATION_PLAN.md` cross-reference. Delete this plan's superseded parts / promote the durable ones.

---

## Risks & open questions

1. **Render time is N×.** A 3-version 4K comparison of a 200-frame stack is three full renders. D3
   makes it visible; there is no way around it under D1.
2. **The napari window is taken over for longer.** The batch already warns; the single-shot record
   does not (it was one pass). The busy state should be surfaced for a multi-pass single record too.
3. **Cancel granularity.** Cancel is checked per frame inside a pass and between passes (both exist);
   the stitch also needs a check, or a cancel during stitching waits for it. Stitching is fast
   relative to rendering, but not free.
4. **Disk.** N temp mp4s live alongside the final one until the stitch completes.
5. **Versions with different channel names.** `imChannelNames` is versioned, so a version *could*
   name its channels differently; then D4's layer-name matching leaves that column with its own
   props. Acceptable (and rare) — log which layers did not match.
6. ~~**Open question:** should a comparison also be offered across **segmentations**?~~ **BUILT**
   (2026-08-08) — and it went further than the question asked. Versions and masks are now the two
   dimensions of a **grid** (versions across, masks down), so picking two of both gives the
   cross-product rather than forcing a choice between them. Three things were added: a second column
   builder (`_segmentation_columns`), the grid builder that arranges them (`_compare_grid` /
   `compareShape`), and — the part D9 could not have predicted — the ability to say "show these masks"
   at all.

   D9 half-held. The column SPEC was indeed enough to vary what a cell shows, but the pass loop itself
   had to grow a second dimension (`_record_columns!` → `_record_grid!`) and the compose became nested.
   `movie_io.stitch_movies` needed no change at all — a grid is two passes of the one-dimensional
   stitcher, rows then strips.

   D1's claim that "overlays … all work, because each pass IS today's recording" was **wrong for label
   masks**: `open_image` clears the canvas and `_apply_movie_config!` restored tracks, points and
   colour-by but never the label layers, so a comparison silently dropped them (and the batch's
   `labels` chip was a no-op on every image after the first). See
   `docs/todo/MOVIE_SEGMENTATION_AUDIT.md` and `docs/NAPARI.md` → *Side-by-side comparison*.

   **New consequence worth stating up front:** the cost is now MULTIPLICATIVE. D3 said an N-version
   comparison is N full renders; a grid is rows × cols. 3 versions × 2 masks is six renders of the
   whole timecourse. The action button's tooltip states the count, and the progress bar spans every
   pass and every compose (`_grid_frame_total`).
