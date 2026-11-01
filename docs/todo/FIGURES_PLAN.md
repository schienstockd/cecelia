# Figures — paper figures from the raw data, in one standard look

**Status:** parked (2026-11-01). Not started. Trigger to un-park: the next "can you give me a still /
strip of …" request. Until then, one-offs go through P0 below.

## Goal

Make a **paper figure** (a still, a filmstrip, a z-strip of one cell) from the raw image at native
resolution, with flexible content (which cell, frames, channels, crop, contrast, rows) and a fixed,
standard look (fonts, labels, scale bar, legend). The app renders it from a stored spec, so Claude
writes a spec and never a renderer.

## Where this came from

Dominik's PI, about a filmstrip a Claude session made on 2026-10-31 (the `feat/roi-triage` session,
zolIMa / UMq4v3, a macrophage with an ingested tdTom fragment):

> "Yes can u give me a still of any of the images where there is fragment inside but high res/contrast?
> Like from this series"

Requests like this keep coming. Today each answer is a bespoke script. **No view in the app made that
strip.** The session wrote `ingest_strip.py`, `ingest_z.py` and `ingest_movie.py` in its scratchpad
(numpy + PIL, ~40 lines each). What they did (verified, read 2026-11-01):

- Read `ccidImage.ome.zarr/0` (**level 0, native**) straight from a hard-coded path, bypassing
  `zarr_utils`. Crop: `H=45`, so 90 × 90 native px at 0.3315 µm/px (also hard-coded).
- z: max over 3 planes (`z-1..z+1`). Channels: c2 tdTom → magenta, c3 Katushka → green (c1 read, weight 0).
- Contrast: per channel, **one** 5th–99.7th percentile range over all frames of the strip (shared).
- Enlarged ×4 with `np.kron` (nearest neighbour), so the "low res" look is native pixels drawn as
  blocks. The strip was already as sharp as the data allows.
- Rows: merge, tdTom alone, macrophage-label outline (orange). Labels came from an ad-hoc gate written as
  thresholds in the script (`arcsinh` cut-offs), not an app population.
- Labels: `t{t}  h:mm`, with the 15-min interval hard-coded. PIL's default bitmap font. 5 µm scale bar.
- `ingest_z.py`: the same cell as a row of z planes (`z±4`, labelled `+/-µm`). `ingest_movie.py`: the same
  frames to mp4 via ffmpeg.

So "high res" cannot mean more detail than the camera recorded. It can mean: a larger output with
smooth interpolation rather than blocks, proper fonts at print size, a tighter contrast, a single
frame instead of a strip. Denoising is already spent: UMq4v3 is itself cropped from a denoised
image (Dominik, 2026-11-01).

## What already exists (verified 2026-11-01)

- **The view snapshot and the movie record.** `docs/todo/ANIMATION_PLAN.md` (built) set out to make
  "the figures and movies people actually publish", reproducible months later, on one persisted,
  GUI-editable view snapshot. Its north star is this request almost word for word ("your boss asks to
  turn the green cells red … reopen the snapshot, change one colour, regenerate"). Movies shipped;
  stills-as-figures did not. A movie's `MovieRecordConfig` (`api/src/movie_config.jl`) carries the
  image, the snapshot's `look` (channels + overlays) and a title card. `register_movie!`
  (`api/src/movies_api.jl:137`) banks it into `settings/movies.json` with `configKind` /
  `configVersion`, tags, star and remake-from-config. The Movies page lists them
  (`docs/todo/MOVIE_MANAGEMENT_PLAN.md`, built). That is the closest existing home for a figure.
- **The shared still renderer** (PR #1393, `docs/todo/STILLS_WORKER_PLAN.md`): the preview worker's
  `render` (`preview/preview_worker.py:1549`) → `render_animation_run.render_stills` →
  `render_frames` (`python/cecelia/writers/render_animation_run.py:270`), on the viewer's shaders.
  It already does: level 0 when it fits a texture, a crop, a z slab as max projection (`zRange`),
  per-channel LUTs, label/mask overlays, and timestamp + scale bar via `title_card.draw_frame_overlays`
  with one font stack (`python/cecelia/utils/title_card.py`). A still and a movie frame of the same
  state are the same pixels.
- **The Analysis board's image strip** (`filmstrip`, `ImageStripView.vue`): screenshots of the pop-out
  viewer at screen resolution, with a `viewState` snapshot + `imageUid` as provenance. Its `viewState`
  is the closest existing description of "what to draw".
- **Card filmstrips** (`render_medoid_filmstrip`, `api/src/behaviour_cards.jl:77`): small previews
  (`max_px` 320) for cards, not figures.
- **Claude's own-only write pattern** (#1856): `add_gate` / `set_gate` / `delete_gate` with `dry_run`,
  where the server enforces `createdBy` (`mcp/cecelia_mcp/gating_views.py:230`, `api/src/gating_api.jl:1401`).

## Decisions (2026-11-01)

1. **Figures are not the Analysis board.** The board is an overview of the analysis. It renders live
   from the data and changes when the data does. A figure is an artifact for a paper: chosen frames,
   crop, contrast and layout, fixed until someone re-renders it. Figures get their own home; the board
   may show one later but doesn't own it.
2. **One renderer: the shared still renderer.** Tiles come from `render_stills` (native level, slab,
   LUTs, label overlays). Figure layout (grid, row and column labels, legend, scale bar, gaps) is a
   composition step over those tiles that reuses `title_card`'s font stack. No numpy/PIL renderer per
   request.
3. **A declarative spec, stored in the project, rendered by the app.** Claude writes a spec and never
   rendering code. **A tile is the existing view snapshot** (the `look` a movie records, which the
   renderer already turns into `states` / `specs` / `zRange` / crop / overlays). The figure spec adds
   only the frame list, layout and style. No second schema for what a tile is. It is versioned and
   migrated as a new `MovieConfig` subtype (`FigureConfig` beside `MovieRecordConfig` /
   `MovieBatchConfig`, `api/src/movie_config.jl:34`), so `register_movie!` takes it unchanged.
4. **Read everything from the image, never hard-code it.** Pixel size, time interval and channel names
   come from the image metadata. Populations and tracks are app references (`valueName/pop`, track id),
   not thresholds in a script. Data access goes through `zarr_utils`.
5. **One display range per channel across a figure by default.** Frames of the same channel must be
   comparable. Per-frame contrast is an explicit opt-in, and the legend says so.
6. **"High res" means native data with honest enlargement.** Default interpolation is smooth, with
   nearest neighbour available. Output size is set in px or by DPI at a physical width. Nothing implies
   detail beyond level 0.
7. **Kinds stay few.** Start with `sequence` (frames of one region in time: the PI's strip, and a single
   still as a one-frame sequence). The second kind is `zstack` (one time point, planes in a row),
   because `ingest_z.py` already shows it's needed. Any further kind needs three recurring bespoke scripts first.
   Data plots stay on the board.

## Open decisions (Dominik's)

| # | Question | Pick |
|---|---|---|
| A | Where figures live | **Widen the movie pages; no new home.** Figures go in the movie registry as `configKind: "figure"` and are listed on the Movies page (star, tags, delete, remake, `.ccbundle` export come free). Authoring sits on the viewer ("Save as figure" next to Record) and on Animation, whose captured snapshot list is already the frame list a strip needs: interpolate it into a movie, or lay it out as panels. Rename by **label only** and keep the routes (`/movies`, `/animation`; 13 route references don't move): Movies → "Movies & figures", Animation → "Compose". Cost: `frontend/src/lib/navGroups.ts:66-68` labels + tips, the `main.ts` meta labels and the regenerated user docs / guide map. A separate Figures page would only pay if figures become multi-panel paper layouts that mix stills and data plots, and that is the Analysis board's job (Decision 1 keeps those apart) |
| B | Style preset scope | One app-wide default plus a per-project override. A preset change marks existing figures stale and never silently re-renders them |
| C | Output formats | PNG and 16-bit-safe TIFF for the image; SVG for the composition with the tiles embedded as raster, so labels stay vector for the journal. The SVG canonical is `frontend/src/plots/export.ts` (`svgDoc` / `svgImage` / `svgText`), so either compose SVG there or say why the backend needs a second serialiser |
| D | Default interpolation | Smooth (bicubic), with nearest neighbour a toggle |
| E | Does P2's GUI ship before the agent tools (P3)? | Yes: the value must stand without an agent |

## Phases

**P0 — the one-off (now, no framework).** Answer the PI with a single frame from the existing script:
one chosen `t`, a larger output with smooth interpolation, a tighter percentile range, and a real font.
Say plainly that the detail is native. It's a scratch script and doesn't need this plan.

**P1 — spec + renderer, headless.** A `sequence` and `zstack` spec, `render_figure(spec) → files` in
the REPL (`docs/REPL.md`). Tiles go through `render_stills` and the composition step through
`title_card` fonts. Writes into the movie store (`_movies_dir`, `api/src/movie_helpers.jl:70`). The store
only accepts `.mp4` today: `_valid_movie_name` is `^[A-Za-z0-9._-]+\.mp4$` (`api/src/routes.jl:21`), and
`register_movie!` drops anything else silently, so P1 widens both to `.png` / `.tif` / `.svg` and tests
that a figure row registers. Acceptance: on zolIMa / UMq4v3 it reproduces the PI's strip
(same cell, t50–t110, 3-plane slab, tdTom magenta + Katushka green, shared range, 5 µm bar) from a
spec, at print size, with the interval and pixel size read from the image. Registered via
`register_movie!` with `configKind: "figure"` (if A holds). Checkable headlessly.

**P2 — figures on the Movies page + "save this view as a figure".** A figure row with preview and a
form editor (frames, channels, range, rows, crop, style). "Save as figure" sits next to the viewer's
Record button and records the same snapshot as a still sequence (the board image strip's snapshot can
seed one too). Needs Dominik's eyes.

**P3 — Claude's tools.** Following #1856: `list_figure_kinds`, `add_figure(dry_run)` returning the
rendered preview as an image, and `set_figure` / `delete_figure` for Claude-made figures only
(`createdBy`). A user edit hands ownership to the user. Observer server; Kiwi's exclusion table
(`api/src/kiwi_turn.jl`) gets the writes; recorders count the preview as a look and a write as a
write. Validation errors name the field and the allowed values. Caps on frames and output px.

**P4 — staleness.** Each figure records the image version, segmentation / track / population run
stamps (see `docs/todo/EVIDENCE_PROVENANCE_PLAN.md`) and the preset version. A changed input marks
the figure stale on its row. That is data staleness; the registry's existing `configStale`
(`_config_stale`, `movies_api.jl:256`) only means the file was edited after recording. Re-render is
explicit.

**Later, if asked:** board placement of a figure; a movie from the same spec (the renderer and the
registry are already shared).

## Ways this goes wrong, and the test that catches each

1. **Wrong channel → colour, or a legend that disagrees with the pixels.** Test: a synthetic image with
   one channel lit per frame. Assert the lit tile's colour matches the spec and that the legend is
   derived from the same channel map.
2. **Contrast differs across frames without anyone choosing it.** Test: render a sequence and assert
   one recorded display range per channel unless `perFrame` is set. The legend prints "per-frame
   contrast" when it is.
3. **Preview ≠ export.** Both go through the one render call. Test: the preview tile and the
   export tile of the same spec are byte-identical at the same size.
4. **Spec creep into a plotting language.** Decision 7. Review gate: a new kind cites the three
   bespoke scripts it replaces.
