# Session A — P5-a: movie rail wiring off napari

Branch: `feat/viewer-p5-movie-rail` (fork from origin/main).

## Goal

The overlay + mask authors + `record_view_movie` are BUILT — verified end-to-end via
`POST /api/viewer/record-test` (see `docs/todo/WEB_VIEWER_PLAN.md` P5). What's left is
routing the actual **record button** and the batch-movie config off the napari path onto
`record_view_movie`, so the movie rail (progress, cancel, config storage) is the same
path the smoke route already exercises.

This is P5's last remaining item. Once it lands, P8 (decommission) unblocks.

## What to build

1. **Route the record button through `record_view_movie`.** Today the Vue record UI posts
   to whatever `handle_movie_record` / `run_single_movie` currently dispatches to (a napari-
   driven path). Redirect it so a single-clip record runs `record_view_movie` on `jobs.jl`
   with:
   - progress rail (WS `job:progress` events; use `on_progress(n, t)` callback)
   - cancel button (register the encoder process via `on_process(proc)` so `task:cancel`
     kills it; `record_view_movie` already threads `cancelled = () -> false` — plumb it)
   - config storage (the record request's view spec → same movies.json entry the smoke
     route writes via `register_movie!`)

2. **Batch-movie config.** Same shape as (1) but for the batch config surface. Read the
   saved config → for each image, call `record_view_movie` with the resolved specs. Reuse
   `resolve_image_version` + `resolved_display_specs` — the smoke route does both.

3. **Overlay + mask kwargs.** The record request already carries a view spec (pops, tail,
   colour mode, mask visibility). Pass them through to `build_overlays_for` +
   `build_mask_for` the same way the smoke route does. Do NOT re-derive the mapping — read
   `api/src/viewer_api.jl → api_viewer_record_test` and copy the wiring block.

## Files to touch

- `api/src/viewer_api.jl` — new/refactored `api_viewer_record` handler (or wherever
  `handle_movie_record` lives on the current bridge path)
- `app/src/jobs.jl` — new/extended job kind if `record_view_movie` needs one
- `frontend/src/**` — the record button + batch-movie config panel (grep for
  `movie:record` / `handle_movie_record` to find the current call site)
- `api/test/runtests.jl` — a testset that exercises the new handler against the fixture

## Files NOT to touch

- `api/src/overlay_author.jl` — the authors are locked; if you find you need to change
  them, that's a separate PR. Add a request-side adapter instead.
- `api/src/movie_render.jl` — the sweep is locked (tested end-to-end via the smoke route).
- `api/src/image_render.jl`, `api/src/frame_overlays.jl` — pure primitives, don't touch.
- `frontend/src/plots/plot.ts`'s palette or `viewerOverlays.ts` — **Session C** is
  refactoring `PALETTES.cecelia` to a shared JSON; don't move it.
- `frontend/src/lib/webgpu/volumeRenderer.ts` — **Session D** is optimising the upload
  path; steer clear.

## Existing code to lean on

- `api/src/viewer_api.jl → api_viewer_record_test` (line ~786) — copy the overlay/mask
  wiring block verbatim. It's the ONE source of truth for how a request's overlay dict
  becomes `build_overlays_for` + `build_mask_for` args.
- `api/src/movie_render.jl → record_view_movie` — the callback contract
  (`on_log`/`on_progress`/`on_process`/`cancelled`) is exactly what the rail needs.
- `app/src/jobs.jl` — for the cancel pattern (existing tasks register process handles
  via `on_process`; see any tracking or segmentation task).
- Frontend: `frontend/src/stores/movies.ts` (or similar) + wherever the current record
  button posts.

## Test obligation

Add a testset in `api/test/runtests.jl` that:
- Posts a minimal record request to the new handler
- Asserts a `job:queued` event fires (or the handler's equivalent)
- Asserts `register_movie!` gets called with the right `produced_by` tag
- Does NOT need to run the encoder end-to-end — the smoke route already covers that

Run: `pixi run test-api`.

## Success criteria

1. `pixi run test-api` passes.
2. Clicking record in the UI produces an mp4 in the project's movies dir, with progress
   bar visible in the task rail and cancel actually killing the encoder.
3. Batch-movie config produces one mp4 per image in the batch.
4. Diff `handle_movie_record` (before) vs the new path (after): no napari calls remain
   on the record path.

## Reservations to state before commit

- Overlay/mask config storage shape may differ from the napari path — if the saved config
  has fields the new authors don't understand, list them.
- The smoke route uses default `crop = nothing`, `max_px = 0`. The record path may need
  both — plumb them if the UI has them.
- Test in a private window if you touch persisted view state — Dominik keeps his own
  running instance on 8080; don't clear his localStorage.

## Explicit constraints

- **Never start/kill the dev server.** Dominik owns 8080/5173/7655.
- **Never write shared dev config.** Use `CECELIA_DEV_DIR` isolation.
- **Branches + PRs only, never push to main.** Ask before commit; state reservations first.
- **Copy `.env`** into your worktree if you're starting fresh.
- **State reservations before every commit.**
