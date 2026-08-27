# Session B — P7: task preview overlays

> **OUTCOME 2026-08-27 — BUILT.** Delivery is a labels-shaped scratch OME-Zarr next to the real one,
> served by `/api/viewer/slab?labels=<vn>&preview=1`. Napari path deleted. Region source is the
> browser viewer store (`useViewerStore().visibleRegion` + `openImage`), body-carried to the API.
> AF-image preview deferred to P7.1 (no browser image-overlay path yet). See
> `docs/todo/WEB_VIEWER_PLAN.md` → *P7 — task preview overlays* for the shipped design.

Branch: `feat/viewer-p7-task-previews` (fork from origin/main).

## Goal

Port `show_task_preview` + `preview_region` off the napari bridge into the WebGPU
viewer. These are the previews `api/src/preview_api.jl` currently pushes into napari so
task parameters can be judged BEFORE the task runs (e.g. a segmentation preview showing
what a diameter/threshold combination will pick up).

Per WEB_VIEWER_PLAN.md P7: "a preview is drawn with the same primitives (points,
shapes, labels)" — so the drawing code already exists (`frame_overlays.jl` server-side,
the WebGPU overlay passes client-side). **This is a wiring job, not a rendering job.**

## What to build

1. **Server route(s):** replace whatever `preview_api.jl` pushes to napari with an API
   the frontend can pull. Two shapes: `show_task_preview` (transient overlay for a
   specific task) and `preview_region` (a bounded region of interest).
2. **Client side:** the current module pages that trigger a preview need to consume the
   new API and paint the preview through the browser viewer's overlay layer. Same layer
   the P3 pops use — the viewer already knows how to draw points/segments/masks.
3. **Lifetime:** a preview is transient (cleared when the module page unmounts or the
   user cancels). Match the existing napari lifetime — a task-cancel or a page-leave
   must clear.

## Files to touch

- `api/src/preview_api.jl` — the primary file; rewire its dispatch to serve the browser
  instead of pushing to napari.
- `api/src/server.jl` — register new routes if needed.
- `api/src/napari_api.jl` — cut the `preview_*` bridge calls out (grep for what
  `preview_api.jl` used to push into it).
- `frontend/src/**` — the module pages that CALL preview (grep for `previewRegion` /
  `showTaskPreview` in Vue).
- `frontend/src/utils/viewerOverlays.ts` — likely gets a new payload type for a
  transient task preview, or the pop payload shape gets a `transient` flag.
- Tests: `api/test/runtests.jl` for the routes.

## Files NOT to touch

- `api/src/overlay_author.jl`, `api/src/movie_render.jl`, `api/src/image_render.jl` —
  offline render path, unrelated.
- `api/src/viewer_api.jl` `api_viewer_record_test` handler — **Session A** is
  refactoring that path.
- `frontend/src/lib/webgpu/volumeRenderer.ts` — **Session D**'s area.
- `frontend/src/plots/plot.ts` (palette) — **Session C**'s area.

## Existing code to lean on

- The P3 overlay path: `/api/viewer/overlays` returns the columnar payload; the browser
  layout code is `frontend/src/utils/viewerOverlays.ts`. A transient preview is the same
  columnar payload with a shorter lifetime — reuse the shape.
- The linked-brushing "transient" pop the pick-cell path already produces
  (`_inject_napari_pop!` + `_broadcast_popmap`) — same pattern for a preview overlay:
  register, broadcast, clear on cancel.
- `preview_api.jl`'s existing shape tells you what the preview payload IS (a set of
  cells, a mask stamp, a shape polygon); the migration is where it GOES.

## Test obligation

Add testsets covering:
- The new preview route(s) return the expected payload shape
- A task-cancel clears the preview registry
- The route rejects previews for images without label props (same guard shape as
  `api_viewer_overlays`)

## Success criteria

1. `pixi run test-api` + `pixi run test-frontend` pass.
2. Opening a module page that had a napari preview now shows the same preview in the
   WebGPU viewer.
3. Cancelling the module clears the preview.
4. No dispatch to napari on any preview path.

## Reservations to state before commit

- Which module pages had previews? Enumerate them — if any are load-bearing for a
  specific workflow you can't test locally, flag it.
- What's the preview REFRESH cadence? Napari previews debounced by the bridge's own
  latency; the browser needs its own throttle (use `debouncedLatest` from
  `frontend/src/utils/debouncedLatest.ts` — the canonical scheduler).
- Preview overlays share the P3 payload shape, so a preview + a live pop set could
  collide on the same viewer layer. Say how you resolved it.

## Explicit constraints

- **Never start/kill the dev server.**
- **Ask before commit; state reservations first.**
- **Branches + PRs only, never push to main.**
- **Copy `.env`** into your worktree.
- **Preview drawing already exists** — do not add a new primitive. Use the P3/P4 layers.
