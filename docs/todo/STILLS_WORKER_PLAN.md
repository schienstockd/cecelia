# Stills on the shared shader, served by the preview worker

**Status:** built (2026-10-03), branch `feat/stills-worker` — Phases 1–4 (see *Result*). Follows
[`SHARED_RENDERER_PLAN.md`](SHARED_RENDERER_PLAN.md), which put every movie on the viewer's shaders.

## Goal

The last pixels Cecelia draws outside the viewer's shaders are the **stills**: the medoid filmstrips
on cell, motif and HMM-state cards, and the animation panel's keyframe thumbnails. They still go
through Julia's CPU `render_view_frame` + `frame_overlays.jl`. Move them onto the shared shader, so a
card's dots and tails are the viewer's and a 3D keyframe's thumbnail is its 3D view — then retire the
CPU renderer.

## The problem: start-up, not rendering

One still through the movie runner (`render_animation_run.py`, a fresh `run_py` process per call)
took about 2.6 s end to end. Measured on yDfwP7 (2026-10-03), the cold set-up before the first frame
is about 1.9 s; the frame itself is milliseconds (14 ms at 1186x999 on the RTX).

| stage | time |
|---|---|
| imports (cecelia + wgpu + runner) | 0.6 s |
| `MipHost()` — device + compiled pipelines | 0.9 s |
| open the zarr | 0.3 s |

A card sheet asks for dozens of stills, and a keyframe strip one per keyframe, so start-up per call is
the whole cost.

## Decisions (2026-10-03)

1. **Serve stills from the preview worker** (`preview/preview_worker.py`, :7656), not a new worker.
   It already has the lifecycle a resident process needs — on-demand launch (`_ensure_preview!`),
   adopt-or-relaunch by protocol, Quit-everything, the log tag, packaging on all three OSes. A second
   worker would copy about 20 files of that wiring (port, shutdown list and its test, diagnostics,
   single-instance, log filter, Settings service control, observer service list, bundle check, dev
   launcher) for a few hundred lines of real code. Dominik's call.
2. **Movies stay on the one-off runner.** A movie runs for minutes; on the worker it would hold
   previews up, and start-up is a small share of it.
3. **One renderer, two outputs.** The runner's frame loop becomes `render_frames(params, host)`
   (states → RGB arrays). The mp4 path and a PNG path both call it, so a still and a movie frame of the
   same state are the same pixels. The params are the movie's (`states`, `specs`, overlays, mask) plus
   `outPaths` (one PNG per state) — no second schema.
4. **Renders don't queue behind cellpose.** The worker runs `render` off the event loop
   (`asyncio.to_thread`) under its own lock, and the preview path under a second lock, so a card sheet
   and a cellpose preview run side by side; two renders still serialise on the one GPU host.
5. **Cold worker → the one-off runner, same pixels.** If `_ensure_preview!` says the worker is still
   starting, the still renders through `run_py` instead of answering 202: a card route keeps its
   synchronous contract, and the next call finds the worker warm. The output is identical either way
   (Decision 3).
6. **The worker keeps one `MipHost`,** built on the first `render` and reused; it uploads each request's
   volume and drops it after, so idle GPU memory is the pipelines, not the last image. A crash
   relaunches the worker on the next request (existing behaviour) — a cellpose preview running at that
   moment fails with it.

## Phases

### Phase 1 — PNG output from the runner

- `render_frames(params, host)` out of `run`; `run` keeps writing the mp4.
- `outPaths` → one PNG per state (`render_stills(params, host)`), through `run_py` for now.
- Julia: the still's states built the way `record_view_movie` builds a 2D movie's (crop + stride → a
  head-on camera), shared rather than copied.
- Test: a still and the matching movie frame are byte-equal.

### Phase 2 — the worker's `render` command

- `{"type": "render", "params": {...}}` → `{"type": "ok", "paths": [...]}`; `PREVIEW_PROTOCOL` 16.
- The two locks and `to_thread` (Decision 4); the cached host (Decision 6).
- Julia `render_stills(...)`: the worker when ready, else `run_py` (Decision 5).
- Measure: warm still time, cold fallback time, a 20-card sheet before and after.

### Phase 3 — move the callers

- `render_medoid_filmstrip` (`behaviour_cards.jl`) — all three card families.
- `api_viewer_thumbnail` (`viewer_api.jl`) — a 3D keyframe renders its 3D view (today: the 2D slice it
  was captured at).
- Visible change: card dots become the viewer's radius-sized discs (the old `draw_points!` took the
  size as a diameter), tails the viewer's.

### Phase 4 — retire the CPU renderer

- Delete `render_view_frame`'s compositing and `frame_overlays.jl`'s `draw_*` once nothing calls them;
  keep what other routes still use (`resolved_display_specs`, `pixel_transform`, …).
- Docs: `docs/inventory/JULIA_API.md`, `docs/ARCHITECTURE.md`, `SHARED_RENDERER_PLAN.md`.

## Result (2026-10-03)

- **Phases 1–4 built.** `render_frames` is the runner's one frame loop; `render_stills` writes PNGs
  (`outPaths`). The preview worker answers `render` (protocol 16) on a kept `MipHost`, off its event
  loop under its own lock. Julia: `render_view_stills` (a 2D view's frames) and
  `render_view_state_still` (one view state, 2D or 3D) share the movie's params builders
  (`_view_render_params`, `_view_state_render_params`); `STILLS_VIA` picks the route — the worker,
  else one-off; the API test suite pins it to one-off so it never touches :7656.
- **Callers moved:** `render_medoid_filmstrip` (cell / motif / HMM-state cards) and
  `/api/viewer/thumbnail` (a 3D keyframe now renders its 3D view, at its captured canvas).
- **Retired:** `render_view_frame`, `frame_overlays.jl` (`draw_points!` / `draw_segments!` /
  `draw_mask_outline!`) and their tests. The shared constants moved to `shader_constants.jl`.
  `render_preview_frame` (the crop panel's preview) stays on the CPU compositor.
- **Measured on yDfwP7** (gBT track 7, a 3-still card, 76x80): cold — imports 0.7 s + host 0.8 s +
  first card 0.5 s; warm host — 0.05–0.12 s per card.
- **Cards keep their look.** The trace is drawn at point radius 4 / tail width 4, measured against the
  CPU renderer's card (same 37-px dot; trace 527 vs 503 px). Figure on the dev machine:
  `~/Downloads/TMP/stills_worker_cards_yDfwP7.png`.
- **Not exercised here:** a live worker over the socket — the route is covered in-process
  (`test_preview_worker.py`); the Julia client's worker branch runs only in the app.

## Open

- **Thumbnail overlays.** Thumbnails are channels only today. With the shader they could show the
  animation's overlays and mask too, as the movie will. Not in scope unless asked.
- **GPU memory beside cellpose.** The host's pipelines are small; a large still's volume is held only
  for its request. Unmeasured next to a loaded cellpose model.

## Cross-references

- [`SHARED_RENDERER_PLAN.md`](SHARED_RENDERER_PLAN.md) — the movie half.
- `docs/SEGMENTATION.md` → the preview worker; `app/src/preview.jl` (`PREVIEW_PROTOCOL`, `send`,
  `launch!`); `api/src/preview_api.jl` (`_ensure_preview!`).
