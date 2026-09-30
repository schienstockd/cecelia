# Shared renderer — the viewer's shaders draw the movies too

**Status:** parked (2026-10-01) — planning, nothing built. Supersedes
[`VIEWER_PARITY_PLAN.md`](VIEWER_PARITY_PLAN.md) Decision 1 ("the two renderers stay") and its
"shared drawing library" non-goal; that plan's shared-JSON work (Phases 1–2, built) stands. Next step
is the Phase 0 spike, which decides whether anything after it happens.

## Goal

One renderer for pixels. The browser viewer's WGSL shaders (`frontend/src/lib/webgpu/`) are the
ground truth for what an image looks like; the movie renderers should run **the same shader source**
on a server-side WebGPU host instead of re-implementing it. Then viewer ↔ movie parity is true by
construction — colours, camera, contrast, masks, overlays — rather than by porting every viewer
feature twice and testing that the two copies agree.

## Why reopen Viewer Parity Decision 1

Decision 1 rested on "the offline path can't run WebGPU without a browser open" and on the offline
path being a CPU compositor. Both have moved:

- **The offline 3D path is already a GPU ray-caster** — `python/cecelia/writers/render_animation_run.py`
  (torch; CUDA, CPU fallback), added 2026-08-28 in b9c3f6c4. It is a *third* renderer, a hand port of
  the viewer's `mipShader.ts`, not the CPU compositor the plan assumed.
- **WebGPU runs without a browser.** `wgpu-py` (Python bindings to `wgpu-native`) compiles and runs WGSL
  on Vulkan / Metal / DX12, and can fall back to a software adapter (e.g. Mesa lavapipe) where there is
  no GPU — to be confirmed per OS in Phase 0. Same shader language, no browser.
- **The drift is in the primitives, not the decisions.** The bugs fixed in PR #1313 were all in
  re-implemented drawing code, where Viewer Parity's decision-level test (Decision 3) could not see
  them: a name-only colour lookup that rendered every hex channel white; the viewer's zoom fed raw to
  a ray-caster with a different zoom convention (~5x too tight); no mask pass in 3D at all, while the
  viewer's shader draws the nearest label along each ray (`mipShader.ts`, "The NEAREST label along the
  ray"). Each viewer feature is a port, and each port is a new place to drift.
- Viewer Parity Phase 3 (the decision-level parity test) was never built.

## Current renderers (what this would replace)

| Surface | Renderer | Language | Shares with the viewer |
|---|---|---|---|
| Browser viewer, 2D / small 3D | `tileShader.ts`, `mipShader.ts` | WGSL (WebGPU) | — (the truth) |
| Browser viewer, large 3D | `brickShader.ts` (brick atlas) | WGSL | — |
| Movie 2D (Record, batch, compare grid) | `record_view_movie` → `render_view_frame` (`api/src/image_render.jl`), overlays via `frame_overlays.jl` | Julia CPU | JSON palette / track modes (Viewer Parity P1–P2) |
| Movie 3D (keyframes, Record 3D, batch 3D) | `render_animation_run.py` | Python torch | the same, plus a hand-kept zoom + colour convention |

Encoder-side overlays (timestamp, scale bar, title card) are not viewer pixels and stay where they
are (`title_card.draw_frame_overlays`, `movie_io`).

## Decisions (proposed — lock after Phase 0)

**1. The shader source is the one truth.** WGSL moves out of the TS template strings into standalone
`.wgsl` files (plus a tiny, shared substitution step for the handful of `${CONST}` values such as
`MAX_CHANNELS`). The browser imports them at build time; the server host reads the same files. No
second shader, no transpile.

**2. The server host is `wgpu-py`, run through `run_py`.** It lives in the analysis env next to the
existing renderer, so it keeps the existing callback contract (`on_log` / `on_progress` / cancel /
`[PROGRESS] n/total`) and the encoder pipeline (`encode_movie_run.py`). Julia keeps what it owns:
resolving the view (`viewstate_to_render_args`, `_renderer_zoom_3d`), overlay authoring
(`build_overlays_for`, `build_mask_for`), and the request rails.

**3. The uniform layout is defined once.** The viewer packs its uniforms in TS (`volumeRenderer.ts`,
`tileRenderer.ts`, `brickVolumeRenderer.ts`); the server host must pack byte-identical buffers. The
layout becomes data (a field list both sides read) with a test that packs the same inputs on both sides
and compares bytes. This is the host-code equivalent of Viewer Parity Decision 2 and the main new risk.
`utils/webgpuBindings.ts` (a stage-usage parse of the WGSL) is prior art for testing layout against
the shaders.

**4. Parity becomes a pixel test.** Same shader, same inputs, so the viewer's frame and the movie's
frame can be compared numerically, with a small tolerance for the adapter. That replaces Viewer Parity
Decision 3's "pixels are a losing bet", which was true only across different rasterisers.

**5. The old renderers retire one surface at a time.** Each is kept until the shared path renders its
case and passes the pixel test, then deleted. No flag day, no two paths for the same surface longer
than one PR.

**6. No GPU still works.** A software adapter is the fallback. If a platform has none, the existing
renderer stays for that platform until one exists — Phase 0 finds out.

## Phases

### Phase 0 — spike (a few hours; decides the rest)

- Take `mipShader.ts`'s WGSL as-is (constants substituted by hand) and run it headlessly in `wgpu-py`
  for one frame: fXgbTl (zolIMa), version `denoised`, t = 3, the camera + layers banked with
  `M2b-MERTK_KAT-SWHL-GFP-Tom-res_cropped_smoothed-with-pops.mp4` in `settings/movies.json`.
- Compare against the viewer at the same state (Dominik's screenshot from 2026-09-30; a browser
  `canvas.toDataURL()` capture for a numeric diff), and against `render_animation_run.py`'s frame.
- Measure: time per frame on the workstation GPU and on a software adapter; whether `wgpu-py` installs
  cleanly into the pixi env on Linux / macOS / Windows.
- **Gate:** the frame matches the viewer by eye and within a small numeric tolerance, and the install
  works on all three OSes. Fail → record why here and in `docs/FUTURE.md`, and stay on Viewer Parity.

### Phase 1 — WGSL out of the template strings

- Move `mipShader`, `tileShader` and `brickShader` sources to `.wgsl` files, with one substitution
  helper used by both hosts. The browser behaviour must not change (existing shader tests + a visual
  check).
- Uniform layout as data + the pack-both-sides byte test (Decision 3).

### Phase 2 — 3D movies on the shared shader

- Replace `render_animation_run.py`'s ray-caster with the `wgpu-py` host running `mipShader`: keyframe
  animations, Record 3D, batch 3D. This brings the viewer's 3D label pass (nearest id, in-plane
  contour, palette, opacity) with it, which closes "no masks in 3D". The pick / focus highlight
  branches stay off in a movie.
- Label palette: the host needs the same palette rows the viewer uploads (`p.lab.z` rows, `labColour`).
  Where the viewer builds them (pop / colour-by colours) is not traced yet — find it, and serve the rows
  from one place both hosts read rather than rebuilding them in Julia.
- Pixel test: viewer frame vs movie frame, per fixture.
- Delete the torch ray-caster.

### Phase 3 — 2D movies on the shared shader

- `tileShader` for the plain Record, batch and compare-grid cells, replacing `render_view_frame` +
  `draw_mask_outline!`. Viewer Parity's deferred Phase 5 (mask outline algorithm) goes away with it.
- The compare grid keeps its stitcher (`movie_io.stitch_movies`) — it composes frames, not pixels.

### Phase 4 — overlays in the shader too

- The viewer draws points and track segments in shader passes that share the camera uniform
  (`mipShader.ts` `ov` uniform; "the overlays pan with the pixels and cannot drift apart"). Run the
  same passes on the server so Julia supplies overlay *data* (positions, colours) and no longer
  rasterises it. Julia's projection code for 3D overlays (`_overlays2d_state`) retires.

### Phase 5 — large volumes

- `brickShader` for volumes that don't fit one texture (the viewer's brick path; the torch renderer
  reads full resolution today). Only once a real movie needs it.

## Open questions (Phase 0 should answer the first three)

- Does `wgpu-native` behave the same as Chromium's Dawn on the features the shaders use (`r32uint`
  label textures, 3D texture limits — cf. `docs/todo/WEBGPU_MULTI_ATLAS_PLAN.md`)?
- Software-adapter speed: is a batch of long timelapses usable on a machine with no GPU?
- Packaging: `wgpu-py` wheels + a Vulkan loader across the three OSes and HPC nodes.
- Does the brick streaming path matter for movies, or can movies always upload the whole timepoint?

## Non-goals

- **A headless browser.** Still rejected (Viewer Parity non-goal; `CLOUD_MIGRATION_ASSESSMENT.md` §3b).
  The point is the shader, not the browser.
- **Moving the view logic into WGSL.** Julia keeps view resolution, overlay authoring and the rails.

## Cross-references

- [`VIEWER_PARITY_PLAN.md`](VIEWER_PARITY_PLAN.md) — the plan whose Decision 1 this supersedes; its
  shared JSON assets stay.
- PR #1313 — the drift bugs that motivated this, and `utils/viewer/viewerLook.ts` (one reader of what
  the viewer draws — stays, it chooses *what* to draw).
- `frontend/src/lib/webgpu/` — the shaders; `python/cecelia/writers/render_animation_run.py`,
  `api/src/image_render.jl`, `api/src/movie_render.jl` — what retires.
