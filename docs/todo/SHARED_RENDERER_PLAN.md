# Shared renderer — the viewer's shaders draw the movies too

**Status:** Phases 0–4 built and merged (2026-10-03): every movie, 2D and 3D, runs the viewer's
shaders (PRs #1345, #1349, #1359, #1371).
- **Stills** (cards, keyframe thumbnails) are on the shader too, served by the preview worker —
  [`STILLS_WORKER_PLAN.md`](STILLS_WORKER_PLAN.md).
- **Open:** the viewer-side pixel capture (needs a browser — Dominik's click); Phase 5 (bricks) for a
  level 0 over the device's 3D-texture limit.

Supersedes [`VIEWER_PARITY_PLAN.md`](VIEWER_PARITY_PLAN.md) Decision 1 ("the two renderers stay")
and its "shared drawing library" non-goal. That plan's shared-JSON work (Phases 1–2, built) stands.

## Scope (locked 2026-10-01)

- **Build Phases 1 and 2.** (Phase 3 followed on 2026-10-02, at Dominik's go-ahead.) 3D movies are
  where the payoff is:
  - The torch ray-caster is the renderer with no parity test.
  - It has no masks.
  - It is CUDA-only: it runs on the CPU on Macs and on AMD/Intel GPUs.
- **Phase 3 (2D) and later wait for a review after Phase 2.** The Julia 2D path already
  sRGB-encodes and matches the viewer, so there is less to gain there.
- **HPC is out of scope.** No Vulkan-loader or headless-node work.
- **The torch sRGB bug was fixed separately** (#1333): mean |Δ| 48.5 → 4.5/255 against the viewer
  screenshot. Torch itself was deleted in Phase 2.

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

## Renderers before this plan (all replaced)

As of 2026-10-01. Every movie and still now runs the viewer's shaders; only the crop panel's preview
(`render_preview_frame`) stays on Julia's CPU compositor.

| Surface | Renderer | Language | Shares with the viewer |
|---|---|---|---|
| Browser viewer, 2D / small 3D | `tileShader.ts`, `mipShader.ts` | WGSL (WebGPU) | — (the truth) |
| Browser viewer, large 3D | `brickShader.ts` (brick atlas) | WGSL | — |
| Movie 2D (Record, batch, compare grid) | `record_view_movie` → `render_view_frame` (`api/src/image_render.jl`), overlays via `frame_overlays.jl` | Julia CPU | JSON palette / track modes (Viewer Parity P1–P2) |
| Movie 3D (keyframes, Record 3D, batch 3D) | `render_animation_run.py` | Python torch | the same, plus a hand-kept zoom + colour convention |

Encoder-side overlays (timestamp, scale bar, title card) are not viewer pixels and stay where they
are (`title_card.draw_frame_overlays`, `movie_io`).

## Decisions (locked after Phase 0; file names are as of then — `encode_movie_run.py`,
`build_overlays_for` / `build_mask_for` and torch have since been deleted)

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

**Result (2026-10-01): the frame passes; the install is verified on Linux only.** Scripts are in
`docs/todo/spike/shared-renderer/`: `export_inputs.test.ts` runs the viewer's own TS to get the WGSL,
the uniforms, the LUT and the palette, and `render_frame.py` uploads them to wgpu-py.

- **Pixels.** The comparison is against the viewer screenshot of this state
  (`~/Downloads/TMP/fXgbTl_viewer_screenshot_t3.png` on the dev machine), registered to the
  render (the capture is scaled). Shared shader with an `rgba8unorm-srgb` target: mean |Δ| 3.0/255,
  median 2, fit `viewer ≈ 1.001·x − 0.0`, which is resampling noise from the capture.
- **Torch ray-caster on main:** mean |Δ| 48.5, fit `viewer ≈ 1.10·x + 43`. It writes linear values
  and never sRGB-encodes, so 3D movies come out about 2x darker in mid-tones than the viewer.
  `fix/torch-3d-srgb` brings it to 4.5/255 (the residual is mostly mp4 compression).
- **Speed at 1186x999, 256 steps, 4 channels, including readback.**
  - RTX 2000 Ada (Vulkan): 10.6 ms/frame, shader compile ~150 ms.
  - llvmpipe (software Vulkan): 483 ms/frame, which is usable for batch.
  - The two adapters' outputs differ by at most 1/255.
- **Install.** `wgpu = ">=0.20"` in `[pypi-dependencies]` resolved to 0.32.0, with wheels locked for
  `linux-64`, `osx-arm64` and `win-64`. It installs and runs on Linux. macOS and Windows resolve
  but have not been run; CI on those OSes is the remaining check.
- **Uniform packing is still hand-mirrored** in `export_inputs.test.ts`. The slot writes are spread
  through `volumeRenderer.ts`, which confirms Decision 3 is the main work.

### Phase 1 — WGSL out of the template strings

- Move `mipShader`, `tileShader` and `brickShader` sources to `.wgsl` files, with one substitution
  helper used by both hosts. The browser behaviour must not change (existing shader tests + a visual
  check).
- Uniform layout as data + the pack-both-sides byte test (Decision 3).
- **Starting point.** The spike branch `spike/shared-renderer` (worktree
  `cecelia-shared-renderer-spike`) carries:
  - `wgpu` in `pixi.toml` and the lockfile;
  - `docs/todo/spike/shared-renderer/` (the TS exporter and the Python host).

  The exporter's hand-mirrored slot writes are the list Decision 3's field table replaces.
- **Close the install gate.** Run the `wgpu` install and a one-frame render in CI on macOS (Metal)
  and Windows (DX12/Vulkan).

**Result (2026-10-01): built.**

- **The shaders are files.** `frontend/src/lib/webgpu/shaders/`: `mip.wgsl`, `mip_points.wgsl`,
  `mip_segments.wgsl`, `mip_common.wgsl` (struct + camera), `tile.wgsl`, `brick.wgsl`,
  `brick_multi.wgsl`, `brick_points.wgsl`, `brick_segments.wgsl`, `brick_common.wgsl`, `pick.wgsl`,
  plus `constants.json`. The reasoning comments moved with the code; `mipShader.ts`, `tileShader.ts`
  and `brickShader.ts` now only expand.
- **One expander per language, two rules.** `#include "x.wgsl" [NAME=VALUE]` and `${NAME}`.
  - TS: `wgslExpand.ts` (pure, Node can run it) behind `shaderSource.ts` (Vite `?raw`).
  - Python: `python/cecelia/utils/wgsl_utils.py`.
  - Both pass the same hand-derived cases in `shaders/golden.json`.
- **Browser behaviour unchanged.** All 11 shader strings the renderers compile were snapshotted
  before the move (MIP/points/segments, tile, brick N = 1–4 + its overlays). After it, the code is
  identical with comments stripped and whitespace normalised; only
  the header comments moved. Frontend suite green.
- **Brick multi-atlas** keeps its generated parts in TS (`ATLAS_DECLS`, the switch arms, the shifted
  bindings are passed as variables). Nothing server-side needs it before Phase 5.
- **Uniform layout as data (Decision 3).** `shaders/uniforms.json` names every lane of the
  `mip`, `tile` and `brick` blocks. `volumeRenderer.ts` and `tileRenderer.ts` write `u[U.cam.dist]`
  and so on, with no numeric slots left. `BU` in `brickShader.ts` is derived from the same table.
  - `shaderSource.test.ts` parses each struct out of its `.wgsl` and checks the fields, order and
    types against the table. Swapping two fields fails it.
  - The golden pins hand-derived slots, e.g. `ch[3].hi` = 41 and `prevGrid.valid` = 43, for both
    languages.
- **Python host.** `python/cecelia/utils/wgpu_host.py` (`MipHost`) uploads volume, LUT, palette and
  labels in the viewer's formats and renders `mip.wgsl` to `rgba8unorm-srgb`.
  - **Real data:** re-running Phase 0's fXgbTl frame through `MipHost` with lanes packed by name
    reproduces the Phase 0 frame exactly (max |Δ| 0). The exporter now packs by name, and its
    uniforms come out identical.
  - **Synthetic (CI):** `test_wgsl_utils.py` renders a 16x12x6 two-channel volume face-on and checks
    it against a NumPy evaluation of the same maths: ≤ 1/255 on the RTX 2000 Ada and on llvmpipe. A
    vertically flipped frame is off by 73, so the check sees orientation. A second frame checks a
    filled label draws its palette row.
- **Install gate: closed.** CI's Python job sets `CECELIA_REQUIRE_WGPU=1`, so a missing adapter
  fails rather than skips. On PR #1345 the one-frame render passed (≤ 1/255 vs NumPy) on all three:
  - Linux: llvmpipe (Vulkan), from Mesa's `mesa-vulkan-drivers`, which CI installs.
  - macOS: Apple Paravirtual device (Metal).
  - Windows: Microsoft Basic Render Driver (D3D12 WARP).

  `wgpu` 0.32.0 installed from the lock on each.
- **Still TS-only:** the LUT bytes (`lutTextureBytes`) and the label palette (`labelPaletteBytes`).
  Phase 2 has to serve both to the host, and already lists the palette.

### Phase 2 — 3D movies on the shared shader

- Replace `render_animation_run.py`'s ray-caster with the `wgpu-py` host running `mipShader`: keyframe
  animations, Record 3D, batch 3D. This brings the viewer's 3D label pass (nearest id, in-plane
  contour, palette, opacity) with it, which closes "no masks in 3D". The pick / focus highlight
  branches stay off in a movie.
- Label palette: the host needs the same palette rows the viewer uploads (`p.lab.z` rows, `labColour`).
  Where the viewer builds them (pop / colour-by colours) is not traced yet — find it, and serve the rows
  from one place both hosts read rather than rebuilding them in Julia.
- Pixel test: viewer frame vs movie frame, per fixture.
  - **The viewer side needs a durable capture.** Take a `canvas.toDataURL()` of the viewer at a
    fixed view state on a committed fixture. Phase 0's screenshot was a scaled screen capture,
    registered by hand.
- **Time torch against wgpu on the same frame before deleting torch.** Not measured in Phase 0.
- Delete the torch ray-caster.

**Result (2026-10-01): built, apart from the viewer-side capture.**

- **Every 3D movie now runs the viewer's shaders.** `writers/render_animation_run.py` drives
  `wgpu_host.MipHost`: `mip.wgsl`, then `mip_segments.wgsl`, then `mip_points.wgsl`, in one pass, in
  the viewer's draw order. That covers keyframe animations, the viewer's Record in 3D and batch 3D,
  which all funnel through `record_keyframes_view_movie`. Torch, the Julia CPU 3D kernel
  (`render_view_frame_3d`, unreachable behind the GPU branch) and the vispy rotation helpers are
  deleted.
- **A bug found on the way: the old 3D path tilted the wrong way.**
  - What: `R = Rz·Ry·Rx` with `rx = angles[0]` is the viewer's camera with the pitch sign flipped.
    Yaw agrees. Both torch and Julia's 3D overlay projection used it, so any browser-authored 3D
    keyframe with pitch recorded mirrored.
  - Why Phase 0 missed it: its camera was `[0, 0, 0]`.
  - Measured on fXgbTl t=3 at pitch +35°: torch's frame is mean |Δ| 47.6 from the shader at +35°
    and 8.6 from the shader at −35°.
  - Figure on the dev machine: `~/Downloads/TMP/shared_renderer_p2_pitch_flip.png`.
- **Julia sends the camera as the viewer stored it** (`_camera3d_payload`), plus the height of the
  canvas its zoom was measured on (`_snapshot_canvas_h`). The host applies it with `view_camera`,
  which is `applyViewStateToBrowser` line for line. `_renderer_zoom_3d` and `_world_per_px_3d` are gone.
- **Overlays arrive as positions, not pixels.** `build_overlays3d_for` returns native-voxel
  points and tail segments. The host packs them in the viewer's instance layout (`POINT_STRIDE` /
  `SEG_STRIDE`), and the shader projects them with the raycast's own uniform block, so overlays
  cannot drift from the volume. That pulls Phase 4 forward for 3D, which was forced by the pitch
  finding: keeping Julia's projection would have needed a third copy of the camera.
  - The 3D tail fade is gone. The viewer draws tails at one alpha (0.85), and now so does the movie.
- **Masks in 3D.** A 3D movie now draws the viewer's 3D mask: every label, nearest along the ray,
  `shaders/label_palette.json` colours, `LABEL_OPACITY`, and the request's `labelContour`.
  - It is not pop-filtered, because the viewer's 3D view isn't either.
  - The viewer's Record and the batch already send `labelValueNames`. `maskValueName` keeps the
    mask's segmentation apart from the overlays'.
- **Shared CPU inputs, golden-pinned on both sides** (`shaders/golden.json`): the view-state camera,
  the LUT rows, and the label palette (`label_palette.json` = `labelPaletteBytes()`).
- **Real data.** fXgbTl t=3, rebuilt from the raw view state through the runner's own path
  (`view_camera` → `lut_rows` → `frame_uniforms` → `MipHost`), matches the Phase 0 frame from the
  viewer's TS exactly (max |Δ| 0). That holds in µm and in the runner's x-voxel units.
- **Speed, same 1186x999 four-channel frame, incl. readback** (torch used about 442 samples per ray,
  wgpu 256 steps):

  | | ms/frame |
  |---|---|
  | torch CUDA | 1366 |
  | wgpu RTX 2000 Ada | 14 |
  | torch CPU | 43 163 |
  | wgpu llvmpipe | 1045 |

  So 3D movies no longer need CUDA. A Mac or AMD/Intel machine gets its own GPU, and a machine with
  no GPU gets llvmpipe at about a second per frame.
- **Level.** The host asks the device for its real 3D-texture limit (16384 on the RTX vs the 2048
  default). The movie uses level 0 when it fits, else the first coarser level that does, with the
  extents still in level-0 µm.
- **Tests.**
  - `test_wgsl_utils.py`: a point sits on its voxel at four cameras, pitched and yawed.
  - `test_render_animation_run.py`: a store → mp4 end to end, covering t, camera, mask and point.
  - API testsets: camera payload, scale bar, world-space overlays.
- **Open.**
  - **The viewer-side capture is still needed.** A `canvas.toDataURL()` of the viewer at a fixed
    view state on a committed fixture closes the pixel test. That needs a browser, so it's
    Dominik's click.
- **Closed after review.**
  - **Perspective.** `buildViewState` records the 3D projection toggle (`camera.perspective` 1/0;
    2D is always 0), and Fill from view carries it on `camera3d`. Applying a view state in the
    viewer still leaves the toggle alone.
  - **Point border and mask opacity.** The look carries `pointBorder` and `labelOpacity`
    (`viewerLook`); `_overlays_raw_from_config` forwards them as `pointBorderPx` / `maskOpacity`
    to both renderers (the 3D host's lanes, and `draw_points!` / `draw_mask_outline!` in 2D). A
    batch config without them draws no border at `LABEL_OPACITY`.

### Phase 3 — 2D movies on the shared shader

- `tileShader` for the plain Record, batch and compare-grid cells, replacing `render_view_frame` +
  `draw_mask_outline!`. Viewer Parity's deferred Phase 5 (mask outline algorithm) goes away with it.
- The compare grid keeps its stitcher (`movie_io.stitch_movies`) — it composes frames, not pixels.
- Fixes on the way: the viewer's point size is a RADIUS (`mip_points.wgsl`: fill radius `ov.pointPx`),
  while `draw_points!` takes it as a diameter, so 2D movie points are half the viewer's size.

**Result (built 2026-10-02, after Dominik's go-ahead).**

- **Not `tileShader`: the viewer's 2D view is `mip.wgsl`.** A normal plane is drawn by the same pass
  as 3D — a texture one plane deep (or the ± window's planes), one step, head-on, orthographic
  (`ViewerWindow.vue` `ensureRenderer`; `tile.wgsl` is only for whole-slide planes over 200 MB, and
  draws no overlays). So every 2D movie now runs `render_animation_run.py` too: states carry
  `ndisplay: 2`, the planes (`zRange`), and the viewer's overlay plane windows (`planeFilter`, its
  z tolerances); the level is the one the viewer's 2D zoom picks.
- **`record_view_movie` keeps its region and size.** The crop + integer stride become a head-on
  camera (one output pixel per `step` image pixels, anchored at the crop's top-left), so plain Record,
  batch and compare-grid cells come out the size they did. 2D keyframes render the viewer's canvas.
- **Population masks stay.** `mip.wgsl` gained a colour-table mode (a negative row count): label →
  colour, alpha 0 = not drawn, hidden labels skipped by the march. Movies send
  `mask_id_colours` (the old `build_mask_for` policy: pop-filtered, pop-coloured, colour-by) as that
  table; "all cells" draws the viewer's palette. The viewer still sends the palette's row count —
  its output is bit-identical to before (max Δ 0 on flat, pitched and contour scenes).
- **What changed on screen.** 2D masks are the viewer's (`labEdge` outline, nearest label in a
  range — not the old max-id projection); points are the viewer's radius-sized discs (the half-size
  bug is gone); a 2D movie on one plane shows only the points / tail ends within the viewer's z
  tolerances (`pointZTol` / `trackZTol` now ride the look, default 2) — the old 2D author drew every
  cell whatever its z.
- **Retired:** `write_raw_frames`, `encode_movie_run.py` / `movie_io.encode_raw_frames`,
  `build_overlays_for`, `build_mask_for`. `render_view_frame` + `frame_overlays.jl` stay for stills
  (cell / behaviour cards, keyframe thumbnails).
- **Only the region a frame shows is uploaded** (`_xy_region`, 2026-10-03): the camera's visible rect
  at the chosen level, with the host treating it as the whole image. Before that a 2D movie uploaded
  the whole plane — a crop of a big tilescan failed on the texture limit (2048 on a Mac) or, on
  f8gzA2, ran out of RAM (25 GB and climbing). A level steps down only if the region itself is over
  the limit.
- **Checked on fXgbTl:** a 2D keyframe pair and a plain 2D Record (z = 7, flowTom tracks) render
  through the shader with the source-coloured tails on the plane ± 2, timestamp and scale bar.

### Phase 4 — overlays in the shader too

- The viewer draws points and track segments in shader passes that share the camera uniform
  (`mipShader.ts` `ov` uniform; "the overlays pan with the pixels and cannot drift apart"). Run the
  same passes on the server so Julia supplies overlay *data* (positions, colours) and no longer
  rasterises it. Julia's projection code for 3D overlays (`_overlays2d_state`) retires.

**Result: done by Phases 2 and 3.** Every movie's points and tails are the viewer's `mip_points` /
`mip_segments` passes over positions Julia sends (`build_overlays3d_for`); `_overlays2d_state` went in
Phase 2.

### Phase 5 — large volumes

- `brickShader` for volumes that don't fit one texture (the viewer's brick path; the torch renderer
  reads full resolution today). Only once a real movie needs it.

## Open questions

- **wgpu-native vs Dawn:** answered. `r16uint` volumes and the MIP pass (Phase 0); `r32uint` label
  textures (every mask movie since Phase 2); the 3D texture limit is the device's own, asked for
  (`MipHost.max_texture_3d`).
- **Software-adapter speed:** answered. llvmpipe takes 483 ms/frame at 1186x999, which is usable
  for batch.
- **Packaging:** answered for Linux, macOS and Windows (Phase 1 CI). HPC is out of scope.
- **Brick streaming:** answered — a 3D movie uploads one whole timepoint at the finest level that fits
  one texture (logged when it has to go lower); a 2D movie uploads only the region it shows, so a crop
  of a plane wider than the device limit renders at level 0 (f8gzA2, 20329 × 16898 × 25 channels: an
  800 × 600 crop in 4.6 s, ~330 MB). Bricks (Phase 5) matter only for a 3D level 0 over the limit.

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
