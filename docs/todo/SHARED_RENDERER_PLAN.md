# Shared renderer — the viewer's shaders draw the movies too

**Status:** in progress (2026-10-01).
- **Phase 0 passed on Linux** (see Phase 0 → Result).
- **Phase 1 built** on `feat/shared-renderer-p1` (see Phase 1 → Result). **Phase 2 is next.**
- **Committed scope is Phases 1–2.** Phases 3–5 are decided only after Phase 2 ships (see *Scope*).

Supersedes [`VIEWER_PARITY_PLAN.md`](VIEWER_PARITY_PLAN.md) Decision 1 ("the two renderers stay")
and its "shared drawing library" non-goal. That plan's shared-JSON work (Phases 1–2, built) stands.

## Scope (locked 2026-10-01)

- **Build Phases 1 and 2.** 3D movies are where the payoff is:
  - The torch ray-caster is the renderer with no parity test.
  - It has no masks.
  - It is CUDA-only: it runs on the CPU on Macs and on AMD/Intel GPUs.
- **Phase 3 (2D) and later wait for a review after Phase 2.** The Julia 2D path already
  sRGB-encodes and matches the viewer, so there is less to gain there.
- **HPC is out of scope.** No Vulkan-loader or headless-node work.
- **The torch sRGB bug is fixed separately** on `fix/torch-3d-srgb` and doesn't wait for this plan.
  Measured against the viewer screenshot: mean |Δ| 48.5 → 4.5/255.

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

## Open questions

- **wgpu-native vs Dawn:** answered for `r16uint` volumes and the MIP pass (Phase 0). Still open:
  `r32uint` label textures and the 3D texture limits (cf. `docs/todo/WEBGPU_MULTI_ATLAS_PLAN.md`).
  Phase 2 is the first to bind real labels.
- **Software-adapter speed:** answered. llvmpipe takes 483 ms/frame at 1186x999, which is usable
  for batch.
- **Packaging:** answered for Linux, macOS and Windows (Phase 1 CI). HPC is out of scope.
- **Brick streaming:** does it matter for movies, or can a movie always upload the whole timepoint?

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
