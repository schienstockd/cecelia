# Web viewer — replacing napari with a browser renderer

**Status:** planned (2026-08-24) · no branch yet · prototypes in `docs/todo/spike/webgpu/`

**Goal: cecelia becomes a sole browser app. napari is removed, not split with.** The cloud mandate
does not tolerate a Qt process, so "napari already does this well" is never a reason to keep a
feature there — it is a competing implementation to beat. Dominik, 2026-08-24.

Evidence this rests on: [`NAPARI_WEBGPU_AUDIT.md`](NAPARI_WEBGPU_AUDIT.md) — every number below was
measured on Dominik's RTX 2000 Ada with real data from `zolIMa`. Read it before revisiting a
decision here. Prior context: [`CLOUD_MIGRATION_ASSESSMENT.md`](CLOUD_MIGRATION_ASSESSMENT.md)
(storage + headless GL, still valid), [`CROP_PANEL_PLAN.md`](CROP_PANEL_PLAN.md) (the first
browser-rendered image surface, built).

## The split: B for interactive, C for offline

| | Renderer | Covers |
|---|---|---|
| **B** | WebGPU in the browser | image display 2D/3D, contrast, camera, labels, points, tracks, t-scrubbing, selection, mask correction |
| **C** | `api/src/image_render.jl`, extended | `record_timelapse`, `record_keyframes`, `stitch_movies`, `save_screenshot`, title cards |

The split is on **interactivity, not difficulty**. B must produce a frame in ~16 ms and does it in
5.3 ms. C's 117 ms/frame is irrelevant offline — a 181-frame movie is ~27 s — and C already reads the
zarr, already applies napari's exported LUTs, and `jobs.jl` already gives the progress/cancel
contract `record_timelapse` needs.

## What is already measured, so it is not re-litigated

| | |
|---|---|
| 3D MIP raycast, real data, 4 ch, 1566x1003, 256 steps | **5.3 ms** (napari: 36.0 ms whole frame, 9.6 ms net of Qt) |
| Real data vs. phantom at identical dimensions | **0.95–1.04x** — real voxels are not slower |
| Cold timepoint: fetch / contrast / upload | 640 / 36 / 535 ms → ~1.2 s (napari's t-slider: 1241 ms) |
| Scrub inside a cached window | **sub-millisecond**, rAF-limited |
| Whole `fXgbTl` movie into VRAM | 5.5 s for 1.47 GB, then every step is a hit |
| Per-chunk HTTP (1116 requests/timepoint) | **5270 ms — dead.** Serve assembled slabs instead |
| VRAM | 351 MB/timepoint at 4 ch; 8 GB card; `maxBufferSize` 1 GiB |
| Zarr read, same timepoint | Julia 533 ms vs Python 1127 ms (**2.1x** — the slow path is napari's) |

## Locked decisions

1. **Delivery is server-assembled slabs, not client-side chunk assembly.** One request per (t, c)
   returning raw `uint16`, compression by HTTP `Content-Encoding`. Per-chunk HTTP measured 4.2x worse
   than the incumbent and plateaued from concurrency 8. Julia assembles; the browser decodes nothing.
2. **`r16uint` 3D textures, channels stacked along z, `textureLoad` (nearest).** `r16uint` is not
   filterable, and MIP does not need interpolation. Converting to `r16float` on the CPU costs 973 ms —
   more than the entire read+decode — so if smooth sampling is ever wanted it happens on the GPU.
3. **`powerPreference: 'high-performance'` is mandatory, and verified from `limits`.**
   `requestAdapter({})` returns the integrated GPU, and Firefox blanks every `adapter.info` field.
   Assert `maxTextureDimension3D > 2048` and surface it if not — the browser-side twin of the PRIME
   trap in `napari.jl:55-59`.
4. **An LRU texture cache under a byte budget, with directional prefetch and cancellation.** This is
   what makes a timecourse usable; nothing about rendering does. Channel count is the VRAM lever.
5. **Contrast is computed once per image and held**, never per timepoint — otherwise playback
   flickers as the window tracks each frame's distribution.
6. **Read `dimension_separator` from `.zarray`; never assume a layout.** Both are live in one project
   (`VJy1Nx` flat, `fXgbTl` nested). Hardcoding flat made every chunk look absent, so a slab came
   back all zeros and would have rendered **black with no error**. New writes are nested only
   (`config.jl:897-901`).
7. **No pyramid exists.** Every store is single-level, so "3D detail level" has nothing to select.
   Building one is separate work that helps whichever renderer wins.
8. **Visual parity with napari is NOT a goal.** "If it looks different it's fine, as long as it shows
   you the data" — Dominik, 2026-08-24. This closes the figure-comparability risk an earlier draft
   raised, and removes any deadline on A/B-ing against napari before it is removed.

## Phases

Each phase ends in something usable, and names what would make it fail.

### P1 — the slab route + a viewer that shows one timepoint
Julia route serving one (t, c) as raw `uint16` with `Content-Encoding`, plus a Vue view with the
WGSL raycaster, camera drag, and per-channel contrast. Reuse: `resolve_image_version` /
`open_level0` / `read_native` (`image_geometry.jl`), `layer_display_specs` for colours
(`image_render.jl`), `debouncedLatest` / `rafCoalesce` for controls.
**Prototype already covers:** shader, camera, contrast, upload, the route shape (`chunk_server.py`).
**Fails if:** the Julia slab read is much slower than the 533 ms measured outside a request handler.

### P2 — the timecourse
LRU VRAM cache, directional prefetch, cancellation, cache-state strip, play. Prototype exists
(`timecourse.html`) and is the reference implementation.
**Fails if:** VRAM pressure destabilises the device at realistic budgets — watch `device.lost`.

### P3 — h5ad-derived overlays: points, tracks, colour-by
**The second make-or-break.** A and C read h5ad server-side and ship identifiers only
(`CLOUD_MIGRATION_ASSESSMENT.md` §3a); B must add routes. Do NOT reimplement `LabelPropsView` in the
browser — serve centroids/tracks/columns as JSON or binary from Julia, through the canonical
`label_props` view.
**Fails if:** the payloads turn out large enough to need their own caching story. Measure first: h5ad
is 139 MB total across 55 files, median 0.77 MB, so probably not.

### P4 — labels / segmentation masks
An extra `r32uint` texture plus a palette lookup. Cheap, and **better than napari**, which cannot
project a Labels layer at all (`projection_mode` accepts only `'none'`) — so 3D masks stop needing
the volumetric workaround.

### P5 — the offline capture path in C
Camera in `image_render.jl` + the four capture commands + title cards, on `jobs.jl` with
progress/cancel. Non-interactive, so 117 ms/frame is fine. Half of C's warm frame is currently PNG
encoding (49.5 ms) — pick a better codec for movie frames.

### P6 — mask correction with intensity context
Needs a **design** before an implementation. The old R version put it in napari precisely because you
must see the intensity channels while correcting; an in-browser surface modelled on the
track-correction timeline is acceptable (Dominik). Everything above is a prerequisite for it.

### P7 — decommission
Delete the bridge, the protocol version and the adoption/relaunch machinery. Already dead and
deletable sooner: `crop_start` / `crop_box` / `crop_apply` / `crop_clear` in `app/src/napari.jl:258-264`
have no dispatch branch left in the bridge.

## Deliberately NOT in scope

- **Visual parity / pixel-matching napari** — see decision 8.
- **Re-chunking the stores.** Earlier advice said "chunk geometry first"; that was reasoning from
  per-chunk HTTP, which is dead. With server-side assembly the 1116-chunk geometry costs ~330 ms of
  Python read (~160 ms in Julia). Worth doing eventually, not a prerequisite. Note `fXgbTl`'s
  whole-plane chunks are a consequence of its size (441 < 512), not a format choice.
- **Rendering modes beyond MIP.** Nothing in the codebase sets `rendering`, so the app only ever uses
  napari's default MIP. If anyone relies on iso/attenuated/translucent from napari's own GUI, that is
  unscoped work — **worth asking before P1**.

## Open, with a mechanism behind it

**The cold path is upload-dominated and the reason is not yet known.** 535 ms for 351 MB is ~0.8 GB/s
marginal. Row alignment was measured and **refuted** (padding to 256 changes nothing), and the GPU
copy itself is below a ~100 ms measurement floor. The remaining candidate is the JS-heap → GPU-visible
staging copy: use a `MAP_WRITE` buffer, write the fetched slab straight into `getMappedRange()`,
unmap, `copyBufferToTexture`. Untested. Also unmeasured: `onSubmittedWorkDone`'s ~100 ms quantum makes
anything faster than that unmeasurable — batch N operations per submit to get under it, as G2 did.
