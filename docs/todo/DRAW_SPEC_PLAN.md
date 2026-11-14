# Draw spec — one description of what the viewer draws, for the viewer and every recording

**Status:** building. Phase 0 decided, Phases 1 and 2 built (2026-11-12). Phase 3 built (2026-11-14):
skeletons, scale bar / timestamp sizes, Steps. Open: scale bar / timestamp on / off (`pending`);
Phase 4.

## Why

Recordings (Record, batch, compare grid, keyframes, stills) must draw what the viewer draws
(`docs/ARCHITECTURE.md` → *Recording parity*). The shader is shared, but deciding WHAT to draw is not:

- the viewer reads its settings directly (`ViewerWindow.vue`, `ViewerPanel.vue`: ~64 settings reads)
  and builds its own requests (slab URL, overlay fetches, uniforms);
- recording has a second reader, `utils/viewer/viewerLook.ts` (23 reads), into a movie config, which
  Julia (`_overlays_raw_from_config` → `movie_mask` / `viewer_overlay_closure`) and Python
  (`render_animation_run.py`) resolve a second time.

A drawing feature that lands in the first reader and not the second ships as a movie that doesn't
match: halo / nuc files (#2162, fixed #2172); mask colour by population and per-image pop eyes landed
viewer-first and reached recordings in later PRs. Nothing in the code makes the second reader
follow the first.

## Decisions

**D1 — One draw spec.** `ViewerDrawSpec` (TS): everything that decides the picture — mask segmentation
and its files, mask colouring, opacity, contour; pop types and eyes; track sources, colours, mode,
tail; point / tail sizes and z tolerances; colour-by; channels and LUTs; z / z window; 3D camera and
projection; on-image layers (scale bar, timestamp, grid, landscape). One pure function
`drawSpec(inputs)` builds it from settings + the viewer's state.

**D2 — The viewer renders from it.** `composables/useVwDrawSpec.ts` exposes the spec; the renderers'
"what to draw" inputs (mask source id, slab query, overlay fetches, label uniforms) read only it. A
setting the spec doesn't carry can't change the picture.

**D3 — The look IS the spec.** Record / batch Fill from view / Animation send the spec (serialised);
`viewerLook` becomes the spec's serialiser, and `readViewerLook` with the viewer closed calls the same
`drawSpec` on settings. No second reader.

**D4 — A field must be decided, not forgotten.** A typed map
`RECORDED: Record<keyof ViewerDrawSpec, 'recorded' | { notRecorded: string }>` — adding a spec field is a
compile error until it says which. A Julia test asserts every `recorded` key is read by
`_overlays_raw_from_config` (or the record request reader). `notRecorded` entries need the maintainer's
sign-off (the Phase 0 table records them).

**D5 — One resolver server-side (later phase).** `resolve_draw_spec(img, spec)` → the mask store and
its files, the colour table, the overlay closures; used by the movie paths and, where they share the
decision, the viewer's routes (slab `files`, overlays). Retires twins such as
`wgpu_host.compose_label_files` vs `compose_label_files!` where a single owner is possible.

## Phases

0. **Gap inventory** (no code). Every drawing input the viewer has that the look lacks, each decided
   with the maintainer: record it, or not-recorded with a reason. Candidates from a first pass
   (2026-11-12):
   | Viewer input | Recording today | Decision |
   |---|---|---|
   | Grid overlay (`viewerGrid`, density) | not drawn | **not recorded** (maintainer, 2026-11-12) |
   | Landscape overlay (`viewerLandscape`, labels) | not drawn | **not recorded** (maintainer, 2026-11-12) |
   | ROI spotlight, tile marks, crop box | not drawn | **not recorded** (maintainer, 2026-11-12) — on-screen aids |
   | Skeletons (`getBranchVisibility`) | not drawn anywhere: the panel's Record sent `branchValueNames`, which only counted toward a compare grid (a napari leftover); the WebGPU viewer drew none (corrected 2026-11-13) | **recorded**, Fill from view too (maintainer, 2026-11-12) — drawn as LINES (2026-11-13, below) |
   | Scale bar / timestamp sizes (`viewerScaleBarPx`, `viewerTimestampPx`) | on/off from the record options; the movie's own sizes | **recorded**: the viewer's sizes (maintainer, 2026-11-12) |
   | 3D ray steps (`viewerSteps`) | separate `renderQuality` knob (draft / standard / high = 128 / 256 / 512 steps — the same quantity as the viewer's Steps slider, 64–512, default 256) | **recorded**: the viewer's Steps by default; Render quality stays as an optional override (maintainer, 2026-11-12) |
   Shader lanes: the movie host sets every `mip` lane except `zwin` (it loads the exact planes, so
   the default is right); `tile` / `brick` are viewer-only renderers for the same picture.
1. **BUILT** — **`drawSpec` + `RECORDED` map**, behaviour-neutral: `utils/viewer/drawSpec.ts`, and
   `readViewerLook` = `viewerLook(readDrawSpec(…))`. D4 checks: `drawSpec.test.ts` and the Julia read of
   `lookKeys.parity.json`. The fields Phase 0 decided to record are `pending` until Phase 3 wires them.
2. **BUILT** — **The viewer consumes the spec** (D2). `drawSpecOf(src)` is the one reader, every field a
   getter (a computed reading one field tracks only that field's settings — `drawSpecRead.test.ts`);
   `ViewerWindow`'s `spec` reads it over its own `meta` names and gating ref, and its draw reads (mask
   pick, pop types, track eyes / colours / mode, point and tail sizes, z tolerances, mask colour /
   opacity / contour, Steps, scale bar / timestamp, grid, landscape, ROI spotlight) go through it.
   `readDrawSpec` is its plain-data snapshot. The viewer publishes its segmentation names
   (`openImage.labelNames` / `trackNames`), so a recording picks the mask and track sources from the
   list the viewer draws from rather than the registry's. `popVisible` is data (per pop type), not a
   function. Left on the settings store: eye reconciles and write-backs, the preview / live mask
   override, `maskFilesOf` (it takes any segmentation, the preview's too).
3. **Close the Phase 0 gaps** marked "record it", each with a runner test that draws it.
   - **Skeletons — BUILT** (2026-11-13). Drawn as lines, not a second mask texture: a texture would be
     the parked multi-mask refactor (`MULTI_MASK_PLAN.md`) and 4 B/voxel per cached timepoint outside
     the brick / tile budget (maintainer: lines, simplified paths). `segment.branching` stores each
     branch's Ramer–Douglas–Peucker-simplified path (`uns/paths`, `pathTolerance` 0.5 px); one reader
     (`skeleton_segments`) feeds the viewer route and the movie closure; the viewer appends them to the
     track segment buffer (`appendSkeletonLines`, end timepoint -1 = unfaded, plane -1 = every plane);
     the look carries `skeletons`. Branch tables written before need a branching re-run. Lines are
     coloured by skan branch type (`palettes.json` → `branchTypeColours`; the look's colour
     `branchType`), as the old R viewer did; the "On the image" overlay has the on / off and the key.
   - **Scale bar / timestamp sizes — BUILT** (2026-11-14). The look carries `scaleBarPx` /
     `timestampPx`; `_frame_text` makes a recording's `FrameTextShow` (off / the movie's own size /
     n px), and `draw_frame_overlays` draws the text at n frame px with the bar 0.3 × as thick, as
     `StillOverlay` does. Frame px, not scaled: a movie larger than the viewer's canvas has
     relatively smaller text. On / off stays the record options' (`scaleBar` / `timestamp` still
     `pending`: not decided in Phase 0).
   - **Steps — BUILT** (2026-11-14). The look carries `steps`; Render quality gains `viewer` (the
     default: those Steps, `raySteps`), and draft / standard / high stay presets that override
     (`_render_quality` → `RenderQuality`).
4. **Server resolver** (D5).

## Not covered

- Pick / hover routes and the correction cockpit read what the viewer shows, not what it draws; out
  of scope unless Phase 0 finds a shared input.
