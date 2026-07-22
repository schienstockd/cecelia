# In-app crop panel — replace the napari 3D crop

**Status:** planned (2026-07-22). Supersedes TODO #00079 ("improve the 3D-crop UX"). Decision by Dominik:
the napari-driven crop has a low ceiling (napari only edits shapes in 2-D, the projection collapses all
channels to one grayscale MIP, and the 2D↔3D dance is clunky). **Remove the napari crop path entirely**
and do the crop **in the app**.

## The idea

Crop is fundamentally: *look at the structure, draw an XY box, pick a z- and t-range, save a new image.*
None of that needs napari. Do it with a **headless composite MIP rendered in the browser** + the app's
own rectangle-draw:

1. Backend renders a **coloured, z-max-projected, per-timepoint** thumbnail of the image (no napari).
2. The app shows it, lets you **scrub timepoints** to find the clearest footprint.
3. You **draw an XY rectangle** on it (reusing the gating rectangle-draw machinery).
4. **z-range + t-range sliders** (with the `5–16/20` slice readout already built in `crop3d.ts`).
5. **Save** → the existing `editImages.cropImage` task (unchanged — it just takes a pixel box).

Rejected alternative — *"control napari over the app"*: keeps exactly the dependency we want gone (a
viewer process + GPU/display + 2D/3D toggling). A headless in-app render removes napari from the loop.

## Locked decisions

- **No napari in the crop flow.** The napari crop path is deleted once the in-app panel works (see Phase 3).
- **Render is IN-PROCESS JULIA** — a *narrow, sanctioned carve-out* of the "one canonical image reader"
  rule (Dominik, 2026-07-22). Julia reads the OME-ZARR directly (`Zarr.jl` + `Blosc.jl`) **only for this
  lightweight MIP preview**; **Python `zarr_utils` stays canonical for ALL processing/analysis reads**.
  Justified because it's tiny read-helpers (a MIP + composite), not a second full image stack — and it
  buys the decisive win: the API server is already running, so the render is an in-process handler (like
  the gating density grid) with **no subprocess spawn**. **Proven on real data (2026-07-22):** Zarr.jl
  reads both cecelia layouts + uint8/uint16 at **~120–390 ms/frame** in-process (spike in scratch env).
  ⚠️ Guard against drift: keep the Julia read to the MIP-preview helper only; never grow it into a
  general image reader (that's what `zarr_utils` is). Document the carve-out in `docs/ARCHITECTURE.md`.
- **Julia read gotchas (from the spike — the real code MUST handle):** (1) Zarr.jl is **column-major →
  reversed axis order** `(x,y,z,c,t)`; map axes via NGFF `read_axes`, never assume `tczyx`. (2) **Two
  on-disk layouts** — flat array at `/0` vs nested bioformats2raw series at `/0/0`; detect + descend
  (the `series_base` case). (3) **dtype varies** (uint8 / uint16); normalise via contrast, not a fixed max.
- **New Julia deps:** `Zarr`, `Blosc`, `PNGFiles`, `ColorTypes` — add to `api/` (the server that renders)
  and re-resolve; if the render helper lives in the `Cecelia` package (`app/`) instead, all THREE
  manifests (app/api/pluto) must be re-resolved + committed (CLAUDE.md dep rule).
- **Composite MIP:** max-project over Z (coarse pyramid level, long side ≤ ~1024 px — reuse the
  `_pick_mip_level` idea so a full-res projection never stalls), colourise **each channel** and blend to
  RGB (the multi-channel view the grayscale napari MIP lacked).
- **Colours/contrast come FROM THE VIEWER (locked requirement, Phase 1 — not a later nicety).** Read the
  per-channel **colormap name + `contrast_limits` + `visible`** the user set in napari, persisted to the
  layer-props store — which becomes **JSON, the single canonical format (pickle dropped)**. Every field the
  viewer saves is JSON-native (`opacity/blending/visible/gamma`, `contrast_limits` [2 floats], `colormap`
  name, `dims.current_step`), so pickle bought nothing; JSON is Julia-readable and matches the rest of the
  codebase (ccid.json, gating json, …). Apply each channel through its colormap, clip to its contrast,
  **additive-blend** (napari's `blending='additive'`), skip `visible=false`. **Fallback** when no props file
  exists (image never opened / autosave off): `CHANNEL_COLORMAPS` (`red/green/blue/yellow`) + percentile
  contrast. Colormap→RGB: the
  additive primaries (`red/green/blue/cyan/magenta/yellow/gray/white`) are simple linear ramps
  implemented directly (fast, no heavy import); a non-trivial/perceptual name falls back to gray (rare for
  raw channels) — revisit with a LUT resolver only if needed.
- **Timelapse is first-class (locked requirement).** Render **one composite MIP per timepoint** (max over
  Z, keep T); the panel **scrubs** frames to pick the clearest footprint; the crop **t-range** is a real
  control (Save writes `t0/t1`). The display scrubber (which frame you look at) is distinct from the crop
  t-range (what Save keeps). **Verify against a real timelapse** (test set `jFWePN`, tracked live images)
  before Phase 2 is called done — not just a static image.
- **Serve strategy: lazy per-frame, in-process, cached.** No precompute, no subprocess — the API server
  renders the composite MIP for timepoint `t` on demand (Julia, ~120–390 ms/frame proven) and caches it
  (in-memory LRU and/or a preview PNG on disk). The scrubber requests frame `t`; first view ~0.1–0.4 s,
  instant once cached. Optionally prefetch neighbours. This is viable *because* the render is in-process
  (no spawn) — the whole precompute-vs-batch problem from the benchmark is moot.
- **Draw + coordinate mapping:** reuse the gating rectangle-draw (`GatePlotPanel` `mode:'rectangle'` +
  canvas overlay + world-coordinate inversion). Map displayed-thumbnail px → full-res px exactly like the
  bridge's old `crop_box` (coarse µm/px → level-0 µm/px), so the saved box matches what you drew.
- **Reuse, don't rebuild:** `crop3d.ts` (`normalizeRange`/`fracToIndexRange`/`fracRangeLabel`) and the
  `editImages.cropImage` task carry over unchanged. The panel opens from a **per-image "Crop" action in
  the image table** (`ImageTable` → `CropDialog` wrapping `CropPanel`) — napari-free and discoverable,
  matching the metadata/physical-size per-image dialogs. (Initially trialled in the napari Viewer panel;
  moved out because gating it on an open napari image hid it and defied the napari-free goal.) The z
  slider **re-projects the preview MIP** to just the kept slices, so you see what you'll keep.

## Phases

**Phase 0 — Layer props: pickle → JSON (small prerequisite, self-contained win).** Replace the layer-props
pickle with JSON as the single format: `save_layer_props`/`load_layer_props` use `json` (drop `import
pickle` — it's the only pickle use); `_props_path` (napari_api.jl) → `.json`. **Migration:** on load, prefer
`.json`; if only a legacy `.pkl` exists, read it once and rewrite as `.json` (so saved contrast isn't lost;
nothing is ever written as pickle again — legacy read-fallback removable a release later). Now both the
Python bridge and the Julia renderer read the same file. Unblocks the colour requirement.

**Phase 1 — In-process Julia composite MIP + show it in the app (the foundation).**
- Julia render helper (in `api/` or a small module): open the OME-ZARR with `Zarr.jl` (handle the reversed
  axis order + flat-vs-`/0/0` layout + uint8/16 — see gotchas), for timepoint `t` do z-subsampled
  max-project → per-channel colourise (from the JSON sidecar, else defaults) → additive blend → RGB →
  PNG (`PNGFiles`). Keep the **colourise+blend a pure function** (array + specs → RGB) so it's unit-tested
  on a synthetic array. Add `Zarr`/`Blosc`/`PNGFiles`/`ColorTypes` deps + re-resolve.
- API: `GET /api/crop/frame?projectUid&imageUid&valueName&t` → `image/png` (renders on demand, caches);
  a small `GET /api/crop/info` → `{nT, nZ, coarseW/H, fullW/H, µmPerPx}` for the panel + box maths.
- Frontend: a minimal `CropPanel` (Viewer page) that shows the MIP with a **t-scrubber**.
- Done when: opening the panel shows the coloured, scrubbable MIP in-app. **Verifiable headless** (pure
  colourise/blend unit test + the Zarr read already proven); the panel display is the live check.

**Phase 2 — Draw + ranges + save.**
- Rectangle draw over the MIP (gating machinery), displayed-px → full-res-px mapping.
- z-range + t-range sliders (reuse `crop3d.ts` + the `fracRangeLabel` readout).
- Save → `editImages.cropImage` with the pixel box (same params the bridge produced).
- Done when: draw → set z/t → Save produces the correct cropped new image (assert box maths in a unit test).

**Phase 3 — Remove the napari crop path.**
- Delete `napari_bridge.py`: `start_crop`/`_z_mip`/`_pick_mip_level`/`apply_crop`/`_read_crop_rect`/
  `crop_box`/`clear_crop`/`_apply_clip_planes`/`_clear_clip_planes`/`_restore_crop_visibility`, the
  `CROP_LAYER`/`CROP_MIP_LAYER` consts and `_crop_*` state + the `crop_*` command dispatch.
- Delete the `/api/napari/crop-*` routes (`server.jl`) and their `napari_api.jl` handlers.
- Delete the ViewerPanel "3D crop" section + its `crop*` refs/functions (the interim polish added while
  scoping #00079 is throwaway — superseded here).
- Docs: replace `docs/NAPARI.md` → "3D crop" with a pointer to the new panel; add the panel to
  `docs/UI.md`/`INVENTORY.md`; delete TODO #00079.
- Done when: no `crop` references remain in the napari bridge/api, and the in-app panel is the only crop.

## Open questions (settle before the phase that hits them)

- ~~Render/serve at scale~~ **RESOLVED:** in-process Julia lazy-per-frame (~120–390 ms proven) + cache;
  no precompute/spawn, so scale isn't a concern.
- ~~Colour fidelity~~ **RESOLVED:** read the viewer's colormap + contrast from the JSON layer-props file
  (Phase 0 converts pickle → JSON, the single format); fall back to `CHANNEL_COLORMAPS` + percentile if absent.
- **Where the Julia render helper lives** (Phase 1): `api/` only (fewer manifests to re-resolve) vs the
  `Cecelia` package (`app/`, reusable/REPL-testable but needs all three manifests). Lean `api/` unless the
  REPL/tests need it.
- **Draw reuse vs. bespoke** (Phase 2): confirm `GatePlotPanel`'s rectangle draw can host an arbitrary
  raster background cleanly; if the coupling to gating data is too tight, factor the rect-on-canvas
  primitive out rather than cloning it (divergent-re-implementation rule).
