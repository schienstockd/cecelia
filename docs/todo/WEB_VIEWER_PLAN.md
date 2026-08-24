# Web viewer — replacing napari with a browser renderer

**Status:** P1 + P2 built incl. the 2D plane view (2026-08-24) · branch `audit/napari-webgpu` · prototypes in `docs/todo/spike/webgpu/`

**Goal: cecelia becomes a sole browser app. napari is removed, not split with.** The cloud mandate
does not tolerate a Qt process, so "napari already does this well" is never a reason to keep a
feature there — it is a competing implementation to beat. Dominik, 2026-08-24.

**The bar is PARITY with what the repo does today, not new capability.** "It's more important to get
parity to what we have now in the repo, rather than adding new stuff" — Dominik, 2026-08-24. So the
phase list is derived from the bridge's own 31 commands (the checklist below), not from what a viewer
could be; anything the browser gains along the way is a bonus, never a reason to reorder.

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
| `Dml3RG` volume timepoint (37 z, 4 ch), server read | 401 ms · 326 MB · **59 GB** per movie — never cacheable |
| `Dml3RG` ONE z plane, server read | **13–22 ms** · 8.8 MB · **1.59 GB** per movie — fits in a 2 GB budget |
| Striding the volume instead | 8x fewer bytes buys **1.6x**, 59x fewer buys 3x — the read is not bandwidth-bound |
| `read_native`'s byte swap when orders already agree | **+65%** on an 81.5 MB read (68 → 112 ms) — fixed |

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
8. **The 2D z-plane view is the timecourse view, and the default.** Dominik: *"I normally switch to
   2D plane rather than 3D MIP to look at the timecourse. A 2D z plane view is needed in any case"*
   (2026-08-24). It is not a lesser mode — it is the one that plays. A plane timepoint of `Dml3RG` is
   8.8 MB and ~13 ms against 326 MB and ~400 ms, and the whole 181-frame movie is **1.59 GB, so it fits
   in VRAM entirely** where the volume is 59 GB and never can. The volume MIP is for looking at
   structure. Both come out of ONE shader: a plane is a volume one deep seen face-on with `steps = 1`,
   which needs orthographic projection (perspective foreshortens a flat plane towards the edges).
9. **Visual parity with napari is NOT a goal.** "If it looks different it's fine, as long as it shows
   you the data" — Dominik, 2026-08-24. This closes the figure-comparability risk an earlier draft
   raised, and removes any deadline on A/B-ing against napari before it is removed.

## Parity checklist — the 31 things the bridge actually does

Derived from `execute_command` in `napari/napari_bridge.py`, so it is the repo's own list rather than a
remembered one. Two entries had no phase in the first draft of this plan and are now P6 and P7; finding
them is the argument for deriving the list instead of writing it from memory.

| Bridge command(s) | Where it lands |
|---|---|
| `open_image`, `set_z_view`, `set_3d_level`, `centre`, `clear`, `show_layer`/`hide_layer`/`remove_layer` | **P1** display + camera |
| `load_layer_props` (contrast, colormap, T/Z) | **P1** reads it; who WRITES it is P8's call |
| — (`scale_bar`, the elapsed-time `text_overlay`; napari's own, not commands) | **P2** — BUILT |
| — (t-scrubbing; napari's own dims slider) | **P2** |
| `show_populations`, `show_tracks` | **P3** |
| `show_labels`, `refresh_labels`, `colour_labels`, `show_branch_labels`, `colour_branch_labels` | **P4** |
| `record_timelapse`, `record_keyframes`, `stitch_movies`, `save_screenshot`, `capture_view_state`, `apply_view_state` | **P5** (renderer C) |
| `start_cell_selection`, `update_selection_scope` | **P6** |
| `show_task_preview`, `preview_region` | **P7** |
| `ping`, `gl_info`, `set_task_dir`, `configure_autosave`, `save_layer_props` | **P8** — bridge plumbing, deleted with the bridge |

## Phases

Each phase ends in something usable, and names what would make it fail.

### P1 — the slab route + a viewer that shows one timepoint — **BUILT**
`/api/viewer/meta` (JSON) + `/api/viewer/slab` (raw voxels, `enc=zstd` optional) in
`api/src/viewer_api.jl`; `/viewer-window` in the frontend — a WGSL MIP raycast with camera drag, wheel
zoom, per-channel contrast and visibility, opened from the ViewerPanel's ↗.
**Measured, not assumed:** `read_slab` is 253 ms/channel on `VJy1Nx` (88 MB) and 17 ms on `fXgbTl`,
against ~330 ms/channel for the Python prototype — so the gate ("much slower than the prototype")
passed. Byte-for-byte identical to `zarr_utils` on both stores, all four probes and the wire order
(`docs/todo/spike/webgpu/slab_bench.jl`, `p1_slab_bench.json`).
**Verified in the browser** (Dominik, 2026-08-24): opened against a restarted backend — the shader
compiles, both routes answer, and the raycast shows the data. A generated harness
(`shader_check.mjs` → `~/Downloads/TMP/shader_check.html`) asserts the shader and the LUT indexing on a
phantom, with no backend, for the next time the shader changes.

### P2 — the timecourse — **BUILT**
One `r16uint` texture per cached timepoint under a VRAM budget (a visible setting, default 2 GB),
evicted least-recently-used and never evicting the frame on screen; a directional prefetch window;
`AbortController` on everything the window leaves; a bucketed cache-state strip; play with an fps
control and a loop toggle; and the **2D/3D switch with a z-plane slider** (decision 8). The decisions are
in `utils/volumeCache.ts` and unit-tested; the textures are in `lib/webgpu/volumeRenderer.ts`.

Four things settled while building it, each of which reads as tidiness and is not:
- **Showing and fetching are separate calls.** Awaiting the prefetch window before painting is the
  obvious shape, and it makes playback advance once per window fill (several seconds) instead of once
  per frame.
- **The scrubber goes through `debouncedLatest`, the paint through `usePlotResize`** — one canonical
  scheduler per half. A slider fires per pixel and each position wants a *different* prefetch window,
  so a hand-rolled version starts a fetch before it can notice it was superseded: dragging across 100
  timepoints put 100 concurrent volume fetches (400 requests) in flight.
- **Playback WAITS for an uncached frame** rather than skipping to whatever is resident. Skipping holds
  the frame rate by silently dropping timepoints, and nothing on screen would say data went past.
- **A reset must FILL the frame, in 3D as well as 2D.** The first fit framed off
  `max(extent) * 1.7`, which left the image at ~64% of the viewport height — a reset that looked zoomed
  out. The second fitted 3D to the bounding SPHERE so rotating could never clip a corner; that traded a
  permanently zoomed-out reset for a problem the wheel already solves, and Dominik asked for fill in
  both. The framing constant now lives in ONE place (`VIEW_HALF_ANGLE`) and is interpolated into the
  WGSL, because the camera solves for `dist` from it and the shader turns `dist` back into a half-height
  — two copies drift, and the symptom is exactly a reset that does not fit. The third attempt fixed the
  *opposite* error in 3D — "it fills the whole width and then clips top and bottom" — and the cause is
  the projection: distance is measured to the box CENTRE, but under perspective what the user sees is
  bounded by the NEAR face, half a depth closer and so magnified. `fitCamera` now takes the projection
  and backs off by `ez / 2` for it; orthographic has no such term, magnification being
  depth-independent. Both projections therefore frame the front face identically, so the 2D/3D toggle
  does not jump.
- **The panel's own numbers must not resize the panel.** Three separate reports of it "jiggling", one
  mechanism each: a `:value` + `@change` z slider was uncontrolled for the whole drag so every playback
  re-render patched the thumb back out from under the pointer (the `driftingTextFields` detector now
  covers that shape — see below); a `Loading frame N` line that appeared and disappeared shoved the cache
  strip up and down, and is now a blinking amber dot sharing the strip's row; and the `140 / 180` readout
  changed width as the number did, resizing the `flex: 1` slider beside it every frame.
- **The prefetch window leans ahead at EVERY capacity.** Reserving a flat two slots for the reversal
  inverted it below capacity 6 — at capacity 3 every slot went behind the playhead, so playing forward
  missed on every frame while the cache held frames already watched. Capacity 2–3 is a big image on a
  modest budget, i.e. the case that matters most.
- **The prefetch walk found a hole in `debouncedLatest`, and the fix belonged there.** Its
  `isCurrent()` compared a token that only moved when a successor RUN started — and a successor cannot
  start while its predecessor is in flight, because runs are serialised. So a queued request did not
  supersede anything, and a walk over a 170-frame window ran to completion wherever the user went. Two
  bugs, one cause (Dominik, 2026-08-24): jumping to timepoint 90 waited for every frame before it, and
  **playback stopped outright** — each tick queued a request that could not start, while the walk filled
  frames nobody was waiting for. `isCurrent()` now also requires that nothing is pending, which is what
  rule 2 of that util always claimed; the viewer's walk is back to a plain loop with a checkpoint
  between fetches. **All 16 consumers had the same hole** — it merely looked like an occasional stale
  paint rather than a stall, because their work is one await and not a walk. `schedulePump` additionally
  abandons an in-flight fetch the new window has no use for, since the walk is awaiting that very fetch
  and aborting is what makes its checkpoint arrive early.
- **The on-image overlays are the stills' component, not a fourth scale bar.** `StillOverlay` +
  `niceScaleBar` + `elapsedLabel` already served the captured stills and the animation timeline, and the
  movie compositor draws the same two things; a viewer-local implementation would have been the fourth.
  Three things had to change for a live canvas rather than a thumbnail, and each was a bug in the shared
  code rather than a special case:
  - **Chrome sized in screen px, not as a fraction of the frame** (`chrome: 'fixed'`). Proportional
    sizing is right for a strip card and renders a 35 px label on a 700 px canvas that also changes size
    as you zoom — "massive scale bar, tiny timestamp".
  - **`micrometer` is a micron.** OME's `PhysicalSizeUnit` is literally that word, which the helper's
    `micron` test did not match, so the bar read "100 micrometer" and never rolled up to mm.
  - **H:MM:SS**, matching napari's `datetime.timedelta` overlay down to its `t = N` fallback, as a
    second style on the one formatter rather than a second formatter.

  The bar is drawn against what the CAMERA sees, not the image, so it tracks the zoom — and it needs no
  orientation gate. An earlier version hid it unless the camera was face-on, reasoning that a rotated
  horizontal axis mixes x and z; that is true of the voxel grid but not of the space actually rendered,
  which is scaled to µm on all three axes and therefore metrically uniform.
- **The 3D view needed a DEPTH control, not a faster fetch** (Dominik's suggestion, 2026-08-24 — "would
  it help if we were to crop the z stack with a double edged slider?"). Yes, decisively: every cost on
  this path is linear in the plane count, so 8 planes of 41 is a ~0.6 s fetch instead of ~5.8 s, and
  five times as many timepoints fit the same VRAM budget. `z=N&zTo=M` on the slab route, a `RangeSlider`
  in the panel, committing on **release** (`@change`, added to that component for this — the range
  refetches every cached texture, so per-pointer-move would be ruinous). It defaults to the full stack:
  a MIP over part of a stack is a different picture, and narrowing it silently would change what the
  view MEANS in order to make it fast.
- **A renderer's own numbers are not reactive, and a computed over them is a lie.**
  `computed(() => renderer.value?.cache.capacity)` never re-evaluates: the renderer is a `shallowRef`
  and its capacity is a closure variable, so Vue has nothing to invalidate on. The panel reported
  `cache 3 / 169` for a cache that held four — and that reading is worse than a missing one, because it
  looked exactly like a real geometry bug and was diagnosed as one twice. The numbers are now snapshot
  into a ref in `syncCacheState`, which already ran at every moment they change.
- **A read-ahead is only worth pre-paying for while a frame is cheap.** Entering the 3D view took ~6 s
  ("now it doesn't load anything, or needs like 5 s" — Dominik, 2026-08-24) because the walk filled the
  whole four-timepoint window before it ended. Measured on the real target, from the client: one 3D
  timepoint is **~1 s to fetch 326 MB** (server read 0.51–0.60 s; all four channels in parallel answer in
  0.68 s wall, so the server is not the constraint) plus ~0.5 s to upload. The read-ahead buys exactly
  one thing — playback — and the 3D view is far too slow to play, so `prefetchDepth` spends nothing on it
  unless playback is actually running. The plane view is unaffected: at 8.8 MB a timepoint it still fills
  the whole movie. Not a setting; the cost is knowable from the data, which is the same reason the VRAM
  slider was wrong.
- **The contrast slider's RANGE follows the data even though its auto WINDOW does not.** They look like
  the same question and are not: a window recomputed per timepoint makes playback flicker (decision 5),
  but a ceiling taken from the first timepoint and held is simply *wrong* on a movie that brightens —
  "you might want to push it up a bit, but you can't because it's clipped" (Dominik, 2026-08-24). The
  ceiling is now the brightest voxel seen in any loaded timepoint, ratcheting upward only, plus 50%
  headroom so the window can always be opened past saturation. Affordable because it needs no
  percentiles: `slabMax` shares `contrastFromSlab`'s strided walk but skips the sort.

**The headline scrub number in `NAPARI_WEBGPU_AUDIT.md` does NOT describe this cache.** "186 hits / 1
miss, sub-millisecond" was measured on `fXgbTl` — **31 timepoints at capacity 63**, so eviction was
structurally impossible. It characterises a fully-resident movie, not a bounded cache. Credit to a peer
review for catching that it was quoted as a general property (2026-08-24). What is *also* true, and is
the reason it matters less than it looks: at one z plane a realistic movie is resident anyway (1.59 GB
of a 2 GB budget for `Dml3RG`), so eviction only bites on the volume path — which is the path we have
now established is the wrong one for playback.

**The bounded cache now has its own number.** Observed in the running window on `Dml3RG` (181 t) at
capacity **58** — well below nT, so eviction was live — during playback at 22 fps: **398 hits, 1 miss**,
last miss 194 ms, fetch 80 ms of which 14.6 ms was server read. A 99.7% hit rate because sequential
playback keeps the directional prefetch ahead of the playhead. Scrubbing at random over a partially
resident movie is still unmeasured and would be the honest worst case.

**Two ways to kill the BROWSER, neither of which raises anything catchable.** Firefox's main process
crashed with `MozCrashReason: Queue[Id(4,2)] does not exist` and
`GraphicsCriticalError: Texture is not submitted` (Dominik, 2026-08-24, RTX 2000 Ada). Both paths were
live in `volumeRenderer.ts`:

1. **Work submitted after the device was gone.** `device.lost` was surfaced to the user but never
   stopped the renderer, so `draw()` kept writing uniforms and submitting to a dead queue every frame.
   A `dead` flag now gates every GPU call, and `destroy()` skips the resources the device took with it.
2. **A bind group outliving its texture.** Eviction spared the timepoint being *loaded*, while the bind
   group pointed at the different timepoint still on screen — which is the normal state of the viewer
   for as long as a load takes. So a jump to a far timepoint could destroy the texture the next draw
   would bind. `lruEvictions` now takes a SET of timepoints to spare (`[keep, boundT]`), and `dropSlot`
   unbinds before destroying, so the invariant holds even if a future caller forgets the set.

The lesson for the rest of this plan: on this path a logic error is not a bad frame, it is the user's
whole browser. Guard invariants at the resource, not only at the policy.

**VRAM exhaustion took three attempts to make impossible, and the first two are instructive.** The
original control was a megabytes-of-VRAM slider, and setting it too high **lost the GPU device**
(Dominik, 2026-08-24). It was unanswerable by construction: WebGPU deliberately exposes no free-VRAM
figure, so neither the user nor the app can compute a safe number.

1. **An `out-of-memory` error scope around each texture** — necessary, not sufficient. Asked for 181
   volume timepoints the driver lost the device outright rather than failing the allocation, which a
   scope cannot intercept. Kept as a second line of defence.
2. **A cap in TIMEPOINTS** — this was the wrong unit, and it is the bug that reproduced the crash. One
   timepoint is 8.8 MB in the 2D view and 326 MB in 3D, so "keep all 181" is 1.6 GB in one and **59 GB**
   in the other. A count cannot express a memory bound.
3. **A hard internal ceiling in BYTES** (`SAFE_CACHE_BYTES`, 1.5 GB) — the only unit that means the same
   thing in both views. It holds ~170 plane timepoints (nearly a whole movie) and ~4 volume ones, which
   is the right shape: the plane view is what plays, and 3D at ~400 ms a frame was never going to stream.

The visible control asks how many timepoints stay instant — the outcome, defaulting to "all" — and is
silently bounded by the byte ceiling, so no setting can crash it and no VRAM concept has to be learned.
A device that is lost anyway now offers a **Reload** rather than a setting to go and adjust: the canvas
context goes with the device, so it cannot be recovered in place.

### P3 — h5ad-derived overlays: points, tracks, colour-by — **POINTS BUILT**
**The second make-or-break.** A and C read h5ad server-side and ship identifiers only
(`CLOUD_MIGRATION_ASSESSMENT.md` §3a); B must add routes. Do NOT reimplement `LabelPropsView` in the
browser — serve centroids/tracks/columns as JSON or binary from Julia, through the canonical
`label_props` view.
**Fails if:** the payloads turn out large enough to need their own caching story. Measure first: h5ad
is 139 MB total across 55 files, median 0.77 MB, so probably not.

**It did not fail — measured before the route was written.** The largest cell table in the dev projects
is 98,610 cells (`WIaUjL/p6t4mC/Tcell`) and the typical one 6,547. At five f32 columns that is 2.0 MB
and 0.13 MB — comparable to a SINGLE 2D slab (8.8 MB). So `/api/viewer/overlays` answers the WHOLE
MOVIE in one request and the client filters by `t` locally: no per-timepoint request path, no second
cache to keep coherent. Warm server time is **196–208 ms for 51,846 cells / 3.27 MB** (`fXgbTl/flowTom`,
three passes), which is a once-per-image cost.

Built so far, and the decisions worth keeping:

- **Membership comes from `resolve_pops`** — the same mtime-keyed cached resolver that feeds napari's
  points layers. A viewer that computed its own membership would be a second answer to "which cells are
  in /A" and could disagree with the plots.
- **The instance buffer is ordered by TIMEPOINT.** Drawing a frame is then one instanced draw over a
  contiguous range — no per-frame filtering, no per-frame allocation, no upload on a scrub step. The
  z plane rides along per instance so the 2D view can hide off-plane points in the shader; a CPU filter
  would mean a rebuild and an upload on every step of the z slider, which is a continuous control.
- **A cell in several populations is drawn once per population**, as napari does. Collapsing would mean
  silently picking a winner, and with a hierarchy the overlap is the normal case.
- **The overlay pass shares the raycast's uniform buffer, and therefore its camera.** `project()` in the
  WGSL is the exact inverse of the ray construction. A second camera copy would put a marker beside its
  cell rather than on it — and would still look plausible, which is why `shader_check.mjs` now
  re-derives the projection in JS and asserts the drawn pixel's POSITION, not just that something drew.
- **`ext.w` carries the z origin of the loaded slab**, so a cropped 3D view still places absolute
  overlay coordinates correctly.
- **Point size is in screen px, not µm** — a marker is annotation: legible zoomed out, not swallowing
  the cell zoomed in. Same choice as napari's `points_size`.
- **Two payload traps, both caught by tests rather than in a browser.** JSON has no NaN literal (JSON3
  refuses to write one) and `null` in a coordinate array becomes 0 through `Float32Array.from` — a cell
  at the origin instead of no cell; so undrawable rows are dropped and counted. And `colourBy` echoed
  back the name that was ASKED for, making a stale saved column look like a colour-by with no values.

**Tracks are built too.** One screen-space QUAD per segment rather than `line-list`, because WebGPU
draws 1px lines only and a 1px tail over a noisy MIP is close to invisible (napari's `tail_width`
defaults to 4). Each endpoint is projected independently and the quad widened perpendicular to the
screen direction, so width is in pixels and stays constant under perspective.

- **Segments are ordered by their END timepoint, which is what makes a TAIL one draw.** A tail of L
  frames ending at `t` is every segment ending in `[t-L+1, t]`, and in that order it is a contiguous
  slice — two monotonic prefix indexes give it in O(1). Rebuilding a buffer per frame would be an
  allocation and an upload on every playback tick.
- **`tail_length` is a count of FRAMES** (napari's meaning), so L gives L segments per track. The other
  reading (`[t-L, t]`) draws two hops at L=1 and reads as the slider ignoring you — the test pins it.
- **No segment is drawn across a gap the tracker bridged.** btrack links over a missed detection; a
  straight line there would assert a path the tracker never claimed.
- **The plane test uses the segment's END**, so a tail arriving on the plane you are looking at is kept.
  Judging by the start drops exactly the hop you most want to see.
- **Colour cycles the population palette by track id** rather than running napari's turbo ramp. The job
  is telling adjacent tracks apart, not reading a value off them, and no continuous colormap exists in
  this repo yet — see open question 1.

**Colour-by is built, and open question 1 was answered by taking the stated assumption.**

- **The SERVER decides categorical vs numeric**, through `Cecelia._is_categorical_col` — the same rule
  the plots use. It is not a one-liner: strings are categorical, any fractional value makes a column
  continuous, a small integer level set is a code set, and there are name carve-outs both ways
  (`clusters.*` is always categorical however many levels; `min_distance#`/`contact#` are quantities
  even stored as 0/1). A TypeScript re-derivation would be a second answer about the same column, and
  the viewer and a plot of it would disagree. The payload carries `valueKind` plus `valueLevels` or
  `valueRange`.
- **`utils/colourRamp.ts` is GENERATED from matplotlib 3.11.0**, not typed by hand: viridis and turbo
  sampled at 32 points, which is the same source napari's own maps come from, so a colour-by here
  matches what napari showed. Regenerate rather than hand-editing a stop:

  ```python
  from matplotlib import colormaps
  N = 32
  [colormaps['viridis'](i / (N - 1))[:3] for i in range(N)]
  ```

  It exists because there was nothing to reuse — `image_render.jl` says outright that napari's
  perceptual maps are not ramps from black and cannot be approximated from a name, and the plots get
  theirs from Observable Plot's d3 scales, which WGSL cannot reach. CHANNEL colours are still resolved
  server-side; the two are separate concerns and must stay that way.
- **A cell with no value gets its own grey**, not the ramp's low end: "not measured" must not read as
  "measured, and lowest". A zero-width range shades at the ramp's MIDDLE, because painting every cell
  "lowest" or "highest" both assert something the data does not say.
- **Colour-by is a REQUEST, not a display toggle** — the values come from the server, so changing the
  column refetches. That is the honest shape: sending every obs column up front would be the payload
  measurement all over again, for columns nobody asked for.

**P3 is complete.** Points, tails and colour-by, all against real data. What is NOT done: showing more
than one segmentation at once (open question 2) and a segmentation picker.

**A namespace trap worth knowing.** `/api/viewer/meta` and `/api/viewer/slab` take an IMAGE VERSION as
`valueName` (which zarr the pixels come from, e.g. `smoothed`); `/api/viewer/overlays` takes a
labelProps key (a SEGMENTATION, e.g. `memTom`). Same parameter name, different namespaces. The viewer
therefore sends no `valueName` to the overlay route at all and reports the segmentation the server
chose — sending the image version resolves to the active segmentation by luck, not by intent.

### P4 — labels / segmentation masks — **SERVER HALF BUILT**
`/api/viewer/slab?labels=<value_name>` serves a segmentation's mask through the SAME reader, headers and
shape guard as the image: a mask is another zarr of the same geometry, which is what makes this phase
cheap. Real stores are `UInt32` (`X-Slab-Bpv` reports it), so the client wants `r32uint`. Paths come
from `img_labels_path`, the image-owned accessor the tasks write through — never a filename built in the
api. `/api/viewer/meta` lists the segmentations that have a mask ON DISK in `labelNames`, because
`labels` and `label_props` are independent registries (an imported track set has a table and no mask)
and a store can be registered before it is written.

**Still to do:** the client half — a second 3D texture plus a palette lookup in the shader, and the
decision of what to do in 3D. napari cannot project a Labels layer at all (`projection_mode` accepts
only `'none'`), so there is no behaviour to match and a choice to make: a MIP of label ids is
meaningless (the maximum id is not a visible feature), so either draw labels only in the 2D view, or
give the 3D view a nearest-surface pass rather than a maximum. The 2D view is the one people gate on.

An extra `r32uint` texture plus a palette lookup. Cheap, and **better than napari**, which cannot
project a Labels layer at all (`projection_mode` accepts only `'none'`) — so 3D masks stop needing
the volumetric workaround. Covers branch labels and colour-by-column, which are the same texture with
a different palette.

### P5 — the offline capture path in C
Camera in `image_render.jl` + the four capture commands + title cards, on `jobs.jl` with
progress/cancel. Non-interactive, so 117 ms/frame is fine. Half of C's warm frame is currently PNG
encoding (49.5 ms) — pick a better codec for movie frames. `capture_view_state`/`apply_view_state` are
the keyframe contract the animation page already speaks, so B must be able to answer them too.

### P6 — the selection round-trip
`start_cell_selection` + `update_selection_scope` and the POST back to gating — napari's ONE write
path into the app (`napari_bridge.py:1564`). Select in the image, get a transient population, see it
highlighted on the plots (`project_napari_linked_brushing`). This was missing from the plan's first
draft and is real parity, not an addition.
**Fails if:** picking a cell from a MIP is too ambiguous to be useful — a ray hits many labels, and
napari answers with the one under the cursor at a given z. Likely needs the P4 label texture first, so
the pick can read the label id rather than infer it.

### P7 — task preview overlays
`show_task_preview` + `preview_region` — the previews `api/src/preview_api.jl` pushes into napari so a
task's parameters can be judged before it runs. Behind P3/P4 because a preview is drawn with the same
primitives (points, shapes, labels).

### P8 — decommission
Delete the bridge, the protocol version and the adoption/relaunch machinery. Decide there where
contrast/colormap state lives once napari no longer writes `save_layer_props`. Already dead and
deletable sooner: `crop_start` / `crop_box` / `crop_apply` / `crop_clear` in `app/src/napari.jl:258-264`
have no dispatch branch left in the bridge.

## Deferred

**Mask correction with intensity context** (was P6). "We can defer P6 on mask correction for now. It's
not critical and will only be used by a niche group that hasn't migrated to the new system anyway" —
Dominik, 2026-08-24. It is also the one item that is NOT parity: the old R version put it in napari
because you must see the intensity channels while correcting, and an in-browser surface modelled on the
track-correction timeline would be a new design, not a port. Everything above is a prerequisite anyway.

## Deliberately NOT in scope

- **Visual parity / pixel-matching napari** — see decision 8.
- **Re-chunking the stores.** Earlier advice said "chunk geometry first"; that was reasoning from
  per-chunk HTTP, which is dead. With server-side assembly the 1116-chunk geometry costs ~330 ms of
  Python read (~160 ms in Julia). Worth doing eventually, not a prerequisite. Note `fXgbTl`'s
  whole-plane chunks are a consequence of its size (441 < 512), not a format choice.
- **Rendering modes beyond MIP and the 2D plane.** ~~Nothing in the codebase sets `rendering`, so the
  app only ever uses napari's default MIP.~~ **That reasoning was wrong and cost a phase.** It inferred
  what people look at from what the code *sets*, and the answer was the 2D z-plane view — which is both
  a parity requirement and the only thing that makes a timecourse playable (decision 8). Iso /
  attenuated / translucent remain out of scope, and this time on the right grounds: nobody has said they
  use them. Ask, do not infer it from the source.

## Read-path knobs — what was measured, taken and rejected

Asked for explicitly (Dominik, 2026-08-24) after "did your profiling find any knobs".

**Taken.** `read_native` broadcast `ltoh`/`ntoh` over the whole block whenever the store declared an
order, even when it already matched the host — element-wise a no-op, but the broadcast still allocated
and copied. **+65%** on an 81.5 MB channel read (68 → 112 ms), on every little-endian store, i.e. every
corrected/cropped version the writers produce. Now guarded on an actual order difference
(`HOST_IS_LITTLE_ENDIAN`), which took `Dml3RG`'s volume timepoint 481 → 401 ms. Pinned by an
identity assertion in the *"API: zarr byte order"* testset — the only way to assert "no copy" without
measuring.

**Rejected: `Blosc.set_num_threads(n > 1)`.** A real **2.2x** on a 4-channel timepoint (401 → 184 ms),
and it is inseparable from a data race. blosc1's header says `blosc_decompress` uses the global context
and `blosc_decompress_ctx` is the form for multithreaded callers; Zarr.jl adds no lock. The scaling
curve proves the concurrency is genuine rather than accidentally serialised — at `blosc=8` two
concurrent decompresses cost 0.86x of one, where a global mutex would force 2x — so N callers really are
driving one unsynchronised pool. 768 concurrent reads showed no corruption, which distinguishes nothing:
the mechanism is present. And the safe form is a **regression**: a lock around reads with `blosc=8` costs
639 ms against the 401 ms we ship, because blosc's threads do nothing for a single 512 KB chunk and only
pay off by overlapping across reads. The one legitimate route is `blosc_decompress_ctx`, which Blosc.jl
does not expose — upstream work, not ours. Numbers: `spike/webgpu/p2_blosc_threads.json`.

**A range slider committed on `@change` drifts, and neither existing detector saw it.** The
continuous-controls rule only inspects `@input` handlers, so a slider that commits on release was
invisible to it, and `driftingTextFields` excluded the type on the grounds that ranges were the other
rule's business. The viewer's z slider fell through both. `DRIFT_PRONE_RANGE` now flags a range with
`@change` and nothing writing the value mid-drag — deliberately NOT the canonical `@input` writes /
`@change` commits shape, which `PoolThrottle` and `PopulationManager` use and which cannot drift. A
first, broader version of the rule reported those three as bugs, which is how the distinction got drawn.

**Rejected: striding the volume.** 8x fewer bytes buys 1.6x, 59x fewer buys 3x. The read is not
bandwidth-bound, so there is no stride that makes a volume timecourse playable.

**Eliminated hypotheses**, two of them mine. `open_level0` on every request — guessed to be the floor,
measured at **2.5 ms**. GC pressure from four 81 MB allocations — **3%** of wall. One zarr call spanning
all channels instead of four — 480 vs 469 ms, no difference. Also worth knowing: parallelism barely
works at `blosc=1` (537 ms serial vs 469 ms across 32 threads), which is what the rejected knob explains.

## Open, with a mechanism behind it

**The cold path is upload-dominated and the reason is not yet known.** 535 ms for 351 MB is ~0.8 GB/s
marginal. Row alignment was measured and **refuted** (padding to 256 changes nothing), and the GPU
copy itself is below a ~100 ms measurement floor. The remaining candidate is the JS-heap → GPU-visible
staging copy: use a `MAP_WRITE` buffer, write the fetched slab straight into `getMappedRange()`,
unmap, `copyBufferToTexture`. Untested. Also unmeasured: `onSubmittedWorkDone`'s ~100 ms quantum makes
anything faster than that unmeasurable — batch N operations per submit to get under it, as G2 did.

## LOOK AT THIS FIRST — the vertical orientation

**Every image was rendering vertically MIRRORED, and it is fixed but unverified** (2026-08-24). Derived,
not guessed: WebGPU's NDC y points up while a framebuffer's rows count down from the top, so the
right-handed camera basis the raycast started with mapped the screen's TOP to the LAST texture row.
Image row 0 therefore appeared at the BOTTOM — and a fluorescence field of scattered cells looks exactly
as plausible either way, which is why it survived P1 and P2 unnoticed.

The fix is one line, in `SHARED_WGSL`: `up = cross(right, fwd)` instead of `cross(fwd, right)`. It is in
the shared prelude because there were THREE copies of the camera by then (raycast, points, tails) and a
sign convention with three copies drifts by definition; extracting it was part of the same change.

**One click confirms or refutes it.** `node docs/todo/spike/webgpu/shader_check.mjs` writes
`~/Downloads/TMP/shader_check.html`; the *orientation* line draws a plane lit only in its top half and
asserts the top of the screen lights. If it says MIRRORED, revert that one line. Nothing else in the
viewer depends on the sign — the overlays project through the same basis, so points and pixels agree
either way, which is also why the bug could not be seen by comparing them.

## Open questions for Dominik — none blocking

Written down rather than guessed at (2026-08-24). Work continued past all of these under the stated
assumption; each is a place where a different answer would change what gets built, not whether.

1. **ANSWERED by taking the assumption — `utils/colourRamp.ts` now exists.** Left here because the
   alternative is still open: resolving overlay ramps server-side would keep one palette owner at the
   cost of a round trip whenever the column changes.
   **Colour-by needs a continuous colormap, and there is no table to reuse.** `track_state` and
   `clusters.*` are categorical and can use the population palette. `live.cell.speed` is continuous, and
   napari renders it through **viridis**. The repo has NO viridis/turbo RGB table anywhere:
   `image_render.jl` says so explicitly and falls back to gray unless a props file carries the LUT, and
   the plots get theirs from Observable Plot's own `scheme: 'turbo'`, which is not reachable from WGSL.
   So one has to be added. *Assumption taken:* add ONE canonical stop table in TypeScript
   (`utils/colourRamp.ts`), viridis + turbo, and have the server keep resolving CHANNEL colours as it
   does now — the two are separate concerns (a channel LUT comes from napari's saved props; an overlay
   ramp is a display choice made in the viewer). The alternative is to resolve overlay ramps server-side
   too, which keeps one palette owner but means a round trip whenever the column changes.
2. **Which segmentation the overlay shows.** The viewer takes the ACTIVE labelProps and reports which,
   because its `valueName` is an image version and not a segmentation (see the namespace trap in P3).
   napari shows EVERY segmentation's populations at once, each as its own layer. *Assumption taken:*
   one segmentation at a time with a picker, because a viewer panel narrow enough for one population
   list will not hold three. Say if the all-at-once behaviour is load-bearing for how you compare
   segmentations.
3. **Track tails: napari's `tail_length` is in frames and `tail_width` in pixels.** WebGPU line-list
   draws 1px lines only, so a width control needs quads (six vertices per segment instead of two).
   *Assumption taken:* build the quad version, since a 1px tail on a noisy MIP is close to invisible and
   `tail_width` defaults to 4.
4. **Point picking (P6) will want the label under the cursor.** The overlay pass already has the exact
   projection, so picking a POINT is cheap now; picking a MASK pixel needs P4's label texture. *No
   assumption taken* — P6 is deferred anyway, this is just a note that the cheap half exists.
