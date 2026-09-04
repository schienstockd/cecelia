# ARCHIVED — Napari vs. a WebGPU web renderer — audit

> **STATUS: audit finished 2026-08-24, decision applied (napari retired).** Archived as a historical
> record of how the WebGPU browser viewer was chosen. Do not act on anything below.
> Nothing here is built; nothing is scheduled. Branch `audit/napari-webgpu`. Plan + gate ladder:
> [`NAPARI_WEBGPU_AUDIT_PLAN.md`](NAPARI_WEBGPU_AUDIT_PLAN.md). Prior decision record:
> [`CLOUD_MIGRATION_ASSESSMENT.md`](CLOUD_MIGRATION_ASSESSMENT.md).
>
> **All stages done: S1 inventory · G0 adapter · G1 napari yardstick · G2 raycast (B and C) · G3
> chunk delivery · S6 protocol split · side-by-side · recommendation.** Everything is traced to a
> file and line or to a saved measurement in `docs/todo/spike/webgpu/`; where a judgement is mine
> rather than measured, it says so. Two harnesses produced invalid numbers before producing valid
> ones, and both failures are recorded rather than quietly dropped.
>
> **Recommendation, up front: build the web renderer for display, and do the chunk geometry first.**
> The one thing that could have killed it — 3D volumetric rendering in the browser — is 6.2x faster
> than the incumbent on the same GPU and data.

## S1.0 — The option set is three, not two

The prompt frames this as Napari-remoted vs. a WebGPU browser renderer. The inventory turned up a
third option that already ships, and the audit would be dishonest to omit it:

**`api/src/image_render.jl` (201 lines) is a server-side image compositor in Julia that already
serves rendered pixels to the browser.** It reads OME-Zarr directly with Zarr.jl, does per-channel
contrast + colour + additive blend, and returns PNG bytes. Its header calls itself "**the only
server-side image renderer in the codebase**". It handles the two store layouts, uint8/uint16, and
takes its colours from napari's own exported `colormap_lut` so the composite matches the viewer.

Today it renders a **z-max-projected single timepoint** — no camera, no rotation. But it means the
contrast/colormap/LUT pipeline, the zarr reads and the browser delivery path already exist
server-side, in-process, with no Qt, no X server and no WebGPU. So the real options are:

| | Where pixels are made | What crosses the wire |
|---|---|---|
| **A. Napari, remoted** | Qt/vispy on the VM's GPU, inside an X server | encoded desktop frames (VNC) |
| **B. WebGPU browser renderer** | the user's GPU, in the browser | zarr chunks (1368/timepoint) |
| **C. Extend `image_render.jl`** | Julia on the VM, in-process | rendered PNG/frames per view change |

C is not a strawman: it is the path the **in-app 3D crop panel** already took when the napari crop
was removed (`docs/NAPARI.md` → *3D crop*, `docs/todo/CROP_PANEL_PLAN.md`) — "renders a coloured,
scrubbable MIP in the browser". The gap between C and a usable 3D viewer is a camera and a raycast,
not an architecture. **Dominik's call, 2026-08-24: benchmark all three.** G2 therefore measures A (napari's own MIP,
= G1), B (browser WGSL) and C (Julia raycast, frames streamed), and reports the new code each
needs. C would not pay G3's chunk-delivery cost at all.

## S1.1 — Feature inventory

Every point of contact, from `execute_command`'s dispatch (`napari/napari_bridge.py:2070-2300`,
31 command types), the three POST-back paths, and the routes that drive them
(`api/src/napari_api.jl`). "R/W" = read-only display vs. writes back into the pipeline.
Criticality is graded from **UI placement** (which component calls the route), not guessed.

### Image display

| Capability | Workflow | R/W | Criticality | Location |
|---|---|---|---|---|
| `open_image` — zarr → Image layers, per-channel colormap + additive blend, contrast-from-sample, scale/units, axis labels, timestamp | every viewing session | R | **core** | `napari_bridge.py:200,219,252`; `napari_utils.py:108-142`; `POST /api/napari/open` ← `ViewerPanel.vue`, `useNapariOpen.ts` |
| `set_z_view` — 2D ↔ 3D volumetric toggle, jump to z | 3D review | R | **core** | `:2100`; `docs/NAPARI.md:832`; `POST /api/napari/set-z-view` ← `ViewerPanel.vue` |
| `set_3d_level` — which multiscale level 3D renders at | 3D performance control | R | high | `:2098`; `napari.jl:226`; `docs/NAPARI.md:842` |
| `centre` — point the camera at a coordinate + timepoint | zoom-to-cell from a plot | R | high | `:2213`; `POST /api/napari/centre` ← `ImageStripView.vue`, `TrackSchemeView.vue` |
| `capture_view_state` / `apply_view_state` | animation keyframes, snapshot restore | R | high | `:2258,2265`; `docs/NAPARI.md:418`; `AnimationPanel.vue` |
| `show_layer` / `hide_layer` / `remove_layer` / `clear` | layer housekeeping | R | high | `:2205-2211,2269` |
| `save_layer_props` / `load_layer_props` / `configure_autosave` | contrast + colour survive a reopen | R | **core** | `:2215-2221`; `docs/NAPARI.md:695` |
| `ping` / `gl_info` — health, canvas size, multiscale levels, detail level | service rail, movie sizing | R | **core** | `:2073,2087`; `GET /api/napari/status` (6 call sites) |

### Overlays on top of the image

| Capability | Workflow | R/W | Criticality | Location |
|---|---|---|---|---|
| `show_labels` — segmentation masks as a Labels layer, incl. `contour` outline width, points-at-centroids, live `preview=True` of a store being written | segmentation review | R | **core** | `:2105`, `:480,490`; `docs/NAPARI.md:898,939` |
| `refresh_labels` — reload after a re-run | task-completion refresh | R | high | `:2124` |
| `colour_labels` — colour masks by an h5ad obs column, categorical vs. numeric auto-detected, returns a legend | phenotype review | R | high | `:2144`; `docs/NAPARI.md:1093` |
| `show_branch_labels` / `colour_branch_labels` | branching analysis | R | medium | `:2155,2163` |
| `show_populations` — gated populations as Points layers | gating ↔ image | R | **core** | `:2175`, `:1140`; `docs/NAPARI.md:967` |
| `show_tracks` — per-segmentation tracks as a Tracks layer, tail width/length, `color_by` | tracking review | R | **core** | `:2186`, `:1422`; `docs/NAPARI.md:1016` |
| `show_task_preview` — a task's candidate output (mask/image layers) over a region, before committing | parameter tuning | R | high | `:2133` |
| timestamp overlay + scale bar | movies, publication stills | R | high | `:275`, `:419`; `docs/NAPARI.md:957` |

### Output / capture

| Capability | Workflow | R/W | Criticality | Location |
|---|---|---|---|---|
| `record_timelapse` — t-sweep to video, fps/size/range, title card, progress + cancel | movies | R | **core** | `:2233`; `docs/NAPARI.md:144` |
| `record_keyframes` — camera animation to video | movies | R | high | `:2249` |
| `stitch_movies` — side-by-side compare | movies | R | medium | `:2256` |
| `save_screenshot` — still + atomic `view_state` + `extent_um` | figures | R | high | `:2270` |

### The only three things that flow napari → pipeline

| Capability | Workflow | What it writes | Location |
|---|---|---|---|
| **Cell selection** — draw a polygon on a Shapes layer; on close, point-in-polygon the centroids and POST the label IDs back | linked brushing: XY → flow plots | a **transient** population, cyan, never persisted (`save_pop_map!` drops transient pops) | `:1436-1568`, `:1470,1474`; `docs/NAPARI.md:1192`; `POST /api/napari/event` |
| **View-changed** — camera/dims moved | live task preview follows the view | the preview region (no data) | `:594,671` |
| **Record progress** | task rail progress bar | a progress event | `:2025,2053` |

**This is the single most important finding in the inventory.** *Nothing in napari writes pixels,
masks, tracks or persisted populations.* The one interactive path produces a transient selection
that is explicitly never written to disk. So the expensive category — "reimplement interactive
editing in a browser" — is **empty today**, and it is empty by design decisions already taken:

- **3D crop: removed from napari**, moved in-app because "its ceiling was too low (napari edits
  shapes only in 2-D…)" — `docs/NAPARI.md:886`. `crop_start`/`crop_box`/`crop_apply`/`crop_clear`
  still exist on the Julia side (`app/src/napari.jl:258-264`) with **no dispatch branch left in the
  bridge** — dead wire, worth deleting independently of this audit.
- **Track correction: authored in the app, not napari.** The engine is Julia
  (`app/src/tracking/track_correction.jl`); the surface is a Vue timeline
  (`TRACK_SCHEME_PLAN.md`, P1–P2 built). The old R version *did* drive correction from napari via a
  `k` hotkey; that was not ported.
- **Gating: never in napari.** The gating engine and Population Manager are Julia-native
  (`project_gating_popmanager`, `docs/POPULATION.md`).
- Segmentation-mask correction (`CORRECTION_PLAN.md` P2) is **open, unbuilt**. **Answered by
  Dominik, 2026-08-24:** the old R version put it in napari *because you have to see the actual
  intensity-channel signal*, but an in-browser surface modelled on the track-correction timeline is
  acceptable. So it need not re-open the writeback category — but it makes a browser-side intensity
  renderer a *prerequisite* for the feature rather than a nicety. See the plan, §5a.

### Generality tax actually observed

The prompt asks where Napari carries machinery this repo never exercises. Traced, not assumed:

- **Points layers** are display-only here (`show_populations` → `add_points`); nothing drags,
  hovers or edits a point. Napari's points layer carries all of that.
- **Shapes layers** are used *only* as a transient polygon capture device, and the code fights the
  generality: drawing forces `dims.ndisplay = 2` because "napari can only edit Shapes in 2-D", and
  mid-draw events (<3 vertices) must be filtered out.
- **Labels layers** are paintable in napari by default; there is **no writeback command**, so any
  paint a user does is silently discarded.
- **~30 colormaps** are reachable, and that generality already cost a real bug: the Julia renderer's
  name table was incomplete, `bop blue` fell through to white and washed out a composite
  (`image_render.jl:24-36`). The fix was to have napari export the LUT rather than re-derive it.
- **The bridge's protocol is versioned and adoption-based** precisely because the viewer is an
  external process that outlives the backend (`docs/NAPARI.md:124`). That whole mechanism —
  `NAPARI_PROTOCOL`, mismatch detection, killing the port listener and relaunching — exists only
  because of the process boundary, and has already produced user-visible failures
  (`unexpected keyword argument 'mask'`, a bare "Preview failed").

## S1.2 — Phase 1.5: the dask question, settled

**Dask *is* in the image path, and it is a user-visible toggle, default on.**

- `napari_bridge.py:219` — `zarr_utils.open_as_zarr(path, as_dask=as_dask)`
- `napari_bridge.py:200` — signature default `as_dask: bool = True`
- `napari_bridge.py:2095` — wire default `cmd.get("as_dask", True)`
- `api/src/napari_api.jl:323` — `Bool(get(data, :asDask, true))`
- `frontend/src/stores/settings.ts:84-85` — `localStorage.getItem('cc.napariAsDask') !== 'false'`, i.e. default true, with a toggle button in `ViewerPanel.vue:732`

So both paths ship and the default is dask. `docs/NAPARI.md:611` states what each buys:
dask gives "fast open; slices computed on demand during pan/zoom"; zarr-direct means "no dask task
graph; napari accesses chunks directly". And explicitly: "**Neither mode loads the full array into
memory.** The old numpy option (`np.asarray`) was removed."

**The prompt's reading is confirmed and it is a real point for the browser.** Dask here is not a
memory mechanism — the doc says so outright, and `zarr.Array` indexing is already chunk-lazy. Its
job is keeping the Qt UI thread unblocked while a slice is fetched. In a browser that job belongs
to the event loop and `fetch`, which are async by construction. **The justification does not
transfer, so this specific cost disappears rather than needing a replacement.**

Two caveats, so this is not oversold:
- Labels always load with `as_dask=True` regardless of the toggle (`:480,581`), and napari's
  `cachey` interaction with dask task keys is already a known trap (`:534`).
- "Not a memory mechanism" is a claim about *dask*, not about the browser. B still has to hold
  351 MB per timepoint of decoded uint16 somewhere (see the plan, §2) — the browser gets no free
  laziness once the volume is on the GPU.

## S1.3 — What Napari buys today that a from-scratch renderer starts without

Stated plainly, as cost, not as a footnote:

- **A tested contrast pipeline.** Per-channel contrast-from-a-middle-sample, additive blending,
  ~30 colormaps, and the `colormap_lut` export that the Julia renderer depends on
  (`docs/NAPARI.md:567,707`).
- **3D camera + interaction.** Rotate, pan, zoom, pick, per-axis sliders with named axes, a scale
  bar in physical units, 6:1 z-anisotropy handled. All of it would be new code in B.
- **Multiscale level selection** already wired to a UI control (`set_3d_level`), including the
  "3D detail is a setting, not napari's default" behaviour (`docs/NAPARI.md:842`).
- **A Tracks layer with tails.** Non-trivial geometry; there is no browser equivalent to reach for.
- **Someone else's bug fixes.** Offset against this: the bridge's own comments record several bugs
  that exist *because* of the boundary (`show_labels` dropping `contour` so every recording came out
  filled; `show_task_preview`'s signature mismatch killing every preview; the headless
  screenshot-ordering trap in `CLOUD_MIGRATION_ASSESSMENT.md` §2.5). Maintenance is not free on
  either side of the ledger.
- **h5ad reads live server-side in the bridge** and only identifiers cross the wire
  (`CLOUD_MIGRATION_ASSESSMENT.md` §3a). B must re-implement `LabelPropsView`-equivalent reads or
  add routes to serve centroids/tracks/columns as JSON. **A and C keep this for free; B pays.**

## G0 — can the browser reach the GPU? **PASS**, with one trap

Attempted headless, so as not to put a window on Dominik's desktop. **Headless Firefox cannot be
used for this at all on this machine** — and the failure is informative rather than merely annoying:

| Attempt | Result |
|---|---|
| `firefox --headless`, stock prefs, over `http://127.0.0.1` | `navigator.gpu` **ABSENT** |
| same + `dom.webgpu.enabled=true`, `gfx.webgpu.force-enabled=true` | `navigator.gpu` **PRESENT**, but the content process dies before `requestAdapter()` resolves: `[GFX1-]: RenderCompositorSWGL failed mapping default framebuffer, no dt`, then repeated `Exiting due to channel error` |

`RenderCompositorSWGL` is the *software* compositor — headless has no real one, so WebGPU has no
surface to attach to and the process falls over. This is the same shape as the finding in
`CLOUD_MIGRATION_ASSESSMENT.md` §2 (`offscreen`/`eglfs` give no GL context), now on the browser
side: **a display server is a hard requirement for the browser too.** It is not evidence about
WebGPU performance.

Two facts that do carry, and are good news:

- **`nvidia_icd.json` is installed** (`/usr/share/vulkan/icd.d/`) with `libvulkan.so.1.3.275`.
  Firefox's WebGPU goes through wgpu → **Vulkan**, not GLX — so it can reach the RTX 2000 Ada
  directly, *without* depending on the GLX PRIME switch that napari needs. Display `:1` presenting
  as `llvmpipe` for GL therefore does not by itself doom the browser path.
- Firefox here is a **snap**. Two confounds it introduced, recorded so they are not re-hit: it
  cannot read `/tmp` at all, and its `home` interface denies top-level dot-directories — a profile
  under `~/.cache` fails with the misleading *"Firefox is already running, but is not responding"*.

### Result from a real window — Firefox 153, Dominik's machine, 2026-08-24

Raw: `docs/todo/spike/webgpu/g0_result.json`. Page:
`docs/todo/spike/webgpu/adapter_probe_standalone.html`.

**WebGPU is on by default** — `navigator.gpu` present with no `about:config` change, secure context,
`requestDevice()` succeeds on every adapter, and an `r16uint` 3D texture creates. So the earlier
ABSENT/crash was purely headless. G0 clears.

**The trap: `powerPreference` picks the GPU, and the default is the wrong one.** Firefox masks
adapter identity (every `info` field is `""` — fingerprinting defence), so the device has to be
identified from its limits. Two clearly different adapters answer:

| | `high-performance` | `low-power` **and `default`** |
|---|---|---|
| `maxTextureDimension3D` | **16384** | 2048 |
| `maxTextureDimension2D` | 32767 | 16384 |
| `maxBindingsPerBindGroup` | 4294967295 | 1024 |
| compression features | BC only | BC **+ ASTC + ETC2** |

BC-only with huge limits is the discrete NVIDIA; BC+ASTC+ETC2 is the Intel iGPU. **`{}` — no
`powerPreference` — returns the iGPU.** This is the exact browser-side counterpart of the PRIME trap
the cloud assessment recorded for napari (`app/src/napari.jl:55-59`, "the cloud image must set it or
it will silently software-render"): a renderer that omits
`requestAdapter({powerPreference: 'high-performance'})` silently runs on integrated graphics. It must
be set, and — because `info` is blank — verified from the limits, not from a device name.

Three things that make the rest of the audit easier, and one design constraint:

- **`timestamp-query` is supported.** This directly fixes the flaw that invalidated the prior
  assessment's numbers ("the readback is racing the draw, so the timer measures Qt plumbing").
  G2 can time the GPU itself rather than wall-clock a readback.
- **`maxBufferSize` and `maxStorageBufferBindingSize` are 1 GiB.** A whole 4-channel timepoint is
  351 MB (plan §2), so it fits in a single buffer with room to spare — no chunked-upload scheme needed.
- **`maxTextureDimension3D` 16384 vs. a 1104x1046x38 volume.** Not a constraint on either adapter.
- **Constraint: `r16uint` is not filterable.** No format ending in `uint` can be sampled with
  linear interpolation, so a raycast wanting smooth sampling must convert to `r16float` on upload
  (filterable in core WebGPU, and `shader-f16` is available). That conversion is a real cost to
  measure in G3, not a footnote — it is a pass over 87.7 MB per channel per timepoint.

Harness note: `docs/todo/spike/webgpu/probe_runner.py` (+ `adapter_probe.html`) is the headless
attempt; it is kept only because its local-HTTP-server half is what G3 needs to serve real zarr
chunks. **Browser stages must run in a real window** — the pattern that works is a self-contained
page in `~/Downloads/TMP` that runs the whole sweep and saves JSON on one click.

## G1 — the Napari yardstick: **MEASURED**, and it is the first valid number for option A

Harness: `docs/todo/spike/webgpu/napari_yardstick.py`. Raw: `g1_nvidia.json`, `g1_llvmpipe.json`.
Real data (`VJy1Nx/ccidSmoothed`, one timepoint, 4 channels, 351 MB resident), 3D MIP, real
z-anisotropy (2.0 / 0.3315 / 0.3315 µm), canvas **1566x1003** — the window manager clamped the
requested 1920x1080, and the numbers are for the actual canvas.

**Three headless designs failed first, all in the way the prior assessment described.** Recorded so
nobody repeats them:

1. `canvas.update()` + `processEvents()` + `glFinish()`: the 3D MIP came out at **0.53 ms** and a 2D
   pan at **0.47 ms** — a 13% gap where there should be orders of magnitude. With `show=False`
   nothing paints, so `glFinish()` returns instantly.
2. Differential `screenshot()`: per-frame pixel std `[0.0, 0.0, 59.54, 0.0, 75.29, 0.0]` — frames
   alternating blank — and the *hidden-layer* baseline returned the **identical** sequence, which is
   only possible if the readback is serving a frame that does not match the scene. Verbatim the
   failure `CLOUD_MIGRATION_ASSESSMENT.md` §2 diagnosed.
3. `GL_TIME_ELAPSED` timer queries (PyOpenGL) **segfault** — napari's offscreen FBO context is not
   current when the query is issued. Separately: `napari_bridge._gl_info()` segfaults the process if
   called *before* napari's canvas exists (reproduced 3x); it has to run last.

What makes it measurable: a **shown** QWidget can be painted *synchronously* with `repaint()`. Each
frame is `set camera → native.repaint() → glFinish()`, with no framebuffer readback in the timed
region. Dominik approved the visible window, 2026-08-24.

### The numbers

| | total ms/frame | fps | net of empty canvas |
|---|---|---|---|
| 3D MIP rotate, **4 channels** | **36.0** | **27.8** | 9.6 ms |
| 3D MIP rotate, 1 channel | 29.4 | 34.0 | 3.0 ms |
| 2D pan/zoom | 27.5 | 36.4 | 1.0 ms |
| **empty canvas, all layers hidden** | **26.4** | 37.9 | — |
| t-scrub, 4 channels, incl. IO | **1241** | 0.8 | — |

**Validity.** All three gates pass. Frames non-blank (stds 41.6–50.7) and varying; net cost above
baseline positive; and the control run with the PRIME env removed lands on
`llvmpipe (LLVM 20.1.2)` at **272.9 ms/frame vs. 36.0** — **7.6× slower** on the identical workload,
28× on the net. Hardware and software landing within 5% is exactly what voided the previous numbers;
a 7.6× separation is the evidence that this harness measures rendering.

Two caveats, stated rather than buried:

- In the llvmpipe control the *later* sweeps (1-channel, 2D, hidden baseline) returned 0.01 ms —
  `repaint()` becomes a no-op once nothing is marked dirty. Those control sub-measurements are void.
  The 3D rotate figure, which is what the gate needs, is valid (non-blank, varying). On the NVIDIA
  run every sweep is coherent with the 26.4 ms baseline, so all rows stand, but the 1-channel and
  2D rows are lower-confidence than the 4-channel row.
- The t-scrub figure (1241 ms) independently cross-checks the per-timepoint measurement below
  (1127 ms, entirely separate harness). Two harnesses agreeing within 10% is the strongest evidence
  in this audit so far.

### What it means for the comparison

1. **Napari's actual volume-render cost is 9.6 ms for a 4-channel, 44 Mvoxel MIP.** That is the
   number a WebGPU raycast has to beat, and in absolute terms it is a low bar — the RTX 2000 Ada is
   barely working.
2. **The empty canvas costs 26.4 ms — 2.7× the volume render.** Napari's own Qt/compositor overhead,
   not the science, sets the frame rate: **37.9 fps with nothing drawn at all.** A purpose-built
   renderer with no widget tree starts with 26 ms of headroom it never has to spend. This is the
   inventory's "generality tax" showing up as a measured number instead of an argument.
3. **Scrubbing is ~1.2 s/timepoint and rendering is 3% of it.** Whoever wins on rendering, the
   timepoint step is an IO/decode problem for all three options.

## The per-timepoint cost — measured, and it corrects the number everything was resting on

Harness: `docs/todo/spike/webgpu/timepoint_cost.py`. Raw: `g3_timepoint_cost.json`. Real store
(`VJy1Nx/ccidSmoothed`), one timepoint, 4 channels, single-threaded Python.

`CLOUD_MIGRATION_ASSESSMENT.md` §3b measured local NVMe at 0.008 ms/chunk and concluded
"1 timepoint (4 ch, 1620 chunks) = **0.013 s**". Reading one real timepoint through zarr takes
**~1.1 s** — about 90× that. **Both are correct:** 0.008 ms/chunk is the cost of getting compressed
*bytes* off the filesystem, and it says nothing about decompressing or assembling them. So the
stages were separated rather than attributed:

| Stage | Median | Per chunk | Note |
|---|---|---|---|
| 1. raw chunk bytes off disk | 71–226 ms | 64–202 µs | Strongly page-cache dependent; the spread is two runs of the same code |
| 2. blosc-zstd decode of those bytes | ~200 ms | ~180 µs | **~3× the cost of reading them** |
| 3. **the real path** — `arr[t, :4]` through zarr | **1127 ms** | — | What napari's t-slider actually pays |
| 4. `uint16` → `float16` | **973 ms** | — | numpy has no SIMD path for f16. A cost only option B would pay |
| 5. naive loop: read + decode + place | **651 ms** | — | **1.7× faster than zarr's own fancy indexing** |

What falls out, in order of how much it matters:

1. **The bar for time scrubbing is ~1.2 s, not 13 ms.** The plan's G3 gate (≤500 ms) was set against
   the wrong baseline and is *stricter than the incumbent*. That single correction materially
   improves option B's case, and it is the opposite of what the prior assessment's numbers implied.
2. **Only ~6–20% of it is disk IO.** Decode is 3× the read, and zarr's per-chunk indexing adds
   ~475 ms on top of a naive loop doing the same work. **This is a speedup available to cecelia
   today, independent of the audit** — nothing in the current path decodes the 1116 chunks in
   parallel.
3. **The store is sparse: 1116 chunk files exist, not 1368.** 252 all-zero chunks were never written
   and read back as `fill_value`. Per-timepoint request counts in the plan and in assessment §3b are
   an upper bound, ~18% high.
4. **`uint16` → `float16` costs more than the whole read+decode.** G0 established `r16uint` is not
   filterable, so a smooth-sampling raycast needs `r16float`. Converting on the CPU would roughly
   double option B's per-timepoint cost, so it must happen on the GPU (upload `r16uint`, convert in
   a compute pass) or be avoided with manual trilinear filtering in the shader. **A design
   constraint found before any renderer code was written**, which is what the audit is for.
5. Compressed bytes per timepoint: **38.2 MB** for 351 MB uncompressed (9.2×). That is option B's
   per-timepoint HTTP payload — and on a LAN it is not the bottleneck; the decode is.

## G2, option C — the existing Julia server-side renderer: **MEASURED**

Harness: `docs/todo/spike/webgpu/julia_render_bench.jl`. Raw: `g2_julia_render.json`. Same image
(`VJy1Nx/ccidSmoothed`), timepoint 0, 4 channels, via `api/src/image_render.jl` as it ships.

A knob to know about, because getting it backwards produced a nonsense table first time: **`z_keep`
is not "number of planes to keep."** The stride is `cld(hi - lo + 1, z_keep)`, so `z_keep=1` reads
**one** plane and `z_keep >= nz` reads all of them. The tell was "full z" coming out 2x *faster*
than "subsampled z".

### Cold: re-reading the volume every frame (what the route does today)

| Config | ms |
|---|---|
| 1 z-plane, 512 px | 32 |
| 10 z-planes, 512 px — **the crop panel's shipped default** | 382 |
| all 38 planes, 512 px | 949 |
| all 38 planes, napari's 1566 px canvas | 1167 |
| all 38 planes, full res (1104x1046) | 1158 |

Note how flat the last three are: going from a 512 px output to full resolution costs +210 ms while
reading all 38 planes instead of 10 costs +567 ms. **The read dominates; the output size barely
matters.**

### Warm: volume resident, which is what a viewer would actually do

| Stage | ms |
|---|---|
| read one timepoint, 4 ch, via Zarr.jl | **533** |
| MIP over z | 32.3 |
| composite to RGB, full res 1104x1046 | 35.6 |
| PNG encode | 49.5 |
| **warm frame = MIP + composite + encode** | **~117 → 8.5 fps** |

### What this settles about option C

1. **C can serve a scrubbable MIP at ~8.5 fps, and cannot serve interactive 3D rotation.** The warm
   frame is 117 ms against napari's 36 ms — 3.2x slower while doing *strictly less* (a fixed z-MIP,
   no camera). Adding a real raycast replaces the 32 ms MIP with a ray march: at napari's canvas and
   256 steps that is 1566 x 1003 x 256 x 4 ≈ **1.6 G samples per frame on the CPU**, which is
   seconds, not milliseconds. There is no version of this that rotates interactively in Julia on the
   CPU.
2. **Half the warm frame is PNG encoding (49.5 ms).** For a streaming path that is the wrong codec —
   but it is also the easiest thing on this whole list to fix, and it is what the existing route
   chose because it serves one still at a time.
3. **A genuinely useful side finding: Zarr.jl reads a timepoint in 533 ms vs. Python zarr's 1127 ms**
   — **2.1x faster on identical data.** Two independent implementations of the same read, measured
   the same day. That is a real lever for cecelia today: the slow path is the one napari uses.
4. So C is **not** a third contender for the 3D question. It is a strong contender for the *2D/MIP
   review* half — which is most of what the inventory says the viewer is used for — and it already
   ships. That is worth saying plainly rather than scoring C as a loser.

## S6 / Phase 2 — the pixel/control split: **already clean, because there is no pixel plane**

The plan's Phase 2 asked whether the Julia WS protocol tangles "pixel addressing" with "state", and
whether untangling them is scoped work. The answer is neither: **no socket in this system carries
image bytes today.**

- **Julia → napari (WS :7655)** sends a *filesystem path* and lets napari read the disk itself:
  `open_image` takes `"path" => path` (`app/src/napari.jl:149`), `set_task_dir` takes
  `img._dir` (`:158`), `show_labels` takes a `value_name`. Every one of the 31 commands is
  identifiers, paths and display parameters. Same for the reverse direction: the three POST-backs
  carry label IDs, a region and a progress fraction.
- **Julia → browser (WS + REST)** carries JSON only, with two byte-body exceptions, and neither is a
  volume: `/api/crop/frame` (one composited PNG, `crop_api.jl:33`) and the gating
  plotdata/density/membership binary responses (`server.jl:610`).

So the protocol is **already pure control plane** — not because it was split, but because pixels were
never in it. Consequences for the audit, and they cut both ways:

- **For option B this is good news and it is also the whole of the work.** Nothing has to be
  disentangled. But there is no pixel-addressing message type to *extend* either: chunk/LOD
  addressing would be entirely new surface. Scope it as new, not as a refactor.
- **For options A and C this is the reason they are cheap.** The bridge already reads both the zarr
  and the h5ad server-side and only ships identifiers (`CLOUD_MIGRATION_ASSESSMENT.md` §3a). Moving
  pixels to the client is the one direction that *adds* a data plane where none exists.
- One thing that *is* already right for B: because the browser never sees a path today, adding a
  chunk route does not conflict with anything. There is no legacy addressing scheme to be compatible
  with.

## G2, option B — the WebGPU raycast: **MEASURED, and it beats the incumbent**

Harness: `docs/todo/spike/webgpu/raycast_bench.html`. Raw: `g2_webgpu.json`. Void first attempt kept
at `g2_webgpu_VOID_v1.json`. Real volume dimensions, dtype, anisotropy and channel count
(`VJy1Nx/ccidSmoothed`: 38 x 1046 x 1104 uint16 per channel, 4 channels, 2.0/0.3315/0.3315 µm);
procedural intensities, because a MIP traverses every step of every ray regardless of the values.
Timed with `timestamp-query` on the discrete adapter.

### It took two attempts, and the first was invalid

v1 reported 4 channels *cheaper* than 1 (0.73x), 512 steps cheaper than 256, and 1920x1080 faster
than 1566x1003. The tell was `wall_ms` = **exactly 100.0 for all twelve configs**: each frame was one
submit followed by a `mapAsync` round trip, so the GPU was idle ~90% of the time, and because nothing
ever read the render target the driver was free to discard the passes entirely. Implied throughput hit
152 Gsample/s.

v3 fixes the cause rather than the symptom: **20 frames in one submit** with timestamps around the
whole batch (submit overhead amortised), **a pixel readback after the last pass** so the work is
provably consumed, and **dynamic uniform offsets** so all 20 passes use different cameras — necessary
because `queue.writeBuffer` is ordered before the entire submit, so writing between passes would give
every pass the same camera and invite the same caching artefact. Plus explicit monotonicity gates.

A gate bug of my own is worth recording: the readback initially sampled the frame's **top-left
corner**, which is background by construction, so all 16 correct renders were flagged blank. Reading
the centre fixed it. And the viewport gate was demoted to advisory: 1920x1080 is only 1.32x the
pixels of 1566x1003, and the one violation was 5% — noise, not a broken harness.

### The numbers

GPU ms per frame, median of 5 batches of 20. Readback max = 255 on every config.

| Viewport | ch | 128 steps | 256 | 512 | 1024 |
|---|---|---|---|---|---|
| 1566x1003 | 1 | 1.41 | 4.15 | 4.88 | 9.60 |
| 1566x1003 | **4** | 3.18 | **5.84** | 10.93 | 20.68 |
| 1920x1080 | 1 | 1.51 | 3.98 | 6.05 | 11.54 |
| 1920x1080 | **4** | 3.90 | **6.97** | 13.04 | 24.86 |

**Validity.** Zero monotonicity violations, zero blank configs. The 4-channel rows scale at
**1.83x / 1.87x / 1.89x** per step doubling — near-perfect linearity in the workload, which is the
signature of a real measurement. The 1-channel rows are noisier (the 256-step point sits above its
trend) because the absolute times are 1.4–9.6 ms; the 4-channel rows are the ones to quote.

Channels scale **~2.15x for 1→4, not 4x**, consistently. That is physically sensible rather than
suspicious: the four channels are four `textureLoad`s at z-offsets 38 apart in the *same* texture, so
they share all the per-step ray arithmetic (position, bounds check, march) and only the loads
multiply. Sub-linear scaling in channels is the expected shape.

### Head-to-head at napari's own canvas, 4 channels

| | ms/frame | fps |
|---|---|---|
| napari MIP, whole frame (G1) | 36.0 | 27.8 |
| napari MIP, net of its Qt overhead (G1) | 9.58 | 104 |
| **WebGPU raycast, 256 steps** | **5.84** | **171** |
| WebGPU raycast, 512 steps | 10.93 | 91.5 |
| WebGPU raycast, 1024 steps | 20.68 | 48.4 |

**Option B clears the gate by a wide margin.** At 256 steps it is 1.6x faster than napari's actual
render work and 6.2x faster than napari's whole frame. Even at **1024 steps** — sampling more finely
along the ray than the voxel grid provides on the long axis, i.e. past the point of diminishing
visual return — it is still under napari's 36.0 ms total frame. The plan's gate (4-channel composite
≥15 fps, single channel ≥30 fps) is met at every step count tested, at both viewports.

Note which comparison matters: napari's 26.4 ms of Qt/compositor overhead is **not** part of the
browser path, so the honest user-facing figure is 5.84 ms vs 36.0 ms. The 9.58 ms net figure is the
fairer *renderer-to-renderer* comparison, and B wins that one too.

### Three caveats, none of which changes the verdict

1. ~~**The phantom is smooth, so texture-cache locality is optimistic.**~~ **CLOSED, 2026-08-24**
   — see *Real data closes G2's last caveat* below. Real voxels cost the same as the phantom.
2. **Rays that miss the slab do zero work.** The volume is a 4.8:1 plate (366 x 347 x 76 µm) and
   fills roughly 70% of the frame, so ~30% of pixels are free. That is true of any renderer drawing
   this data, napari included, but it means `Gsample/s` figures in the raw JSON are upper bounds.
3. **This measures rendering only.** Getting a timepoint to the GPU is G3, and the earlier
   measurement already puts that at ~1.2 s server-side today — three orders of magnitude above the
   frame cost. The renderer was never going to be the problem; the audit now has the number to prove
   it rather than assert it.

## Real data closes G2's last caveat

Harness: `docs/todo/spike/webgpu/real_volume.html` + `chunk_server.py`. Raw: `g2_real_volume.json`.
The **real** `VJy1Nx/ccidSmoothed` timepoint 9 — fetched, contrast-windowed and uploaded — rendered
against a phantom at **identical dimensions**, so the only variable is the data.

| steps | real ms | phantom ms | ratio |
|---|---|---|---|
| 64 | 2.24 | 2.35 | 0.95 |
| 128 | 3.51 | 3.37 | 1.04 |
| **256** | **5.34** | 5.43 | 0.98 |
| 512 | 9.80 | 9.85 | 0.99 |

**Real data is not slower.** The ratio sits between 0.95 and 1.04 at every step count, so the
cache-locality worry was unfounded and G2's 5.84 ms stands as a real-data figure.

It is a hostile test rather than a flattering one: this data is **almost entirely zeros**. The
per-channel 99.9th percentiles are `[12, 46, 138, 54]` out of 65535. The shader's contrast window
does all the visible work — without percentile windowing (the convention `image_render.jl`'s
`percentile_spec` already uses) the frame renders black. Anything rendering these stores has to
compute it; napari does the equivalent in `set_contrast_from_sample`.

Time to first volume, same run: fetch **640 ms** (328 ms of it the Python server's read), contrast
**36 ms**, GPU upload **535 ms** — ~1.2 s total, consistent with G3's 1123 ms from a separate harness.

Two on-disk facts found while wiring this up, both worth knowing beyond this audit:

- **Both chunk-key layouts are live in the same project.** `VJy1Nx` is flat (`.`), `fXgbTl` is nested
  (`/`). Hardcoding flat made every `fXgbTl` chunk look absent, so the slab came back all zeros —
  which would have rendered **black with no error**. Read `dimension_separator`, never assume it.
  Moot for new data: `config.jl:897-901` offers only `nested` and `v3` (also nested), so flat is
  legacy that `zarr_utils.py:146` keeps readable (Dominik, 2026-08-24).
- **`fXgbTl`'s whole-plane chunks are a consequence of its size, not a format choice.** The writer
  asked for 512x512 tiles and the image is 420x441, so one tile covers the plane. Large images still
  land on 3x3 tiles per plane, so per-plane chunking for big stores is a `chunkSizeY`/`chunkSizeX`
  change in the writers, independent of the separator.

## G3 — chunk delivery: **one design dies, the other reaches parity with the incumbent**

Harness: `docs/todo/spike/webgpu/chunk_server.py` + `chunk_bench.html`. Raw: `g3_chunk_bench.json`.
Real store, one 4-channel timepoint, over loopback. Yardstick: napari's measured **1241 ms** per
timepoint step (G1).

Two delivery designs, both through the same server:

- **A — the store as it is.** One HTTP request per blosc chunk: **1116 requests** of ~14 KB.
- **B — server-assembled slabs.** Four requests, one per channel; the server reads and assembles the
  (z,y,x) volume and `Content-Encoding: zstd` carries it, so **the browser decodes nothing itself** —
  the network stack does it natively, off the main thread.

### Design A — 1116 requests

| concurrency | total | per chunk |
|---|---|---|
| 1 | 39,026 ms | 28.5 ms |
| 8 | 5,683 ms | 4.15 ms |
| 32 | 5,322 ms | 3.89 ms |
| 64 | 5,313 ms | 3.88 ms |
| **128** | **5,270 ms** | 3.85 ms |

**Design A is dead.** 5.3 s against a 1.24 s incumbent — **4.2x worse** — and it *plateaus from
concurrency 8 onwards*, which is the signature of a server-CPU bound rather than a bandwidth or
latency bound. The harness's server is Python's `ThreadingHTTPServer`, so the absolute figure is
pessimistic and a real static host would do better; but the plateau says the shape is wrong, not just
the implementation. This is `CLOUD_MIGRATION_ASSESSMENT.md` §3b's conclusion 4 — "the 20 KB chunk
geometry is pathological for any network backend" — reproduced over HTTP.

### Design B — four slabs

| | total | server read | server zstd | wire bytes |
|---|---|---|---|---|
| cold (t=1) | 1948 ms | 1665 ms | 195 ms | ~49 MB |
| **warm (t=2)** | **737 ms** | 330 ms | 208 ms | ~49 MB |
| GPU upload, 351 MB → `r16uint` 3D texture | **386 ms** | — | — | 909 MB/s |
| **B total, warm: fetch + upload** | **1123 ms** | | | |

**B reaches parity, slightly ahead: 1123 ms vs the incumbent's 1241 ms.** Note the plan's ≤500 ms
gate is *not* met — but that gate was set against the prior assessment's 13 ms byte-fetch figure and
was wrong by ~90x. Against the real incumbent, B wins.

A correction to my own instrumentation: the page reports `arrayBuffer().byteLength`, which is the
**decoded** size (351 MB), not what crossed the wire. Wire bytes were measured server-side:
**12.3 MB per channel, ~49 MB per timepoint** (7.1x from zstd level 1).

### Where B's remaining second goes — and it is mostly avoidable

1. **GPU upload, 386 ms (34%).** 909 MB/s for 351 MB. Already minimal precision (`r16uint`, 2 bytes),
   so the lever is uploading fewer channels or a coarser level, not a smaller dtype.
2. **Server zstd re-compression, 208 ms (19%) — pure overhead my harness invented.** The store is
   *already* blosc-zstd compressed. Serving the blosc bytes at slab granularity and decoding in the
   browser would skip both the server-side decode *and* the re-compress. Untested, and it is the
   obvious next optimisation.
3. **Server read/assemble, 330 ms (29%).** In the real app the server is Julia, and Julia reads a
   timepoint **2.1x faster than Python** (533 ms vs 1127 ms, measured above). This harness paid the
   Python price.
4. So B's realistic floor is well under a second, and the single highest-value change is not in the
   renderer at all: **store bigger chunks.** Per-plane or per-slab chunking turns 1116 requests into
   tens, and cecelia already owns that decision surface (`store_layout` / `store_compressor`,
   `app/src/config.jl`). The prior assessment flagged the same lever for archive/transfer; this
   measurement extends it to the viewer.

## G4 — the timecourse slider, and where the cold path actually goes

Harnesses: `docs/todo/spike/webgpu/timecourse.html`, `upload_bench.html`. Raw:
`g4_timecourse.json`, `g4_upload_bench.json`.

### The slider works, and caching is the whole trick

`fXgbTl` (31 t x 4 c, 47.4 MB/timepoint) with a 3 GB VRAM budget holds **all 31 timepoints**:

| | |
|---|---|
| whole movie into VRAM | **5.5 s** for 1.47 GB |
| scrubbing afterwards | **sub-millisecond per frame** — 186 cache hits, 1 miss |
| cold load, median | fetch **86 ms** · upload **148 ms** (server read 27 ms) |

Once resident, the slider is limited by `requestAnimationFrame`, not by us. So the design that makes
a browser timecourse usable is not clever rendering — it is an **LRU texture cache under a byte
budget, with directional prefetch and cancellation**, which the prototype implements. At 4 channels
`VJy1Nx` is 351 MB/timepoint, so a 3 GB budget holds ~8 of 181 and the misses are felt; at 1 channel
four times as many fit. **Channel count is the practical VRAM lever.**

Two design points the prototype had to get right, both of which the real viewer will need:

- **Contrast is computed once and held.** Recomputing percentiles per timepoint makes playback
  flicker as the window tracks each frame's own distribution. napari fixes it per layer for the same
  reason (`set_contrast_from_sample`).
- **Coalesce at two levels.** Paint through `requestAnimationFrame` (a dragged slider fires per
  pixel), and carry a sequence token so fetches for timepoints already scrolled past are
  `AbortController`-cancelled rather than queued. This is the same rule `frontend/CLAUDE.md` states
  for continuous controls, and it is load-bearing here rather than cosmetic.

### Upload: a hypothesis raised and REFUTED

The cold path is upload-dominated (148 ms vs 86 ms on `fXgbTl`; 535 ms vs 640 ms on `VJy1Nx`), and
cost tracked **rows** rather than bytes across the two images — time 3.61x where rows were 2.96x and
bytes 7.41x. That suggested `queue.writeTexture` was repacking every row because `bytesPerRow` is not
a multiple of 256 (1104x2 = 2208; 441x2 = 882).

**Wrong.** Padding rows to a 256-byte pitch changes nothing:

| config | 256-aligned | writeTexture tight | writeTexture padded | buffer + copy | copy only |
|---|---|---|---|---|---|
| 441x1046x38 (35 MB) | no | 99 ms | 100 ms | 100 ms | 100 ms |
| 1104x1046x38 (88 MB) | no | 209 ms | 212 ms | 219 ms | 100 ms |
| 1152x1046x38 (92 MB) | **yes** | 214 ms | 214 ms | 213 ms | 100 ms |
| 2048x1046x38 (163 MB) | **yes** | 301 ms | 299 ms | 301 ms | 100 ms |
| 1152x1046x36 (87 MB) | **yes** | 208 ms | 208 ms | 206 ms | 100 ms |

Aligned 1152 costs the same as unaligned 1104. The hypothesis is dead.

**What the table does show is a measurement floor.** "copy only" is **exactly 100.0 ms for every
size** — 35 MB through 163 MB. That is not a copy time; it is `onSubmittedWorkDone()` resolving on a
~100 ms boundary, the same artefact that invalidated the first G2 attempt (`wall_ms` pinned at 100.0).
So the GPU-side copy is faster than this timer can resolve, and all four methods carry that floor.
Subtracting it, the marginal rate between configs is **~0.8 GB/s** (+75 MB costs +92 ms).

**So the cost is neither row repacking nor the GPU copy — it is moving bytes from the JS heap into
GPU-visible memory.** `buffer_copy` used `queue.writeBuffer`, which still performs that staging copy,
which is exactly why it matched `writeTexture` to within noise. The untested lever is a `MAP_WRITE`
buffer: `mapAsync`, write the fetched slab directly into `getMappedRange()`, unmap, then
`copyBufferToTexture` — so a network payload lands in GPU-visible memory **once** instead of twice.
Not implemented, not measured; recorded because it is the one remaining idea with a mechanism behind
it, and because two upload paths were already mislabelled here once.

Whatever the outcome, this is an optimisation, not a gate: the slider is already usable inside a
cached window, and unusable outside one for reasons that caching solves.

## The side-by-side, per inventory item

Verdicts: **=** equal · **+** web renderer better · **−** web renderer worse. "Web renderer" here
means option B, since C cannot rotate and A is the incumbent.

| Inventory item | B vs. remoted napari | Why |
|---|---|---|
| 3D volumetric render (the make-or-break) | **+** | 5.84 ms vs 36.0 ms whole-frame; no Qt overhead, no VNC encode |
| 2D pan/zoom | **+** | Same reason, smaller margin |
| Time scrubbing | **=** | 1123 ms vs 1241 ms today; both dominated by read+decode, not rendering |
| Contrast / colormap / LUT | **=** | In-shader, and `image_render.jl` already proves the LUT convention transfers |
| Multiscale level selection | **=** | Neither has a pyramid today; equal work for both |
| Labels / segmentation masks | **=** | An extra `r8uint`/`r32uint` texture and a palette lookup |
| Points (populations) | **+** | The inventory's generality tax: display-only here, so no drag/hover/history machinery |
| Tracks with tails | **−** | Non-trivial geometry napari gives free; no browser equivalent to reach for |
| h5ad-derived overlays (pops, tracks, colour-by) | **−** | A and C read h5ad server-side and ship identifiers; B must add routes or reimplement `LabelPropsView` |
| Movie recording / screenshots | **−** | napari's `record_timelapse`, keyframes, stitching and title cards are ~600 lines of working code |
| 3D camera controls | **−** | Rotate/pan/zoom/reset with sane inertia is more work than it looks |
| Cell-selection polygon → transient pop | **=** | The only writeback path; trivial in a browser, and napari fights its own Shapes layer for it (forces `ndisplay = 2`) |
| Mask correction with intensity context (unbuilt) | **−** | A already shows intensity next to a paintable Labels layer. See plan §5a |
| Process/protocol maintenance | **+** | Deletes the bridge, the protocol version, the adoption dance and the relaunch-on-mismatch machinery |
| X server + GPU in the image | **+** | B needs neither; A needs both (assessment §2) |

**Where B is strictly worse, stated plainly:** tracks-with-tails, h5ad-derived overlays, movie
recording, 3D camera controls, and mask-correction-with-intensity. None is a rendering problem; all
are "napari already wrote this" problems — and under the full-replacement goal every one of them is
**required work**, not a reason to keep napari. The recommendation below assigns each to B or C.

## The make-or-break, ranked

1. **Chunk geometry, not rendering.** 1116 requests per timepoint is the single number that decides
   whether B is pleasant or painful, and it is fixable in the writer rather than the viewer. If
   per-slab chunking does not happen, B's delivery stays at best at parity.
2. **Re-implementing the h5ad-derived overlay reads.** The pops/tracks/colour-by path is what makes
   the viewer *useful* rather than pretty, and it is the one thing A and C get for free.
3. **Not** volumetric rendering. That was the audit's stated one-real-risk, and it came in **6.2x
   faster than the incumbent**. It is closed.

## Recommendation

> **SCOPE CORRECTION (Dominik, 2026-08-24).** An earlier draft of this section recommended keeping
> napari for the subset it is better at (movies, keyframes, mask correction). **That is not the
> goal.** The aim is to *replace* napari and ship a sole browser app — the cloud mandate does not
> tolerate a Qt process, so "napari does this well" is not an argument for keeping it, it is a
> competing implementation to be beaten. Everything in the strictly-worse list below is therefore
> **required work, not optional scope.** The measurements are unaffected; what changes is the shape
> of the answer.

**Build it, and the replacement is B + C — not B + napari.**

The one thing that could have failed did not: a WebGPU raycast is 6.2x faster than napari's whole
frame on the same GPU and data. Under a full-replacement goal the interesting question is no longer
"which option wins" — **option A is excluded by definition, because it keeps napari** — but how the
whole inventory gets covered. It splits cleanly, and the split is the useful output of this audit:

| | Renderer | Why it lands here |
|---|---|---|
| **Interactive** — image display, 2D/3D, contrast, camera, labels, points, tracks, scrubbing, selection, mask correction | **B**, WebGPU in the browser | Needs a frame in ~16 ms. B does it in 5.84 ms |
| **Offline / batch** — `record_timelapse`, `record_keyframes`, `stitch_movies`, `save_screenshot`, title cards | **C**, extended `api/src/image_render.jl` | Needs no interactivity at all. C's 117 ms/frame is irrelevant offline: a 181-frame movie is ~27 s |

**This is why option C matters more than the 3D verdict suggested.** C was measured as a poor
interactive renderer (117 ms, no camera) and that stands. But movie rendering is the one part of
napari's job that is *inherently* non-interactive, and C is already the codebase's only server-side
compositor, already reads the zarr, already applies napari's exported LUTs, and already has the
infrastructure around it — `jobs.jl` background jobs with progress and `cancel_job!`, which is
exactly what `record_timelapse`'s progress/cancel contract needs. Adding a camera to C for offline
frames is cheap, because 117 ms/frame is a non-problem when nobody is waiting on a single frame.

So the port list, with nothing deferred to napari:

1. **Chunk geometry first** — per-slab (or per-plane) chunking. On today's 1116-chunks-per-timepoint
   store, everything downstream is measured against the wrong baseline and will feel wrong for
   reasons unrelated to the feature being built. It also speeds up napari while it still exists, and
   the `.ccbundle` transport. Cecelia already owns the surface (`store_layout` / `store_compressor`).
2. **h5ad-derived overlays** — pops, tracks, colour-by. Make-or-break #2: it is what makes a viewer
   useful rather than pretty, and it is the thing napari currently does server-side for free
   (`CLOUD_MIGRATION_ASSESSMENT.md` §3a). New routes, or a browser-side equivalent of
   `LabelPropsView`.
3. **3D camera controls** — rotate/pan/zoom/reset with sane inertia. More work than it looks, and the
   feature people will judge "does this feel like napari" on.
4. **Tracks with tails** — real geometry, nothing in the browser stack to borrow.
5. **Labels/masks** — cheap: one more texture and a palette lookup.
6. **The offline render path in C** — camera + the four capture commands + title cards, on `jobs.jl`.
7. **Mask correction with intensity context** — the one item that needs a design before an
   implementation (plan §5a). It is *why* the old R version put correction in napari, and the
   replacement has to answer it rather than inherit it.

### The risk that only exists under full replacement, and it has a deadline

Replacing the renderer changes the *picture*, not just the frame rate. Published figures and movies
were made with napari's MIP; a WGSL raycast and a Julia compositor will not be pixel-identical, and
"is this the same cell I saw last year" is a real question for a lab.

**The only way to answer it is to A/B against napari — which stops being possible the moment napari
is removed.** So the comparison harness is not a nice-to-have and it is not something to build
later: it is the instrument that has to exist *while the reference still does*. `capture_view_state`
/ `apply_view_state` (`python/cecelia/utils/napari_utils.py:587`) already returns
`{camera, dims, layers}` — angles, centre, zoom, T/Z position, per-layer colormap and contrast. It
was built for animation keyframes, but it is exactly a viewer-state contract: feed the same snapshot
to both renderers and the comparison is honest rather than eyeballed. Do this early, and keep the
stills.

## Still not measured

Stated so the recommendation is not read as more complete than it is.

- **Remoted napari's interactive latency** (VNC/VirtualGL encode + WAN). Third time it has gone
  unmeasured; it needs a real ARE/cloud desktop, not this laptop (assessment §3d). **Under the
  full-replacement goal this stops being load-bearing** — option A is excluded because it keeps
  napari — so the audit's main stated weakness closes, though the number would still be worth having
  if the goal ever softens.
- **Real-data texture-cache behaviour.** G2 used a smooth phantom; fine structure would lower hit
  rates. Sample counts are unaffected (MIP has no early termination).
- **Serving blosc bytes at slab granularity with WASM decode** — the obvious optimisation from G3,
  untested.
- **Whether a browser raycast is scientifically trustworthy.** This is a frame-rate audit. Whether
  the picture is good enough to judge segmentation on needs Dominik's eyes on a side-by-side still,
  not a benchmark.
