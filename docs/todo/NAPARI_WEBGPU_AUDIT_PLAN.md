# Napari vs. WebGPU web renderer — audit plan

> **STATUS: parked plan, 2026-08-24.** Branch `audit/napari-webgpu`, worktree `cecelia-webgpu`.
> Ask: [`docs/archive/napari-webgpu-audit-prompt.md`](../archive/napari-webgpu-audit-prompt.md).
> This is the *plan* for the audit, not the audit. The verdict lands in
> `NAPARI_WEBGPU_AUDIT.md` next to this file.
>
> **Read [`CLOUD_MIGRATION_ASSESSMENT.md`](CLOUD_MIGRATION_ASSESSMENT.md) first.** That assessment
> (2026-08-21, declined) already answers most of the surrounding question and its headline is
> "**do not rewrite the viewer, remote the existing one**". This audit exists because of the one
> hole it admits: *"there is no evidence about interactive latency, which is the one thing that
> decides whether remoting is tolerable"*. So the incumbent's deciding number was never taken
> either. Comparing a measured web renderer against an unmeasured remoted Napari would be
> dishonest, so **this audit measures both sides on the same GPU with the same data.**

## 1. What is already established — do not re-derive

From `CLOUD_MIGRATION_ASSESSMENT.md`, measured on `origin/main` @ 2e5b89fd:

| Fact | Consequence for this audit |
|---|---|
| Working store must be instance-local NVMe. CIFS 12.4 ms/chunk, gdrive 9.7 s/chunk | Pixel serving is local-disk-backed on the VM either way. Not a differentiator |
| h5ad is read server-side by the bridge; 139 MB total vs 192.6 GB zarr (1:1380) | A browser client must **re-implement** those reads. Counts against the web renderer |
| `offscreen` and `eglfs` give no GL context; an X server is a hard dependency for remoting | Cost on the *remoting* side of the ledger |
| Hardware GL only via `__NV_PRIME_RENDER_OFFLOAD=1` + `__GLX_VENDOR_LIBRARY_NAME=nvidia` (`app/src/napari.jl:55-59`) | Applies to the browser too — see gate G0 |
| Multi-user is cheap: per-process singletons, container per user, 193 routes untouched | Confirmed by Dominik: **one session per workstation, one VM per user.** Phase 4 is closed, see §6 |
| Store has a **single multiscale level** — no coarse browse level | Required work for *both* options. See §5, item L |
| `.ccbundle` packing is 6.1× faster than a live-store copy | Session import/export shape, unchanged by this audit |
| Napari throughput numbers in that doc are **invalid** (readback raced the draw; NVIDIA and llvmpipe landed within 5%) | Gate G1 re-takes them with an explicit render-complete sync. This is the yardstick |

## 2. The datasets — real numbers, from disk

Dominik's call: `fXgbTl` for small tests, any image from set `OLifi6` for the real target. Both in
project `zolIMa`. Read from `.zarray`/`.zattrs`, not estimated.

| | `fXgbTl` (small test) | `OLifi6` / `VJy1Nx` (real target) |
|---|---|---|
| shape `t,c,z,y,x` | 31, 4, 32, 420, 441 | 181, 4, 38, 1046, 1104 |
| chunks | 1,1,1,420,441 (**whole plane**) | 1,1,1,512,512 (3×3 tiles/plane) |
| dtype | `<u2` (uint16) | `<u2` (uint16) |
| compressor | blosc/zstd, clevel 3, shuffle 1 | blosc/zstd, clevel 3, shuffle 1 |
| pyramid levels | **1** | **1** |
| voxel size µm `z,y,x` | 2.0, 0.3315, 0.3315 (6:1 anisotropy) | 2.0, 0.3315, 0.3315 |
| frame interval | 15 s | 15 s |
| voxels per (t,c) | 5.93 M | **43.9 M** |
| bytes per (t,c) uncompressed | 11.9 MB | **87.7 MB** |
| bytes per timepoint, 4 ch | 47.4 MB | **351 MB** |
| chunk files per (t,c) | 32 | **342** |
| chunk files per timepoint, 4 ch | 128 | **1368** |
| compressed on disk per timepoint, 4 ch | ~7.7 MB | **~39 MB** (`ccidSmoothed` 7.1 GB / 181) |
| whole store | 238 MB | 7.1 GB |

`OLifi6` is a set named "MERTK" with 6 images (`VJy1Nx WHkik3 R8m1Yo ldYr8J Dml3RG 2h06xA`); all are
within ±20% of the figures above, so `VJy1Nx` is representative and `ldYr8J` (z=45) is the worst case.

**Two numbers set the whole problem.** 351 MB of uint16 per timepoint at 4 channels — comfortable
against 8 GB of VRAM, ~20 timepoints cacheable. And 1368 chunk requests per timepoint step — the
scrub cost, which is a request-count problem, not a bandwidth problem (39 MB).

## 3. The rig

Benchmarks run on Dominik's machine now; if the concept holds, re-run on the real VM.

- GPU: **NVIDIA RTX 2000 Ada Generation Laptop, 8188 MiB**, driver 580.173.02.
- Browser: **Firefox 153.0.4** — the only browser installed. No Chrome/Chromium.
- Display `:1` reports `llvmpipe … Accelerated: no` with no env set; `xrandr --listproviders` shows
  0 providers. Hardware GL is reachable only through the PRIME offload env (`~/use_gpu.sh`).
- Not installed: `vglrun`, `x11vnc`, `Xvfb`, TurboVNC. So a real remoting stack cannot be stood up
  here — which is why G1 measures *render* latency, and encode+network is treated as an additive
  term on the remoting side rather than guessed at.
- Node 24 / npm available for the WGSL harness.

Spikes go in `docs/todo/spike/` alongside the existing `qpa_gl_probe.py` /
`headless_render_spike.py`. **Throwaway harness code, not renderer code in the app** — nothing in
`frontend/` or `app/` is touched by this audit.

## 4. Stages, cheapest gate first

Each stage has a stated artifact and a stated kill condition. A stage that fails its gate stops the
audit there and the recommendation becomes "remote Napari" — that is a legitimate output.

### S1 — Napari surface-area inventory (Phase 1 + 1.5)
Grep-only, no measurement. Every real point of contact: `napari.*` calls, WS bridge messages in
`napari/napari_bridge.py` (2416 lines) and `app/src/napari.jl`, layer types actually constructed,
event callbacks actually bound, plugins. Cross-reference `docs/NAPARI.md` (91 KB — slice it),
`docs/inventory/NAPARI.md`, `INVENTORY.md`. Record read-only vs. writes-back-into-the-pipeline, and
criticality. Also settle the dask question: find the actual `add_image` call site and record whether
it goes through dask or `as_dask=False`.
- **Artifact:** feature inventory table + the dask finding.
- **Gate:** none — but the prompt mandates a **check-in with Dominik here** before spending GPU time.

### S2 — G0: does the browser even get the GPU?
Load a local page reporting `navigator.gpu` adapter info, in a throwaway Firefox profile (his
profile is not touched), with and without the PRIME env. Confirm WebGPU is enabled on Linux in FF
153 and that the adapter is the RTX 2000 Ada, **not llvmpipe** — the prior assessment's
software-renders-silently trap, now on the browser side.
- **Artifact:** adapter name, backend, and the `limits` that matter: `maxTextureDimension3D`
  (need ≥1104), `maxBufferSize`, `maxStorageBufferBindingSize`, `maxComputeWorkgroupStorageSize`.
- **Kill:** no WebGPU, or llvmpipe-only, with no fix inside a day → audit stops, verdict is remoting.

### S3 — G1: the Napari yardstick (fixes the prior invalid numbers)
Same GPU, same `VJy1Nx` data, honest timing: an explicit render-complete sync before stopping the
clock, per the prior doc's own diagnosis. Measure (a) 3D MIP rotate frame time at a 1080p canvas,
1 channel and 4 channels; (b) t-slider step latency, cold and warm.
- **Artifact:** the incumbent's real interactive numbers — the first time these exist.
- **Sanity check that the fix worked:** forced `llvmpipe` must now be *far* slower than NVIDIA. If
  they land within 5% again the harness is still racing and the numbers are still invalid.

### S4 — G2: volumetric raycast — **three renderers, one yardstick**
Dominik, 2026-08-24: **benchmark all three options.** So G2 is not "can WebGPU do it" but "which of
the three makes an acceptable frame on this GPU, and at what cost in new code":

- **A** — Napari's own 3D MIP (that is S3/G1, and it is the yardstick).
- **B** — WGSL raycast in the browser, volume on the client GPU.
- **C** — a raycast inside `api/src/image_render.jl`, frames streamed to the browser. Same Julia
  process, same zarr reads, same contrast/LUT code that already ships; what is new is a camera and
  the ray loop. C needs **no chunk delivery to the client at all**, which sidesteps G3 entirely —
  so if C clears the frame-rate gate, G3 stops being make-or-break for the whole project and
  becomes a cost that only option B pays.

Report per option: frame time at 1080p × {1, 4} channels, and an honest count of what has to be
written that does not exist yet.

#### G2 detail — B: WebGPU volumetric raycast
WGSL raycaster over `r16uint`/`r16float` 3D textures, front-to-back with early termination,
1080p canvas. Ladder: synthetic volume at real dimensions → real `fXgbTl` timepoint → real
`VJy1Nx` timepoint → 4-channel composite. Sweep step count (128/256/512) and viewport size to find
where it breaks, and report GPU time, not just wall clock.
- **Artifact:** frame time vs. channels × step count on the real 38×1046×1104 volume.
- **Gate (set before measuring):** 4-channel composite rotate **≥15 fps**, single channel
  **≥30 fps**, at 1080p on this GPU. Below that, and not fixable by step count or a coarse level,
  the concept fails and the verdict is remoting.

### S5 — G3: chunk delivery and decode — **option B only**
The other half of the browser cost, and the part the prior assessment never had to think about
because remoting keeps pixels server-side. **A and C do not pay this at all.** Serve the real store over HTTP with range/keep-alive from
localhost, then in-browser: fetch 1368 chunks → blosc-zstd-shuffle decode (WASM, `numcodecs`) →
assemble → upload to 3D texture. Measure each of the four separately; report request concurrency
and decode CPU time per chunk.
- **Artifact:** cold time-to-first-volume and per-timepoint step cost, broken down.
- **Gate:** timepoint step **≤500 ms** cold on localhost, with a prefetch path that hides it during
  playback. Compare against Napari's local-NVMe 0.013 s of pure IO for the same 1368 chunks.

### S6 — Protocol split (Phase 2)
Does the existing Julia WS protocol already separate pixel addressing from state, or are they
tangled? If tangled, that separation is scoped work, not free.
- **Artifact:** message-by-message classification + a size estimate for the split.

### S7 — Deliverable
`NAPARI_WEBGPU_AUDIT.md`, in the order the prompt asks for: inventory → what Napari buys for free →
per-feature feasibility with the volumetric verdict backed by S4/S5 numbers → item-by-item
side-by-side incl. **where the web renderer is strictly worse** → ranked make-or-break → the
one-session-per-VM note → an unhedged recommendation.

## 5. Work items that exist either way (so they don't get charged to one side)

- **L — build a multiscale pyramid.** Neither option has a coarse browse level today. It is
  `importImages.omezarr` with `pyramidScale: 1` on these stores. It helps Napari (prior doc,
  conclusion 5) and it is close to mandatory for the browser. Charge it to neither column.
- **Instance-local NVMe** for `projects_dir` — settled, both options.
- **Auth at a reverse proxy** — 0 existing sites, both options.
- **Session import/export via `.ccbundle`** — existing functionality, both options.

## 5a. Segmentation-mask correction — a requirement, not just a risk

Dominik, 2026-08-24, on `CORRECTION_PLAN.md` P2 (unbuilt): *"in the previous R version this was done
in napari because you had to see the actual signal from the intensity channels. but we could come up
with an alternative way that is more similar to the track correction in browser."*

So this is **not** the binary the audit assumed. Two consequences, and they pull in opposite
directions:

1. **It does not have to live in napari** — an in-browser surface modelled on the track-correction
   timeline is on the table. So "nothing writes back from napari" can stay true.
2. **But whatever surface it lands on must show the real intensity channels**, because that is the
   judgement being made. That is the *reason* the old R version put it in napari, and it does not go
   away by moving the surface.

Therefore an in-browser mask-correction surface **presupposes** a browser-side intensity renderer of
usable fidelity — option B or C. It is not a downstream nice-to-have that a display-only verdict can
defer: it is a second, independent consumer of the same capability G2 is measuring. The audit records
it that way, and the deliverable must say which of B/C could carry it.

Corollary for the side-by-side: this is the one place where *remoting* (option A) has a real
advantage that survives everything else — napari already shows the intensity channels next to a
paintable Labels layer, so A gets this feature for close to free while B and C have to build the
renderer first.

## 6. Phase 4 is closed

Dominik, 2026-08-24: **one session per workstation, one VM per user.** No concurrency sizing is
needed — there is no shared-instance case to size, and the prior assessment already established
that per-process singletons make container-per-user free of route changes. Nothing further to
compute; the audit records this and moves on.

## 7. Assumptions, stated

- "Usable" is defined as *no worse than what Napari does on this GPU today*, which is exactly what
  S3 measures. No absolute frame-rate opinion is imposed beyond the S4 gate.
- Numbers are laptop numbers. The real VM will differ; direction of the difference is unknown for
  the browser and unknown for remoting, so it cancels for the *comparison* while leaving both
  absolute figures provisional.
- Remoting's encode + WAN latency is **not** measured here (no VNC stack installed, no VM). It is
  carried as an explicit additive unknown on the remoting side, not as zero.

## 8. Reservations

- Firefox is the only browser on this machine and Linux WebGPU support there is the weakest of the
  major targets. A G0 failure would be a *tooling* result, not a verdict about WebGPU — say so
  rather than concluding the concept is dead.
- S4 measures rendering, not scientific accuracy. Whether a browser MIP/raycast is *visually
  trustworthy enough to judge segmentation on* is a separate question that needs Dominik's eyes on
  a side-by-side still, not a frame rate.
- The audit can honestly conclude something narrower than either option, e.g. browser for
  display-only review and Napari retained for the correction/editing subset. That is on the table.
