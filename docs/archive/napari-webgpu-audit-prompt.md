> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.
>
> **OUTCOME (2026-08-24):** answered in
> [`docs/archive/napari-webgpu-audit.md`](../todo/NAPARI_WEBGPU_AUDIT.md), with the gate ladder and the
> real dataset geometry in [`NAPARI_WEBGPU_AUDIT_PLAN.md`](../todo/NAPARI_WEBGPU_AUDIT_PLAN.md).
> Recommendation: build the web renderer for display, chunk geometry first; keep napari for movies,
> keyframes and (until designed) mask correction. The prompt's stated one-real-risk — 3D volumetric
> rendering in a browser — measured **6.2x faster than napari** on the same GPU and data, so it is
> closed. Two things the prompt assumed that turned out false: option 2 was not the only alternative
> (a Julia server-side compositor already ships), and a timepoint costs ~1.2 s rather than the 13 ms
> implied by the prior assessment's byte-fetch figure.

# Phase 0: Napari vs. Web Renderer — Honest Comparative Audit

## Context

IT is mandating cloud-only compute (no new local analysis machines), which
reopens the visualization question: can Napari be replaced with a custom
WebGPU-based web renderer? No off-the-shelf tool (vizarr/avivator/
itk-vtk-viewer/neuroglancer) supports 3D+time volumetric interaction at the
level needed, and a full vispy/Napari port to WebGPU is not realistic scope
— so the real option on the table is a narrow, purpose-built web renderer
covering only what this repo actually uses Napari for.

## What this audit is actually for

**This is not a feasibility-building exercise for a decision already made.**
It is an honest comparison of two real options:

1. Keep Napari, make it cloud-compatible (stream/remote it — NoVNC or
   equivalent — onto a cloud instance)
2. Replace it with a custom WebGPU web renderer

Option 2 should lose if the audit finds real reasons it should lose. Do not
write around inconvenient findings, and do not treat "we already started
down this path" as a reason to keep going. A conclusion of "stick with
Napari, streamed" is a legitimate and acceptable output of this audit — say
so plainly if that's what the evidence points to.

Do not write renderer code in this phase. Do not propose an implementation
plan yet.

## Ground rule for the inventory

We do not need "what Napari can do." We need "what Napari does **in this
repo, right now**." Most functionality has already been ported off the old
R/Shiny stack into Cecelia.jl. Napari's remaining footprint should be small
and specific. Treat any Napari capability not actually invoked in this
codebase as out of scope — do not include it in the inventory just because
Napari supports it.

## Phase 1: Napari surface-area audit

1. Grep the full repo (Python + Julia) for every point of contact with
   Napari: direct `napari.*` API calls, the WebSocket bridge protocol
   messages sent to/from the daemon, layer types actually constructed, viewer
   methods actually called, event callbacks actually bound, and any
   Napari plugins in use.
2. For each distinct capability found, record:
   - What it does and where it's invoked (file/function)
   - Which user-facing workflow it supports (segmentation review, gating,
     population highlighting, movie export, etc.)
   - Whether it's read-only display or involves interactive editing that
     writes back into the pipeline (this distinction matters most)
   - Frequency/criticality — is this core to daily use or a rarely-hit path
3. Cross-reference against `INVENTORY.md` and the existing napari bridge
   protocol docs so we're not re-deriving what's already documented.
4. Output: a single feature inventory table. Columns: capability, workflow,
   read-only vs. interactive, criticality, current implementation location.

## Phase 1.5: What Napari is actually buying us today

Before comparing, be honest about what the current stack gets right — this
is not just a list of Napari's flaws to justify replacing it.

**Async/non-blocking display, correctly understood.** Zarr arrays are
already lazy and chunked on their own — indexing a `zarr.Array` only reads
touched chunks, no dask required for that. `zarr_utils.open_as_zarr()`
already has an `as_dask=False` path, and task code (e.g. the temporal/
spatial smoothing task) uses it explicitly: <cite index="0">"every read is one
chunk-aligned plane, so a dask graph only adds overhead."</cite> So dask is NOT
there to avoid loading the whole array into memory — that was never the
risk. What dask actually provides Napari specifically is async slicing on
the Qt UI thread: scrubbing a slider needs the chunk fetch off-thread so the
UI doesn't freeze, and dask's lazy graph plugs into Napari's async-slicing
machinery to make that non-blocking. It's a UI-responsiveness mechanism, not
a memory mechanism.

Audit task: find the actual call site that feeds `napari.add_image` (or
equivalent) and confirm whether it goes through dask or `as_dask=False`.
Record the finding either way — don't assume.

**Why this matters for the comparison, not just as trivia:** if dask's real
job is "keep the Qt thread unblocked," that specific justification does not
transfer to a browser client — the browser's fetch is already async by
nature, and the "don't block the UI" job belongs to the browser's own event
loop, not a Python-side scheduler. This is a point *in favor* of the web
renderer on this specific axis. Note it as such. But also identify what
else Napari's stack quietly provides that has no equivalent yet: mature
interaction handling (drag, zoom, pan, pick), a tested contrast/colormap
pipeline, 3D camera controls, and a maintained codebase with other people's
bug fixes behind it. A from-scratch WebGPU renderer starts with none of
that — say so plainly as a real cost, not a footnote.

**Cross-process/cross-language bridge elimination.** The current stack's
real accumulated cost isn't just Napari's rendering — it's the architecture
built up around it: multiple server instances to keep alive, and a
constant R-to-Napari bridge (IPC, reconnect handling, protocol debugging)
that exists purely because Napari is an external Python/Qt process the rest
of the stack has to talk to. A browser client eliminates this category of
problem entirely — no cross-language bridge, no external process to babysit,
because the client lives in the same request/response world as everything
else being served over HTTP/WebSocket. This is a genuine, non-theoretical
point in the web renderer's favor and should be weighed as such, not just
listed as "nice to have."

**Generality tax.** Napari's layer system is built to be general-purpose,
which means it carries interaction machinery for use cases this repo
doesn't have — e.g. the points layer supports interactively dragging points
around, when nothing in this workflow ever does that. A purpose-built
renderer sheds this: a points layer here can just mean "render dots at
given coordinates," full stop — no drag handles, no hover state, no move
history, because that capability was never needed. Note where else in the
inventory this generality tax shows up (features carrying complexity for
interaction patterns that are never exercised).

## Phase 1.6: Where the actual risk lives (read this before scoping effort)

Be explicit in the deliverable about this priority split, because it
changes how much scrutiny each part of the audit deserves:

**The one real open question is 3D+time volumetric image rendering
performance and accuracy** — can zarr-backed multi-channel 3D+time volumes
be rendered in-browser via WebGPU at a speed and visual accuracy that's
actually usable for scientific review, on realistic dataset sizes and lab
hardware? This is the only part of the project that could fail outright.
Everything else (points/label overlays, contrast controls, session state,
the pixel/control backend split) is expected to be straightforward,
solvable engineering that may be messy or take iteration, but has no
realistic path to "this doesn't work at all." Do not spend disproportionate
audit effort hedging on the easy parts — spend it establishing real
confidence (numbers, not vibes) on the volumetric rendering question
specifically. Phase 3's "Volumetric rendering" bullet is the one that
should get the most rigorous treatment: real voxel counts from actual
datasets in this repo, actual channel counts, actual frame-rate targets,
and either a working micro-benchmark or a concrete literature/prior-art
reference point — not a guess.

**Example of the "sort itself out" bucket, for calibration:** object
picking (click a rendered object to select it) is a solved, well-trodden
WebGPU problem — either ray-sphere intersection against proxy geometry, or
render-to-texture with each object's ID encoded as a unique color and read
back the pixel under the cursor. For label/segmentation data specifically,
color-coding is nearly free since a label ID per voxel already exists — no
proxy geometry needed. This is representative of the confidence level to
apply everywhere except volumetric rendering: don't over-invest audit time
proving out mechanisms that already have standard, cheap solutions.

## Phase 2: Backend architecture split

Two backends, not one — the pixel path and the control path have different
requirements and should not be conflated:

- **Pixel serving**: dumb, high-throughput, stateless. Zarr chunks are just
  byte-range-addressable files — any HTTP range-request host works (S3/GCS/
  Azure blob directly, or a static server in front of the zarr store). Julia
  is not in this path at all; the browser client fetches chunk bytes
  directly using multiscale metadata to pick LOD/chunk indices. This is the
  part the #315 per-plane chunking already makes viable — it's the unit a
  browser would actually request.
- **Control plane**: stateful, interactive, low-bandwidth. Gating state,
  Population Manager, contrast/colormap/LUT settings, current
  timepoint/channel, session state, and processing edits (gating polygons,
  corrections) written back into the pipeline. This is the existing Julia
  WebSocket bridge, extended rather than replaced. Julia never proxies pixel
  bytes here — it only tells the client what to fetch and processes what
  comes back from interaction.

Audit task: check whether the current Julia WebSocket protocol already
cleanly separates "pixel addressing" messages from "state" messages, or
whether the two are tangled together. If tangled, that separation is itself
required work and should be scoped as its own item, not assumed free.

## Phase 3: Bottleneck identification (per-feature feasibility)

For each item in the Phase 1 inventory, assess WebGPU feasibility
independently — don't roll this into one vague verdict:

- **Data delivery**: given the #315 per-plane-chunked zarr streaming
  writers, what's actually required to serve those chunks to a browser
  client (HTTP range-request store, auth, LOD selection for the multiscale
  pyramid)? Is this solved, partially solved, or unsolved in the current repo?
- **Volumetric rendering**: raycasting a multi-channel 3D+time OME-Zarr
  volume at interactive frame rates in WGSL — what's the actual voxel
  count / channel count / bit depth we're dealing with on real datasets, and
  does that plausibly hit acceptable frame rates on realistic lab hardware?
- **Time scrubbing**: what latency does chunk-fetch-per-timepoint impose on
  scrubbing, and is that tolerable against current Napari desktop scrubbing
  behavior users are used to?
- **Interactive editing**: which inventory items are read-only display vs.
  which involve the user drawing/adjusting something that writes back
  (gating polygons, segmentation correction, ROI selection)? These are the
  expensive ones to reimplement — call out explicitly if any exist that
  aren't already handled by the native Julia gating engine / Population
  Manager.
- **Contrast/colormap/LUT**: per-channel windowing and colormap blending
  live in-shader — straightforward in WebGPU, but confirm nothing unusual
  (log scales, custom LUTs) is in play.

## Phase 4: Multi-user architecture (downstream, not blocking)

Does not affect the rendering feasibility verdict — scope separately, after
the comparison in Phase 3.

Default assumption: no in-process multi-tenancy. Spawn one Julia/Cecelia
process per active user/session (matches current per-desktop usage today,
just relocated to a container/VM instead of a local machine). Isolation is
the process boundary — no shared session state, no cross-user resource
contention, one user's crash doesn't affect others.

The pixel-serving layer (object storage) is naturally shared/stateless
already and needs no per-user duplication — this is "shared dumb storage +
per-user stateful compute," not full multi-tenancy anywhere.

Audit task: rough headcount × peak-concurrent-sessions estimate, since that
drives cluster sizing and idle-shutdown policy — the real cost here is
resource provisioning, not code. This estimate should inform whether IT's
cloud budget tolerates N concurrent Julia processes before committing to the
approach.

## Deliverable

A single markdown doc:
1. Feature inventory table (Phase 1)
2. Honest account of what Napari's stack currently provides for free —
   including the dask/async-slicing finding and anything else surfaced in
   Phase 1.5 (interaction handling, contrast pipeline, camera controls,
   maintenance burden of "we wrote this ourselves")
3. Per-feature feasibility verdict for the web renderer (solved /
   straightforward / hard / unclear), Phase 3 — with the volumetric
   rendering item backed by real numbers (voxel/channel counts from actual
   datasets, frame-rate targets, benchmark or concrete prior-art reference),
   not a qualitative guess. This is the one verdict the whole approach
   actually hinges on; treat it accordingly.
4. **A direct side-by-side**: for each inventory item, does the web renderer
   path look equal, better, or worse than streaming/remoting Napari? Don't
   average this into one score — list where the web renderer is strictly
   worse.
5. **The one or two bottlenecks that are actually make-or-break** — not a
   list of concerns, a ranked call-out of what would kill this approach if
   it doesn't pan out
6. Rough concurrent-session sizing estimate (Phase 4), for later cloud
   budget/orchestration planning — informational only, not part of the
   feasibility verdict
7. A clear recommendation, stated as a decision, not hedged: build the web
   renderer, stick with streamed/remoted Napari, or something narrower than
   either (e.g. web renderer for display-only workflows, Napari retained
   for the interactive-editing subset). If the honest answer is "kill this
   and go back to streaming Napari," say that as the headline, not buried
   in caveats.

Re-read `CLAUDE.md` and `INVENTORY.md` before starting. Check in after Phase 1
before proceeding further — the inventory itself may change scope.
