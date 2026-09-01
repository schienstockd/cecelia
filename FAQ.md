# FAQ — why is Cecelia built the way it is?

Short answers to the questions people actually ask about Cecelia Feijoa — why it's built the way
it is, and how the pieces fit. Deeper reasoning lives in [`docs/`](docs/); this is the quick version.

## Languages & architecture

**Why three languages — Julia, Python, and Vue?**
Each does a job the others can't do as well: Python for image I/O and ML (Cellpose, btrack,
PyTorch), Julia for orchestration, gating, and statistics (ported cleanly from the original R and
fast without a C extension layer), Vue for the interface. The split is a firm rule — see
[`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md).

**Why not Rust?**
Cecelia doesn't have a systems-performance problem to solve. The expensive work is imaging + ML
(already in Python) and research statistics (already in Julia, close to C speed); a fourth language
would add build and interop cost for performance the current stack already delivers.

**Why not port the remaining Python to Julia and drop a language?**
The Python that remains is there for *libraries* with no equivalent in another ecosystem — Cellpose,
btrack, scanpy/Leiden — so "porting" would mean reimplementing published algorithms. Full audit:
[`docs/archive/python-audit-report.md`](docs/archive/python-audit-report.md).

**Why keep all analysis out of the frontend?**
So the core package can run and be tested from the Julia REPL with no interface attached. The API is
a thin layer on top; the UI is just a view.

## Distribution

**Why is the app just a web browser instead of a desktop app?**
The Julia server serves its own frontend, and the window you see is your default browser pointed at
`localhost:8080`. No Electron, Tauri, or bundled Chromium — no second runtime to ship or maintain.
More in [`docs/SHIPPING.md`](docs/SHIPPING.md).

**Why is there no traditional per-OS installer?**
The hard part is provisioning a multi-gigabyte Julia + Python + CUDA environment. A single bootstrap
command sets up Pixi and Juliaup and builds that environment reproducibly; only the install script
differs per platform.

**Do I have to choose GPU or CPU?**
No — it's detected at runtime (CUDA, Apple MPS, or CPU). Deliberately no checkbox: one less setting
to get wrong.

## Image processing

**Everyone says "use Dask" for out-of-memory images. Why doesn't Cecelia?**
Dask's default block layout over-fetches for the tiled/per-plane access Cecelia's hot paths use, and
going per-frame (one frame in memory, written straight to the output store) beats it on both speed
and peak RAM. Numbers and the `da.store` correctness edge case in
[`docs/todo/ZARR_STREAMING_PLAN.md`](docs/todo/ZARR_STREAMING_PLAN.md).

**Then how do live task previews work, if not by re-evaluating a lazy graph?**
Each task splits into a global part (normalisation window, background level — computed once and
cached) and a per-pixel part applied only to the visible region. A lazy graph over the crop would
recompute the statistic *from* the crop, so the preview would disagree with the real run. See
[`docs/SEGMENTATION.md`](docs/SEGMENTATION.md) → *Previewing params BEFORE a run*.

**Why does autofluorescence correction have almost no settings?**
Because a knob nobody can set by eye is a fossil, not a setting. Background levels are derived per
channel from the image itself (triangle thresholding, Zack et al. 1977), and competing channels
share credit via `out = b × b²/Σbᵢ²` — no scale, no ceiling, output in input counts.

## How it was built

**Was this really written by an AI?**
Almost all of the source, yes — by Claude Code under Dominik's direction. Cecelia is a port of an
existing peer-reviewed tool (the R/Shiny `cecelia`, *Nature Communications* 2025), so the design and
the science were already validated; the work was translating them into the new stack.

**Then who verified the science?**
Dominik. The AI never had access to a microscope or real imaging data beyond small test fixtures,
so all scientific and visual validation was done by the human author. Early releases haven't yet
been independently tested by other users — treat them accordingly.

**What license is it under?**
GPL-3.0-or-later, inherited from the original `cecelia` R package rather than chosen fresh.
Third-party components are acknowledged in [`THIRD_PARTY.md`](THIRD_PARTY.md).
