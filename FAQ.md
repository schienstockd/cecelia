# FAQ — why is Cecelia built the way it is?

Short answers to the questions people actually ask about Cecelia Feijoa — why it's built the way
it is, and how the pieces fit. Deeper reasoning lives in [`docs/`](docs/); this is the quick version.

## Languages & architecture

**Why three languages — Julia, Python, and Vue?**
Each one does a job the others can't do as well. Python handles image I/O and machine learning
(napari, Cellpose, btrack, PyTorch), because that ecosystem only really exists in Python. Julia
handles orchestration, gating, and statistics, because the analysis ported cleanly from the original
R and runs fast without a C extension layer. Vue is the interface and nothing more. The split
between them is a firm rule, documented in [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md).

**Why not Rust?**
Rust is worth reaching for when you have a systems-performance problem. Cecelia doesn't. The
expensive work is imaging and ML (already in Python) and research statistics (already in Julia,
which is close to C speed). A fourth language would add real build and interop cost in exchange for
performance the current stack already delivers.

**Why Julia rather than doing everything in Python?**
The statistical work — gating, tracking measures, spatial stats, clustering — maps almost directly
from the original R into Julia, and Julia runs it fast without dropping into C. Keeping Python only
for imaging and ML means each language stays in the domain where it's strongest.

**Why not finish the job — port the rest of Python to Julia (or Rust) and drop a language?**
Because the Python that remains isn't there for the *language*, it's there for *libraries* with no
equivalent in any other ecosystem: Cellpose, btrack, scanpy/Leiden clustering, napari. "Porting"
those wouldn't mean rewriting glue — it would mean reimplementing published algorithms, the hardest
work there is, and the ecosystem best at that is Python, then Julia, and (distantly) Rust. The few
pieces that genuinely *could* move to Julia are numeric image-correction steps, and moving them buys
tidiness, not a smaller install — the multi-gigabyte weight (PyTorch, Cellpose) is exactly the part
that can't move. So the remaining Python is a deliberate, permanent rim, not unfinished business. And
the `.h5ad` files aren't the lock-in people assume: that format is the one both Julia and Python read
natively, which is *why* it stays — what keeps Python in the loop is the clustering algorithm, not
the file format. Full breakdown in
[`docs/prompts/python-audit-report.md`](docs/prompts/python-audit-report.md).

**Why keep all analysis out of the frontend?**
So the core package can run and be tested from the Julia REPL with no interface attached. The same
task code runs identically whether it's called from a test, the REPL, or the GUI. The API is a thin
layer on top; the UI is just a view.

## Distribution

**Why is the app just a web browser instead of a desktop app?**
The Julia server serves its own frontend, and the window you see is your default browser pointed at
`localhost:8080`. That avoids bundling Electron, Tauri, or a copy of Chromium, and it means there's
no second runtime to ship or maintain. More in [`docs/SHIPPING.md`](docs/SHIPPING.md).

**Why is there no traditional per-OS installer?**
The genuinely hard part of shipping this kind of software is provisioning a multi-gigabyte
Julia + Python + CUDA environment on someone else's machine. A single bootstrap command sets up Pixi
and Juliaup and builds that environment reproducibly; only the install script differs per platform.

**Do I have to choose GPU or CPU?**
No. It's detected at runtime (CUDA, Apple MPS, or CPU). There's deliberately no GPU checkbox — it's
one less setting a user can get wrong.

**Why is Cellpose pinned to version 3?**
Version 4 removed the denoising models, which Cecelia's pipeline relies on. Until that changes, v3
is a hard pin rather than an oversight.

## Image processing

**Everyone says "use Dask" for out-of-memory images. Why doesn't Cecelia?**
Because the OOM problem and the *access pattern* problem have different answers, and Dask solves the
first by making the second worse. Dask's default chunking packs the whole timecourse into one ~128 MB
block, so reading a single 512×512 tile over-fetches the entire block — and Cecelia's hot paths are
all tiled or per-plane (segmentation tiles, napari slicing one z per frame). The original code worked
around that by loading whole images into RAM, which is what actually caused the OOM: one channel's
timecourse as float64 is ~47 GB on a large movie.

The fix wasn't laziness, it was **granularity**. Every image task now holds exactly one frame at a
time and writes it straight into the output store. Bounded memory *and* fast in-RAM tiling. Measured
on a real 0.78 GB store, copying per-timepoint from plain zarr takes 2.71 s at 1.2 GB peak RSS;
the same copy from a Dask array takes 6.09 s, and `da.store` with the required rechunk takes 8.31 s
at 3.6 GB. Dask lost 3× on speed and 3× on memory in its own best case.

There's a correctness edge too. `da.store(lock=False)` silently corrupts output when the source
blocks straddle the destination's chunk grid — two tasks read-modify-write the same chunk file. We
reproduced that 10 times out of 10. It's fixable by rechunking first, and the one place Cecelia still
uses `da.store` does exactly that, but it's a footgun a sequential per-frame write doesn't have.

Dask hasn't been thrown out — it's still the lazy container that napari renders from. It's just not
the compute engine. Full rationale and the numbers:
[`docs/todo/ZARR_STREAMING_PLAN.md`](docs/todo/ZARR_STREAMING_PLAN.md).

**Then how do the live task previews work, if not by re-evaluating a lazy graph?**
By splitting each task into the part that needs the whole image and the part that doesn't. The
expensive global statistic — a normalisation window, a background level — is computed once and
cached; the per-pixel work is then applied to just the region you're looking at. That's why changing
Cellpose's diameter re-previews in 0.14 s while changing its input channel costs a fresh statistic.

A lazy graph over the visible region would be simpler *and wrong*: it would recompute those
statistics from the crop, so the preview would be normalised differently from the run it is
supposed to be previewing. Being fast is not the hard part — agreeing with the real run is.
See [`docs/SEGMENTATION.md`](docs/SEGMENTATION.md) → *Previewing params BEFORE a run*.

**Why does autofluorescence correction have almost no settings?**
Because it used to have a dozen, and that was the bug. Two background percentiles, a rescale ceiling, a
median filter, a Gaussian, a rolling ball, a wavelet denoiser — each one added while fitting somebody's
particular dataset, none of them revisited afterwards. A parameter that exists because one image once
needed it is not a setting, it's a fossil.

They're gone. You pick which channel to correct and which to correct it against; the background levels
and the rescale ceiling are derived from the image's own histogram (triangle thresholding, Zack et al.
1977). The one surviving knob chooses *how* they're derived, not what they are.

The reason to prefer derivation over a knob here is that nobody can set these by eye. A rescale ceiling
is "the brightest real voxel", and on one test image a **single** voxel out of 5.88 billion was setting
it — a value you cannot see, cannot guess, and would get wrong on the next image in the set. Whether it
landed well *is* checkable after the fact, so that became QC (`clippedFrac`, `levelsUsedFrac`) instead
of a parameter.

## How it was built

**Was this really written by an AI?**
Almost all of the source, yes — written by Claude Code under Dominik's direction. What makes that
workable is that Cecelia is a port of an existing, peer-reviewed tool (the R/Shiny `cecelia`,
published in *Nature Communications* in 2025). The design and the science already existed and had
been validated; the work was translating them into the new stack.

**Then who verified the science?**
Dominik. The AI never had access to a microscope, the running GUI, or real imaging data beyond small
test fixtures, so it couldn't judge whether a result was biologically correct. All scientific and
visual validation was done by the human author. Early releases haven't yet been independently tested
by other users, so treat them accordingly.

**What license is it under?**
GPL-3.0-or-later, inherited from the original `cecelia` R package rather than chosen fresh.
Third-party components are acknowledged in [`THIRD_PARTY.md`](THIRD_PARTY.md).
