# FAQ — why is Cecelia built the way it is?

Short answers to the questions people actually ask about Cecelia Pineapple — why it's built the way
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
