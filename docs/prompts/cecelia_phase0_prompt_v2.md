# Cecelia Refactor — Phase 0: Architecture Design

## What is Cecelia

Cecelia is a local desktop application for immunological image analysis, used by a
single biologist at a time on their own machine. Published in Nature Communications
2025. Source: https://github.com/schienstockd/cecelia

It integrates:
- Multidimensional microscopy images (OME-ZARR format, GB scale)
- Cell segmentation and tracking (Cellpose, StarDist, btrack)
- Population gating and clustering (Leiden, UMAP)
- Flow cytometry-style analysis on image-derived cell measurements (AnnData)
- Live cell behaviour analysis (HMM, spatial interactions)

---

## Current Stack (what we are replacing)

- **R/Shiny** — UI, orchestration, all data analysis, gating via flowWorkspace
- **Python** — image processing, segmentation, deep learning, Napari control
- **Napari** — image viewer, currently controlled via a Jupyter kernel spawned by Shiny
- **RMarkdown** — secondary interface for biologists to query data programmatically
- **Storage** — OME-ZARR (images + segmentation masks), AnnData (cell measurements)
- **IPC** — JSON files (task params), CSV files (Napari cell selections), disk polling

### Why we are replacing it

R is slow for data manipulation at GB scale. Shiny's reactive model adds overhead.
The Jupyter kernel approach for Napari is fragile. Disk-based IPC is unnecessary
overhead. The goal is a faster, cleaner architecture — not a rewrite of the science.

---

## Current UI (important context)

The Shiny app is a complex, data-driven scientific application. Key panels:

- **Processing workflow sidebar** — stages: Import → Denoise → Segment → Gate →
  Cluster → Plot. Each stage has sub-modules selectable from a dropdown.
- **Task Manager** — concurrent tasks with real-time status (running/queued/OK/FAIL),
  log access, cancellation, GPU toggle, HPC toggle, parallel task count, task history.
  One async observer per task, polling log files every few seconds.
- **Dynamic parameter panels** — every module's parameters are defined in JSON files
  at `inst/app/modules/inputDefinitions/`. There are ~20 widget types (slider,
  sliderGroup, group, box, channelSelection, popSelection, labelPropsColsSelection,
  checkbox, textInput, radioButtons, etc.). These JSON files can be reused directly
  in the new frontend — this is a significant reuse opportunity.
- **Module output** — plots and tables per module: UMAPs, heatmaps, scatter plots,
  frequency comparisons, gating plots
- **Viewer controls** — toggles for what Napari shows: Labels, Points, Tracks,
  Populations, Neighbours, 3D, Lazy loading, Pyramids, etc.
- **Define Populations** — list of gated/clustered populations with colours,
  rename, propagate across images
- **Project/Version management** — load, save, import, export, versioning

---

## New Stack (decisions already made — do not re-debate these)

| Layer | Choice | Replaces | Reason |
|---|---|---|---|
| Frontend | Vite + Vue 3 + Pinia | R/Shiny UI | Component model fits the dynamic widget system. SPA only — no SSR needed, app runs on localhost. |
| Orchestrator + data analysis | Julia | R + Shiny orchestration | Fast numerical computing, eliminates R entirely. PythonCall.jl for Python interop. |
| Gating engine | Native Julia | flowWorkspace (R) | Cecelia gates image-derived AnnData measurements, not FCS files. flowWorkspace's complexity (cytolib C++, protobuf, FCS parsing, flowJo XML import) is solving problems we don't have. Rectangle, polygon, ellipse, quadrant, and boolean gates on a DataFrame are ~300 lines of Julia. Gate definitions serialise to JSON. |
| Image processing + DL | Python via PythonCall.jl | Python subprocesses | Cellpose, StarDist, denoising are PyTorch — no Julia alternative. Called in-process from Julia where possible. |
| Zarr + AnnData I/O | Python via PythonCall.jl | disk polling | Use Python's zarr and anndata libraries from Julia. Napari already reads Zarr; no need for a second reader. |
| Image viewer | Napari (standalone Python process with embedded WebSocket server) | Napari via Jupyter kernel | No Julia equivalent of Napari exists. See Napari architecture section below. |
| Notebook interface | Quarto (Julia kernel) | RMarkdown | Same workflow feel for biologists, multi-language support |
| IPC | WebSocket messages | JSON files + CSV polling | Julia coordinates all Napari ↔ Vue communication |
| Storage formats | OME-ZARR + AnnData | unchanged | No change to data formats or on-disk layout |

**Not using:**
- R or RCall.jl — eliminated entirely. Julia native gating replaces flowWorkspace.
- Rust — orchestrator is I/O-bound, not compute-bound. Wrong tool.
- Nuxt — SSR overhead irrelevant for a localhost app. Plain Vite + Vue 3 suffices.

---

## Napari Architecture (replaces Jupyter kernel)

**Why Jupyter is being removed:** Shiny currently spawns a Jupyter kernel and sends
Python code strings to it for execution. This is fragile, slow to start, and couples
Napari control to the Jupyter protocol. We replace it with a dedicated Python process
that Napari lives in permanently.

**How the Napari process works:**

Napari has a hard constraint: it must run on the main thread of a Qt application.
The Qt event loop owns the main thread and cannot be moved. This means any server
that wants to receive commands must run on a background thread.

The Napari process does two things simultaneously:

```
napari_server.py
├── Main thread  — Napari Qt event loop (napari.run() blocks here)
└── Daemon thread — WebSocket server on localhost:7655
                    receives commands from Julia
                    sends events (cell selections) to Julia
```

Qt-safe rule: the background thread cannot touch Napari's viewer objects directly.
Instead it schedules updates onto the main thread using `QTimer.singleShot(0, fn)`.

**Julia's role:** Julia is a WebSocket CLIENT that connects to the Napari server at
startup. Julia sends commands to Napari and receives events from it. Julia does not
execute Python code strings — it sends structured JSON messages only.

**Example: user draws a gate in Vue → population appears in Napari**

```
1. User draws polygon gate in Vue scatter plot
         ↓ WebSocket  (Vue → Julia)
   {type: "gate_defined", name: "CD4+", gate: {type: "polygon",
    channels: ["CD4","CD8"], coords: [...]}}

2. Julia evaluates gate against AnnData in-process (PythonCall.jl)
   → computes which label_ids pass: [1, 45, 102, ...]
   → stores population in PopulationStore

3. Julia notifies Vue of new population
         ↓ WebSocket  (Julia → Vue)
   {type: "population_added", name: "CD4+", colour: "#FF0000", count: 312}

4. Julia instructs Napari to colour those cells
         ↓ WebSocket  (Julia → Napari server, port 7655)
   {type: "set_populations", uid: "img_001",
    populations: [{name: "CD4+", label_ids: [1,45,102,...], colour: [255,0,0]}]}

5. Napari WebSocket thread receives message
   → schedules Qt-safe update: QTimer.singleShot(0, lambda: recolour_labels(...))

6. Napari main thread executes update
   → labels layer recoloured, red cells appear in viewer
```

**Example: user selects cells in Napari → Vue plots update**

```
1. User clicks/lasso-selects cells in Napari labels layer
2. Napari layer event fires on main thread
3. Event handler collects selected label_ids
4. Sends to WebSocket server thread (thread-safe queue)
5. WebSocket server sends to Julia
         ↓ WebSocket  (Napari → Julia)
   {type: "cell_selection", uid: "img_001", label_ids: [45, 102, ...]}

6. Julia broadcasts to Vue
         ↓ WebSocket  (Julia → Vue)
   {type: "cell_selection", uid: "img_001", label_ids: [45, 102, ...]}

7. Vue plots highlight selected cells
```

**How Napari is launched:** Julia spawns the Napari process at startup using
`run(`python napari_server.py`, wait=false)` and waits for a ready signal over
the WebSocket before accepting user interactions that involve the viewer.

---

## Architecture Constraints

- **Single user, local machine only.** No auth, no multi-tenancy, no remote access.
- **Concurrent tasks.** One user runs many tasks in parallel. Julia manages a task
  queue with one `@async` monitor coroutine per task, polling the task's log file.
- **One AnnData writer at a time.** Tasks that write AnnData must serialise via a
  Julia-side lock. ZARR reads during writes are fine (user watches segmentation
  progress live in Napari).
- **Task lifecycle:** spawned → running → complete/failed/cancelled. User can cancel
  mid-run (SIGTERM to subprocess). Task history persists to disk as JSON.
- **Julia is single source of truth.** Holds all state: image list, AnnData,
  populations, task queue. Pushes to Vue and Napari. Neither holds independent state.
- **Performance target:** fast enough not to annoy the user. Tasks themselves take
  minutes to hours — orchestrator latency is irrelevant.

---

## Key Reuse Opportunity

The JSON input definitions in `inst/app/modules/inputDefinitions/` describe every
module's parameter UI declaratively. These do not need to be rewritten. The Vue
frontend should implement a single `DynamicWidget` component that reads widget `type`
from JSON and renders the correct Vue component. All module UIs are then generated
from the existing JSON files at no additional cost.

---

## Phase 0 Request (this session — design only, no code)

Please produce a design document with the following sections:

### 1. System diagram
ASCII diagram showing how Vue, Julia, Napari server, and Quarto communicate at
runtime. Include data flow directions and transport layer (WebSocket, HTTP, function
call, subprocess, PythonCall.jl in-process).

### 2. Project folder structure
Top-level layout for the project. Annotate what lives where and why. Don't go
deeper than two levels — we don't need every filename, just the shape of the project.

### 3. Tech stack decisions
For each of the following, give a specific choice and one sentence of reasoning.
Do not hedge or give multiple options — pick one:
- Julia WebSocket library
- Julia async task strategy (`@async` vs `Distributed.jl` vs threads)
- Vue router (if any)
- Charting library for Vue (needs: scatter, UMAP, heatmap, frequency bar, gating plot)
- Napari server framework
- Task history persistence format
- How `DynamicWidget` maps JSON widget type strings to Vue components
- How Julia calls Python (PythonCall.jl vs subprocess — and when to use which)

### 4. Four key integration points
Describe precisely how each of these interactions works end-to-end. The Napari
architecture section above already covers the gating and cell selection flows as
examples — use those as a reference and fill in the remaining two:
- User clicks "Run Task" in Vue → Julia spawns subprocess + monitor
- Julia task monitor detects progress → Vue task manager updates in real time
- User selects cells in Napari → Vue plots update (see Napari section above)
- User defines a gate in Vue → Napari label colours update (see Napari section above)

### 5. Rough sketch of subsequent phases
Not a detailed spec — just a ordered list of phases with a one-line description of
what gets built and what the user can do at the end of each phase. The point is to
give a sense of the build order and dependencies, not to commit to scope. We will
write the detailed prompt for each phase just before starting it.
