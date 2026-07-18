# Phase: Population Manager + Gating UI

## How to use this document

This is an **Opus planning prompt**. Your job is to produce a detailed, step-by-step implementation plan — not to write code yet. Sonnet will execute against your plan. Read every referenced file before producing the plan, then design something informed by the logic in those files but not constrained to replicate their structure.

## Files to read first (before producing the plan)

The following files are in the repository. Read them all thoroughly before planning anything:

- `R/populationUtils.R` — generic population utilities: pop type conventions, path logic, pop map structure
- `R/populationManager.R` (or equivalent) — the generic population manager used across all pop types
- `R/flowHelpers.R` — flow-specific helpers: path normalisation, root conventions, pop tree traversal
- `inst/app/modules/server/gatePopulationsServer.R` — gating UI server (~1100 lines): gate shapes, recompute, re-entrancy guard, cascade delete/rename, multi-plot state, Napari link
- `inst/app/modules/server/plotFlowGatingServer.R` — the scatter plot rendering server for gating
- `inst/app/modules/server/flowPlotManager.R` (or equivalent) — manages multiple simultaneous gating plot panels
- `inst/py/pop_utils.py` — Python-side population utilities: how populations are stored in AnnData, how Python reads/applies population membership
- `inst/py/labels_props_utils.py` — chainable H5AD accessor utility
- `inst/py/label_props_view.py` — view layer of the chainable accessor; together with `labels_props_utils.py` this is the pattern to preserve and improve

Use these files as the source of truth for what the population manager needs to do. Pay particular attention to:
- How population types (`"flow"`, `"live"`, `"clust"`) differ and what they share
- How `popDT()` works as a unified accessor across types
- How `pop_utils.py` stores and retrieves population membership from AnnData — this interface must be preserved (or deliberately replaced) in the new design
- How the population map is structured on disk

## Context: what you are designing

### The population manager is generic

The population manager is not a gating module. It is the central abstraction for all population types in Cecelia:

- **`"flow"`** — populations defined by gates (polygon, rectangle) on scatter plots of cell measurements
- **`"live"`** — populations defined by tracking (cells linked across timepoints by btrack)
- **`"clust"`** — populations defined by clustering (Leiden, UMAP cluster membership)

All three types share: a population map (name, colour, path, parent), persistence to disk, population membership stored in AnnData, and a unified `pop_dt()`-style accessor that returns a filtered DataFrame regardless of type. The gating UI and the gate engine are built on top of the population manager, not the other way around.

### Julia + Python interface must match

Population membership must be readable from both the Julia side (Cecelia.jl, for analysis and REPL use) and the Python side (for AnnData writes, Napari label colouring, and any Python-backed module that needs filtered cell data). The original `inst/py/pop_utils.py` did this for R→Python. The new design must do the same for Julia→Python.

Concretely: whatever format population definitions are stored in (JSON in the population map, AnnData `.obs` columns, or both), Julia and Python must be able to read and apply those definitions independently. Design the interface so that:
- Julia can call `pop_dt(cciaObj, pop_type, pops; uIDs)` and get a DataFrame
- Python can call the equivalent and get a pandas DataFrame from the same underlying data
- They agree on what "membership" means for each population type

### The gating engine

The gating engine is the Julia implementation that evaluates gate definitions against cell measurement data. It does NOT wrap flowWorkspace, flowCore, or any R library. It does NOT parse FCS files. It gates directly on image-derived AnnData measurements — a polygon gate is a point-in-polygon test, a rectangle gate is four comparisons. See Phase 0 document for full rationale.

Gate definitions serialise to JSON so both Julia and Python can read them.

### The gating UI

The gating scatter plot is module-page-only — not a whiteboard node. It renders millions of points (flow cytometry scale) at 60fps using WebGL. Key decisions already made:

- **`regl-scatterplot`** for WebGL point rendering — handles millions of points, accepts `Float32Array` directly
- **`canvas2D` overlay** for gate drawing — separate from the WebGL canvas, positioned on top
- **Binary data transfer** — scatter plot data served as raw `Float32Array` (not JSON) from the API
- **Density fallback** — for very large datasets, Julia bins to a 2D histogram server-side; Vue renders as heatmap; gate drawing layer is identical
- **No lasso selection** — gates are permanent population definitions, not transient selections. Every drawn shape defines a named, saved, persisted population.
- **Re-entrancy guard** — when the server pushes updated gate shapes to the client for rendering, the client must not re-emit gate-mutation events. Vue ref `listeningToGating`, set false before programmatic redraw, true after.

## What the plan must cover

Structure the plan as numbered steps. Each step should be self-contained enough that Sonnet can execute it, stop, and report back before the next step begins. Flag dependencies between steps explicitly.

The plan must cover at minimum:

**Population manager (Cecelia.jl)**
- Data structures for the population map (all three types)
- Population map persistence format (what goes to disk, in what format)
- `pop_dt()` unified accessor for all three types
- REPL contract: `pop_dt(cciaObj, "flow", ["cd4+/cd8+"]; uIDs=...)` must work without API, without Vue
- How population membership is stored in AnnData `.obs` (so Python can read it)

**LabelProps fluent API (Julia-native, reading H5AD via HDF5.jl)**
The original `labels_props_utils.py` / `label_props_view.py` provided a chainable accessor for H5AD cell measurement data. The equivalent in the new stack should be **Julia-native**, not routed through PythonCall.jl — H5AD is just HDF5 under the hood, and `HDF5.jl` reads it directly. For the hot path (loading cell measurements for gating, analysis, `pop_dt()`), going through the Python bridge adds overhead for no benefit.

The fluent API should be Julia-native and lazy — column selection and population filters resolve before any HDF5 read, so only the requested columns are loaded from disk:

```julia
LabelProps(cciaObj; value_name="tracks.clusters.tcells.movement.sc")
  |> filter_pops(["cd4+"])
  |> select_cols(["area", "intensity_mean"])
  |> sort_by("area", rev=true)
  |> as_df()   # only here does HDF5 I/O happen
```

**Read/write split:**
- **Read** → Julia via `HDF5.jl` — fast, lazy column access, works headless from the REPL with no Python initialisation
- **Write** → Python via PythonCall.jl — Python modules (Cellpose, btrack, clustering) write H5AD; safer to let Python maintain format compatibility for files it wrote

**Python `pop_utils.py` / `labels_props_utils.py`** remain as a parallel implementation for Python-backed modules that need to filter their own output data internally — not the primary data access path.

**Caveat for Opus**: AnnData encodes some column types non-trivially in HDF5 (categoricals, sparse matrices, string arrays). Before designing the Julia reader, inspect the actual H5AD file from image `KDIeEm` in the project — this is a real Cecelia-produced H5AD file. Open it with `HDF5.jl` or `h5ls`/`h5dump` and audit the actual column types, group structure, and any AnnData-specific encoding metadata (`encoding-type`, `encoding-version` attributes). Flag any column types that are not plain float/int arrays — those need explicit handling in the Julia reader. Do not assume a structure; read the file first.

**Julia/Python interface**
- What Python `pop_utils.py` equivalent looks like in the new stack
- How Julia calls it via PythonCall.jl vs. reading shared formats directly
- What the contract is: given a population path and type, both sides return the same cells

**Gating engine (Cecelia.jl)**
- Gate type structs (polygon, rectangle — extensible to ellipse/quadrant later)
- GatingSet: population tree, multiple named sets per image
- `recompute!`: applies gates in tree order, parent membership propagates to children
- Cascade delete and rename
- Gate definition serialisation (JSON, readable by Python)
- REPL contract: `add_pop!`, `set_gate!`, `recompute!`, `cells_in_pop`, `save_pop_map!` all work headless

**API layer**
- Routes for gating operations (add, edit, delete, rename population; get plot data; get density)
- Binary Float32 response format for scatter plot data
- WebSocket push after any population mutation (tree update to all connected clients)

**Vue gating component**
- regl-scatterplot setup, binary data loading
- canvas2D overlay for gate drawing (polygon + rectangle)
- Population tree panel (hierarchical, all three pop types eventually — design generically)
- Multiple simultaneous scatter plot panels
- Re-entrancy guard implementation
- Napari bidirectional link (reuse existing Napari WebSocket channel)

**Verification steps**
- Headless Julia tests for the population manager and gating engine
- End-to-end gating flow test
- Julia/Python interface agreement test (same cells returned from both sides for a given population)

## Out of scope for this phase

- Lasso/transient selection
- Compensation matrices
- FCS file import/export
- flowWorkspace compatibility
- Clustering UI (populations created by clustering are consumed by the population manager but the clustering module itself is a separate phase)
- Tracking UI (same — tracking populates the `"live"` type, the population manager just reads it)
