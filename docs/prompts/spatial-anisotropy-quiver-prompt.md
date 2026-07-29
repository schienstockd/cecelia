# Opus Plan: Spatial Anisotropy, Quiver Plots + Temporal Network

Opus planning pass. Read everything listed before producing a plan. No code yet.

## Read first

**Legacy vignettes** — read all of them, not just behaviourTcells3P.Rmd. Search every
`.Rmd` in `old-R-shiny-version/vignettes/` for:
- `ggquiver::geom_quiver` usage
- `ilee_` prefixed variables (ILEE eigenvector data)
- `spatialAnalysis.networkWeights`
- `segment.binariseTracks`
- any other spatial/anisotropy/quiver patterns

The vignettes are the ground truth for what readouts researchers expect. Read the
actual R code, not just comments.

**Legacy spatial analysis modules** — `old-R-shiny-version/inst/modules/sources/spatialAnalysis/`
Find `networkWeights` and any ILEE-related module.

**New codebase** — confirm what branching already delivers (PR #396 is merged or
nearly so): skan skeleton, branch-type labels, napari visualisation. The quiver
and temporal network build on top of this foundation.

**CytoMAP MATLAB source** — check whether CytoMAP has any equivalent anisotropy
or vector field computation that could inform the approach.

---

## What this builds

Three related but distinct capabilities:

### 1. ILEE anisotropy computation (structure tensor)

ILEE (Image Local Eigenvector Estimation) computes the local orientation of
fibrous structures (collagen SHG, vessel networks) from image intensity. For each
spatial grid point, it returns:
- `ilee_coor_list` — grid point coordinates
- `ilee_eigval` — eigenvalues (anisotropy magnitude)
- `ilee_eigvec` — eigenvectors (orientation direction)

These feed the quiver arrows: `u = eigval_1 * eigvec_1_x`, `v = eigval_2 * eigvec_2_y`.

**Implementation options** (Opus to evaluate and recommend):
- `scikit-image` structure tensor (`skimage.feature.structure_tensor`) via
  PythonCall.jl — mature, well-tested, directly applicable
- Julia native: `Images.jl` has some structure tensor support — assess whether
  it's mature enough
- The old R version used a custom implementation — check what it actually did

The output should be stored in the image's H5AD `uns` dict (matching the old
`ilee_coor_list` etc. naming for backward compatibility) or in a dedicated file
under the task directory. Opus: recommend the storage format.

This is a **module function** in the spatial analysis module — `spatialAnalysis.computeAnisotropy`
or similar. REPL-runnable, same contract as all other module functions.

### 2. Temporal network: branching over time with user choice

The branch/skeleton currently runs per-timepoint (or on a single static image).
For live imaging, the user needs a choice:

**Per-timepoint network**: one skeleton per timepoint — shows how the structure
changes over time. Computationally expensive for long movies.

**Max-projection network**: collapse all timepoints into one network by taking the
maximum intensity projection over t, then skeletonise once. Gives a single
representative network for the whole movie. Computationally cheap. This is what
the old `segment.binariseTracks` approach approximated — a single spatial summary
of where structures existed across the whole timecourse.

**User controls**:
- Toggle: "Per timepoint" vs "Max projection over t"
- If per timepoint: which timepoints to include (all, or a range)
- If max projection: which channels to include in the projection

The max-projection network is the default for most intravital use cases —
researchers typically want one network for the whole movie to correlate with
cell tracks pooled across time.

Implementation: max projection is a pre-processing step before skeletonisation.
The existing branching module (`branching_run.py`) gets a `temporalMode` param:
`"single"` (current behaviour, uses one timepoint or a static image) or
`"max_projection"` (collapses t first, then skeletonises).

### 3. Quiver plot on the spatial module page

A 2D plot showing:
- **Quiver arrows**: one arrow per grid point, direction = eigenvector,
  length = eigenvalue (anisotropy magnitude). Shows local structure orientation.
- **Track overlay**: cell tracks from `pop_dt("live", ...)` drawn on top.
  Coloured by population, HMM state, speed, or turning angle — user selects.
- **Optional**: branch/skeleton overlay from the temporal network.

**Controls**:
- Which segmentation/channel provides the anisotropy (SHG, vessels, etc.)
- Which population's tracks to overlay
- Track colouring: solid colour by population, or coloured by a measure
  (speed, turning angle, HMM state)
- Timepoint slider (for per-timepoint network) or "show max projection"
- Grid density for quiver arrows

**Plot placement**: on the spatial module page (the dedicated spatial analysis page).
Not a floating panel — a first-class module page plot. Follows the same
whiteboard-compatible flagging convention as other module page plots.

**Rendering**: Observable Plot or D3 SVG. The quiver arrows are SVG `<line>` elements
with arrowhead markers — straightforward to implement. NOT regl-scatterplot (no
need for WebGL here, this is a 2D spatial plot not a high-N scatter).

The plot should be exportable as SVG/PNG and the underlying data (arrow coordinates,
track coordinates, colouring values) exportable as CSV — standard export pattern.

### 4. Angle correlation analysis

From the vignette: for each grid point, compute the angle between:
- The local structure eigenvector (collagen/vessel orientation)
- The mean cell movement vector in that grid region

Result: a heatmap showing where cells move parallel vs. perpendicular to the
structure. The vignette used `matlib::angle` for this. Julia equivalent:
`LinearAlgebra.dot` + `acos`.

This is a **separate module function** (`spatialAnalysis.angleCorrelation`) that:
1. Takes the ILEE output and a population's track data
2. Computes per-grid-point angle between structure vector and mean track vector
3. Returns a grid of angles (0°–180°) for plotting

The plot: a heatmap of angles overlaid on the image coordinates, with optional
quiver overlay. Same module page as the quiver plot.

### 5. Network weights

`spatialAnalysis.networkWeights` from the old version: for each branch segment,
count how many T cells passed within `maxDist` pixels of it. Returns a weighted
branch network where branch weight = cell density.

This already existed in the legacy codebase — port it. The output weights are
stored back into the branch H5AD `obs` (as a `weight` column) so the existing
napari branch visualisation can colour by weight without a new rendering path.

---

## Data model decisions (Opus must resolve)

1. **ILEE output storage**: H5AD `uns` (matching old naming), dedicated JSON/HDF5
   file in task dir, or stored in the branch H5AD? Consider: needs to be accessible
   from Julia (for angle correlation) and from the plot API.

2. **Temporal network mode**: does the `temporalMode` param live in the existing
   branching JSON input spec, or is it a new module function? The existing branching
   module already handles the skeletonisation — extending it with a pre-processing
   step (max projection) is simpler than a separate module.

3. **Track-to-grid assignment**: assigning each cell position to the nearest grid
   point for vector field computation. The vignette used `dbscan::kNN`. Julia:
   `NearestNeighbors.jl`. Should this be a shared spatial utility or inline in
   each function that needs it?

---

## MCP accessibility

All analysis outputs must be MCP-accessible in structured form:
- ILEE anisotropy: mean anisotropy per image, grid coordinates + eigenvectors as
  flat arrays
- Angle correlation: per-grid-point angle distribution, mean and std
- Network weights: branch segment → weight table

Same rule as all spatial analysis outputs: structured tables, not raw matrices.

---

## Build order

1. Temporal network mode (max projection param) — simplest, builds on existing
   branching module, unlocks the quiver plot for live imaging data
2. ILEE anisotropy computation module
3. Network weights port from legacy
4. Quiver plot component on the spatial module page
5. Angle correlation analysis
6. MCP tool additions for the new outputs

---

## Constraints

The max-projection temporal mode is NOT the default — it is a user choice. Both modes are equally valid depending on the scientific question. Optimise for neither.
- Quiver plot is SVG/Observable Plot, not WebGL
- ILEE output naming should be backward-compatible with the old `uns` dict naming
  where possible, so old Pluto notebooks that reference `ilee_eigval` etc. still work
- All module functions REPL-runnable without API or Vue
- Export to SVG/PNG and CSV is required for the quiver and angle correlation plots
