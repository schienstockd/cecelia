# Phase: TrackMeasures.jl — Port celltrackR Measurement Functions to Julia

This is a focused porting phase. The math is not complex but must be numerically faithful to the R originals. Work one step at a time, stop and report after each step, re-read this document before starting the next step.

Reference CLAUDE.md and ARCHITECTURE.md. This module lives in `Cecelia.jl/src/` and follows the same REPL-first, headless-runnable contract as everything else in the package.

## Attribution

This module is a Julia port of measurement functions from the **celltrackR** R package (Wortel et al., 2021, doi:10.1016/j.crmeth.2021.100006; https://github.com/ingewortel/celltrackR), developed by Inge Wortel and Johannes Textor as part of the MotilityLab project. celltrackR is released under GPL-2.

Include a prominent attribution comment at the top of every source file in this module:

```julia
# TrackMeasures.jl
# Julia port of track measurement functions from the celltrackR R package
# Original authors: Inge Wortel, Johannes Textor (MotilityLab / Radboud University)
# Source: https://github.com/ingewortel/celltrackR
# Reference: Wortel et al. (2021) Cell Reports Methods, doi:10.1016/j.crmeth.2021.100006
# This port covers the measurement subset used by Cecelia. Simulation functions
# (random walk models, bootstrapping) are not included.
# Port is not one-to-one — see inline notes where behaviour was corrected or extended.
```

Where individual functions deviate from the original (corrected, extended, or renamed), add an inline comment citing the original function name and the reason for the change.

## Latitude to correct or extend (use it, but be explicit)

You are not required to replicate celltrackR exactly. If during the port you find:
- A numeric edge case handled poorly in the original (e.g. division by zero not guarded, single-step tracks returning `Inf`)
- A more natural Julia idiom that produces the same result more clearly
- A closely related measure that would be trivially cheap to add alongside an existing one (e.g. if implementing `mean_turning_angle`, `std_turning_angle` is one line more)
- A measure that Cecelia's vignettes appear to expect but that isn't in celltrackR

...then make the correction or addition. But:
- **Flag every deviation explicitly** — in code as an inline comment, and in your Step 3 report as a named list: function, what changed, why
- **Don't add measures speculatively** — only add something if it is either a clear correction or has direct evidence of use in Cecelia
- **Don't silently change numeric behaviour** — if you change how a measure is computed, the cross-check in Step 4 must still verify the common-case output matches the original

## Background

Cecelia uses celltrackR (R package) to compute track statistics from tracked cell coordinates — speed, turning angle, displacement, confinement ratio, etc. These are then used as inputs to the HMM behaviour analysis and for population-level motility comparisons. celltrackR is an R-only package and has no Julia equivalent. We are porting the subset of it that Cecelia actually uses. We are NOT porting the simulation half (random walk models, bootstrapping, `simulateTracks`).

## Step 1 — Audit: what does Cecelia actually call? (read before writing any code)

Before writing a single function, read the following files to build an exact list of celltrackR functions Cecelia uses:

- `R/cciaImage.R` — find the `tracks()` function and every celltrackR call within it
- `R/trackHelpers.R` — all celltrackR calls
- Any vignette `.Rmd` files that call celltrackR functions directly (search for `celltrackR::` or any imported function names like `speed`, `meanTurningAngle` etc.)

Also read the following celltrackR source files for the exact implementations of any functions you find in the audit:
- `celltrackR/R/measures.R` — the core measurement functions
- `celltrackR/R/tracks.R` — track data structure and accessor methods

The celltrackR source is at `https://github.com/ingewortel/celltrackR` — it is accessible in the repo. Read the actual R implementations, not just the documentation, so the port is numerically faithful (e.g. how exactly turning angle is computed, whether step-based or cell-based is the default, how edge cases like single-step tracks are handled).

Report back: exact list of celltrackR functions used, their signatures, and any non-obvious numeric details (edge cases, step vs. cell based, 2D vs. 3D handling). Do not proceed to Step 2 until this list is confirmed.

Also read and understand the following Cecelia-own functions in `R/cciaImage.R` — these are not celltrackR functions, they are Cecelia's higher-level wrappers that the rest of the codebase calls directly:

- **`trackMeasures()`** — the primary accessor that returns per-cell/per-track computed motility statistics (the `live.cell.speed`, `live.cell.angle` etc. columns). This is what the HMM behaviour module reads as `modelMeasurements`. Understand exactly: what it loads, what it computes or reads from cache, what it returns (DataFrame shape, column naming convention), and whether it writes anything back to H5AD.
- **`trackInfo()`** — track structural metadata: duration, number of steps, which population a track belongs to, whether it passes quality filters. Understand its return shape and what downstream code depends on it.

Both need Julia equivalents with the same semantics. They are the public API that the rest of Cecelia calls — the raw measure functions from celltrackR are the implementation underneath. In Julia, the layering should be:

```
track_measures(cciaObj; ...)   # Cecelia.jl equivalent of trackMeasures()
track_info(cciaObj; ...)        # Cecelia.jl equivalent of trackInfo()
    ↓ calls into ↓
speed(track), mean_turning_angle(track), ...   # ported celltrackR measure functions
```

Include the signature and return contract for both in your Step 1 report.

## Step 2 — Input data contract

Track data in Cecelia lives in H5AD files. Before implementing any measure, establish the exact input contract:

- Track coordinates come from H5AD — determine the exact column names and structure used (check the real H5AD at `projects/NRUBxU/1/KDIeEm/labelProps/` and any live-cell H5AD files available)
- Track ID: which column in `obs` identifies which cell belongs to which track
- Time: which column or `obsm` array encodes the timepoint
- Spatial coordinates: which columns or `obsm` arrays encode x, y, (z)
- Time step: is it uniform? Is it stored per-image or per-frame?

The input to every measure function should be a `Track` struct or equivalent — a time-ordered sequence of positions for one cell. Establish this struct before implementing any measures:

```julia
struct Track
  id::String
  t::Vector{Float64}       # timepoints
  coords::Matrix{Float64}  # n_steps × n_dims (2D or 3D)
end
```

Or adapt based on what you find in the H5AD. Report the input contract before Step 3.

## Step 3 — Implement measures

Implement only the functions identified in Step 1's audit. Likely set (confirm from audit):

- `speed(track)` — mean speed: total path length / duration. Also `step_speeds(track)` for per-step speeds.
- `displacement(track)` — Euclidean distance from first to last position
- `square_displacement(track)` — squared displacement (for MSD)
- `displacement_ratio(track)` — displacement / total path length (confinement ratio / straightness)
- `outreach_ratio(track)` — max displacement from start / total path length
- `mean_turning_angle(track)` — mean angle between consecutive step vectors
- `duration(track)` — last timepoint minus first timepoint
- `track_length(track)` — total path length (sum of step distances)
- `angle_to_x_axis(track)` — if used in Cecelia (check audit)

Each function:
- Takes a `Track` struct (or Vector of positions)
- Returns a scalar (per-track) or Vector (per-step where applicable)
- Handles edge cases: single-step tracks (return `NaN` or `missing` for measures that need ≥2 steps), zero-displacement tracks
- Works for both 2D and 3D coordinates

Verify each implementation numerically against the R original: pick a short hand-computable track (3-5 positions), compute expected values in R using celltrackR, assert Julia matches. This cross-check is mandatory — do not skip it.

## Step 4 — Verify numeric faithfulness

For each implemented measure: compute it on the same 5-position test track in both R (celltrackR) and Julia. Assert values match to float tolerance. If any diverge, find and fix the discrepancy before proceeding.

Also test:
- Single-step track: assert no crash, returns `NaN`/`missing` as appropriate
- Zero-displacement track: assert `displacement_ratio` doesn't divide by zero
- 2D vs. 3D: assert the same function handles both without branching at the call site

## Step 5 — Storage: cache to H5AD, invalidated by the tracking task

Track measures are deterministic functions of track coordinates. Two options: derive every time (what the old Cecelia did — no storage problem, always fresh) or cache to H5AD obs columns. The old approach was derive-every-time only because there was no clean storage solution. Use the cache approach here, with one simple invalidation rule:

**Track measure columns are owned by the tracking task.** When btrack runs and produces new tracks, it deletes or overwrites the existing measure columns as part of that task. The task boundary is the invalidation event — no separate invalidation logic needed elsewhere.

`track_measures` checks whether measure columns already exist in H5AD for this image. If present and non-stale, read and return them via the `LabelProps` fluent reader. If absent or stale, compute from track coordinates, write back to H5AD, then return.

**No Python involved.** Track measures are computed entirely in Julia from track coordinates. Julia writes the resulting columns directly to H5AD via `HDF5.jl`. The read-Julia / write-Python split applies to files that Python modules own (Cellpose output, btrack output etc.) — columns that Julia computes are Julia's to write. No PythonCall.jl needed here.

**AnnData encoding attributes — read this before writing anything.** H5AD is HDF5 with AnnData metadata conventions on top. Every dataset in `/obs/` must have two HDF5 attributes: `encoding-type` and `encoding-version`. For plain float arrays (which all track measures are), this is `encoding-type = "array"` and `encoding-version = "0.2.0"`. Without these, `anndata.read_h5ad()` in Python will warn or misread the column. Before implementing the write path: open the real `KDIeEm` H5AD, read the attributes on an existing float obs column (e.g. an intensity measurement), and confirm the exact attribute names and values Cecelia's AnnData files use. Write new columns with identical attributes.

This is also a **general data model decision** that needs resolving for the whole project, not just track measures. As Cecelia.jl takes on more writing responsibility (any Julia-computed column that goes back to H5AD), the question of what Julia must do to produce AnnData-compatible output will recur. Flag this in your Step 5 report with a recommendation: either a thin `write_obs_column(path, name, data)` utility in Cecelia.jl that always writes the correct AnnData attributes (a ~10 line helper that centralises this concern), or a decision that all H5AD writes go through Python regardless of who computed the data. This should be documented in `docs/DATAMODEL.md` (or added as a section if the file already exists) so it becomes the established pattern rather than being re-solved per module.

**Workaround while the data model utility is being designed:** do not block track measures on the `write_obs_column` utility being finalised. For now, route the H5AD write through Python via PythonCall.jl — call `anndata.read_h5ad()`, assign the new columns to `adata.obs`, call `adata.write_h5ad()`. This is guaranteed AnnData-compatible and is a known-correct placeholder. Mark the write path clearly with a `# TODO: replace with write_obs_column() once data model utility is settled` comment so it is easy to find and swap out. The compute path (Julia doing the actual measure calculations) is unaffected by this — only the write step is temporary.

**Workaround for batch column writes:** HDF5 doesn't append columns cheaply — each new dataset write touches the file separately. Don't write measure columns one at a time. Compute all measures for all tracks first, collect them into a single DataFrame, then write all columns in one open/write/close cycle. Whether going through Python or a future Julia utility, this is the same rule: batch the results, single write operation, not a loop of individual column writes.

This batch-write rule applies retroactively across the codebase — not just track measures. When reviewing or touching any other module that writes to H5AD (population manager, gating, clustering, spatial analysis), check whether it is writing columns one at a time and refactor to batch if so. Note any instances found during this phase in your Step 5 report so they can be tracked and fixed systematically rather than left for later discovery.

The public API that the rest of Cecelia calls:

```julia
# mirrors cciaImage$trackMeasures() and $trackInfo()
track_measures(cciaObj; value_name="default", measures=[:speed, :displacement_ratio, :mean_turning_angle])
track_info(cciaObj; value_name="default", pops=["tcells/tracked"])
```

Both work headless, no API, no Vue. Column naming matches the existing `live.cell.speed`, `live.cell.angle` convention so downstream modules (HMM, vignettes) need no changes.

## Step 6 — Verify end-to-end

- Run `track_measures` on image `KDIeEm` — assert measure columns appear in H5AD with correct naming
- Run `track_measures` a second time — assert it reads from cache, does not recompute
- Simulate a btrack rerun (delete measure columns) — assert `track_measures` recomputes and writes back correctly
- Assert values are in plausible ranges (speed > 0, displacement_ratio between 0 and 1, angles between 0 and π)
- Cross-check a sample of per-cell values against old R output for the same image if available

## Out of scope

- Track simulation (random walks, bootstrapping) — not needed
- MSD curves / autocorrelation plots — analysis outputs, belong in a plotting phase
- Track clustering / `trackFeatureMap` — belongs in the clustering module phase
- Quality control / gap repair (`repairGaps`, `interpolateTrack`) — later phase if needed; btrack already handles gap closing

## Documentation

Once the implementation is complete and verified, write `docs/TRACKING.md` documenting this component. Follow the same structure and level of detail as the existing docs for other components — consistency matters here. The document should cover:

- Architecture overview: the two-layer design (`track_measures`/`track_info` public API over raw measure functions) and why it mirrors `cciaImage$trackMeasures()`/`trackInfo()`
- The `Track` struct and input data contract (where coordinates come from in H5AD, column naming)
- The read-Julia / write-Python split and why
- Each public function: signature, return shape, column naming convention
- The celltrackR attribution and which measures were ported vs. extended vs. corrected, with reasons
- REPL usage examples matching the vignette patterns already established in the codebase
