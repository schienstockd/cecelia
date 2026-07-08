# Audit: Python Task + Utils Classification

Opus planning pass. Produce a classification report and mesh/spatial feasibility assessment. No code changes. Sonnet executes based on your findings.

## End goal

Julia owns all computation except napari. Napari stays Python for now — no Julia alternative exists. Everything else is a candidate for Julia. Default to Julia unless there is a concrete blocker.

## Read first

All files under:
- `app/py/tasks/` — task scripts called by Julia (new pineapple repo, `main` branch)
- `app/py/utils/` — shared Python utility classes and functions
- `old-R-shiny-version/inst/modules/sources/` — the full legacy module list
- `../coastal/` — separate optical flow segmentation package in the same workspace. Read its Python utils and helpers alongside cecelia's. Flag any utils shared between the two packages — porting these to Julia once serves both. Flag every R module that has no corresponding Python task in the new repo — these are unported modules that need to be accounted for (either port to Julia, port from R to Python first, or explicitly drop).

## Classification: tasks

For each file in `app/py/tasks/`, classify as:

**PORT_JULIA** — port to Julia if ALL of:
- Pure numerical/array ops (FFT, filters, morphology, geometry, stats)
- No DL inference (no torch, no Cellpose, no StarDist)
- No napari dependency
- No Python-only library with no Julia equivalent

**KEEP_PYTHON** — keep if any of:
- Calls Cellpose, btrack, StarDist, napari, torch
- Wraps a Python-only library with no viable Julia equivalent
- Complexity of port outweighs benefit

**UNPORTED** — exists in legacy R but has no task in `app/py/tasks/`. Flag with: (a) should it be ported to Julia directly, (b) does it need a Python intermediate, or (c) should it be dropped.

**UNCERTAIN** — flag explicitly with reason

For PORT_JULIA candidates: name the Julia packages that would replace the Python dependencies.

## Classification: utils

For each class/module in `app/py/utils/`, classify as:

**ELIMINATE** — Julia already has an equivalent (e.g. `label_props_utils.py` is mirrored by `LabelProps` chain)

**KEEP_PYTHON_ONLY** — needed only by KEEP_PYTHON tasks

**SHARED** — consumed by both Julia and Python tasks. Flag as **DUPLICATION_RISK** — if a PORT_JULIA task uses it, the util needs a Julia equivalent or the port must reimplement it.

**CONSOLIDATE** — overlapping utils doing the same thing

## Spatial analysis + mesh (investigate specifically)

Spatial analysis is the one major outstanding module. Investigate Julia alternatives for:

- **Mesh construction from label masks** — `Meshes.jl`, `GeometryBasics.jl`, `MarchingCubes.jl`. Can these replace the trimesh/marching cubes Python pipeline that produces per-cell `.stl` files?
- **Mesh property measurement** — surface area, volume, convex hull, sphericity, compactness, ellipticity. Are these computable from `Meshes.jl` primitives?
- **Cell-cell contact detection (mesh collision)** — does Julia have a viable BVH/collision detection library? `AcceleratedArrays.jl`, `NearestNeighbors.jl`, or similar? This is the hardest part — flag if no viable path exists.
- **Neighbour detection (non-mesh)** — spatial KD-tree neighbour queries. `NearestNeighbors.jl` is mature here.

For each: feasible in Julia (yes/no/partial), recommended library, known gaps vs. the Python trimesh implementation.

## Report format

### Tasks
| File | Classification | Reason | Julia packages (if PORT_JULIA) |
|---|---|---|---|

### Unported legacy modules
| R module | Status | Recommendation |
|---|---|---|

### Utils
| Module/Class | Classification | Notes | Duplication risk |
|---|---|---|---|

### Spatial/mesh feasibility
Narrative section — what's feasible in Julia, what's the gap, recommended approach.

### Duplication risks (summary)
List every util that would need to exist in both Julia and Python. For each: write a Julia equivalent or keep the task Python.

### Recommended port order
Ordered by effort/benefit. Drift correct and AF correct already identified as first candidates.

## Notebooks: port QMD/Jupyter to Pluto.jl

If Python tasks fall away, QMD and Jupyter notebooks lose their justification — they existed because of Python. Replace with Pluto.jl notebooks.

- Port existing `.qmd` and `.ipynb` vignettes/examples to Pluto notebooks
- Create a small `notebooks/` directory with example Pluto notebooks showing how to use `Cecelia.jl` from the REPL/notebook: load a project, run a module function, access population data, plot results
- The Vue frontend gets a **Playground** link that launches Pluto pointing at `notebooks/`— custom analysis not yet in the GUI
- Pluto does NOT connect to the running Julia API server — that's what the debug console is for. Pluto runs its own Julia session against the package directly (`using Cecelia`)
- **Notebook management**: users can add and remove notebooks from the Playground via the UI — not locked to the shipped examples. Same table pattern as the image table, with a description/notes field per row (similar to the exclusion notes already on the image table, but here describing what the notebook does). Users can annotate their own notebooks with a short description visible in the table.
- Flag any QMD/notebook that cannot be ported (e.g. still depends on Python-only output) as KEEP_QMD with reason


Do not recommend porting anything that creates net complexity increase. If a util would need duplicating just for one small task, that task stays Python.


## Additional considerations

**Notebook versioning**: custom user notebooks belong in the project directory, versioned with the project — not a global `notebooks/` folder. A notebook for experiment A should not appear in experiment B.

**Pluto startup time**: Pluto spins a new Julia session on launch — cold-start recompilation is slow. Investigate `DaemonMode.jl` or a persistent Pluto server. Flag recommendation.

**Shared package**: `coastal` will depend directly on `Cecelia.jl` — no separate shared package needed. Note: `coastal` currently only uses Cecelia Python helpers in its Jupyter notebooks — the package itself does not depend on them. Migrating those notebooks to Pluto severs the Python dependency cleanly. Flag `coastal` as a candidate for full Julia migration once it can depend on `Cecelia.jl` directly.

**Python env shrinkage tracking**: as tasks move to Julia, flag which Python dependencies become removable per port batch. Track explicitly — don't let orphaned deps accumulate in `pixi.toml`.

**napari footprint**: once all other Python is gone, napari is the last dependency. Audit exactly what napari pulls in (PyQt5/PySide2, vispy, etc.) so the cost of eventually dropping it is visible. Don't drop it now — but know the number.

**napari replacement (longer term — think independently)**: the goal is eventually a Julia-native viewer. Key constraints discussed:
- Must be Qt-based, not WebGL/browser — not a step backwards
- Full 3D volumetric rendering + time — not Z-slider slicing, that would be a step backwards
- Required layers: labels, points, tracks, spots
- Lazy pyramidal Zarr loading is the hardest part — no LOD tile scheduler exists in GLMakie
- napari's plugin system is not needed — Cecelia owns the full stack
- No need for volumetric raycast rendering (Blender-style) — standard orthogonal slicing with Z/T slider is sufficient

Known Julia candidates: `GLMakie.jl` for rendering, `Mousetrap.jl` for Qt, `Zarr.jl` for data. The gap is the microscopy scaffolding napari built over years (pyramid LOD, label colourmap shader, linked cursor planes). Rough estimate: months of focused work for something usable, not years.

Opus: think independently on this. Is there a more direct path? Could Cecelia embed napari as a pure display component with no plugin system, stripping it to just the viewer? Could `GLMakie` with a custom tile scheduler close the gap faster than a ground-up Qt build? Flag your own recommendation.

After writing your assessment, add an entry to `docs/FUTURE.md` documenting the Julia-native viewer as a deferred goal. Read the existing `FUTURE.md` first — match its format and tone exactly.
