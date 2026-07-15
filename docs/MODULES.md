# Module Functions & Pages

This document is the authoritative how-to for adding a new analysis function (task) to Cecelia and wiring it to a module page. Follow it in order — every step is required.

---

## Concepts

**Task** — a unit of work that runs on a single image or a set of images. Always backed by a Julia struct + a co-located JSON spec. Optionally invokes a Python subprocess.

**Module page** — a Vue route that gives users access to one or more tasks for a particular analysis stage (Import, Cleanup, Segmentation, …). A module page is mostly a wrapper around `<TaskRunner>` — the rest is automatic.

**`fun_name`** — the canonical dotted identifier `"category.taskName"` that ties together the JSON spec, the Julia registry, the WS protocol, and the frontend. It is the single source of truth for what a task is.

---

## 1. Julia task handler

Create two co-located files under `app/src/tasks/<category>/`:

```
app/src/tasks/<category>/<name>.jl     ← struct + _run_task
app/src/tasks/<category>/<name>.json   ← param spec (served to Vue via API)
```

The filenames must match (`drift_correct.jl` ↔ `drift_correct.json`).

**Naming convention (keep it consistent — no outliers without a reason):**
- Category dirs are camelCase: `cleanupImages`, `clustPops`, `clustTracks`, `tracking`, …
- `fun_name` is `<category>.<name>` — category camelCase, `<name>` snake_case
  (`clustPops.cluster`, `tracking.bayesian_tracking`, `cleanupImages.afCorrect` (legacy)).
- The Python **runner** is `python/cecelia/tasks/<category>/<name>_run.py` — the `_run` suffix marks a
  subprocess entry point; reusable logic lives in `python/cecelia/utils/*.py`.
- Python subprocess entry points that are **not** scheduler tasks (data-layer writers invoked by
  the engine, e.g. the categorical-obs writer) live in `python/cecelia/writers/`, **not** under `tasks/`.

### Struct

```julia
struct MyTask <: CciaTask end
```

One struct per task. No fields needed.

### `_run_task` signature

```julia
function _run_task(task::MyTask, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
```

Never implement the public `run_task` — only `_run_task`. The scheduler's `run_task` wraps it: validates params, acquires a pool slot, writes the log file, then calls `_run_task`.

### Reading the input image

```julia
value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
ccid       = joinpath(img._dir, "ccid.json")
raw        = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))

filename = versioned_get_field(raw, "filepath", value_name)
if isnothing(filename)
    on_log("[ERROR] No filepath for valueName='$value_name'")
    return nothing
end

proj_dir = dirname(dirname(img._dir))
im_path  = joinpath(proj_dir, "0", img.uid, string(filename))
```

`img._dir` is `{proj}/1/{uid}/` — the metadata directory.  
`{proj}/0/{uid}/` is the image data directory.

### Writing the output + registering it

```julia
out_path       = joinpath(proj_dir, "0", img.uid, "ccidMyResult.zarr")
out_value_name = _spec_output_value_name(task, "myResult")   # from the JSON, not hardcoded
out_filename   = "ccidMyResult.zarr"

# … run the actual work (Python subprocess or pure Julia) …

raw2 = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
versioned_set_field!(raw2, "filepath", out_filename, out_value_name)
open(ccid, "w") do io; JSON3.write(io, raw2); end

return Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
```

Return `nothing` on failure. The scheduler marks the task failed and the frontend shows it in red.

**Declare a fixed output value_name in the JSON, don't hardcode it in the `.jl`.** A producer's
output handle is a **single source of truth**: a top-level `"outputValueName"` in the task spec.
Read it with `_spec_output_value_name(task, "<fallback>")` (`app/src/tasks/task.jl`) rather than
writing a bare string literal, so the whiteboard can introspect it and prefill a downstream node's
input `valueName` (see *Value-name propagation* in `docs/SCHEDULER.md`). The fallback keeps a task
working if the JSON field is ever missing.

```json
{ "task": "cellposeCorrect", "fun_name": "cleanupImages.cellposeCorrect",
  "resource_pool": "gpu", "outputValueName": "cpCorrected", "params": [ … ] }
```

Two other output shapes exist, both already introspectable and not to be re-expressed as a bare
literal: a task whose output name is **user-chosen** exposes an `outputValueName` **param** (e.g.
`segment.cellpose`), and a **composite** declares a top-level `outputValueName` that its executor
collapses `ccid.json` down to (see *`outputValueName` — canonical output registration* below).

**`qcPlot` (optional).** A top-level `"qcPlot": "<plotDefId>"` on a task spec declares its default QC
plot — a `plotDefinitions/*.json` registry id (e.g. `segment.cellposeMeasure` /
`segment.measureLabels` → `"segmentation_qc"`). The whiteboard Live view then auto-shows a QC
thumbnail for that node (see `docs/SCHEDULER.md` → *Live QC row*); no other wiring needed.

### Running a Python subprocess

Spawn the task's Python runner through **`run_py`** (`app/src/py_runner.jl`) — the single place
Cecelia launches Python (the Julia analogue of the old R `self$pyScript`). It writes the params
JSON, sets `PYTHONPATH=python/` (so the script does `import cecelia.*` with **no** `sys.path` bootstrap),
streams stdout (`[PROGRESS] n/total` → `on_progress`, the rest → `on_log`), registers the process
for cancellation, and returns clean-exit (checks both `exitcode` AND `termsignal` — libuv reports
`exitcode==0` for killed procs):

```julia
ok = run_py("tasks/<category>/<name>_run.py",
    (; imPath = im_path, imCorrectionPath = out_path, myParam = params["myParam"]),
    task_run_dir(img._dir);          # set-scope tasks pass the set/project `_dir` instead
    on_log = on_log, on_progress = on_progress, on_process = on_process)
ok || return nothing
```

The params file lands in `task_run_dir(<obj>._dir)` (the run's task dir — `<_dir>/tasks/`, **never**
a temp dir; consistent across every task) and the Python script deletes it after reading. Pass the
exact params your runner expects as the second arg (a `NamedTuple` or `Dict`).

> **Migration note:** several existing tasks (cleanup/segment/tracking) still inline the spawn loop
> and bootstrap `sys.path` in their runner — that older pattern works but is being migrated to
> `run_py`. **New tasks must use `run_py`.** `clustPops.cluster` is the reference example.

---

## 2. Python script

Location: `python/cecelia/tasks/<category>/<name>_run.py`

### Boilerplate

Launched via `run_py`, which sets `PYTHONPATH=python/`, so `import cecelia.*` resolves directly — **no
`sys.path` manipulation**:

```python
"""One-line description of what this script does."""

# `cecelia.*` resolves via PYTHONPATH=python/ (set by the Julia launcher, app/src/py_runner.jl::run_py).
import cecelia.utils.zarr_utils    as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils  as script_utils
from cecelia.utils.dim_utils import DimUtils
# import other cecelia.utils.* as needed (e.g. correction_utils)

import json, argparse

def run(params: dict) -> None:
    log = script_utils.Log(params)          # wraps print() to stdout
    total = ...                             # compute before loop so bar is determinate
    log.progress(0, total)

    for i, item in enumerate(items):
        # ... process ...
        log.progress(i + 1, total)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--params', required=True)
    args = parser.parse_args()
    with open(args.params) as f:
        params = json.load(f)
    run(params)

if __name__ == '__main__':
    main()
```

### Progress protocol

Write `[PROGRESS] n/total` to stdout. Julia's `eachline` loop parses this and calls `on_progress(n, total)` which drives the task's progress bar. Every other stdout line is forwarded to `on_log` and appears in the GUI task log and in the per-task log file (`{img._dir}/logs/<fun_name>.log`).

```python
# log utility (wraps print to stdout)
class Log:
    def __init__(self, params): pass
    def log(self, msg): print(msg, flush=True)
    def progress(self, n, total): print(f'[PROGRESS] {n}/{total}', flush=True)
```

`script_utils.Log` is already implemented this way in `python/cecelia/utils/script_utils.py`.

> **Required for every Python task.** The GUI progress bar only moves when `[PROGRESS]` lines are emitted. Without them the bar stays at 0% for the full duration of the task. Call `log.progress(n, total)` at the start (0/total) and after each major step. Three to five steps is typical — open, process, save. Tasks that omit this will silently show no progress.

### Zarr conventions

Two OME-ZARR layouts coexist — always detect rather than assume:

```python
im_dat, zarr_group_info = zarr_utils.open_as_zarr(params['imPath'], as_dask=True)
# im_dat[0] is the full-res array; im_dat[1], im_dat[2], ... are downsampled
```

Write output with:
```python
zarr_utils.create_multiscales(result_array, params['imCorrectionPath'],
                               dim_utils=dim_utils, nscales=len(im_dat))
ome_xml_utils.save_meta_in_zarr(params['imCorrectionPath'], params['imPath'],
                                 changed_shape=result_array.shape, dim_utils=dim_utils)
```

---

## 3. JSON spec

Location: `app/src/tasks/<category>/<name>.json` — served to Vue via `GET /api/tasks/definitions?category=<category>`. Never copy it to `frontend/`.

### Minimal structure

```json
{
  "task":          "myTask",
  "fun_name":      "myCategory.myTask",
  "label":         "My Task",
  "category":      "My Category",
  "env":           ["local"],
  "resource_pool": "default",
  "params": []
}
```

`fun_name` is the canonical identifier — must match the `_fun_name_map` key and the WS `funName` field.

`resource_pool` sets the default pool for this task. Pools are defined in `app/config.toml`. Standard values:

| Pool | Limit | Use for |
|------|-------|---------|
| `gpu` | 1 | GPU-accelerated tasks (Cellpose, etc.) |
| `gpu-light` | 4 | GPU-assisted tasks that don't saturate the GPU |
| `io` | 8 | I/O-bound tasks (OME-ZARR import, etc.) |
| `default` | 20 | CPU tasks |

### Param types

Full reference (see CLAUDE.md for the concise table):

**`int` / `float`** — slider widget when `min`+`max` are set:
```json
{ "key": "radius", "label": "Radius", "type": "int", "min": 0, "max": 40, "step": 1, "default": 10 }
```

**`bool`** — checkbox:
```json
{ "key": "applyGaussian", "label": "Apply Gaussian", "type": "bool", "default": true }
```

**`text`** — free-text input:
```json
{ "key": "outputName", "label": "Output name", "type": "text", "default": "result" }
```

**`select`** — dropdown. Use `options` array; `multiple: true` for multi-select:
```json
{
  "key": "mode", "label": "Correction mode", "type": "select", "default": "divide",
  "options": [
    { "label": "Divide",   "value": "divide" },
    { "label": "Subtract", "value": "subtract" },
    { "label": "None",     "value": "none" }
  ]
}
```

**`channelSelection`** — channel picker populated from image metadata. `multiple: true` for multi-channel:
```json
{ "key": "driftChannel", "label": "Drift channel", "type": "channelSelection", "multiple": false, "default": [] }
```

**`valueNameSelection`** — picks from registered image versions. Add `"field": "labels"` to list segmentation label sets (`img.labels` keys) instead of the default image-version keys:
```json
{ "key": "valueName", "label": "Segmentation", "type": "valueNameSelection", "field": "labels", "default": "default" }
```

**`popSelection`** — two modes.

- **Single** (default): a dropdown of populations for the selected image + sibling `valueName`, plus `"NONE (whole segmentation)"`. The widget fetches `GET /api/gating/popmap`, flattens the tree to paths, and emits one path (or `"NONE"`). Used by `tracking.bayesian_tracking`.
- **Across segmentations** (`"multiple": true` or `"acrossSegmentations": true`): a chip multi-select listing populations from **every** segmentation, each value **prefixed with its value_name** (`"A/_tracked"`, `"B/qc"`, `"C/qc/_tracked"`, …). Source is `GET /api/plots/populations` (the same cross-segmentation picker the summary canvas uses), which injects the **derived `/_tracked`** pop (`track_id > 0`, not stored in a gating file) alongside the stored gates — the flat popmap endpoint would miss it. The value is a `string[]`. The Julia handler passes these straight to `pop_df(imgs, uids, pop_type, pops)`, whose **prefix syntax** (`"A/qc"` → value_name `A`, path `/qc`) pools cells across segmentations into one frame tagged with `value_name`. This is how `behaviour.hmm_*` fit tracked `A`, `B`, `C` from every image in one run — there is **no separate `valueName` param**; the segmentations are exactly those named by the pops.

**`popScope` — the two defined object scopes a module function's picker offers.** Instead of hand-rolling a raw `popType` (+ `includeRoot`) per module, a `popSelection` param declares **`"popScope": "cells"`** or **`"popScope": "tracks"`** — the Julia parity of the old R `isTrack` attribute + `tracksOnly` flag. The backend (`population_scope_groups` in `population_manager.jl`, package-side + tested) resolves the scope across **multiple sources** and cell-vs-track filters the result:
- **`"cells"`** → flow gates (`/qc`, …) + cell clusters (`clust`) + an **existence-checked all-cells root** (`"<seg> · all"`). Drops the derived `/_tracked` sets.
- **`"tracks"`** → the derived tracked sets (root `/_tracked` — only when the segmentation has **ungated** tracks — and per-gate `/qc/_tracked`) + per-track gates (`track`) + track clusters (`trackclust`). Drops plain cell gates like `/qc`, so a picker never offers a non-track population, and never fabricates an "all" root that has no tracks.

Cluster pops are included by default; set **`"includeClusters": false`** to omit `clust`/`trackclust`. `popScope` is preferred over the raw `popType`/`includeRoot` knobs (still honoured when `popScope` is absent, e.g. the summary-canvas picker). Membership is **not** resolved in the widget — the Julia handler resolves label IDs via the gating engine.
```json
{ "key": "pops", "label": "Populations", "type": "popSelection", "popScope": "tracks", "multiple": true, "acrossSegmentations": true, "default": [] }
```
Current users: `behaviour.hmm_states` / `behaviour.hmm_transitions` / `clustTracks.cluster` → `"tracks"`; `clustPops.cluster` / `tracking.bayesian_tracking` → `"cells"`.

**`labelPropsColsSelection`** — multi-select chip list of per-cell measure columns. Fetches `GET /api/gating/channels` for the segmentation of the **first selected population** (pops carry their value_name as a prefix; falls back to a sibling `valueName`, then the image's first segmentation). Renders **two groups** (matching the R UI): **Tracking** — obs columns starting with `live.` (`live.cell.speed`, `live.cell.hmm.state.*`, …); and **Object** — the var feature columns, with **intensity columns shown by channel name** (`mean_intensity_0` → `channelNames[0]`, `nuc_mean_intensity_0` → `nuc_<name>`, mirroring Julia `_channel_label`) plus shape columns. Structural columns (`label`, `track_id`, lineage, `pop`, `value_name`) are excluded. The stored **value is always the raw column name**; only the displayed label changes. Optional `"trimPrefix"` collapses to one flat group filtered to that prefix with the label trimmed — e.g. the standalone transitions state picker offers only `live.cell.hmm.state.*` shown as their bare suffix. Used by `behaviour.hmm_states` (`modelMeasurements`) and `behaviour.hmm_transitions` (`hmmStates`):
```json
{ "key": "modelMeasurements", "label": "Properties", "type": "labelPropsColsSelection", "multiple": true, "default": ["live.cell.speed", "live.cell.angle"] }
```

**`motionDimsSelection`** — `auto | 2D | 3D` selector for whether track/behaviour measures are computed in-plane or in full 3D (used by `tracking.track_measures`). In `auto` it calls `GET /api/tracking/motion-dims` for the selected image + sibling `valueName` (server-side detector, cached by h5ad mtime) and shows the recommendation **+ a warning before Run** (e.g. "Auto → 2D — z is non-migratory jitter…"), with override to 2D/3D. Value sent: `"auto"`/`"2D"`/`"3D"`; the handler resolves `auto` via `detect_motion_dims`. See `docs/TRACKING.md` → *Motion dimensionality*.
```json
{ "key": "dims", "label": "Motion dimensions", "type": "motionDimsSelection", "default": "auto" }
```

**`section`** — collapsible group of params, not repeatable. Use for Advanced / Filters sub-panels:
```json
{
  "key": "advanced", "label": "Advanced", "type": "section", "collapsed": true,
  "params": [
    { "key": "sigma", "label": "Sigma", "type": "float", "min": 0, "max": 5, "step": 0.1, "default": 1.0 }
  ]
}
```

**`group`** — repeatable/sortable list of sub-param sets. Each entry is keyed `"0"`, `"1"`, … in the values dict. Use for things like "one denoise model per set of channels":
```json
{
  "key": "models", "label": "Denoise models", "type": "group",
  "repeatable": true,
  "default": { "0": { "model": "denoise_cyto3", "modelChannels": [], "diameter": 10 } },
  "params": [
    { "key": "model",         "label": "Model",    "type": "select",           "options": [...] },
    { "key": "modelChannels", "label": "Channels", "type": "channelSelection", "multiple": true, "default": [] },
    { "key": "diameter",      "label": "Diameter", "type": "int", "min": 1, "max": 200, "default": 10 }
  ]
}
```

`section` inside `group.params` works — use it for Advanced sub-panels within each group entry.

`tip` on any param adds a tooltip to the label in the UI.

---

## 4. Register the task

**`app/src/tasks/task_registry.jl`** — two additions:

```julia
# 1. spec path (used by _task_spec to load and cache the JSON)
function _spec_path(::MyTask)
    joinpath(@__DIR__, "<category>", "<name>.json")
end

# 2. fun_name → struct instance (used by _task_from_fun_name)
function _fun_name_map()::Dict{String, CciaTask}
    Dict{String, CciaTask}(
        # ... existing entries ...
        "myCategory.myTask" => MyTask(),
    )
end
```

**`app/src/Cecelia.jl`** — include the `.jl` file **before** `task_registry.jl`:

```julia
include("tasks/<category>/<name>.jl")
include("tasks/task_registry.jl")   # already there — keep this order
```

Both `app/src/` files are Revise-tracked. Adding a new struct requires a server restart; changing a function body does not.

### Tasks launched outside a module page (computed params, new-image output)

A task doesn't have to be driven by a module-page form. **`editImages.cropImage`** is launched from the
napari **Viewer panel**: the user draws a crop box, the bridge computes the pixel bbox, and the frontend
fires `task:run` with those params directly (no page fetches its category's definitions). Two patterns it
demonstrates:

- **Computed params, minimal spec.** Only `valueName` + `x0/x1/y0/y1` are declared in the JSON (for
  validation); the optional `z0/z1/t0/t1` are sent as extra params and pass through untouched
  (`_validate_params_against_spec` only iterates declared params). Use this when a UI computes params
  rather than asking for them.
- **Producing a NEW image (not a version).** A crop changes the extent, so it can't be a version of the
  source. The handler recovers the parent set from `img._dir` (`load_project` → find the set containing
  `img.uid`), calls `add_image!` (new uid + `{proj}/0|1/{uid}`), writes the zarr into the new image's zero
  dir, then registers `filepath`/`imChannelNames` on the **new** image's `ccid.json`. It returns
  `{newImageUid, setUid}`; the frontend adds the image live via `/api/images/meta` + `addImagesFromApi`
  (see `stores/ws.ts`). Contrast with the correction tasks, which register a new **version** of the same
  image via `versioned_set_field!`.

---

## 5. Composite tasks

A composite task chains two or more existing tasks in sequence, reusing their Python scripts and Julia handlers without any duplication. It is defined by JSON only — no new `.jl` file needed.

### JSON definition

```json
{
  "task":            "afDriftCorrect",
  "fun_name":        "cleanupImages.afDriftCorrect",
  "label":           "AF & Drift Correction",
  "category":        "Cleanup",
  "env":             ["local"],
  "resource_pool":   "default",
  "composite":       ["cleanupImages.afCorrect", "cleanupImages.driftCorrect"],
  "outputValueName": "driftCorrected"
}
```

The `"composite"` array lists the `fun_name`s of constituent steps in execution order. No `"params"` block is needed — the GUI merges the param specs from each constituent task automatically (dedup by key, first step wins). A constituent param marked `"hideInComposite": true` is **omitted** from the merged form — use it for a value the composite derives internally rather than asking the user for. Example: `behaviour.hmm_transitions.hmmStates` is hidden in the `behaviour.hmm` composite because the states step's output column is threaded into it automatically (see *Runtime behaviour*).

### `outputValueName` — canonical output registration

If `"outputValueName"` is set, the composite executor runs a clean-up pass on `ccid.json` after all steps succeed:

- It snapshots the existing `filepath` keys before the first step runs.
- After the last step, it removes every filepath entry that was added during the composite run and is not the canonical name.
- It writes a single entry `outputValueName → last_step_filename` and sets `_active` to that name.

**Why this matters:** each constituent task registers its own output value name in `ccid.json` (`afCorrected`, `driftCorrected`, …). Without `outputValueName`, all of those entries would persist. With it, only the canonical name survives.

Set `outputValueName` to the value name that should be visible after the composite completes. It does not have to match any individual step's value name — the executor will insert/replace it.

### Intermediate file cleanup

After the last step succeeds, the executor deletes from disk any file returned as `"filename"` by steps 1..N-1 (all steps except the final one). The path resolved is `{proj}/0/{uid}/{filename}`.

This is automatic for any composite — constituent tasks do not need to know they are running inside a composite. The AF+drift composite therefore leaves only `ccidDriftCorrected.ome.zarr` on disk; `ccidAfCorrected.ome.zarr` (the intermediate written by `afCorrect`) is deleted once `driftCorrect` finishes.

Files created by the final step are never deleted.

### Runtime behaviour

A generic `CompositeTask` struct in `task.jl` handles execution:

1. Looks up each step's task via `_task_from_fun_name`.
2. Passes the full composite params dict to step 1.
3. After each step, injects `params["valueName"] = result["valueName"]` so the next step reads the previous step's output. Each step re-reads `ccid.json` from disk and finds the filepath registered by the previous step. The set-scope composite additionally threads `result["stateColumn"]` → `params["hmmStates"] = [stateColumn]`, so `behaviour.hmm` chains states → transitions on a single user-set `colName` without exposing the derived state column (see `hideInComposite` above).
4. If any step returns `nothing` (failure), the composite aborts and returns `nothing` immediately — no cleanup is attempted.
5. Progress from each step is scaled into a `1/N` slice of the total bar (step 1 → 0–50%, step 2 → 50–100% for a two-step composite).
6. Intermediate files and ccid.json entries are cleaned up (see above), then the final result dict is returned.

### Registration

Two additions to `task_registry.jl` — no `.jl` file for the composite itself:

```julia
# 1. Register the JSON spec path (outside _fun_name_map)
_COMPOSITE_SPEC_PATHS["myCategory.myComposite"] =
    joinpath(@__DIR__, "myCategory", "my_composite.json")

# 2. Add to the dispatch map (single-arg constructor — looks up path from _COMPOSITE_SPEC_PATHS)
function _fun_name_map()::Dict{String, CciaTask}
    Dict{String, CciaTask}(
        # ...
        "myCategory.myComposite" => CompositeTask("myCategory.myComposite"),
    )
end
```

No `.py` script for the composite — the steps' Python scripts are reused.

### When to use composite vs a new standalone task

Use composite when two or more tasks are naturally sequential and share no new logic. Write a standalone task when the combined operation has meaningful optimisations that compositing would lose (e.g., holding an intermediate array in memory across steps to avoid a write-then-read cycle).

---

## 6. Set-scope tasks (run once over many images)

Most tasks are **image-scope**: the GUI runs them once per selected image. A **set-scope** task runs **once over the whole selected image vector** — used when the computation spans images jointly (e.g. `behaviour.hmm` fits one HMM across all selected images so the states are comparable). The clustering tasks (Phase 2) will use the same mechanism.

A set-scope task differs from an image-scope task in three places:

1. **`_run_task` dispatches on a vector.** Define `_run_task(::MyTask, imgs::Vector{CciaImage}, params; on_log, on_progress, on_process)` instead of the single-`img` form. Build the pooled cross-image table with `pop_df(imgs, uids, pop_type, pops; …)` (it stacks per-image rows and tags each with a `uID` column). Write results back **per image** (loop `imgs`, write each one's labelProps). See `app/src/tasks/behaviour/hmm_states.jl`.
2. **The JSON spec declares `"scope": "set"`.** This is what routes the run. `task_scope(task)` reads it (default `"image"`). It also makes the task a **picnic node** on the whiteboard automatically — a chain node inherits its scope from this field (`_task_default_scope`, see `docs/SCHEDULER.md` → *Node scopes*), so you never restate scope when building a chain.
   ```json
   { "fun_name": "behaviour.hmm_states", "task": "hmmStates", "label": "HMM States", "category": "Behaviour", "scope": "set", "env": ["local"], "resource_pool": "default", "params": [ … ] }
   ```
3. **Invocation passes all image UIDs.** The scheduler has a set-scope overload `run_task(task, imgs::Vector{CciaImage}, params; …)` that queues through the pool like the single-image form (status + logfile attach to the first image as representative). The WS `task:run` handler (`api/src/sockets.jl`) checks `task_scope`: a `"set"` task reads `imageUids` from the message and runs the vector form once; an `"image"` task keeps the per-image loop. The frontend `TaskRunner` sends `imageUids: selectedUids` (one `task:run`) when `def.scope === 'set'`.

**Composites can be set-scope too.** A composite with `"scope": "set"` (e.g. `behaviour.hmm` = `hmm_states` → `hmm_transitions`) runs each step's set-scope form over the image vector in sequence; `_run_task(::CompositeTask, imgs::Vector{CciaImage}, …)` handles this (no per-image ccid.json rewriting — set-scope behaviour tasks add obs columns rather than creating value_names).

The module page hosts a set-scope task exactly like any other — `ModuleLayout` already allows multi-image selection; `TaskRunner` does the scope-aware submit. `BehaviourModule.vue` is the reference (TaskRunner in `#right`, plot canvas in the `#plots` slot).

---

## 7. Tests

Every new task ships with tests in `app/test/runtests.jl`. Run the suite:

```bash
cd app && julia --project test/runtests.jl
```

Minimum coverage per task:

```julia
# fun_name resolves
@test _task_from_fun_name("myCategory.myTask") isa MyTask

# param validation — bad input rejected
@test_throws ParamValidationError run_task(MyTask(), img, Dict("radius" => -1))

# param validation — good input accepted (no throw)
run_task(MyTask(), img, Dict("valueName" => "default", "radius" => 5); on_log=_->nothing)
# assert result dict shape, or use @test isnothing(result) for tasks needing external binaries
```

For tasks requiring external binaries (bioformats2raw, napari/.venv), gate behind a presence check and skip gracefully:

```julia
if isfile(bioformats2raw_bin())
    @testset "omezarr real import" begin ... end
end
```

---

## 7. Module page

A module page is a Vue component at `frontend/src/modules/<Name>Module.vue`. Most pages are just `ModuleLayout` + `TaskRunner`.

### Minimal template

```vue
<script setup lang="ts">
import ModuleLayout from '../components/ModuleLayout.vue'
import TaskRunner   from '../tasks/TaskRunner.vue'
import { useTaskDefs } from '../composables/useTaskDefs'

const { defs, reload: reloadDefs } = useTaskDefs('myCategory')   // must match JSON "task" category
</script>

<template>
  <ModuleLayout module="myCategory" :show-attrs="true" :show-filter="true">
    <template #right="{ selectedUids, selectedNames }">
      <TaskRunner
        :defs="defs"
        :on-reload-defs="reloadDefs"
        module="myCategory"
        :selected-uids="selectedUids"
        :selected-names="selectedNames"
      />
    </template>
  </ModuleLayout>
</template>
```

`useTaskDefs('myCategory')` fetches `GET /api/tasks/definitions?category=myCategory` on mount and returns `{ defs, loading, reload }`. Pass `defs` to `TaskRunner` and `:on-reload-defs="reload"` — `TaskRunner` shows a **Reload** button when `defs` is empty so the user can recover without navigating away. The category string must match the value in the JSON specs' `"task"` field (or more precisely, the category grouping in the API's definition endpoint — currently the directory name under `app/src/tasks/`).

> **Functions not showing on the module page?** `useTaskDefs` fetches on mount and auto-retries up to 5 times (2 s apart) if the server is still starting. If defs are still empty after that, `TaskRunner` shows a **Reload** button — click it to retry. The most common causes: (1) the Julia server isn't running yet; (2) a task JSON file has a syntax error (check the Julia console for `Skipping malformed task spec` warnings); (3) `api/src/routes.jl` was changed but the server wasn't restarted — those files are not Revise-tracked.

### Remembering task params

`TaskRunner` does **not** keep param values in `localStorage`. They are remembered **per object in
`ccid.json`** (`meta["funParams"]["<fun_name>"]`), mirroring the old R `moduleFunParams`:

- **On run**, `api/src/sockets.jl` (`_remember_fun_params`) saves the submitted params to **each
  processed image** (a record of what params produced it) *and* to the **set** (the shared
  last-used default). The frontend sends `setUid` in `task:run` for this.
- **On load**, `TaskRunner` fetches `GET /api/tasks/funparams?projectUid&fun&imageUid?&setUid?` and
  populates the form **image → set → task-defaults**. It passes `imageUid` only when exactly one
  image is selected (the form is one config applied to all selected images, so with several
  selected it shows the set-level default). The fetch re-runs on function change and on project/set
  switch — which is what stops one project's params leaking into another (there is no project-keyed
  localStorage; `ccid.json` is the single source of truth).

Whiteboard chain nodes are unaffected — their params live in the per-project chain template, not in
`funParams`.

`ModuleLayout` provides the image table on the left and the `#right` slot for the task panel. Key props:

| Prop | Default | Effect |
|------|---------|--------|
| `module` | required | Key used by the task store and log store |
| `show-attrs` | `false` | Show attribute columns and filter in the image table |
| `show-filter` | `false` | Show the text filter row above the image table |
| `allow-manage` | `false` | Show set management controls |
| `allow-delete` | `false` | Show image delete buttons |
| `no-set-hint` | `''` | Text shown when no set is selected |

### Custom actions

To add a button above the table (e.g. "Add images"):

```vue
<template #actions="{ hasSet }">
  <button class="cc-btn cc-btn-primary" :disabled="!hasSet" @click="doSomething">
    <i class="pi pi-plus" /> Do something
  </button>
</template>
```

### Below-table content

**RULE — a plot below the image table MUST use the plot registry + `SummaryCanvas`. Never a bespoke
chart component or a bespoke `/api/plots/*` route.** There is **one** way to host a plot (module page
*and* the `/analysis` board): a `plotDefinitions/*.json` registry entry rendered by `SummaryCanvas` →
`SummaryPanel` → `PlotChart`. Reusing `PlotChart` alone in a hand-rolled panel is **not** compliance
— that's the anti-pattern (it was done once for segmentation QC and had to be reworked). See
`docs/PLOTS.md` → *Hosting* and `docs/ANALYSIS.md` → *Plot families*.

To add a module-page plot:
1. Write `app/src/plotDefinitions/<id>.json` — `{ id, label, module: "<thisModule>", family: "summary",
   chartTypes, dataSource: { popType, granularity, measure, measureOptions }, scopeModes,
   whiteboardCompatible }`. Served via `GET /api/plots/definitions`. Data comes from the existing
   `POST /api/plot_data` (`plot_summary_data`) — no new route.
2. Host it in the `#plots` slot with `SummaryCanvas` (the reference is `BehaviourModule.vue`).
   `ModuleLayout` wraps `#plots` in the shared, collapse-persisted section — do NOT add your own
   `CollapsibleSection` (that's the divergence this replaced):

```vue
<template #plots="{ selectedUids }">
  <SummaryCanvas :image-uids="selectedUids" module="<thisModule>" />
</template>
```

`whiteboardCompatible: true` also makes it available on the whiteboard — one definition, both places.

`CollapsibleSection` (from `'../components/CollapsibleSection.vue'`) is only for **non-plot**
supplementary panels (previews, tables, controls) placed in the rare `#below-table` slot — never wrap
a bespoke chart in it, and never hand-wrap the `#plots` canvas (ModuleLayout already does).

### RULE: persist every user-settable option (do NOT use a bare `ref()`)

**Any UI option on a module page or canvas — chart type, scope toggle, compare mode, highlighted
pops, slider values, etc. — MUST live in persisted view state, not a plain `ref()`.** A `ref()`
silently resets when the user navigates away and back (the page remounts); options must survive that.

- **Canvas pages** (anything using `useCanvasPanels`): per-panel options go in the panel's `state`
  object; canvas-level options go in the per-canvas **`shared`** bag via **`useViewState(shared,
  defaults)`** — declare every option in the one `defaults` literal and it persists automatically,
  with no per-field wiring. See `composables/useViewState.ts` and docs/UI.md → "Persisting view
  state — the three scopes".
- **Plain module pages**: image-table selection is already persisted by `ModuleLayout` (per
  `${module}|${setUid}`). For other page-level options, use the same pattern — a store-backed bag +
  `useViewState`, keyed by the module.

This is a hard convention: when adding a new page or option, the persistence is part of "done".
The bug class it prevents (plots reverting, selections vanishing on navigation) is easy to ship and
tedious to catch by clicking through — so put the option in the `defaults` bag from the start.

### Custom task:result handling

If a task sends back a result that needs frontend side-effects beyond the default filepath update, add a handler in `frontend/src/stores/ws.ts` inside the `task:result` block:

```typescript
// ws.ts — inside the `if (type === 'task:result')` block
const addedValueName = meta.valueName as string | undefined
const addedFilename  = meta.filename  as string | undefined
if (addedValueName && addedFilename) {
    // already handled generically — update img.filepaths[addedValueName] = addedFilename
}
// custom: update something else specific to your task
if (meta.myCustomField) {
    // e.g. update channel names, set a flag, etc.
}
```

The generic handler already covers the common case (`valueName` + `filename` → `img.filepaths` update). Only add custom logic for fields beyond that.

---

## 8. Wire up the route and navigation

### Route — `frontend/src/main.ts`

```typescript
import MyModule from './modules/MyModule.vue'

const router = createRouter({
  routes: [
    // ... existing routes ...
    { path: '/mymodule', component: MyModule, meta: { label: 'My Module' } },
  ]
})
```

### Navigation — `frontend/src/components/AppSidebar.vue`

Add an entry to the `navItems` array:

```typescript
{ to: '/mymodule', label: 'My Module', icon: 'pi-<icon-name>', tip: 'One-line tooltip.', requiresProject: true }
```

`requiresProject: true` greys out the item when no project is open. Use `false` for Settings-style pages that don't need a project. Browse available PrimeIcons at `primefaces.org/primeicons`.

---

## 9. API category registration

The API endpoint `GET /api/tasks/definitions?category=X` reads specs by looking up tasks registered in `_fun_name_map` whose `fun_name` starts with `X.` and whose JSON spec has a matching category.

**No explicit API registration step is needed.** Once the task is in `_fun_name_map` and its JSON has the right `fun_name`, the endpoint automatically serves it. The category grouping in the response is derived from the part before the `.` in `fun_name` (`cleanupImages`, `importImages`, etc.).

---

## 10. Common gotchas

**`api/src/` is not Revise-tracked.** Changes to `api/src/server.jl` or `api/src/sockets.jl` require a full server restart. Changes to `app/src/` (the Cecelia package) are hot-reloaded by Revise — except struct definitions and `include` statements, which also need a restart.

**Adding a Julia dependency.** Add it to `app/Project.toml`, then run `julia --project -e 'import Pkg; Pkg.instantiate()'` from the `api/` directory before restarting. The `api/` environment has its own manifest and won't pick up new deps automatically — the server will fail to precompile Cecelia until you do this.

**`fun_name` must be consistent.** The same string must appear in: the JSON spec's `"fun_name"` field, the `_fun_name_map` key in `task_registry.jl`, and the `funName` field sent in the WS `task:run` message. The frontend derives `funName` from `TaskDef.fun_name` which it reads from the JSON spec. A mismatch between the map key and the spec field causes the task to be found by fun_name but dispatched to the wrong handler.

**Group param values arrive as a dict with string keys.** In Julia: `params["models"]` is a `Dict` (or JSON3 object) with keys `"0"`, `"1"`, … If you iterate entries, coerce with `String(k)`. See the cellpose_correct handler for the pattern.

**JSON3 yields `Symbol` keys, not `String` keys.** When reading `ccid.json`, always do:
```julia
raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(...))
```
without this, `get(raw, "filepath", nothing)` silently returns `nothing`.

**`proc.exitcode == 0` doesn't mean success on cancel.** libuv sets `exitcode = 0` for signal-killed processes. Always check `proc.termsignal == 0` as well.

**The frontend never has its own task JSON copies.** `frontend/src/tasks/definitions/` is intentionally empty. Specs are always fetched from the API. If you accidentally put a JSON there, the API copy is still used — but the stale file will confuse future contributors.

**Params are persisted to localStorage per (module, task) pair.** `TaskRunner` saves param values on function switch and on run. Users reopening the module will see their last-used values, not the JSON defaults. This is intentional — don't fight it, just make sure JSON defaults are sensible starting points.
