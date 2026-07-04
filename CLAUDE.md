# Cecelia Pineapple — Project Guide

Cecelia is an immunological image analysis tool (Nature Communications 2025).
Stack: **Julia** (backend/WS server) · **Vue 3 + TypeScript** (frontend) · **Python/Napari** (image viewer)

See also:
- [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) — layer boundaries, WS protocol, data model contracts, Napari lifecycle
- [`docs/SCHEDULER.md`](docs/SCHEDULER.md) — chain executor design: per-image threading, resource pools, barriers, resume semantics, event bus, concurrency invariants
- [`docs/UI.md`](docs/UI.md) — frontend conventions, component catalog, how to add module pages and plots
- [`docs/MODULES.md`](docs/MODULES.md) — complete guide to adding task functions and module pages
- [`docs/NAPARI.md`](docs/NAPARI.md) — napari integration: process model, restart rules, OME-ZARR layouts, byte order, contrast limits, layer props, unimplemented options
- [`docs/OBJECTMODEL.md`](docs/OBJECTMODEL.md) — CciaProject/Set/Image hierarchy, disk layout, ccid.json shape, versioned fields, transactions
- [`docs/SEGMENTATION.md`](docs/SEGMENTATION.md) — segmentation pipeline: class hierarchy, label type convention (base/nuc), tiling, output zarr layout, napari integration, future tracking/gating
- [`docs/TRACKING.md`](docs/TRACKING.md) — cell tracking (btrack): whole-segmentation vs gated-population input, in-process gated membership, track lineage columns in H5AD obs, vendored config, deferred track-property gating (celltrackR port)
- [`docs/DATAMODEL.md`](docs/DATAMODEL.md) — AnnData conventions: .h5ad layout, feature names, ccid.json label_props, mesh file paths
- [`docs/POPULATION.md`](docs/POPULATION.md) — population manager & gating: pop types, transforms, gating/{value_name}.json storage, pop_df unified accessor, gate↔track composition, membership access
- [`docs/API.md`](docs/API.md) — HTTP/WS surface: routing conventions, binary responses, route index, gating routes (popmap/CRUD/plotdata/density/membership/stats)
- [`docs/PLOTS.md`](docs/PLOTS.md) — summary-plot design: chart types × data source (one/multi/pooled) × measure type (numeric/categorical), encoding model, the agreed renderer spec
- [`docs/ANALYSIS.md`](docs/ANALYSIS.md) — the Analysis board (`/analysis`): tabs + comic-plate layout + persistence keys, the registry-driven plot families (summary/interactive/cluster/image), the read-only cluster manager, the gating-strategy plot, and PDF/CSV export (light theme, shared hi-res raster path)
- [`docs/FUTURE.md`](docs/FUTURE.md) — deliberately deferred optimisations (known-better alternatives set aside): what, why deferred, when to revisit
- [`docs/ROADMAP.md`](docs/ROADMAP.md) — temporary forward goals: phases (behaviour/HMM → clustering → freeze v1.0 → packaging/distribution → self-update) + post-v1 backlog. Consult before starting a new phase.
- [`docs/MILESTONES.md`](docs/MILESTONES.md) — durable, append-only ledger of what landed and how it was packaged (the counterpart to the throwaway roadmap). Add an entry at each freeze/release.
- [`docs/SHIPPING.md`](docs/SHIPPING.md) — distribution architecture & rationale: Pixi/constructor + browser stack (Julia serves the built frontend; no Tauri/Electron), packaging/update model, and non-obvious env decisions (cellpose-v3 pin, dropped coastal, GPU/RAPIDS parked, run-via-`pixi run`). The *why*, paired with INSTALL.md's *how*.
- [`docs/DEV.md`](docs/DEV.md) — development workflow: branch + PR conventions (never commit/push to `main`), commit style, how PRs are opened/merged (gh not in the agent env → relay the PR URL), and how releases are tagged off `main`.

**Keep the docs current — update the relevant file in the same change, not after.**

| Changed area | Update |
|---|---|
| Layer boundaries, contracts, hidden invariants | `docs/ARCHITECTURE.md` |
| Scheduler, resource pools, barriers, event bus | `docs/SCHEDULER.md` |
| UI patterns, components, design tokens | `docs/UI.md` |
| Analysis board: tabs/layout, plot-hosting registries, board export | `docs/ANALYSIS.md` |
| Task JSON, registry, module pages, composite pattern | `docs/MODULES.md` |
| Napari bridge, commands, OME-ZARR, contrast, layer props | `docs/NAPARI.md` |
| Object model, ccid.json, versioned fields, disk layout | `docs/OBJECTMODEL.md` |
| Segmentation pipeline, label types, tiling, output zarr | `docs/SEGMENTATION.md` |
| Cell tracking (btrack), track lineage in H5AD, track-property gating | `docs/TRACKING.md` |
| AnnData, cell-level data storage and access | `docs/DATAMODEL.md` |
| Population manager, gating engine, pop_df, gate↔track | `docs/POPULATION.md` |
| HTTP/WS routes, request/response shapes, binary responses | `docs/API.md` |
| Deferring a known-better approach (ecosystem/scale not ready) | `docs/FUTURE.md` |
| Packaging, distribution, env rationale (Pixi/constructor, why) | `docs/SHIPPING.md` |
| Branching, commits, PRs, release tagging (dev workflow) | `docs/DEV.md` |

> **Watch for divergent re-implementation — flag it, don't add another variant.** The most
> expensive mistakes here are doing the same *cross-cutting* thing more than one way — e.g. touching
> `.h5ad` outside the label view, or spawning Python without `run_py`. The moment you notice you're
> hand-rolling something that already has (or obviously should have) a single canonical helper,
> **stop and say so** — propose centralising it (one helper, used everywhere) instead of writing a
> second variant. "I'll just inline it here" is how the duplication starts. One way to do each
> thing; the second way is the bug.
>
> Same reflex for going in circles or losing the thread: if two+ rounds pass on one question without
> clear progress, or an important aspect keeps being deferred/glossed over, surface it explicitly for
> the user to decide (or add it to `docs/TODO.md` and move on) rather than pushing through.

## Previous prompts

Completed prompt files are kept in a `previous-prompts/` folder in the **workspace root** (outside `cecelia-pineapple/`) — set aside but accessible. They are **reference only**: historical context for finished work, not instructions to be re-run.

## TODO.md

`docs/TODO.md` tracks open work and fixed items.
- When fixing something from the list, move it to the `## Fixed` section with a date — don't delete it.
- Backfill any significant fixes made outside the TODO flow so the history stays complete.

## Parked plans (`docs/todo/`)

`docs/todo/*.md` holds **parked plans** — full, standalone design docs for a feature too big for a
`docs/TODO.md` item. See [`docs/todo/README.md`](docs/todo/README.md) for the convention. In short:
a TODO item is a paragraph; a parked plan is a `*_PLAN.md` with **locked decisions + a phased build
sequence + cross-file architecture**, created when a feature needs real design before/while building,
when a topic is paused but the thinking must be preserved, or when code needs a stable pointer
(`see X_PLAN.md`). Promote the durable parts into a permanent `docs/<AREA>.md` once built.

## INSTALL.md

`docs/INSTALL.md` installation instructions for Unix and Windows systems. Needs to be reviewed before production deployment.

## SHIPPING.md

`docs/SHIPPING.md` is the **distribution architecture & rationale** — the Pixi/constructor + browser stack, the
packaging/update model, and the non-obvious environment decisions (cellpose pinned to v3, the
dropped `coastal` dep, GPU/RAPIDS parked, the run-via-`pixi run` model). It is distinct from
`docs/INSTALL.md`: *commands and setup steps* live in INSTALL; *why it's built this way, and how
it ships and updates* lives in SHIPPING. Keep both current when changing install-related code
(`pixi.toml`, launcher scripts, `python_bin_path()`), and cross-reference SHIPPING rather than
duplicating rationale into INSTALL.

---

## Cite sources for non-trivial algorithms

Applies to **all languages** (Julia, Python, TypeScript/Vue). When implementing a non-trivial
or published algorithm — numeric transforms, methods from a paper, or code ported from a
reference implementation — add a comment with the **citation** (paper + DOI, and/or the
reference-implementation URL) and, where feasible, **validate against golden values** from that
reference in the test suite. Reserve this for the parts where "is this actually correct?"
genuinely matters — not ordinary code or small helpers.

Example: `app/src/gating/transforms.jl` (logicle ← Moore & Parks 2012, cross-checked against
FlowUtils' `logicle_c`, golden values asserted in `app/test/runtests.jl`).

---

## H5AD / cell-data access — always go through the readers/writers

**Never touch `.h5ad` (or its HDF5 internals) directly. There are dedicated readers and writers
in both languages — use them. This applies to every cell-level read and write.** See
[`docs/DATAMODEL.md`](docs/DATAMODEL.md) → *Reading `.h5ad`* for the full idiom.

The interface is the same chain in both languages — build a view, refine it, finish with a
terminal verb. `as_df` reads a label-keyed DataFrame; `add_obs(df).save!`/`.save()` writes one
back (aligned by `label`, correct AnnData encoding). You read a labeled DataFrame, you write a
labeled DataFrame — one idiom, no guessing.

| | Julia (`app/src/label_props.jl`) | Python (`app/py/utils/label_props_utils.py`) |
|---|---|---|
| **Read**  | `label_props(img\|path) \|> select_cols/view_centroid_cols/filter_rows \|> as_df` | `LabelPropsView(path).view_centroid_cols().filter_by_label(ids).as_df()` |
| **Write** (append obs cols to an existing file) | `label_props(path) \|> v -> add_obs(v, df) \|> save!` | `LabelPropsView(path).add_obs(df).save()` |

- **Do not** call `h5open`/`HDF5.*` (Julia) or `h5py`/`anndata` (Python) on cell data, and **do
  not** read the whole table and filter in memory — push the column/row selection into the view.
- **Deviate only when it is measurably more efficient/faster** to hit HDF5 directly (e.g. a
  cheap one-attribute metadata peek). When you do, **add an inline comment on that exact line**
  explaining why the view was bypassed. No silent raw access.
- The only place raw `HDF5.jl` lives is the reader/writer itself (`label_props.jl`).
- **One sanctioned exception — file *creation*.** Building a *new* `.h5ad` from scratch (the `X`
  matrix + `var` + `obsm`, e.g. segmentation measurement output in `app/py/utils/measure_utils.py`)
  is the producing task's job and uses `anndata` directly — the view wraps an *existing* file
  (read + obs-append), it does not create one. Structural changes to `X`/`var` likewise go through
  the producing Python task, not the view.

---

## Spawning Python — always go through `run_py`

**Never spawn a Python subprocess by hand. There is one launcher — `run_py` in `app/src/py_runner.jl`
— use it for every Python task runner and data-layer writer.** It writes the params JSON to the
run's task dir (`task_run_dir(<obj>._dir)`, never a temp dir), sets `PYTHONPATH=app/` (so the script
does `import py.*` with **no `sys.path` bootstrapping**), streams `[PROGRESS] n/total` → `on_progress`
and the rest → `on_log`, registers the process for cancellation, and returns clean-exit (checks
`exitcode` AND `termsignal`). It's the analogue of the old R `self$pyScript`.

```julia
ok = run_py("tasks/<category>/<name>_run.py", (; …params…), task_run_dir(img._dir);
            on_log = on_log, on_progress = on_progress, on_process = on_process)
ok || return nothing
```

- **Do not** write `run(pipeline(\`$python …\`))`, build a params file, or parse `[PROGRESS]`
  inline in a task — that boilerplate (and the bugs that come with hand-rolling exit/signal checks
  and param-file locations) is exactly what `run_py` exists to delete.
- **Python runners therefore carry NO `sys.path` manipulation** — `import py.*` resolves via the
  PYTHONPATH `run_py` sets. A new `sys.path.insert(... __file__ ...)` in a runner is a red flag.
- This is the same principle as the H5AD rule above: a cross-cutting operation gets **one**
  canonical helper, and reimplementing it inline is the bug. (See `docs/MODULES.md` → *Running a
  Python subprocess*.)

---

## Python environment

**Location:** the Pixi-managed env at the repo-root `.pixi/` (NOT under `napari/`). Don't reference an
interpreter path — launch via `pixi run`, which puts the env's `python3` on PATH (that's how the Julia
server's subprocesses and the napari bridge find it). See `docs/SHIPPING.md`.  
**Add a package:** `pixi add --pypi <package>` (PyPI) or `pixi add <package>` (conda-forge), then commit
the updated `pixi.toml` + `pixi.lock`.  
**Pin versions in `pixi.toml`** — the single source of truth (the old `napari/requirements.txt` is gone).

### Key version pins

| Package | Pin | Reason |
|---------|-----|--------|
| `cellpose==3.1.1.2` | exact | `DenoiseModel` removed in v4; we need it for cleanup/denoise tasks. Do NOT upgrade. |
| `zarr>=3.0` | lower bound | v3 API: string keys only (`"0"` not `0`), `create_array` not `create_dataset`, `zarr.Array` not `zarr.core.Array`. `zarr_utils.py` is already updated. |

### GPU detection (cellpose tasks)
Auto-detected in Python — no user checkbox needed:
```python
if torch.cuda.is_available():
    use_gpu, gpu_device = True, torch.device('cuda')
elif hasattr(torch.backends, 'mps') and torch.backends.mps.is_available():
    use_gpu, gpu_device = True, torch.device('mps')
else:
    use_gpu, gpu_device = False, None
```
Do not add a `useGPU` param to task JSON or Julia handlers.

---

## Windows compatibility

All code must run on Linux, macOS, and Windows. Each of these has already caused a real bug —
use the named helper, don't re-derive the platform branch inline:

- **Python venv path** differs (`bin/python3` vs `Scripts\python.exe`) — always branch on
  `Sys.iswindows()`, never hardcode one.
- **bioformats2raw binary name** — use `bioformats2raw_bin()` in `config.jl`, don't hardcode
  `.bat` vs no-extension.
- **Process killing** — use `_kill_tree(pid)` in `api/src/sockets.jl`; never write
  `kill`/`pgrep`/`taskkill` inline. (`Base.Process` has no `.pid` field — `_kill_tree` already
  handles getting the OS pid via libuv.) Never `taskkill /IM julia.exe` — it kills every Julia
  process on the machine, which is why `stop`/`stop-backend`/`stop-napari` kill by **listening
  port** instead of process name.
- **`proc.exitcode == 0` doesn't mean success on cancel** — libuv sets it to 0 for signal-killed
  processes too. Always check `proc.termsignal == 0` as well (see *Task system* below).
- **Directory size** — use `_dir_bytes(path)` in `app/src/utils.jl`, not a hardcoded `du`/`walkdir`.
- **Path separators** — always `joinpath()`, never string-concatenate paths.
- **`[PROGRESS]` line endings** — `eachline()` already strips `\r\n` on Windows, no special-casing
  needed.

Launcher logic for all of the above lives in `pixi.toml` tasks (`dev`/`prod`/`frontend`/`napari`/
`stop`), not shell scripts — add a `[target.<platform>.tasks]` override for OS-specific commands
rather than a separate script.

---

## Architecture

```
cecelia-pineapple/
  app/          Julia package — Cecelia.jl (Revise-tracked)
  api/          Julia API server scripts — NOT a package, NOT Revise-tracked
  frontend/     Vue 3 (Vite, TypeScript, Pinia, PrimeVue)
  napari/       Python bridge (napari_bridge.py); the env is the repo-root .pixi/, not here
  pixi.toml     Python env + run templates (`pixi run dev|prod|frontend|napari|stop`)
  docs/         Extended architecture and design reference
```

**Critical**: `api/src/*.jl` files are `include`d by the server script — they are **not** Revise-tracked. Changes to them require a server restart. Only changes to `app/src/` (the Cecelia package) are picked up by Revise. Napari logic lives in `api/src/napari_api.jl`, not `app/src/`.

**Adding a Julia dependency to `app/`**: edit `app/Project.toml`, then run `julia --project -e 'import Pkg; Pkg.instantiate()'` from the `api/` directory before restarting the server. The `api/` environment has its own manifest and won't pick up the new dep automatically — the server will fail to precompile Cecelia until you do this. (`cd app && julia --project test/runtests.jl` passes regardless because it uses the `app/` environment directly.)

### Data layout
```
{proj}/0/{uid}/    image data (OME-ZARR, written by bioformats2raw)
{proj}/1/{uid}/    metadata (ccid.json, labels, labelProps/)
```

### Ports
- `8080` — Julia WS/HTTP server
- `5173` — Vite dev (proxies `/ws` → `8080`)
- `7655` — Napari bridge WS

### Module pattern
Every task = exactly two co-located files, **same base name**:
```
app/src/tasks/<category>/<name>.jl    # Julia: struct + _run_task implementation
app/src/tasks/<category>/<name>.json  # Param spec — single source of truth
```
The `.jl` and `.json` filenames must match — `remove.jl` pairs with `remove.json`, `imageTask.jl` pairs with `imageTask.json`. Never bundle multiple tasks into one `.jl` file.

The JSON spec is served to Vue via `GET /api/tasks/definitions?category=X` — Vue never maintains its own copy. Do **not** add task JSONs to `frontend/`. The `frontend/src/tasks/definitions/` directory is intentionally empty.

When adding a new task, also register it in `app/src/tasks/task_registry.jl`:
- Add a `_spec_path(::MyTask)` overload pointing at the `.json` file
- Add `"category.myTask" => MyTask()` to `_fun_name_map()`

---

## Development

**Git workflow:** branch + PR for everything; **never commit or push to `main`** (releases are
tagged off `main` after merge). Full conventions — branch naming, commit style, how PRs are
opened, release tagging — are in [`docs/DEV.md`](docs/DEV.md). **Agents: ask before every commit
and before opening/pushing a PR — explicitly, each time; don't commit or push proactively** (a
"go ahead" to do the work is not approval to commit it).

**Dev dir config — single source of truth:** `cecelia-pineapple/.env`
```
CECELIA_DEV_DIR=~/cecelia-pineapple/dev
```
This file is git-ignored (machine-specific). `init_cecelia!` reads it automatically — no env var export needed. The `CECELIA_DEV_DIR` env var still overrides it if set.

```bash
pixi run dev        # Julia server (Revise hot-reload) → :8080
pixi run frontend   # Vite → http://localhost:5173
```
`pixi run prod` runs the server without Revise (production). `pixi run stop` stops all three by port.
Revise reloads function bodies on save. Struct/macro changes still need a restart.

---

## Testing

Package tests live in `app/test/runtests.jl` and run fully headless — no API, WS, or Vue:
```bash
cd app && julia --project test/runtests.jl
```

**Rule: any change to core package functionality ships with a test in the same change.**
Core = the data model and its persistence, the versioned-variable convention, task dispatch,
and param validation. Specifically:
- **ccid.json round-trip** — every persisted field must survive `save!` → `init_object`. When
  you add a field to `CciaImage`/`CciaSet`/`CciaProject`, assert it round-trips. (`save!`
  silently dropping `status`/`attr`/`imChannelNames` went unnoticed precisely because no test
  covered this.)
- **Versioned fields** — anything stored versioned in `ccid.json` (`filepath`, `imChannelNames`,
  …) must be read/written through the `versioned_*` helpers and land at the agreed location.
  Assert the on-disk shape (`{value_name => …, "_active" => …}`), not just the in-memory value —
  a convention test like this catches a field quietly using the wrong storage location.
- **Task dispatch** — a new `fun_name` resolves to its struct via `_task_from_fun_name`.
- **Param validation** — at least one bad-param case per task asserting `ParamValidationError`.

### Test data fixtures

Tests must **not** depend on the dev projects dir (`projects_dir()`) — it can be deleted, and the
test would then silently skip. When a test needs real data, copy a **small** fixture into the
version-controlled fixtures dir at the **workspace root**:

```
<workspace-root>/test-data/projects/<proj>/1/<img>/labelProps/<name>.h5ad   # mirror the real layout
```

Resolve it in `runtests.jl` with the generic helpers — `fixture_path("proj","1","img","labelProps","x.h5ad")`
and `have_fixture(path)` (override the root with the `CECELIA_TEST_DATA` env var). Gate the testset
on `have_fixture(...)` and `@test_skip` otherwise; `have_fixture` emits a single strong `@warn` per
missing path. Unrelated tests must still pass. Example: the LabelProps/`pop_df` testsets.

**Keep fixtures small** — e.g. a `labelProps/*.h5ad` (hundreds of KB), not GB-scale raw images or
OME-ZARR pyramids. Document any fixture you add in `test-data/README.md`.

---

## Julia conventions

- Mutating functions: `!` suffix (`open_image!`, `set_channel_names!`)
- Strings: double quotes only (single quotes = `Char`)
- Multiple dispatch: separate method per type, not OOP overloading
- `@infiltrate` = `browser()` from R
- Shell commands: always platform-safe. Use `_kill_tree` (`api/src/sockets.jl`) and `_dir_bytes` (`app/src/utils.jl`); never write `pgrep`/`kill`/`du` inline.
- **Don't `export` generic names that collide with common deps or Base.** Exports land in any
  user's namespace; if Cecelia and another `using`'d package both export the same name, Julia
  leaves it *unbound* (ambiguous), breaking unqualified calls. In particular avoid clashing with
  **DataFrames** (`transform`, `select`, `groupby`, `combine`, `subset`, `rename`, `stack`,
  `unstack`, `nrow`, `describe`, `order`) and Base. Prefer specific names — e.g. we export
  `apply_transform`/`invert_transform`, not `transform`/`invert`. If you must share a generic
  verb, extend the owner's function (`import DataFrames: transform`) rather than exporting your own.

### HTTP.jl v2 WebSocket
Use `HTTP.listen`, not `HTTP.serve` — the latter is request→response only and doesn't support
WS upgrades. Full stream-handler/WS-upgrade/response conventions are in
[`docs/API.md`](docs/API.md) → *HTTP.jl v2 conventions* — read that before touching
`api/src/server.jl` or `api/src/sockets.jl`.

---

## Frontend conventions

See [`docs/UI.md`](docs/UI.md) for the full reference: design tokens, button utilities, module page authoring, component catalog, plot integration, and WS event patterns.

**Persist every user-settable option.** Any option on a module page / canvas (chart type, scope, compare mode, highlights, sliders, …) MUST live in persisted view state (`useViewState` over a store-backed bag / panel `state`), never a bare `ref()` — a `ref()` resets on remount, so options vanish when the user navigates away and back. This is a hard convention for new pages; see `docs/MODULES.md` → "RULE: persist every user-settable option" and `docs/UI.md` → "Persisting view state".

---

## Task system

See [`docs/MODULES.md`](docs/MODULES.md) for the complete step-by-step guide: Julia handler, Python script, JSON spec, registry, module page, route/nav wiring, composite tasks, and tests.

### Key invariants (read before writing any task)

**Tasks are sink-agnostic.** They report through injected callbacks and never call `ws_progress`/`ws_log` directly — those are API-layer concerns. The same `_run_task` runs unchanged from the REPL, a test, or the GUI.

```julia
# inside _run_task — always use callbacks, never ws_* directly
on_progress(n, total)
on_log("message")
```

**Implement `_run_task`, not `run_task`.** The scheduler's public `run_task` validates params, acquires a pool slot, writes the log file, then delegates to `_run_task`. Overriding `run_task` bypasses all of that.

**`on_process(proc)` is required** when launching a subprocess. It registers the process handle with the cancellation system. Omitting it means `task:cancel` cannot kill the subprocess.

**`proc.exitcode == 0` doesn't mean success on cancel.** libuv sets `exitcode = 0` for signal-killed processes. Always check both:
```julia
ok = proc.exitcode == 0 && proc.termsignal == 0
```

**`resource_pool` is required in every task JSON.** Standard values: `"gpu"` (limit 1), `"gpu-light"` (4), `"io"` (8), `"default"` (20). Defined in `app/config.toml`. The `tasksLimit` field and concurrent-task slider have been removed — use pools instead.

---

## OME-ZARR dual-format

Two layouts coexist — the reader handles both:

| Source | Layout | `multiscales` location |
|--------|--------|------------------------|
| bioformats2raw | Series wrapper: data at `zarr/0/[level]` | `zarr/0/.zattrs` |
| `create_multiscales()` | Flat: data at `zarr/[level]` | root `.zattrs` |

Detection in `zarr_utils.py` → `open_as_zarr`:
```python
if omezarr is True and 'multiscales' not in zgroup.attrs:
    zgroup = zgroup["0"]   # step into bioformats2raw series wrapper
```
Never assume one format — always detect.

**Exception, and a trap:** `read_ome_metadata` (`app/src/tasks/importImages/omezarr.jl`) does **not**
do this detection — it hardcodes the bioformats2raw nested layout (`zarr/0/.zattrs`), because it
only ever needs to read the *original import's* calibration metadata (`PhysicalSize*`,
`TimeIncrement*`). Downstream processed variants (drift/AF-correct, cellpose-correct) write the
**flat** layout — multiscales at the *root* `.zattrs` — so this reader, looking at `zarr/0/.zattrs`,
finds nothing there. (Those variants *do* carry calibration — `create_multiscales` writes the
`.zattrs` scale and `save_meta_in_zarr` writes an `OME/METADATA.ome.xml` sidecar; they only omit
the NGFF axis `unit`. So napari renders them correctly; it's specifically `read_ome_metadata`'s
nested-layout assumption that can't read them.) Pointing this reader at whichever zarr is currently
`active` therefore silently returns nothing (this bit `resync_ome_meta!` once: it originally read
`img_filepath(img)` and quietly no-opped on any image with a processed variant active). Any caller
of `read_ome_metadata` must resolve `img_filepath(img, VERSIONED_DEFAULT_VAL)` — the `"default"`
zarr — never the active one.

---

## Versioned variable pattern (ccid.json)

```json
{ "default": "ccidImage.ome.zarr", "_active": "default" }
```
- Read: `versioned_get_field(raw, "filepath", value_name)` (falls back to `"default"`)
- Write: `versioned_set_field!(raw, "filepath", value, value_name)`

**JSON3 gotcha — Symbol keys**: JSON3 yields Symbol keys (`:default`, `:_active`). Always convert when building a `Dict`:
```julia
Dict{String,Any}(String(k) => v for (k, v) in obj
                 if string(k) != VERSIONED_ACTIVE_KEY)
```
Without this, `get(dict, "default", nothing)` returns `nothing` silently even when the key exists.

**JSON3 gotcha — `isa Dict` vs `isa AbstractDict`**: `JSON3.Object <: AbstractDict` but `JSON3.Object isa Dict` is `false`. Any type guard that checks `isa Dict` will fail for values read from JSON3. Use `isa AbstractDict` everywhere. The versioned-field helpers (`versioned_set_field!`, `_to_str_dict`) already handle this — don't add new `isa Dict` checks.

---

## Input definition — param type reference

Full widget-type reference (every type, extra fields, JSON examples) lives in
[`docs/MODULES.md`](docs/MODULES.md) — don't duplicate it here.
