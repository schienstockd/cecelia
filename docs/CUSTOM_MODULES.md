# Custom modules — user drop-in tasks

Add your own task function to Cecelia by **dropping files into your config directory** — no package
edit, no rebuild. This restores the old R-version capability (`source()`ing a `modules/` folder) for
the Julia rewrite. A dropped task appears in the GUI like any built-in and runs through the normal
scheduler/chain machinery.

> **Trust model.** A custom module is arbitrary Julia (and optionally Python) with **full access to
> your machine** — exactly like the old R `source()`. There is no sandbox. Only run modules you
> wrote or trust; only you can drop files into your own config dir. This is a power-user feature.

## Where the files go

Everything lives under `modules/` inside your **config directory** (the same dir that holds
`custom.toml`) — resolved by `config_dir()`:

- Installed app: `~/.cecelia/`  →  `~/.cecelia/modules/`
- Dev checkout: your `CECELIA_DEV_DIR` (from `.env`)  →  `<dev>/modules/`

Layout — **co-located**, identical to a built-in task under `app/src/tasks/`: all of a task's files
share one `<category>/` folder and the same base name.

```
<config_dir>/modules/
  <category>/<name>.jl       # Julia: struct <: CciaTask + _run_task + register_task!
  <category>/<name>.json     # param/UI spec (same schema as the built-in app/src/tasks/*.json)
  <category>/<name>_run.py   # OPTIONAL heavy compute, launched via run_py
```

`<category>` (the sub-directory name) + `<name>` (the file stem) form the `fun_name`,
`"<category>.<name>"` — exactly like built-ins. The loader scans `<config_dir>/modules/<category>/`
for `.jl` files; the `.json` and `_run.py` are resolved next to the `.jl` (see the three files
below).

**Category = which module page it shows on.** If `<category>` matches an existing page
(`import`, `cleanupImages`, `segment`, `tracking`, `behaviour`, `clustPops`, `clustTracks`,
`editImages`), the task appears in that page's task list automatically. A **brand-new category** gets
a **generic page** at `/custom/<category>` and a **"Custom"** group in the sidebar (image picker +
task runner, no plot canvas) — nothing to wire up.

## The three files

### 1. `<category>/<name>.json` — the form

Identical schema to the built-in specs in `app/src/tasks/*/*.json` (see
[`docs/MODULES.md`](MODULES.md) for the full param-type reference). Minimum:

```json
{
  "fun_name": "behaviour.exampleNormalise",
  "label": "Example: normalise a measure (custom)",
  "category": "behaviour",
  "resource_pool": "cpu",
  "scope": "image",
  "params": [
    { "key": "column", "label": "Measure column", "type": "text", "required": true }
  ]
}
```

`resource_pool` is required (`"cpu"` / `"gpu"` / `"io"` / `"network"`). `scope` is `"image"`
(default) or `"set"`.

### 2. `<category>/<name>.jl` — the code

The file is `include`d **into the `Cecelia` module**, so reference package names with the `Cecelia.`
prefix (or unqualified — both work). Define a struct, implement `Cecelia._run_task`, and finish by
calling `Cecelia.register_task!` with the path to your JSON spec:

```julia
struct ExampleNormalise <: Cecelia.CciaTask end

function Cecelia._run_task(::ExampleNormalise, img::Cecelia.CciaImage, params::Dict{String,Any};
                           on_log::Function = _ -> nothing,
                           on_progress::Function = (n, t) -> nothing,
                           on_process::Function = _ -> nothing)
    # ... read/compute/write; report via the callbacks, never ws_* directly ...
end

Cecelia.register_task!("behaviour.exampleNormalise", ExampleNormalise();
                       spec = joinpath(@__DIR__, "exampleNormalise.json"))   # co-located
```

Follow the same invariants as built-in tasks (see [`docs/MODULES.md`](MODULES.md)):

- Implement **`_run_task`**, not `run_task` (the scheduler wraps it: validates params, acquires a
  pool slot, writes the log).
- Report only through the injected callbacks (`on_log`, `on_progress`, `on_process`) — a task is
  sink-agnostic.
- **Read/write cell data only through the label-props view** (`label_props |> … |> as_df`;
  `label_props |> add_obs |> save!`) — never touch the `.h5ad` directly.
- **Open images only through `zarr_utils`**, never a bare `zarr.open`.

Two runnable examples ship in [`docs/examples/custom-modules/`](examples/custom-modules/) — copy the
category folders into `<config_dir>/modules/`:
- `behaviour.exampleNormalise` — minimal, Julia-only, in an existing category.
- `customExamples.trackContext` — Julia **and** Python, nested params, in a **new** category (so it
  also demonstrates the generic `/custom/:category` page).

### 3. `<category>/<name>_run.py` — optional compute

For heavy compute, add a Python runner **beside your `.jl`** and launch it with **`run_py`** — the one
sanctioned Python launcher (never spawn Python by hand). Pass the **absolute path** to your script
(co-located, so `@__DIR__`):

```julia
script = joinpath(@__DIR__, "exampleThreshold_run.py")
ok = Cecelia.run_py(script, (; someParam = 1), Cecelia.task_run_dir(img._dir);
                    on_log = on_log, on_progress = on_progress, on_process = on_process)
ok || return nothing
```

`run_py` puts both `python/` (so `import cecelia.*` works) and your `<config_dir>/modules/`
on `PYTHONPATH`, writes the params JSON, streams `[PROGRESS] n/total` → `on_progress`, and checks
clean exit. (Your runner's own category folder is also on `sys.path[0]` since it's launched by
absolute path, so a co-located sibling `.py` imports directly.) Your `_run.py` reads params via
`cecelia.utils.script_utils.script_params()` — see the built-in runners under `app/src/tasks/` for
the pattern. **No `sys.path` bootstrapping.**

## QC (recommended)

Like a built-in task, a result-producing custom module should bank advisory QC so its output flows to
the image badge, the `[Cecelia]` lab-log digest and the observer. Call **`Cecelia.write_qc(img, fun,
value_name, findings; metrics)`** after the work succeeds — `metrics` is an objective count, `findings`
a vector of `Cecelia.qc_finding("warn", code, short, long)` for the bad case (advisory only; never
blocks). Write it under **your own `fun_name`** — that is the fun the image badge and the digest ⚠️
resolve against.

To make your metric cohort-comparable across a set (the `get_cohort_qc` / `/api/qc/cohort` outlier
check), declare its keys **at load time**, next to your `register_task!`:

```julia
Cecelia.register_cohort_metrics!("customExamples.myTask", ["nCells"])
```

The category you tag in the JSON (`"category": "customExamples"`) automatically appears in the lab-log
mute bar's **Module pages** group — so a user can mute your module's `[Cecelia]` digest lines.

## Loading & reloading

Custom modules are loaded once on server start. To pick up **newly dropped** files without a restart,
use **Settings → Custom modules**: it shows the modules directory, lists every module with its
loaded/error status, and has a **Reload** button. (Under the hood: `GET /api/tasks/custom-modules`
for status — `{ dir, modules: [{path,status,error}], categories: [...] }` — and
`POST /api/tasks/custom-modules/reload` to rescan.)

Newly dropped `.jl` files are `include`d on reload; **edits to an already-loaded `.jl` need a full
server restart** (re-`include`ing a Julia `struct` isn't allowed — the same rule as any `app/` struct
change). Edits to a `.json` spec are picked up live (the definitions endpoint rescans on every
request). A broken module is logged and reported, never crashes the server.

## Limits / not-goals

- Not a sandbox and not a marketplace — local, trusted, single-user drop-in only.
- A new category's generic page has an image picker + task runner but **no plot canvas** (custom
  categories have no registered plot specs); results are still plottable on the Analysis board / in
  the built-in Explore pages once written to the `.h5ad`.
- This does not change the in-repo task authoring flow — [`docs/MODULES.md`](MODULES.md) stays for
  tasks that ship inside the package.

## Pointers

Registry + loader: `app/src/tasks/task.jl` (`register_task!`, `_task_from_fun_name`) and
`app/src/tasks/custom_modules.jl` (`load_custom_modules!`). Definitions scan:
`api/src/routes.jl` `api_task_definitions`. Python launcher: `app/src/py_runner.jl` `run_py`.
Config resolver: `app/src/config.jl` `config_dir`.
