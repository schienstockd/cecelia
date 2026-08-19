# Extending Cecelia — custom modules and plugins

You can add your own analysis step to Cecelia without touching the package or rebuilding anything.
Two ways to do it, and the difference is only **how it is packaged**:

|  | **Custom module** | **Plugin** |
|---|---|---|
| What it is | loose files you drop in a folder | one directory, usually a git repo |
| What you get | your task, on an existing or new page | the same, **plus a page that plots its output** |
| Who it is for | yourself, one machine | anything you want to share, version or reinstall |
| How you install it | copy the files | Settings → Plugins, paste a URL |
| How you update it | edit the files | reinstall; the version is recorded |

**They are the same underneath.** A plugin *is* a set of custom modules with a `plugin.json` beside
them, and the task files inside it are byte-for-byte what you would have dropped in loose. Nothing
you learn for one is wasted on the other, and moving a working module into a plugin is a matter of
putting it in a folder and adding a manifest.

**So which do you want?**

- Trying something out, or it only matters on your own machine → **custom module**. Fewest moving
  parts, no manifest, no repo.
- Someone else will run it, or you want it back after reinstalling Cecelia → **plugin**.
- You want a **page of your own** — a task on the left, plots of its results below — → **plugin**.
  That is the one thing a loose drop-in cannot give you, and it is the reason plugins exist at all.

Everything else in this document applies to both. Where something is plugin-only it says so.

> **Neither is sandboxed.** A custom module is arbitrary Julia (and optionally Python) with **full
> access to your machine**, exactly like the old R version's `source()`. Installing a plugin is
> trusting whoever wrote it, the same way installing an R package is. Only you can drop files into
> your own config directory; only run code you wrote or trust.

## Start here — the smallest thing that works

You need **two files** with the same name, in a folder named after the page you want the task on:

```
<config_dir>/modules/tracking/myFirstTask.json    ← the form the user fills in
<config_dir>/modules/tracking/myFirstTask.jl      ← what happens when they press Run
```

`<config_dir>` is where your `custom.toml` lives — `~/.cecelia/` for an installed app. The folder
name (`tracking`) decides which page it appears on; a name that is not an existing page gets you a
new page under **Custom** in the sidebar, with nothing to wire up.

Then **Settings → Custom modules → Reload**, and your task is in the list. There is no restart and no
rebuild.

Two complete, runnable examples live in the repo, and CI loads both on every commit, so they cannot
quietly stop working:

- [`docs/examples/custom-modules/`](examples/custom-modules/) — loose drop-ins, including one with a
  Python runner.
- [`docs/examples/plugins/ccia-importTracks/`](examples/plugins/) — a real plugin: importing tracks
  from ImageJ, TrackMate or Imaris.

Copy one and change it. That is the fastest correct route, and it is the route that stays correct,
because those files are tested.

## What you can put in the form

The `.json` is the whole form — you are declaring controls, not writing any interface code. Sliders,
dropdowns, file pickers, population pickers, channel pickers, collapsible sections, and rules like
"only show this field when that one says *attach*" are all fields in the JSON.

The full list, with an example of each, is in
[`docs/MODULES.md` → *Param types*](MODULES.md#param-types) and *Fields any param may carry*. It is
the same reference the built-in tasks use, because your task's form is built by the same code.

Worth knowing it exists, because it saves people writing Julia they did not need to:

| You want | Field |
|---|---|
| a field that only applies sometimes | `showIf` |
| a required field, with your own wording | `required` + `requiredMessage` |
| a picker filled from installed models | `optionsFrom` |
| a default that follows a Settings choice | `defaultFrom` |
| the user to choose a file on disk | `type: "filePath"` |
| a short either/or shown as buttons | `variant: "chips"` |

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
- **Create any subdirectory of `img._dir` you write into** — `mkpath(dirname(out))`, or Python's
  `atomic_path` / `write_h5ad_atomic`, which do it for you. Nothing under `1/{uid}/` is pre-created;
  a subdirectory exists iff a task has written there (docs/OBJECTMODEL.md → *Disk layout*). A module
  written against the old behaviour, which created a fixed set of directories at import, has to add
  the `mkpath` it was relying on. `Cecelia.task_run_dir` makes its own, so `run_py` needs nothing.

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
for status — `{ dir, modules: [{path,plugin,status,error}], plugins: [...], clashes: [...],
categories: [...] }` — and `POST /api/tasks/custom-modules/reload` to rescan.)

Newly dropped `.jl` files are `include`d on reload; **edits to an already-loaded `.jl` need a full
server restart** (re-`include`ing a Julia `struct` isn't allowed — the same rule as any `app/` struct
change). Edits to a `.json` spec are picked up live (the definitions endpoint rescans on every
request). A broken module is logged and reported, never crashes the server.

Modules load in a fixed order — **hand-dropped first, then plugins, each path-sorted**. That order is
what makes name precedence below reproducible instead of depending on the filesystem.

## Plugins — a module set in one directory

A **plugin** ships a custom task **and the custom module page that inspects it**, as ONE directory —
installed, updated and removed as a unit (typically a git repo). The page is the part that matters: the
drop-in loader above already gives you the task, so packaging alone would add nothing.

```
<config_dir>/modules/plugins/<plugin>/
  plugin.json                      # manifest: name, version, description, homepage, requiresCecelia
                                   # + OPTIONAL `contributions` — see below
  <category>/<name>.{jl,json,_run.py}   # the same co-located layout as above
  plotDefinitions/<id>.json        # OPTIONAL plot specs — the page's canvas
  python/                          # OPTIONAL shared Python, importable as a top-level module
```

A complete runnable example (loaded by CI, so it cannot rot):
[`examples/plugins/`](examples/plugins/).

### Declaring what you contribute — optional

**You never have to.** The layout above already says everything: a `<category>/<name>.json` is a task,
a `plotDefinitions/*.json` is a plot. `ccia-importTracks` declares nothing and is complete.

The manifest may nonetheless spell it out, in a `contributions` block:

```json
"contributions": {
  "tasks": [{ "funName": "trackTools.cumulativeChange" }],
  "plots": [{ "spec": "plotDefinitions/cumulative_change.json" }]
}
```

What you get for writing it is a **check**, not a capability: a declared task whose `fun_name` no file
defines, or a declared plot spec that is not there, is reported against your plugin in Settings. It is
useful when you rename something — the manifest is the second place that has to change, and it says
so instead of silently disagreeing. `ccia-trackMeasures` declares its two contributions for exactly
that reason.

Declaring never *restricts*. A task on disk is a task whether or not the manifest mentions it, so
adding a block and forgetting a line cannot hide your own work.

### Borrowing a built-in plot — `contributions.views`

Your page's canvas draws the plot specs you ship. It can **also** show one of Cecelia's own
interactive plots, pointed at your data — so `ccia-trackMeasures` shows its cumulative measures *and*
the track paths those numbers came from:

```json
"contributions": {
  "views": [{ "module": "trackTools", "view": "trackPaths", "label": "Tracks" }]
}
```

`module` is the category whose page it appears on; `view` is the plot's **stable id**; `label` is
optional and defaults to the plot's own name. It then shows up under **Interactive** in that page's
`+ Plot…` picker. These plots fetch their own data and carry their own controls, so the population
picker does not drive them.

Ids you may name today:

| `view` | What it draws |
|---|---|
| `trackPaths` | tracks as paths, coloured by any track column |
| `trackDiagnostics` | the celltrackR QC battery for a tracking result |
| `gatingStrategy` | the gating tree for one segmentation |
| `filmstrip` | a montage of napari screenshots |

Not every built-in plot is on this list, and the reason is worth knowing: some need a side rail your
page does not render, and `trackCorrection` **mutates**. An id that is not offered — or does not exist
— is reported on the canvas ("Plot not available here: …") rather than quietly missing from the
picker.

What this makes public is view **ids**, not components: renaming `trackPaths` would break plugins that
named it, but rewriting the plot behind it does not.

The remaining kind, `layers` — saying how your task's output should be drawn in napari — is
**understood but not yet acted on**. Declaring one today gets you a message saying so. The design is
[`todo/PLUGINS_PLAN.md`](todo/PLUGINS_PLAN.md) → *The contribution model*, Decision 12.

### The module page

Give a task a **new** category and it gets the generic page at `/custom/<category>` plus a **Custom**
sidebar group. Drop a plot spec in `plotDefinitions/` declaring `"module": "<category>"` and that page
gains a plot canvas over whatever the task wrote — run on the right, inspect below, exactly like a
built-in page.

Both halves are declarative, so **a plugin ships JSON, not Vue**:

| Half of the page | Declared by | Rendered by |
|---|---|---|
| the task form | the task spec's `params` | `ParamRenderer` |
| the plot canvas | a `plotDefinitions/*.json` (`module: "<category>"`) | `SummaryCanvas` |
| a borrowed built-in plot | `contributions.views` naming a view id | `InteractivePanel` on that canvas |

That is a deliberate choice, not a hard limit. A stable install ships a prebuilt `frontend/dist` and
precompiles SFCs, so a plugin's `.vue` file could not be compiled there — but pre-compiled ESM using
render functions would load fine. It is excluded because shipping renderable code makes the frontend a
**plugin ABI**: a component contract that cannot be refactored freely, plus a loader and version skew
between a plugin and the app drawing it. Declarative specs cost a plugin author far less to maintain
across releases.

**If you are coming from napari, this is less of a difference than it looks.** napari's own first
tutorial builds its widget with `autogenerate: true` — generated from a function signature by
magicgui — which is the same pattern as a task spec's `params` here. Hand-written widgets are its
escape hatch, not its standard path. And napari's most-used contributions put data on screen by
*returning a data tuple* (`LayerData = (data, [attributes, [layer_type]])`), not by drawing.

A plugin CAN now borrow one of cecelia's own interactive plots for its page
(`contributions.views`, above) — so "a plugin gets a real, non-declarative page" is settled, without
making any component a contract. What it still cannot do is say what its output should LOOK LIKE in
the VIEWER — draw my tracks as tracks, add my points as a layer. The grammar for that exists
(`contributions.layers`) and is checked; nothing renders through it yet. Designed, not built:
[`todo/PLUGINS_PLAN.md`](todo/PLUGINS_PLAN.md) → *The contribution model*, Decision 12.

Worth being clear about what is **not** narrower: a plugin's compute. Both napari and cecelia run
plugin code unsandboxed with full machine access — Python there, Julia plus a Python runner here. The
only narrower surface is browser rendering, and only because a `.vue` needs compiling.

Plot spec ids follow the same precedence as task names — **built-ins win**, so a plugin cannot replace
a package plot by reusing its id.

**The category is the directory below the plugin root, never the plugin's name** — a task in
`plugins/trackimport-smithlab/tracking/` lands on the **Tracking** page, exactly as if it had been
dropped at `modules/tracking/`. A plugin directory may be named anything a repo may be named; it is
not required to be a Julia or Python identifier.

**Name precedence: built-in > hand-dropped > plugin.** Within a tier the first module loaded keeps
the `fun_name`. A module that loses a clash still loads fine — it just doesn't get the name — so it
reports `ok` in the module list and appears instead in the **clashes** list, which names the winner.
That list is the only place explaining why a task is absent from the UI.

`requiresCecelia` is **advisory**: a mismatch warns and never blocks (otherwise every cecelia release
would break every plugin at once), and the check is skipped entirely on a dev checkout, where the
running version is the literal `"dev"`.

### Installing, updating, removing

**Settings → Plugins.** Paste a repo URL and press install; the table lists what is installed with its
version, and removes one in a click. No `git` binary is needed — GitHub serves any ref as a tarball,
and an installed Cecelia has no git. A curated list of known plugins is offered alongside the URL box.

The ref you installed is recorded in a file **beside** `plugin.json`, never inside it: `plugin.json`
ships from the plugin's own repo, so writing into it would dirty the checkout and be overwritten by
the next update.

Cloning by hand into `<config_dir>/modules/plugins/` still works and always will — that is all an
install does. A hand-cloned plugin simply has no recorded ref, and is not treated as broken for it.

> **A plugin is not sandboxed.** Its Julia is `Base.include`d into `Cecelia` with full access to your
> machine, exactly like a module you dropped in yourself. Installing one is trusting whoever wrote it.

## Limits / not-goals

- Not a sandbox — plugin and drop-in code both run unconfined, by design.
- Plugins may only use what the Python env already ships; a plugin cannot declare its own pip deps.
- A plugin ships **no Vue**. Both halves of a page are declarative — the form from your task spec, the
  plot canvas from a `plotDefinitions/*.json` — so there is nothing to compile. That is a deliberate
  choice explained under *The module page*, not a gap to work around.
- A new category's generic page has an image picker + task runner but **no plot canvas** (custom
  categories have no registered plot specs); results are still plottable on the Analysis board / in
  the built-in Explore pages once written to the `.h5ad`.
- This does not change the in-repo task authoring flow — [`docs/MODULES.md`](MODULES.md) stays for
  tasks that ship inside the package.

## Pointers

Registry + precedence: `app/src/tasks/task.jl` (`register_task!`, `_task_from_fun_name`,
`custom_task_clashes`). Loader: `app/src/tasks/custom_modules.jl` (`load_custom_modules!`,
`_custom_module_sources`). Plugin layout + the ONE spec enumerator both API scans use:
`app/src/tasks/plugins.jl` (`user_task_specs`, `plugin_roots`, `read_plugin_manifest`). Definitions
scan: `api/src/routes.jl` `api_task_definitions` + `_custom_module_categories`. Python launcher:
`app/src/py_runner.jl` `run_py` (`_custom_modules_pydirs` puts each plugin root on `PYTHONPATH`).
Config resolver: `app/src/config.jl` `config_dir`.
