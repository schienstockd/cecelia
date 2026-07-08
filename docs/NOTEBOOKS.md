# Notebooks — the Pluto downstream-analysis Playground

The **Notebooks** section is a home for structured, per-project, **pure-Julia** downstream analysis:
load objects, pull cell tables via `pop_df`, plot with AlgebraOfGraphics + CairoMakie, export CSVs —
the work the old R Markdown vignettes did, now versioned and organised. Notebooks run in
[Pluto](https://plutojl.org/), reactive and reproducible, with no Python in the loop.

> Origin/design history: `docs/todo/NOTEBOOK_PLAYGROUND_PLAN.md`. This file is the durable reference.

## Using it

- **From the app:** the **Notebooks** sidebar item (Analysis group). *Launch server* starts Pluto;
  *Open Notebooks* opens it in a new tab. The table below manages this project's notebooks.
- **From the terminal:** `pixi run notebooks` (Pluto on **:7660**), `pixi run stop-notebooks`.
- **First plot is slow (~20 s)** unless a sysimage is built — `pixi run notebooks-sysimage` (dev).
  See *Sysimage* below.

## Where things live

```
pluto/                       ← the notebook ENGINE (its own Julia env; path-sources Cecelia like api/)
  Project.toml / Manifest    ← Pluto + CairoMakie + AlgebraOfGraphics + DataFrames + CSV + Cecelia + CeceliaNb
  launch.jl                  ← starts Pluto (:7660), wires the sysimage + CECELIA_PLUTO_ENV
  build_sysimage.jl          ← deps-only sysimage (dev)      → pluto/deps.so (git-ignored)
  build_sysimage_full.jl     ← full sysimage (release)       → pluto/deps.so
  notebook_template.jl       ← starter copied by "Add notebook"
  CeceliaNb/                 ← small notebook-side helper package (aggregation + AoG plot shortcuts)
notebooks/                   ← shipped EXAMPLE notebooks (UID-free, versioned with the code)
  example_populations.jl · example_pop_df.jl · example_object_model.jl
{project}/notebooks/         ← a project's own notebooks (created/managed from the UI)
  .snapshots/<name>@v<N>.jl  ← version snapshots
{project}/settings/notebooks.json  ← per-project registry: { file → {description, version, updatedAt} }
```

## Authoring a notebook

The **first cell** activates the engine env (so the dev `Cecelia` + `CeceliaNb` resolve — Pluto's
built-in package manager is registered-only and can't load them):

```julia
begin
    import Pkg
    Pkg.activate(get(ENV, "CECELIA_PLUTO_ENV", joinpath(@__DIR__, "..", "pluto")))
end
```

Then the headless REPL contract is all available:

```julia
using Cecelia, DataFrames, CeceliaNb, AlgebraOfGraphics, CairoMakie, CSV
Cecelia.init_cecelia!()
img = init_object(proj_uid, uid)                       # a CciaImage / CciaSet (dispatches on class)
df  = pop_df(img, "flow", ["/qc"]; value_name = "A")   # cell table (gates evaluated in-process)
df  = label_props(img; value_name = "A") |> as_df      # or the raw segmentation table
nb_hist(df, :volume; bins = 40)                        # CeceliaNb plot shortcut
CSV.write(joinpath(projects_dir(), proj_uid, "exports", "cells.csv"), df)
```

`run_task(...)` also works — a notebook can re-run pipeline tasks (respect the project lock when
mutating). Keep shipped examples **UID-free**: read the project/image from `CECELIA_EXAMPLE_*` env or
editable top cells, never hard-code a UID.

### `CeceliaNb` helpers

Thin, deliberately minimal (grow from real use). `nb_count(df)` / `nb_summary(df, measure; by=…)`
aggregate a `pop_df` table — **same numbers as the `/analysis` board** (both build on `pop_df`; only
the rendering differs). `nb_hist` / `nb_box` / `nb_scatter` are one-liners over AlgebraOfGraphics.
For anything more, use AoG directly: `data(df) * mapping(...) * visual(...) |> draw`.

### Gotcha — `md"..."` interpolation

Julia's single-quoted `md"..."` **cannot contain nested double-quotes inside `$(…)`** (e.g.
`md"$(join(x, ", "))"` is a parse error). Use **triple-quoted** `md"""..."""` (nested `"` are safe
there), or compute the string in plain code and interpolate a single variable. Backticks are fine.

## Versioning — snapshots & restore

Provenance without git or file-watching:
- **Snapshot** (📷) freezes the current notebook to `.snapshots/<name>@v<N>.jl` (N = next number). This
  is the **only** thing that creates a version.
- **History** (🕘) opens a version dropdown; **Restore** overwrites the live notebook with the chosen
  snapshot. It does **not** create a snapshot (so repeated restores don't pile up versions); a
  two-click confirm guards un-snapshotted edits — snapshot first if you want to keep the current state.
  Pluto auto-reloads the file (`auto_reload_from_file`), so an open notebook updates live.
- The **Ver** column shows which snapshot the notebook currently reflects: a fresh notebook is `—`,
  Snapshot advances it to the new number, and Restore sets it to the version you restored (restore v3
  → the column reads `v3`). It is a *pointer to current state*, not a monotonic counter.

## Sysimage (why the first plot isn't slow)

Makie compiles plotting code on first use (~20 s cold). A PackageCompiler sysimage bakes that in
(measured cold-start 32 s → 7.6 s). Built to `pluto/deps.so` (git-ignored, ~1.4 GB, ~10 min):
`notebooks-sysimage` (dev, deps only — excludes Cecelia so Revise still hot-reloads it) /
`notebooks-sysimage-full` (release, bakes Cecelia + CeceliaNb in). `launch.jl` picks up `deps.so`
automatically and passes it to notebook workers; without it, notebooks still work, just slow-first-plot.
Release packaging: see `docs/SHIPPING.md`.

## API surface (`api/src/notebooks_api.jl`)

The Pluto server is a separate process; lifecycle mirrors napari (probe :7660 → adopt or spawn).
**Security:** Pluto's secret protection is left ON (its secure default) — Pluto is a browser-reachable
code-execution surface, so without the secret any website you visit could drive it (CSRF/RCE). We do
**not** disable it. `launch.jl` publishes the session secret to `pluto/.plutosecret` (git-ignored);
`launch`/`status` return it and the frontend appends it to URLs (`…/?secret=…`, `…/open?path=…&secret=…`).
Routes:

| Route | Purpose |
|---|---|
| `POST /api/notebooks/launch` | ensure the server is up (202 while starting) → `{url}` |
| `GET  /api/notebooks/status` | `{running, starting, url}` |
| `GET  /api/notebooks?projectUid=` | list notebooks (project + example scopes) with description/version/path |
| `POST /api/notebooks/create` | new notebook from the template |
| `POST /api/notebooks/describe` | set a notebook's description |
| `POST /api/notebooks/duplicate` | copy a project/example notebook into the project |
| `POST /api/notebooks/delete` | delete a project notebook |
| `POST /api/notebooks/snapshot` | freeze a version |
| `GET  /api/notebooks/snapshots?projectUid=&file=` | list a notebook's snapshots |
| `POST /api/notebooks/restore` | restore a snapshot into the live notebook |
| `POST /api/notebooks/shutdown` | stop the server (only one this app spawned) |
| `POST /api/notebooks/restart` | stop + relaunch |

### Lifecycle & cleanup

The server is spawned by the API server (`wait=false`) and is **not** bound to it — so it must be
stopped explicitly, exactly like the napari bridge. Three ways: the **Shut down** / **Restart**
buttons on the page, `pixi run stop-notebooks` (or `pixi run stop`, which also does :7660), and an
`atexit` hook that kills a server *this* API process spawned when the server exits cleanly (won't fire
on SIGKILL — that's what stop-by-port is for). Shutdown/restart can only kill a server this app
spawned (it holds the process handle); one started by `pixi run notebooks` must be stopped by port.
Destructive ops (`delete`/`restore`) require `force` when the server is up (the UI's confirm supplies
it) — see *Versioning*. First-run: if the `pluto/` env isn't set up, launch fails with a hint to run
`pixi run notebooks-instantiate`.

Frontend: `modules/NotebooksModule.vue` (launch + status) + `components/NotebookTable.vue` (registry
table, mirrors `ImageTable`). `api/src/*.jl` is **not** Revise-tracked — restart the server after edits.
