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
- **First plot is slow (~20 s)** until the fast-plot sysimage is built. Build it from the Notebooks
  page — an **Enable fast plots** button (background, ~10 min); after an update it becomes a
  **Rebuild** prompt. `pixi run notebooks-sysimage` is the manual/dev path. See *Sysimage* below.

## Where things live

```
pluto/                       ← the notebook ENGINE (its own Julia env; path-sources Cecelia like api/)
  Project.toml / Manifest    ← Pluto + CairoMakie + AlgebraOfGraphics + DataFrames + CSV + Cecelia + CeceliaNb
  launch.jl                  ← starts Pluto (:7660), wires the sysimage + CECELIA_PLUTO_ENV
  build_sysimage.jl          ← deps-only sysimage (dev)      → pluto/deps.so (git-ignored)
  build_sysimage_full.jl     ← full sysimage (release)       → pluto/deps.so
  sysimage_stamp.jl          ← writes/checks deps.so.stamp (Julia+Manifest) for update-staleness
  notebook_template.jl       ← starter copied by "Add notebook"
  CeceliaNb/                 ← small notebook-side helper package (aggregation + AoG plot shortcuts)
notebooks/                   ← shipped EXAMPLE notebooks (UID-free, versioned with the code)
  example_populations.jl · example_pop_df.jl · example_object_model.jl
{project}/notebooks/         ← a project's own notebooks (created/managed from the UI)
  .snapshots/<name>@v<N>.jl  ← version snapshots
{project}/settings/notebooks.json  ← per-project registry: { file → {description, version, updatedAt} }
```

> **The `pluto/` env path-sources `../app` and pins Cecelia's deps in its own `pluto/Manifest.toml`.**
> So **whenever a Julia dep is added to `app/Project.toml`, you must re-resolve this env** (`pixi run
> notebooks-instantiate`, which now runs `Pkg.resolve()` first) and commit the updated
> `pluto/Manifest.toml` — otherwise every notebook fails to precompile with `Package Cecelia does not
> have <Dep> in its dependencies`. This is one of the three envs in CLAUDE.md → *Adding a Julia
> dependency to `app/`*; a stale pluto manifest here (missing `Clustering`/`NearestNeighbors`) is what
> broke notebooks after the spatial-analysis merge.

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

## AI-generated notebooks (`create_notebook`)

The observer can generate a notebook for the user: `POST /api/notebooks/write { projectUid, name,
cells, description }` serialises a list of Julia cell sources into valid Pluto format
(`_pluto_notebook_source` in `notebooks_api.jl` — the env-activation cell is prepended, so the result
is self-contained/runnable), registers it, and snapshots v1. **Create-only** (409 on an existing name)
— it never overwrites a notebook the user may have edited, so the read-only observer stays
non-destructive. The user then opens it in the Notebooks page and edits/owns it in Pluto. Backed by
the `create_notebook` MCP tool; the code-generation guidance for Claude lives in `docs/REPL.md`.

To **change an existing** notebook, the observer uses `POST /api/notebooks/revise` (MCP tool
`revise_notebook`): it **snapshots the current notebook first** (freezing whatever is live — including
the user's un-snapshotted edits — as a restorable version) then overwrites its cells. That's the
versioned-revision path — the observer never makes a `<name>-v2` copy, and nothing is lost (History →
Restore brings the prior version back). Descriptions (create/describe/revise) are capped to one short
line (`_NB_DESC_MAX`).

## Sysimage (why the first plot isn't slow)

Makie compiles plotting code on first use (~20 s cold). A PackageCompiler sysimage bakes that in
(measured cold-start 32 s → 7.6 s). Built to `pluto/deps.so` (git-ignored, ~1.4 GB, ~10 min):
`notebooks-sysimage` (dev, deps only — excludes Cecelia so Revise still hot-reloads it) /
`notebooks-sysimage-full` (release, bakes Cecelia + CeceliaNb in). Both run the same
`create_sysimage`; the **only** difference is the package list — the `-full` build adds `CSV`,
`Cecelia`, and `CeceliaNb`, so a release image also skips the first-`pop_df` compile, at the cost of
being frozen (no Revise). `launch.jl` picks up `deps.so` and passes it to notebook workers; without
it, notebooks still work, just slow-first-plot.

**Built from a button, not by hand.** An end user never runs the `pixi` task. The Notebooks page shows
an **Enable fast plots** button (→ `POST /api/notebooks/build-sysimage`) that builds `deps.so` in a
**background process** while notebooks stay fully usable (a banner explains the slow-first-plot until
it lands). The fresh image is used from the **next** server launch — we never restart a running server
out from under an open session. The build is opt-in (a ~10 min, ~1.4 GB job shouldn't start on a
stray click); `pixi run notebooks-sysimage` remains the manual/dev path.

**Update-safe (the stamp).** A sysimage is native code tied to the exact Julia version + baked package
versions, so it can't be shipped prebuilt as one universal artifact, and after an update the on-disk
image goes **stale**. Each build writes a sidecar `deps.so.stamp` (`{julia, hash(Manifest.toml)}`, see
`pluto/sysimage_stamp.jl`). Freshness = both fields match the current Julia + Manifest. `launch.jl`
ignores a stale image (falls back to slow-first-plot rather than handing workers an incompatible one),
and the status endpoint reports `stale` so the page shows a **Rebuild** button — same flow as first-run.
`_classify_sysimage` (in `notebooks_api.jl`) is the pure, tested classifier: `ready` / `stale` /
`building` / `error` / `absent`. Release packaging (ship a prebuilt image per platform vs. build on
first run): see `docs/SHIPPING.md` and TODO #00070.

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
| `GET  /api/notebooks/status` | `{running, starting, url, sysimage}` (`sysimage`: ready/stale/building/error/absent) |
| `POST /api/notebooks/build-sysimage` | start the background fast-plot build (idempotent) → `{status}` |
| `GET  /api/notebooks?projectUid=` | list notebooks (project + example scopes) with description/version/path |
| `POST /api/notebooks/create` | new notebook from the template |
| `POST /api/notebooks/describe` | set a notebook's description |
| `POST /api/notebooks/duplicate` | copy a project/example notebook into the project |
| `POST /api/notebooks/revise` | new version of an existing notebook: snapshot then overwrite cells (the MCP's versioned-revision path) |
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
