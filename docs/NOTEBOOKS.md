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
  sysimage_stamp.jl          ← writes/checks deps.so.stamp (Julia+Manifest+variant); staleness + which recipe
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

### Refreshing after a pipeline re-run — **notebooks do NOT invalidate when data changes**

This bites every notebook, so it is a convention, not a per-notebook detail.

**Pluto's reactivity is over cell code, not over files.** It has no filesystem watcher, so when a
task re-runs and rewrites an `.h5ad`/zarr, an open notebook keeps showing the **old numbers with no
indication anything is stale** — the worst failure mode available, because the plot still renders.
Nothing in Julia or Pluto detects this for you: `pop_df` was already called, the DataFrame is in
memory, and no cell changed.

The pattern: one **stamp cell** listing the files the notebook reads, and a bare `DATA_STAMP`
reference at the top of every cell that reads from disk. Re-running the stamp cell (Shift+Enter)
re-reads the timestamps and cascades to all of them.

```julia
# ⟳ RE-RUN THIS CELL after a pipeline task wrote new data.
DATA_STAMP = let
    _mt(f) = isfile(f) ? mtime(f) : 0.0
    (branch = _mt(img_branch_props_path(img, "SHG")),
     tracks = _mt(img_label_props_path(img, "T")),
     read_at = time())     # makes the value change even if mtime granularity hides a fast rewrite
end
```

```julia
begin
    DATA_STAMP          # ← the dependency that makes the refresh work; do not remove
    df = pop_df(img, "live", ["/_tracked"]; value_name = "T")
end
```

`pluto/notebook_template.jl` ships this (commented, with the explanation), so every notebook created
from **Add notebook** starts with it. Generated notebooks should include it too — `docs/REPL.md`.

**Why not something automatic?** Three alternatives, all worse today:

| option | why not |
|---|---|
| A watcher that re-runs cells on file change | A pipeline task rewrites files *while* running; re-running mid-write reads a torn file. Would need write-completion events, which the task rail doesn't publish to Pluto. |
| `PlutoUI.Button` as the trigger | Nicer UX (and would also give sliders for a timepoint), but **PlutoUI is not in `pluto/Project.toml`** — adding it means re-resolving all three manifests. A real decision, not a freebie. |
| Restart the Pluto **server** (the Notebooks page's Restart) | Works, but kills **every** open session, and loses unsaved edits in all of them. |

A **per-notebook** reset is the right fix and is buildable — Pluto exposes `GET /notebooklist` and
`GET|POST /shutdown?id=…`. Not built yet; see `docs/TODO.md`.

### Structure anisotropy — quiver, tracks, per-image scalar

The `segment.branching` anisotropy pass computes a fibre-orientation field but has **no plot in the
app** — it is figure-shaped, so it lives here. Three package accessors return tidy frames
(`app/src/anisotropy.jl`); the contract they read is in `docs/SEGMENTATION.md`.

```julia
q = quiver_df(img; value_name = "SHG", t = 0)   # t,iy,ix,x,y,u,v,coherence,length — the arrows
b = branch_segments(img; value_name = "SHG")    # label,x1,y1,x2,y2,branch_type — the network
a = anisotropy_df(images(set))                  # uID,value_name,t,anisotropy,… — one row per image
```

**Tracks use `pop_df`** — there is no separate accessor, because there doesn't need to be:

```julia
tr = pop_df(img, "live", ["/_tracked"]; value_name = "T",
            pop_cols = ["track_id", "centroid_x", "centroid_y", "centroid_t"])
sort!(tr, [:track_id, :centroid_t])             # ALWAYS sort before a Lines mark, or it zig-zags
```

Overlaying the three (Figure 4 panel B): draw arrows from the unit `(u, v)`, tracks as `Lines`
grouped by `track_id`, and **reverse the y axis** — image y grows downward.

```julia
f = Figure(); ax = Axis(f[1, 1]; yreversed = true, aspect = DataAspect())
q = q[q.length .> 0, :]                          # drop boxes with no structure in them
arrows2d!(ax, q.x, q.y, q.u .* 12, q.v .* 12; color = :white)   # NOT arrows! — deprecated
for g in groupby(tr, :track_id); lines!(ax, g.centroid_x, g.centroid_y); end
```

Three things that are easy to get wrong:
- **The direction is an axis, not a vector.** `(u, v)` and `(-u, -v)` are the same fibre; sign is
  meaningless. Don't read a "flow" into it.
- **Never index `orientation_eigvec` by hand.** The fibre is the structure tensor's **minor**
  eigenvector — `quiver_df` is the only place that resolves that, and a hand-rolled read draws
  every arrow 90° off while still looking like a plausible field.
- **`anisotropy` is 0 = uniform, 1 = non-uniform**, length-weighted. Real fibrous tissue sits
  around **0.1–0.4** — a low number is not a defect.

`anisotropy_df` is long-format across images and `value_name`s, so panel D is a join against
whatever per-image composition you computed (e.g. behaviour-state fractions from `pop_df`) on
`uID` — the same shape the old `exp.info[SHG.anisotropy]` merge produced.

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
- **Prune** (in History, when >1 snapshot exists) deletes every snapshot EXCEPT the current version —
  "I'm happy with this one, drop the rest". Two-click confirm. It touches only `.snapshots/` files:
  the live notebook and the registry entry (**including the description**) are left as-is, and it
  aborts (409) rather than wiping history if the current pointer is unset or its snapshot is missing.
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
`notebooks-sysimage` (dev, deps only — excludes Cecelia so workers load it from source and see edits) /
`notebooks-sysimage-full` (release, bakes Cecelia + CeceliaNb in). Both run the same
`create_sysimage`; the **only** difference is the package list — the `-full` build adds `CSV`,
`Cecelia`, and `CeceliaNb`, so a release image also skips the first-`pop_df` compile, at the cost of
being frozen at build time. `launch.jl` picks up `deps.so` and passes it to notebook workers; without
it, notebooks still work, just slow-first-plot.

**Built from a button, not by hand.** An end user never runs the `pixi` task. The Notebooks page shows
an **Enable fast plots** button (→ `POST /api/notebooks/build-sysimage`) that builds `deps.so` in a
**background process** while notebooks stay fully usable (a banner explains the slow-first-plot until
it lands). The fresh image is used from the **next** server launch — we never restart a running server
out from under an open session. The build is opt-in (a ~10 min, ~1.4 GB job shouldn't start on a
stray click); `pixi run notebooks-sysimage` remains the manual/dev path.

**Update-safe (the stamp).** A sysimage is native code tied to the exact Julia version + baked package
versions, so it can't be shipped prebuilt as one universal artifact, and after an update the on-disk
image goes **stale**. Each build writes a sidecar `deps.so.stamp` (`{julia, hash(Manifest.toml), variant}`,
see `pluto/sysimage_stamp.jl`). Freshness = the first two fields match the current Julia + Manifest —
`variant` never affects staleness. `launch.jl`
ignores a stale image (falls back to slow-first-plot rather than handing workers an incompatible one),
and the status endpoint reports `stale` so the page shows a **Rebuild** button — same flow as first-run.
`_classify_sysimage` (in `notebooks_api.jl`) is the pure, tested classifier: `ready` / `stale` /
`building` / `error` / `absent`. Release packaging (ship a prebuilt image per platform vs. build on
first run): see `docs/SHIPPING.md` and `docs/TODO.md` → *Ship a prebuilt Notebooks sysimage in the bundle*.

**Which recipe built it (`variant`).** Both builds write the same `deps.so`, but the deps-only one
excludes `Cecelia` so workers load it from source, while `-full` bakes it in. That made them
indistinguishable on disk: running `notebooks-sysimage-full` on a dev machine silently handed workers
a **frozen `Cecelia`** — `app/src` edits stopped reaching notebooks — while `launch.jl` still logged
"deps sysimage". The stamp now records `"deps"` or `"full"`, and `launch.jl` says which it loaded,
warning on `full` that app/src edits won't reach workers. A stamp written before this field reads as
`unknown` and stays **fresh** (no forced ~10 min rebuild).

**Rebuild is like-for-like.** `_ensure_sysimage_build!` used to always run `build_sysimage.jl`, so a
release shipping a `full` image would silently drop to deps the first time it went stale and the user
pressed *Rebuild* — plots still fast, but the first `pop_df` slow again, with nothing to explain it.
It now picks the recipe matching the stamped variant; `unknown`/absent → deps (the first-run default).

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
| `POST /api/notebooks/prune` | keep only the current version's snapshot, delete the older ones (description untouched) |
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
