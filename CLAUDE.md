# Cecelia Feijoa — Project Guide

Cecelia is an immunological image analysis tool (Nature Communications 2025).
Stack: **Julia** (backend/WS server) · **Vue 3 + TypeScript** (frontend, WebGPU browser viewer) · **Python** (analysis env: cellpose/btrack/scanpy)

## How to read the docs without burning the context window

`docs/` is ~1 MB. **Read a slice, not a file.** For anything over ~40 KB:

```bash
grep -n '^#\{2,3\} ' docs/UI.md      # section index with line numbers — ~400 tokens
sed -n '1918,2137p' docs/UI.md       # then read only the section you need
```

- **Inventory lookups are a grep, not a read.** `docs/inventory/*.md` are flat bullet lists —
  `grep -n -i '<thing>' docs/inventory/FRONTEND.md` gives you the whole answer.
- **Two directories are excluded from default search — for different reasons.** Pass
  `--exclude-dir=archive --exclude-dir=todo` (grep) or skip `docs/archive/**` and `docs/todo/**`
  (Glob) unless you are deliberately going there. Together they are 1.6 MB and supply ~40% of doc
  grep hits on ordinary terms — measured at ~30% of the tokens a 20-grep discovery sweep returns.
  - `docs/archive/` (380 KB) — **not authoritative**, superseded by definition. Only open one if you
    want the historical ask.
  - `docs/todo/` (1.2 MB) — **authoritative but narrow**: a plan is reference for whoever is working
    *that* plan. It is reached **by name**, from a pointer (a code comment, `docs/TODO.md`, this file);
    349 code citations already do exactly that, so grep was never the access path. **Before designing
    anything, grep [`docs/todo/README.md`](docs/todo/README.md)** — a complete one-row-per-plan index
    with each plan's status — so you find a locked design instead of rebuilding it. Skipping that
    check is how a parked design gets re-derived. The index's completeness is test-enforced
    (`python/cecelia/tests/test_doc_index_convention.py`), so it can be trusted.
- **Area rules live in nested `CLAUDE.md` files, loaded on demand:** [`frontend/CLAUDE.md`](frontend/CLAUDE.md)
  (Vue/CSS + the two mandatory UI lookups), [`app/CLAUDE.md`](app/CLAUDE.md) (Julia conventions, tasks,
  `run_py`, `channel_indices`, ccid.json versioning). Don't duplicate their content up here.

## Doc index — what it covers, and when to update it

**Keep the docs current — update the relevant file in the same change, not after.**

| Doc | Covers — and update it when you change this |
|---|---|
| [`INVENTORY.md`](INVENTORY.md) | Index → `docs/inventory/*.md`: what exists and where. **Check before building.** Add a line per new shared component |
| [`FAQ.md`](FAQ.md) | Highlight reel of the *counterintuitive* why (AI-written, no Rust, browser-not-Electron). Punch lines only — detail stays in `docs/` |
| [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) | Layer boundaries, WS protocol, data-model contracts, log rail, repo layout, ports, OME-ZARR dual-format, hidden invariants. **41 KB — slice it** |
| [`docs/SCHEDULER.md`](docs/SCHEDULER.md) | Chain executor: per-image threading, resource pools, barriers, resume, event bus. **56 KB — slice it** |
| [`docs/RUNNER.md`](docs/RUNNER.md) | The detached task runner (dev only): process, lifecycle, routes, staleness, chain claims |
| [`docs/JOBS.md`](docs/JOBS.md) | Background jobs vs scheduler tasks: `jobs.jl` registry, process-kill primitives, data patches, export/import |
| [`docs/UI.md`](docs/UI.md) | Frontend conventions, component catalog, module pages, plots, design tokens. **190 KB — slice it.** Mandatory subsets: [`ui/PRIMITIVES.md`](docs/ui/PRIMITIVES.md), [`ui/COPY.md`](docs/ui/COPY.md) |
| [`docs/MODULES.md`](docs/MODULES.md) | Adding task functions and module pages; task JSON, registry, param widgets, composite pattern, the module file pattern. **88 KB — slice it** |
| [`docs/CUSTOM_MODULES.md`](docs/CUSTOM_MODULES.md) | User drop-in tasks: `<config_dir>/modules/`, `register_task!`, `/api/tasks/custom-modules` |
| [`docs/OBJECTMODEL.md`](docs/OBJECTMODEL.md) | Project/Set/Image hierarchy, disk layout, ccid.json shape, versioned fields, transactions, calibration |
| [`docs/SEGMENTATION.md`](docs/SEGMENTATION.md) | Segmentation pipeline: class hierarchy, base/nuc label types, tiling, output zarr, staged stores. **81 KB — slice it** |
| [`docs/TRACKING.md`](docs/TRACKING.md) | Cell tracking (btrack): gated-population input, track lineage in H5AD obs, vendored config. **50 KB — slice it** |
| [`docs/DATAMODEL.md`](docs/DATAMODEL.md) | AnnData conventions: `.h5ad` layout, feature names, `label_props`, mesh paths |
| [`docs/POPULATION.md`](docs/POPULATION.md) | Population manager & gating: pop types, transforms, `gating/{value_name}.json`, `pop_df`, gate↔track. **56 KB — slice it** |
| [`docs/API.md`](docs/API.md) | HTTP/WS surface: routing conventions, binary responses, route index, HTTP.jl v2 conventions. **102 KB — slice it** |
| [`docs/PLOTS.md`](docs/PLOTS.md) | **Adding ANY plot** — registry + `SummaryCanvas`, never a bespoke panel/route. Chart types, encoding model, renderer spec. **52 KB — slice it** |
| [`docs/ANALYSIS.md`](docs/ANALYSIS.md) | The Analysis board (`/analysis`): tabs, plates, persistence keys, plot-family registries, PDF/CSV export |
| [`docs/NOTEBOOKS.md`](docs/NOTEBOOKS.md) | Notebooks Playground (`/notebooks`): Pluto engine, `CeceliaNb`, registry + snapshots, `/api/notebooks/*` |
| [`docs/DEV.md`](docs/DEV.md) | Branches, commits, PRs, tagging, `pixi run dev`, test categories + fixtures, **Windows compatibility helpers** |
| [`docs/INSTALL.md`](docs/INSTALL.md) | Installation, Unix + Windows — the *how*. Needs review before production deployment |
| [`docs/SHIPPING.md`](docs/SHIPPING.md) | Distribution architecture — the *why*: Pixi/constructor + browser stack, update model, Python env + version pins |
| [`docs/RELEASING.md`](docs/RELEASING.md) | Release *policy*: when to tag, rc-vs-release-vs-milestone, pre-1.0 versioning, cutting checklist |
| [`docs/FUTURE.md`](docs/FUTURE.md) | **Deliberately deferred**: known-better alternatives, non-goals, work gated on a trigger that may never fire |
| [`docs/ROADMAP.md`](docs/ROADMAP.md) | Temporary forward goals: phases + post-v1 backlog. Consult before starting a new phase |
| [`docs/MILESTONES.md`](docs/MILESTONES.md) | Append-only ledger of what landed and how it was packaged. Add an entry at each freeze/release |

### Where a note goes — four trackers, four jobs

- **`docs/TODO.md` — open work only.** Someone intends to do it. When an item is done, **delete it** —
  no "Fixed" log (git history, merged PRs and the auto-generated release notes already have it; a
  hand-maintained list caused recurring merge conflicts). Items are keyed by **title**, not a number
  (numeric IDs retired 2026-08-05 — half the code comments citing one pointed at a deleted item). Cite
  as `docs/TODO.md` → *Title*. **From code, prefer a permanent reference** — a `docs/<AREA>.md` section
  or a `docs/todo/X_PLAN.md` path, which cannot dangle when the work ships. There's a routing table at
  the top of TODO.md — check it before adding an entry.
- **`docs/FUTURE.md` — nobody should act on it.** A deliberate non-goal, or something conditional on a
  trigger that may never fire. **A fact worth recording that nobody should act on is not a TODO item.**
- **`docs/todo/*_PLAN.md` — parked plans.** A standalone design doc for a feature too big for a TODO
  item: locked decisions + a phased build sequence + cross-file architecture. Created when a feature
  needs real design before/while building, when a topic is paused but the thinking must be preserved,
  or when code needs a stable pointer. Promote the durable parts into `docs/<AREA>.md` once built.
  **Excluded from default search** — find one via the complete status index in
  [`docs/todo/README.md`](docs/todo/README.md), which is also the convention doc. Add a row there in
  the same change that adds a plan; a plan absent from the index is invisible.
- **`docs/archive/` — do not act on anything in here.** Shipped feature briefs and one-off audits, kept
  as a record of what was asked (the project is openly AI-assisted; the asking is part of how it was
  built). **They are not documentation and nothing in them is authoritative** — a brief reads like a
  confident spec long after the design moved, was rejected, or shipped differently. Every file carries
  an `ARCHIVED` banner on line 1. If a grep leads you here, go find the current answer in
  `docs/<AREA>.md` or `docs/todo/*_PLAN.md`. When a brief's work lands, add an outcome note under the
  banner in the same PR. Convention: [`docs/archive/README.md`](docs/archive/README.md).

---

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

## Before implementing anything — mandatory discovery step

Fresh context windows don't know what already exists — which is how we ended up with duplicates
(two shutdown buttons, hand-rolled zarr access, a private napari reader stack). Before writing any
code, find the existing implementation of everything the task touches:

1. Check the matching [`docs/inventory/*.md`](INVENTORY.md) for the canonical component/helper — use
   it, don't rebuild it. It's a **grep**, not a read.
2. Grep/find for the specific function, component, or pattern (a reader, a store, a base component).
3. Report what you found before writing code.
4. Build on what exists — only write new if the search genuinely comes up empty.
5. If in doubt: search, don't build.

Using a hand-rolled solution when a util or component already exists is a bug, not a style choice.
When you add a significant new shared component, add a line to the matching `docs/inventory/*.md` in
the same change.

**Rendering UI? The primitive catalog is mandatory** — see [`frontend/CLAUDE.md`](frontend/CLAUDE.md),
which loads automatically when you touch `frontend/`.

## Cite sources for non-trivial algorithms

Applies to **all languages**. When implementing a non-trivial or published algorithm — numeric
transforms, methods from a paper, code ported from a reference implementation — add a comment with the
**citation** (paper + DOI, and/or the reference-implementation URL) and, where feasible, **validate
against golden values** from that reference in the test suite. Reserve this for the parts where "is
this actually correct?" genuinely matters — not ordinary code or small helpers.

Example: `app/src/gating/transforms.jl` (logicle ← Moore & Parks 2012, cross-checked against
FlowUtils' `logicle_c`, golden values asserted in `app/test/runtests.jl`).

---

## H5AD / cell-data access — always go through the readers/writers

**Never touch `.h5ad` (or its HDF5 internals) directly.** There are dedicated readers/writers in both
languages — one idiom: build a view, refine it, finish with a terminal verb. You read a labeled
DataFrame, you write a labeled DataFrame.

| | Julia (`app/src/label_props.jl`) | Python (`python/cecelia/utils/label_props_utils.py`) |
|---|---|---|
| **Read** | `label_props(img\|path) \|> select_cols/view_centroid_cols/filter_rows \|> as_df` | `LabelPropsView(path).view_centroid_cols().filter_by_label(ids).as_df()` |
| **Write** (append obs cols) | `label_props(path) \|> v -> add_obs(v, df) \|> save!` | `LabelPropsView(path).add_obs(df).save()` |

- No `h5open`/`HDF5.*` (Julia) or `h5py`/`anndata` (Python) on cell data, and don't read the whole
  table and filter in memory — push the selection into the view.
- **Every `.h5ad` write goes through `write_h5ad_atomic`** (`python/cecelia/utils/atomic_io.py`), creating
  one *or* rewriting one. Never `adata.write_h5ad(final_path)`. Same family for other durable output:
  `write_json_atomic`, `write_atomic`, `atomic_path`; Julia's counterpart is `write_atomic`.
- **One sanctioned exception — file *creation*.** Building a *new* `.h5ad` is the producing task's job
  and uses `anndata` directly; the view wraps an *existing* file.
- Deviating (e.g. a cheap one-attribute metadata peek) needs an **inline comment on that exact line**
  saying why. No silent raw access.

Why each of these exists, and what a truncated HDF5 costs: [`docs/DATAMODEL.md`](docs/DATAMODEL.md) →
*Reading and writing `.h5ad` — the full rule*.

---

## Image / OME-ZARR access — always go through `zarr_utils`

**The same rule, for image data.** No bare `zarr.open` / `da.from_zarr` / `tifffile.imread` on image or
label stores, and never read NGFF `.zattrs` or OME-XML yourself. ONE set of readers —
`python/cecelia/utils/zarr_utils.py` (+ `ome_xml_utils.py`) — used by the pipeline tasks, the napari
bridge, and external consumers (coastal).

| Need | Use |
|---|---|
| Open an OME-ZARR (image **or** labels) as a level list | `open_as_zarr(path, as_dask=…)` / `open_zarr(path, multiscales=N, as_dask=…)` |
| **Write** a store | `with zarr_utils.staged_store(final_path) as staging:` — then `create_multiscales`/`open_multiscales_for_writing` on `staging`, **never** on `final_path` |
| **Compression** for any array you create | `compressor=zarr_utils.store_compressor(kind)`, `kind='image'` or `'labels'`. NEVER omit it, never hand-build a `Blosc`/`Zstd` |
| Resolve the series wrapper (bioformats2raw `0/` vs flat root) | `zarr_utils.series_base(path)` — structural, read-only |
| NGFF axes / per-axis scale | `read_axes(path)` / `read_scale(path)` |
| OME-XML parse / pixel unit / frame interval | `ome_xml_utils.load_ome_xml/read_pixel_unit/read_scale_from_ome_xml/read_time_increment` |

- **Don't copy these readers into a new module** or re-open a store you already opened.
- **Reads are read-only** — `zarr_data_to_list` only mutates on a WRITE-mode open.
- **The compressor is a decision, not a default**, and the two kinds need *opposite* settings — pass the
  right `kind`. Selectable in Settings → Storage; enforced by `test_store_compressor_convention.py`.
- **Never write a store at its final path — stage it.** A writer that opens the final path destroys the
  previous store then fills it over minutes; a cancelled re-run leaves `ccid.json` pointing at a
  truncated store, and on a single-level store the missing frames read as **zeros with no error**.
  Enforced by `test_store_staging_convention.py`.
- **One sanctioned exception — file *creation*,** via `zarr_utils.create_multiscales`.

The drifted private napari reader stack, the measured compressor numbers, and the full rationale:
[`docs/SEGMENTATION.md`](docs/SEGMENTATION.md) → *Image / OME-ZARR access — the full rule*.

---

## Spawning Python — always go through `run_py`

**Never spawn a Python subprocess by hand.** One launcher — `run_py` in `app/src/py_runner.jl` — for
every Python task runner and data-layer writer. It writes the params JSON to the run's task dir, sets
`PYTHONPATH=python/` (so runners `import cecelia.*` with **no `sys.path` bootstrapping** — a new
`sys.path.insert(... __file__ ...)` in a runner is a red flag), streams `[PROGRESS] n/total`, registers
the process for cancellation, and checks `exitcode` **and** `termsignal`. Signature, options and the
anti-patterns it exists to delete: [`app/CLAUDE.md`](app/CLAUDE.md) → *Spawning Python*.

---

## Windows compatibility

**All code must run on Linux, macOS, and Windows.** Every item below has already caused a real bug —
use the named helper, never re-derive the platform branch inline: `python_bin_path()`,
`bioformats2raw_bin()`, `expand_user()` (never `Base.expanduser` — a silent no-op on Windows),
`ensure_config_dir()`, `agent_bin_path()`, `_kill_tree`/`free_port` (never inline
`kill`/`pgrep`/`taskkill`), `_dir_bytes`, always `joinpath()`, and **always pass `encoding="utf-8"` to
Python text I/O** (the default is cp1252 on Windows). Launcher logic lives in `pixi.toml` tasks, not
shell scripts. The full table — which helper, which bug, and why each one exists — is in
[`docs/DEV.md`](docs/DEV.md) → *Windows compatibility*. **Read it before writing any path, process, or
file-encoding code.**

---

## Testing

Write AND run the matching category in the same change as the code. All four run in CI on every OS.

| You changed… | Command |
|---|---|
| Julia package core (`app/`) — data model, persistence, task dispatch, param validation, scheduler/chain | `pixi run test-pkg` (add testsets to `app/test/suite.jl`) |
| An API handler/adapter (`api/src/*.jl`) with logic worth pinning | `pixi run test-api` |
| Frontend logic — **extract it out of the `.vue` SFC into `frontend/src/utils/*.ts` first** | `pixi run test-frontend` |
| Python analysis-env code (`python/cecelia/**`) | `pixi run test-py` |

**Any change to core package functionality ships with a test in the same change** — core = the data
model and its persistence, the versioned-variable convention, task dispatch, param validation. The
four specific obligations (ccid.json round-trip, versioned-field on-disk shape, task dispatch, one
bad-param case), the fixture conventions and the enforced fixture size cap are in
[`docs/DEV.md`](docs/DEV.md) → *Core-functionality test rule* / *Test data fixtures*.

Tests must **not** depend on the dev projects dir — use the committed `test-data/` fixtures via
`fixture_path(...)` + `have_fixture(...)`.

---

## Git & commits

**Branch + PR for everything; never commit or push to `main`** (releases are tagged off `main` after
merge). Full conventions — branch naming, commit style, how PRs are opened, release tagging — in
[`docs/DEV.md`](docs/DEV.md).

**Agents: ask before every commit and before opening/pushing a PR — explicitly, each time; don't
commit or push proactively.** A "go ahead" to do the work is not approval to commit it.

**Agents: state your reservations BEFORE every commit.** When asked to commit/push (or asked for the
PR url — that request itself calls the commit), first volunteer honest reservations about the change —
what's unverified (e.g. never run in a browser, an untested regression surface), plus real limitations
(perf, edge cases, silent no-ops) — as a short prioritized list. Don't reassure or wait to be asked
"any reservations?". Surface the risk at the decision point, then commit on the go-ahead. See
[`docs/DEV.md`](docs/DEV.md) → *Commits*.

---

## Dev dir config — single source of truth

`cecelia-feijoa/.env` (git-ignored, machine-specific):
```
CECELIA_DEV_DIR=~/cecelia-feijoa/dev
```
`init_cecelia!` reads it automatically — no env var export needed; the `CECELIA_DEV_DIR` env var still
overrides it. `config_dir()` in `app/src/config.jl` is the **one resolver**: explicit arg →
`CECELIA_DEV_DIR` env → `.env` → `~/.cecelia` (installed-app default). Don't re-derive the path.

```bash
pixi run dev     # supervises BOTH the Revise backend (:8080) AND the frontend — ONE command
```
`api/dev.jl` supervises the frontend too, so do NOT run `pixi run frontend` alongside `dev`.
`pixi run prod` = no Revise. `pixi run stop` stops all by port. Details, ports and the worktree-sharing
caveat: [`docs/DEV.md`](docs/DEV.md) → *Development environment*, [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) → *Repository layout*.
