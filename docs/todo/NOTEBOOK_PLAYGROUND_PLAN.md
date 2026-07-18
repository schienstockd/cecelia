# Notebook Playground — Parked Plan

> Status: **planned, not started.** Parked design (locked decisions + phased build + cross-file
> architecture) for integrating **Pluto.jl** notebooks into the Cecelia UI as the structured home for
> post-pipeline downstream analysis. Promote durable parts to a permanent `docs/NOTEBOOKS.md` once built.
>
> Origin: the `docs/prompts/python-audit-report.md` *Notebooks* section + follow-up discussion. Grounded
> in the real workflow it replaces — see Motivation.

---

## Motivation (why this is real, not speculative)

The old R version's *actual* downstream analysis lived in ~60 R Markdown vignettes
(`old-R-shiny-version/vignettes/`), per-experiment. Measured across them: **437 `initCciaObject`,
170 `runTask`, 169 `popDT`, 806 `ggplot`, 664 `ggsave`, 196 `fwrite`.** This is where the paper
figures and result tables actually got made — post-pipeline, per-experiment.

Two facts from that corpus shape the whole design:
1. **It is the primary output-generating workflow** — not exploratory scratch. 664 `ggsave` = these
   notebooks produce publication figures. So **versioning is provenance** ("which notebook version
   made Figure 3"), not a nicety.
2. **It got messy for lack of structure** — 60 ad-hoc files, some 2700 lines, no per-experiment
   isolation, no descriptions, one-giant-notebook-per-experiment. The structure here targets exactly
   that.

It is also **100% pure Julia, zero Python** (the Python `.ipynb` were only ever for *developing*
cecelia's Python parts, never analysis). So this feature has no entanglement with the
irreducible-Python boundary (`python-audit-report.md`) — it's the cleanest possible addition, and it
is the **staging ground that makes "don't build a GUI module for every niche analysis" sustainable**:
a user prototypes a niche analysis in a notebook against `Cecelia.jl`; only if broadly useful does it
graduate to a GUI module.

The workflow maps ~1:1 to Julia, and the foundation already exists:

| R vignette | Julia | Status |
|---|---|---|
| `initCciaObject(pID, uID, versionID)` | `init_object(proj_uid, uid)` | ✅ exists |
| `cciaObj$runTask(funName, funParams)` | `run_task(...)` | ✅ exists |
| `cciaObj$popDT(popType, pops, …)` | `pop_df(obj, pop_type, pops; …)` | ✅ exists (documented accessor) |
| `data.table` wrangling | `DataFrames.jl` | ✅ |
| ggplot2 / tidyverse | **AoG + CairoMakie** (see Locked #2) | ⚠️ the one new piece |

---

## Locked decisions

1. **Pluto.jl, not Jupyter/IJulia.** Reactive + reproducible (no hidden execution-order state in a
   shipped artifact), notebooks are `.jl` (text-diffable, versions cleanly with the project — unlike
   `.ipynb` JSON), Julia-only (no Jupyter/Python kernel stack to add). No `IJulia`/`jupyter` is in the
   env today, so this adds Pluto rather than replacing anything.

2. **Plotting: AlgebraOfGraphics.jl + CairoMakie.** The notebooks' figure stack — grammar-of-graphics
   (familiar from ggplot2) with CairoMakie's publication-quality vector output (PDF/SVG/PNG), matching
   the figure-export workflow. AoG over TidierPlots on maturity (AoG is the established GoG on Makie;
   TidierPlots is the younger ggplot-syntax clone). Makie's one weakness — slow first plot (TTFP) — is
   neutralized by the sysimage (Locked #6).
   - *Out of scope for this plan:* GLMakie / interactive rendering / the future viewer, and the
     browser-side `/analysis` canvas (Observable Plot + regl — settled, unchanged). Notebooks use
     CairoMakie only.

3. **Storage — per-project, versioned with the project.**
   - User notebooks: `{projects_dir}/{proj_uid}/notebooks/*.jl` — isolated per experiment (a notebook
     for experiment A never appears in B). Registry (path + description + version metadata) in the
     project manifest `project.json` (written by `save!(proj::CciaProject)`; `docs/OBJECTMODEL.md`).
   - Shipped **example** notebooks: repo `notebooks/` — versioned with the *code* so they track API
     changes. **Must be UID-free** (lift hardcoded UIDs like `NRUBxU`/`jFWePN` to a top parameter
     cell) or a "global example" is secretly dataset-bound.
   - UI supports "duplicate this notebook into project X" so reuse across experiments doesn't fight
     the isolation.

4. **Pluto runs its OWN Julia session (`using Cecelia`), NOT the API server.** It is a full analysis
   surface — **read + `run_task` + plot + export** (not read-only; the R workflow re-ran tasks 170×).
   - **Concurrency note (not a prohibition):** a `run_task` from a notebook writes to disk directly;
     if the GUI is concurrently open on the same project, it needs a refresh to see the change. The
     notebook session must still respect the project lock (`with_transaction`/`.cecelia.lock`,
     `docs/OBJECTMODEL.md`) when mutating.
   - This is distinct from the debug console (which talks to the running server) — Pluto is
     package-direct.

5. **API surface = the existing headless REPL contract:** `init_object`, `run_task`, `pop_df`,
   `label_props` (+ chain verbs). No new engine code needed for v1 — the notebook just calls these.

6. **Cold-start: sysimage + persistent server, NOT DaemonMode.** (DaemonMode dispatches CLI scripts
   to a daemon; Pluto manages its own workers and won't route through it — wrong tool.)
   - **Dev:** a *deps-only* PackageCompiler sysimage (bundle Makie/CairoMakie + DataFrames + HDF5 +
     HTTP — the stable heavy stuff), **not** Cecelia, so Revise still hot-reloads Cecelia.
   - **Release:** a *full* sysimage (Cecelia + deps) baked into the Pixi/constructor flow
     (`docs/SHIPPING.md`) → near-instant first plot.
   - **Floor (interim):** a `PrecompileTools.@compile_workload` in Cecelia.jl exercising
     `init_object → pop_df` so even without a sysimage the second launch onward is tolerable.
   - **Persistent Pluto server** (one process, pointed at the active project's `notebooks/`) so
     server-launch cost is paid once.

7. **Process/lifecycle mirrors the napari bridge.** New `pixi run notebooks` task launching Pluto on a
   dedicated port; add a stop-by-port entry to the `stop` task (Linux `lsof`/Windows PowerShell, per
   the existing pattern in `pixi.toml`). Pick a fixed port (proposed **7660**; confirm free).

8. **UI mirrors `ImageTable.vue` + `stores/project.ts`.** A Playground link launches Pluto; a notebook
   table lists notebooks with add/remove + a **description/notes** field per row (same pattern as the
   image table's exclusion notes) + version. Persist all table state per `useViewState` conventions
   (`docs/UI.md`).

9. **Shared helpers to prevent mess re-accreting.** Ship a few plot/summary helpers so users don't
   re-roll ggplot boilerplate (the R side extracted `vignettes/app/*.R` for exactly this). Where
   possible, expose the `pop_df → summary` aggregation so notebooks and the `/analysis` board compute
   comparisons the same way (rendering differs — Makie vs Observable Plot — but the numbers shouldn't).

---

## Architecture summary

```
{projects_dir}/{proj_uid}/
  project.json        ← + "notebooks": [{ file, description, version, … }]  (registry)
  notebooks/          ← user notebooks (.jl, Pluto), versioned WITH the project
  ...
cecelia-pineapple/
  notebooks/          ← shipped example notebooks (UID-free, track the API), versioned with code
  pixi.toml           ← + `notebooks` task (Pluto on :7660) + stop-by-port entry
  frontend/src/       ← Playground link + NotebookTable.vue (mirrors ImageTable.vue)
  api/src/            ← routes: list/add/remove/describe notebooks; launch/status of Pluto server
  app/src/            ← PrecompileTools workload; (later) shared plot/summary helpers
  sysimage build      ← deps-only (dev) / full (release, docs/SHIPPING.md)
```

Data/render flow: **Vue Playground → launch Pluto (own session, `using Cecelia`) → notebook calls
`init_object`/`pop_df`/`run_task` → AoG+CairoMakie plots + CSV export saved under the project dir.**
No API-server round-trip (unlike the debug console).

---

## Phase 0 results (spike complete — 2026-07-08)

**Verdict: the trio does NOT fight. All three friction points cleared; the UX assumption holds.**
Spike was a throwaway env (Pluto 1.0.3, CairoMakie 0.15, AoG 0.13, Makie 0.24, Julia 1.12) with a
`Pkg.develop`-tracked Cecelia, run headless against real data (`NRUBxU/KDIeEm`, 1201×45 `.h5ad`).

1. **Dev-Cecelia in a Pluto worker — SOLVED.** Pluto's built-in package manager is registered-only
   and cannot install an unregistered local dev package. The mechanism that works: the notebook's
   **first cell does `import Pkg; Pkg.activate(<shared env>)`**, which makes Pluto *disable* its own
   pkg management for that notebook and inherit the shared env (where Cecelia is dev-tracked). All 8
   cells of a headless notebook ran clean (`SessionActions.open(...; run_async=false)`), `using
   Cecelia` → `init_object` → `label_props |> as_df` → AoG/CairoMakie plot.
2. **Sysimage plumbs through `compiler_options` — CONFIRMED.** `Pluto.Configuration.Options(compiler
   = CompilerOptions(sysimage = "deps.so"))` reaches the worker; headless notebook run dropped
   **45.4s → 24.3s**.
3. **TTFP payoff — real and large** (standalone, warm):

   | | cold (no sysimage) | deps-only sysimage |
   |---|---|---|
   | load plotting stack | 5.1s | **0.0s** |
   | first plot render | **18.8s** | **3.9s** |
   | total cold-start | 32.3s | **7.6s** |

**Costs quantified (feed into Phase 5 / `docs/SHIPPING.md`):** the deps-only sysimage is **1.4 GB**
and takes **~10 min** to build (Makie dominates both). One-time build/disk cost, not per-launch. The
1.4 GB must either ship in the installer or be built on first run — a real shipping decision.

**Resolved open questions:** deps-only sysimage (Makie/CairoMakie/AoG/DataFrames + **HDF5+HTTP as
direct deps** — `PackageCompiler` requires named pkgs be direct, they're only transitive via Cecelia)
builds cleanly and keeps Cecelia out so Revise still hot-reloads it (Locked #6 confirmed workable).
Spike artifacts (throwaway): `scratchpad/nbspike/` — `setup.jl`, `ttfp.jl`, `build_sysimage.jl`,
`run_pluto_headless.jl`, `spike_notebook.jl`.

---

## Phased build sequence

- **Phase 0 — De-risk spike. ✅ DONE (2026-07-08).** See *Phase 0 results* above. Proved Pluto + a
  `Pkg.develop`-tracked unregistered Cecelia.jl + a deps-only sysimage via `compiler_options`; TTFP
  32.3s→7.6s standalone, 45.4s→24.3s in a Pluto worker. Trio does not fight.
- **Phase 1 — Minimal launch slice. ✅ DONE (2026-07-08).** Shipped:
  - `pluto/` engine dir (own Julia env, path-sources Cecelia like `api/`): `Project.toml` +
    committed `Manifest.toml`, `launch.jl` (Pluto on **:7660**, detects `deps.so` → worker
    `compiler_options`, exports `CECELIA_PLUTO_ENV`, points the file picker at the notebooks dir),
    `build_sysimage.jl` + `precompile_workload.jl`.
  - `notebooks/example_populations.jl` — **UID-free** parameterized example (data via editable cells
    /`CECELIA_EXAMPLE_*` env, self-guiding when blank): `init_object` → `pop_df(img, "labels", [])`
    (dataset-agnostic ungated accessor) → AoG/CairoMakie histogram → `CSV.write` to `{proj}/exports/`.
  - `pixi.toml`: `notebooks` / `notebooks-sysimage` / `notebooks-instantiate` + `stop-notebooks`
    (and :7660 added to `stop`, both linux + win-64). `.gitignore`: `pluto/deps.so`.
  - **Verified end-to-end headless** against real data (`NRUBxU/KDIeEm`): all cells run clean, plot
    renders, CSV lands (55 KB). `pixi run notebooks` serves on :7660 (HTTP 403 without secret = alive);
    `pixi run stop-notebooks` kills it cleanly.
  - **Gotcha found & fixed:** the `md"..."` macro **cannot take nested double-quotes inside `$(…)`**
    interpolation (e.g. `md"$(join(x, ", "))"` → parse error). Build such strings in plain code and
    interpolate a single variable. Backticks inside `md"..."` are fine.
- **Phase 2 — Notebooks section in Vue. ✅ DONE (2026-07-08).** (Renamed **Playground → Notebooks** —
  matches `pixi run notebooks`, `/api/notebooks`, the `notebooks/` dir.) Shipped:
  - **Secret handling — REVISED (kept ON).** An earlier cut disabled Pluto's secret for a clean URL,
    but `require_secret_for_open_links=false` is a real CSRF/RCE risk (any website could drive
    localhost:7660 → arbitrary code). Reverted to Pluto's secure defaults; instead `launch.jl` writes
    the session secret to `pluto/.plutosecret` (git-ignored) and the API returns it so the frontend
    appends `?secret=…` / `&secret=…`. Verified: `open?path` without secret → 404 (blocked), with
    secret → 302 (works).
  - **Backend** `api/src/notebooks_api.jl` (mirrors napari lifecycle: probe :7660 → adopt or spawn
    `julia --project=pluto launch.jl` pointed at the project's `notebooks/`): `api_notebooks_launch`
    (POST, 202-while-starting/200), `api_notebooks_status` (GET), `api_notebooks_list` (GET, project +
    shipped-example scopes). Registered in `server.jl` (GET `/api/notebooks`, `/api/notebooks/status`;
    POST `/api/notebooks/launch`). **api/ isn't Revise-tracked → restart the server to pick these up.**
  - **Frontend** `modules/NotebooksModule.vue` + route `/notebooks` (main.ts, lazy) + sidebar entry
    (Analysis group, `pi-book`). Launch button ensures the server is up, then reveals an
    `<a target=_blank>` **Open** link (popup-safe — no programmatic `window.open` after await).
    Read-only notebook list with project/example badges. `vue-tsc -b` clean.
  - **Verified**: handler direct-call test — launch spawns Pluto on :7660; status→running; list→example
    (+400/404 error paths); bare URL→HTTP 200. (Did NOT bind a second :8080 — tested handlers directly
    so the user's running dev server stayed untouched.)
  - **Known Phase-2 limitation (fine per plan):** one persistent server; its file-picker dir is set at
    first launch, so switching active project doesn't re-point a running server. `api_notebooks_list`
    still reads the correct per-project dir regardless. Revisit under the "multi-project" open question.
- **Phase 3 — Notebook management + versioning. ✅ DONE (2026-07-08).** Shipped:
  - **Registry in `settings/notebooks.json`** (NOT `project.json` — followed the established
    chains/boards `settings/` convention instead of adding a persistence-critical `CciaProject`
    field; canonical-framework rule). Keyed by filename → `{description, version, updatedAt}`; tracks
    project notebooks only (examples are read-only, versioned with code).
  - **Backend** (`notebooks_api.jl` + `server.jl`): POST `create` (copies `pluto/notebook_template.jl`),
    `describe`, `delete`, `duplicate` (project OR example → project, auto-unique name), `snapshot`.
    `api_notebooks_list` now merges `description`+`version`+`path`. `_safe_nb_file` rejects path-like
    input (separators/`..`/dotfiles), not just strips it.
  - **Versioning = manual Snapshot** (chosen from the open-question options — lightest that delivers
    provenance, no git/file-watching): freezes `notebooks/.snapshots/<name>@v<N>.jl` + bumps a version
    counter. Answers "which version made Fig 3".
  - **Frontend** `components/NotebookTable.vue` (mirrors `ImageTable` inline-edit for the description;
    add / duplicate / snapshot / two-click-confirm delete; per-row Open link when server running) +
    rewired `NotebooksModule.vue`.
  - **Verified**: backend CRUD flow (create/dup-409/bad-name-400/list-merge/describe/snapshot×2/
    duplicate-example/delete) direct-call test; `vue-tsc -b` clean; `pixi run test-api` green.
- **Phase 4 — Shipped helpers + example port. ✅ DONE (2026-07-08).**
  - `pluto/CeceliaNb` mini-package (dev-tracked): `nb_count`, `nb_summary` (aggregate a `pop_df`
    table — **board-identical numbers**, both build on `pop_df`), `nb_hist`/`nb_box`/`nb_scatter` AoG
    shortcuts. Kept minimal by design.
  - Ported both **Julia** `.ipynb` demos to UID-free Pluto examples: `populations.ipynb` →
    `example_pop_df.jl` (flow/live/pooled/tracked tutorial, adaptive to the dataset's gates),
    `backend_model.ipynb` → `example_object_model.jl` (read-only load/inspect tour; dropped
    create/import/napari). All three examples verified headless against real data (0 errored cells).
  - Gotcha refined: single-quoted `md"..."` breaks on nested `"` in `$(…)`; **triple-quoted
    `md"""..."""` is safe** (that's why one ported cell failed and the other didn't).
- **Phase 5 — Release hardening. ✅ DONE (2026-07-08).**
  - **Full sysimage** `build_sysimage_full.jl` + `pixi run notebooks-sysimage-full` (bakes Cecelia +
    CeceliaNb in for release; dev build still deps-only for Revise). `docs/SHIPPING.md` note added
    (1.4 GB, ship-or-build-on-first-run, no-secret localhost model).
  - **Windows lifecycle** — `stop-notebooks` win-64 target already added in Phase 1.
  - **Versioning completed with Restore** (gap found in testing: Snapshot had no reverse). Added
    `GET /api/notebooks/snapshots` + `POST /api/notebooks/restore` + Pluto `auto_reload_from_file` so a
    restore shows live; History → **version dropdown + Restore** (two-click confirm) in `NotebookTable.vue`.
    Restore is a **plain overwrite** — it does NOT auto-snapshot or bump the version (an earlier
    auto-snapshot-on-restore design piled up a version per click; removed). Snapshot is the only thing
    that creates a version. Verified: repeated restores leave snapshots + version counter unchanged.
  - **Promoted to permanent `docs/NOTEBOOKS.md`** + CLAUDE.md doc-index/table rows.
  - **Post-build hardening (from testing/review):** (a) versioning "Ver" column now shows the *current*
    version (restore v3 → v3), not a monotonic counter; restore is a plain overwrite (no version churn);
    (b) **Shutdown/Restart** buttons + `POST /api/notebooks/{shutdown,restart}` + `atexit` cleanup (the
    spawned Pluto isn't bound to the API server, so it needs explicit stop — like napari); (c) server-
    running **guard** on delete/restore (require `force`; UI confirm supplies it) since Pluto owns an
    open file; (d) friendly **first-run message** when the `pluto/` env isn't instantiated; (e) **API
    tests** added (`api/test/runtests.jl` — 28 assertions over sanitisation + CRUD + versioning).

---

## Open questions (resolve during build, not blocking the plan)

- **Versioning mechanism.** ✅ **Decided (Phase 3): manual Snapshot** — a `version` counter +
  copy-to-`.snapshots/<name>@v<N>.jl` on demand. Lightest option that delivers provenance; no git, no
  file-watching. Could later add auto-snapshot on significant edit or figure-stamping if wanted.
- **Pluto server port** — ✅ **7660** confirmed free + wired (Phase 1). Still open: whether the backend
  auto-launches it (like napari) or it stays a manual `pixi run notebooks`. Lean manual for v1.
- **Windows lifecycle** — mirror the PowerShell stop-by-port pattern already in `pixi.toml`.
- **Multi-project / concurrent sessions** — one persistent Pluto server serving whichever project is
  active vs one per project. Start with one server, active-project-scoped.
- **How much to expose as shared helpers** vs leave to example-notebook patterns (avoid over-building
  a plotting API nobody asked for — start minimal, grow from real notebook usage).

---

## Cross-references

- `docs/prompts/python-audit-report.md` — Notebooks section (origin) + the Python-boundary context.
- `docs/prompts/julia-port-watchlist.md` — the porting discipline this feature complements.
- `docs/FUTURE.md` — Julia-native viewer (the GLMakie half of the Makie house convention).
- `docs/ANALYSIS.md`, `docs/PLOTS.md` — the `/analysis` canvas (browser-side; NOT this).
- `docs/OBJECTMODEL.md` — `project.json`, disk layout, project lock.
- `docs/SHIPPING.md` — where the release sysimage slots into packaging.
- `docs/POPULATION.md` — `pop_df` accessor + the headless REPL contract.
```
```
