# Plugins — distributable custom modules

**Status:** **P1 built** on `feat/plugins` (layout + both scans + precedence + PYTHONPATH; Settings
rendering deferred to P3). P2–P4 not started. Decisions were verified against the code 2026-08-17: two
of the four open questions are now **Resolved** (see that section) and Decision 3's scope was
corrected — it was new registry work, not the one-liner this plan first implied.
**Origin:** a lab that used the old R version wants two format-specific importers back — external
**tracking** and external **segmentation**, in their own format (conference, 2026-08). Neither belongs
in the app: nobody else has that format. Their two other asks are general and are handled in
[`CORRECTION_PLAN.md`](CORRECTION_PLAN.md).

## The premise correction — read this first

**Cecelia already has a plugin system.** It is called custom modules, it shipped in three phases
(P1–P3), and it does the whole hard part: a user drops `<category>/<name>.jl` + `.json` [+ `_run.py`]
into `<config_dir>/modules/`, a runtime `register_task!` puts it in dispatch, and a task in an
**existing** category (e.g. `tracking/`) surfaces on that built-in page automatically. See
[`../CUSTOM_MODULES.md`](../CUSTOM_MODULES.md) and [`CUSTOM_MODULES_PLAN.md`](CUSTOM_MODULES_PLAN.md).

> **Premise corrected again, 2026-08-17 (Dominik).** "Distribution only" was too narrow, and would
> have shipped something that did not earn its name: *"custom task, custom module page, a plugin —
> both are important, that's the whole point of the plugin. We already have the custom module loader
> otherwise."* A plugin that only groups tasks in a directory adds packaging and nothing else, because
> the drop-in loader already delivers the task. **The distinguishing capability is that a plugin
> provides its own module PAGE.**
>
> That turned out to cost almost nothing, because both halves of a module page are already
> declarative: the form comes from the task spec's `params` (→ `ParamRenderer`) and the canvas from a
> plot spec (→ `SummaryCanvas`, one directory-scanned registry, `plot_specs()`). So a plugin ships
> `plotDefinitions/*.json` beside its tasks and gets a real page — no Vue, no framework. **Built** as
> part of P1; see the Phases section.
>
> **Shipping Vue is a DECISION, not an impossibility — an earlier draft of this plan claimed
> otherwise and was wrong.** The accurate facts: the frontend precompiles SFCs via
> `@vitejs/plugin-vue` with no runtime-compiler alias, and a *stable* bundle ships prebuilt
> `frontend/dist`, so a plugin-supplied **`.vue` file** cannot be compiled by a stable install. That
> does not generalise to "no Vue":
> - the **dev channel builds the frontend on the machine and requires Node** (`docs/SHIPPING.md` →
>   *The one extra requirement: Node*), so a `.vue` could be compiled there;
> - a plugin could ship **pre-compiled ESM using `h()` render functions**, which needs no compiler and
>   would `import()` fine against the bundle as it stands;
> - the runtime compiler could be aliased in (~40 kB) to accept template strings.
>
> **The real reason to stay declarative** is that any of those makes the frontend a **plugin ABI**: a
> component contract that cannot be refactored freely, plus a loader, plus version skew between a
> plugin and the app that rendered it. That is the framework this plan rules out — and it is a cost
> worth refusing, which is a different claim from "it cannot be done". If declarative proves too thin,
> pre-compiled ESM is the escape hatch to evaluate first, because it needs no bundle change.
>
> **A second, concrete reason arrived 2026-08-17: the icon ratchet.** `frontend/src/lib/iconLegend.ts`
> + `iconLegend.test.ts` (on `feat/view-profile-badge`) scan every glyph rendered under `frontend/src`
> and fail the suite on one that is not in the legend — one meaning per glyph, one glyph per meaning.
> **Markup shipped by a plugin would sit outside that scan entirely**, so every convention the app
> enforces on itself — icons today, and whatever ratchets follow — would stop applying exactly where
> the least-reviewed code is. A plugin that ships only JSON cannot introduce an unlisted glyph, because
> it renders nothing. That is a checkable argument, unlike the "it's impossible" one this block
> replaced.

So beyond the page, this plan is **distribution** — install / update / remove a module set from a URL,
and host a few. It is explicitly **not** a plugin framework:
`CUSTOM_MODULES_PLAN.md` **Decision 2 (no new package deps, pure Julia `include` + registry) stands
and is not reopened.** Anyone who finds themselves designing a lifecycle API, a hook system or a
plugin base class has taken a wrong turn.

## What this reverses, explicitly

`CUSTOM_MODULES_PLAN.md` → *Non-goals* says: **"Not a sandbox / not a marketplace. Local, trusted,
single-user drop-in only."** Hosting plugins on GitHub reverses the marketplace half. State it as a
reversal in that file rather than letting it drift, and keep the other half: **still no sandbox.**

That distinction is the whole trust story. A custom module is arbitrary Julia `Base.include`d into the
`Cecelia` module with full machine access (`custom_modules.jl:81`) — which was defensible when *the
user* placed the file, exactly as old R `source()`d a folder. Having *the app fetch and run code from
a URL* is a change in kind, not degree. Decisions 5–7 below are what makes that acceptable.

## Why this is worth building for more than one lab

Three candidate plugins exist today, so the catalogue is not a one-user feature:

| Candidate | Old R source | Why not a built-in |
|---|---|---|
| The lab's external **track** importer | not in the repo (see below) | one lab's format |
| The lab's external **segmentation** importer | not in the repo | one lab's format |
| **cell2location** import | `inst/modules/sources/importImages/cellToLocation.R` + `py/from_cellToLocation.py` — reads an external `.h5ad`, synthesises the image extent from `obsm['spatial']`, writes labels + props | spatial-transcriptomics niche; never ported to Feijoa |
| **10x Xenium** import | `inst/modules/sources/importImages/tenxXenium.R` + `py/from_tenx_xenium.py` — reads `transcripts.csv.gz`, builds image + segmentation | same; `docs/ROADMAP.md:140` lists it as "more import", unported |

The last two are the useful precedent: real, working, format-specific importers that Feijoa
deliberately never absorbed. They are what a plugin *is*.

**The lab's own two importers are not in the old R checkout — confirmed 2026-08-17.**
`inst/modules/sources/tracking/importTracking.R` is a no-op stub and
`inst/app/modules/inputDefinitions/tracking/trackmate.json` is an empty spec with no `.R` beside it.
Searched again for the format itself (`manual track`, `Track n`, `Slice n`, `°`, plus every CSV reader
in `R/` and `inst/modules/`): nothing. The only `read.csv` calls are Xenium and a compensation matrix,
and `inst/IJ/` is cecelia *driving* Fiji — a producer of tracks, not a reader of someone else's export.
Old R `source()`d the user's own `modules/` folder, so their importer lived there, outside the repo.

**So the format cannot be recovered from the repo, and it should not need to be.** Rather than
hard-code a guessed layout, the importer takes a **column mapping** with shipped
`templates/*.json` (ImageJ Manual Tracking, TrackMate, Imaris) that the user's own field entries
override. The lab's file is then a *template*, not a code change — and an unlisted tool is supported
by mapping its four columns. Still worth asking the lab for one real export, but only to pin a
template's defaults, which is no longer blocking.

Two design points that fell out of building it, both non-obvious:
- **There is no id to join on.** An external tracker knows nothing about cecelia's labels, so each
  cell takes the track of the nearest spot *in its own frame*, within a distance cutoff. The cutoff is
  the safety margin: without it every spot finds some nearest cell, and an export from the wrong image
  produces a full, plausible, wrong column rather than an obvious failure.
- **Units and axis order are where this breaks.** External coordinates are usually calibrated (µm)
  while cecelia centroids are pixels, and `img_physical_sizes` returns **Z,Y,X** — indexing it as
  X,Y,Z divides x by the z spacing and, on anisotropic data, matches nothing. Both were caught only by
  running against real tracks; see the verification note in the Phases section.

## The verified blocker — the two halves scan at different depths

This is the one thing that makes `git clone` not already work, and it is a real asymmetry, not a
theory:

| Half | Code | Behaviour |
|---|---|---|
| **Julia loader** | `app/src/tasks/custom_modules.jl:72` | `walkdir(root)` — **fully recursive**, any depth |
| **Definitions merge** | `api/src/routes.jl:350` | one-level `readdir` — `<config_dir>/modules/<category>/<name>.json`; the immediate subdir name **is** the category |
| **Categories endpoint** | `api/src/routes.jl:478` | same one-level `readdir` |

Both API scans also skip the names `sources`, `inputDefinitions`, `python` as "legacy layout dirs"
(`routes.jl:353`, `:481`) — a leftover from the old split layout that the shipped loader no longer uses.

Consequence: clone a plugin to `<config_dir>/modules/<plugin>/tracking/importX.{jl,json}` and the task
**registers and runs, but has no form and no nav entry** — its `.json` is one level too deep to be
seen, and `<plugin>` is misread as a category name. A plugin therefore cannot be a self-contained
directory today, which is exactly what a git repo has to be.

**The `plugins/` root is invisible today, not broken-visible — and that is what makes P1 shippable
alone.** Under Decision 1's layout, both API scans read `<config_dir>/modules/plugins/` as a category
literally named `plugins`, find no `.json` directly inside it, and drop it — `_custom_module_categories`
at `routes.jl:490` (`isempty(funs) && continue`), and the definitions merge simply pushes nothing. So
there is no phantom "plugins" category to clean up, no migration, and no existing behaviour to
preserve: P1 is **purely additive**. (The Julia loader meanwhile already `walkdir`s into it and
registers the tasks — hence "registers and runs, but has no form".)

## Decisions

1. **A plugin is one directory: `<config_dir>/modules/plugins/<plugin>/`**, containing a manifest plus
   the same `<category>/<name>.{jl,json,_run.py}` co-located layout as a hand-dropped module. One
   directory = one git repo = one unit to install, update and remove. Hand-dropped modules keep
   working unchanged at `<config_dir>/modules/<category>/...`; `plugins/` is additive.
2. **Category is derived from the directory *below* the plugin root**, never from the plugin name.
   Teach the two API scans about `plugins/*/` explicitly rather than making them blindly recursive —
   an unbounded `walkdir` for categories would turn any stray nested folder into a phantom category.
   The Julia loader needs **no change** (it is already recursive).
3. **Built-ins still win on clash, and a plugin cannot shadow another plugin.** Target precedence:
   built-in > hand-dropped > plugin, and a plugin-vs-plugin `fun_name` collision is a load **error**
   reported in Settings, not a silent last-one-wins.

   **This is new work, not an extension of a shipped rule — do not scope it as a one-liner.** Only the
   *built-in* tier exists today, and it is enforced in two places independently: `_task_from_fun_name`
   (`task.jl:914`, dispatch) and the definitions merge (`routes.jl:359`, the form). There is **no
   custom-vs-custom rule at all**:
   - `register_task!` (`task.jl:238`) is documented as *"re-registering the same `fun_name` replaces the
     entry"* — silent last-one-wins, and the winner is decided by `walkdir` order, i.e. the filesystem.
   - The definitions merge skips only built-in clashes, and never dedupes customs against each other —
     so **two clashing customs render two forms on the same page while dispatch runs one of them.**
     That asymmetry *is* what "silent last-one-wins" looks like from the UI today.

   So both new tiers have to be built in `app/src/tasks/task.jl` (the registry), which is a different
   file from the two API scans the rest of P1 touches. Budget for it, or move it to P2 deliberately.

   **A clash also has nowhere to be reported yet.** `custom_modules_report()` (`custom_modules.jl:105`)
   is keyed by path with `status ∈ {ok, error}` and only sees load *failures* — a clash `include`s
   cleanly, so it would report `ok`. Surfacing it needs a new field on the report, on the
   `/api/tasks/custom-modules` payload, and on `CustomModule` in `frontend/src/stores/customModules.ts`.
4. **The manifest is small and declarative.** `plugin.json` at the plugin root:
   ```json
   {
     "name": "trackimport-smithlab",
     "version": "0.2.0",
     "description": "Import tracks from the Smith lab CSV export",
     "homepage": "https://github.com/...",
     "requiresCecelia": ">=0.1.3",
     "categories": ["tracking"]
   }
   ```
   `requiresCecelia` is checked and **warned** about, not enforced — refusing to load on a version
   mismatch would make every cecelia release break every plugin.

   **The check must no-op on `"dev"`.** `_running_version()` (`api/src/update_api.jl:25`) returns the
   literal `"dev"` whenever there is no `VERSION` file at the install root — which is every source
   checkout and every developer. Comparing `">=0.1.3"` against `"dev"` must be *skipped*, not warned
   about, or the panel shouts at every plugin on every dev machine forever.
5. **Install is a pinned fetch the user confirms, showing what it will run.** Source = a URL plus a
   **commit or tag**. The confirm dialog names the repo, the ref, and says plainly that plugin code
   runs with full access to the machine and is not sandboxed. **No auto-update, no background fetch,
   no install-on-startup.**

   **The install record goes in a sibling `.install.json`, NOT in `plugin.json`.** `plugin.json` ships
   *from the repo*; writing the resolved ref back into it dirties the checkout and the next update
   overwrites it. A sibling file inside the plugin directory still satisfies Decision 9 ("nothing is
   written outside the plugin's own directory") while staying disjoint from what the author ships.
6. **A curated registry, not open search.** A small JSON list in this repo (or the docs site) of
   plugins we vouch for; anything else installs by explicit URL. "Hosted on GitHub" means *we publish
   a few repos and list them*, not that cecelia browses GitHub.
7. **Update = fetch + restart, and the UI must say so up front.** Editing an already-loaded `.jl`
   cannot be hot-reloaded (Julia struct redefinition — `custom_modules.jl:45-47`), so updating an
   installed plugin requires a server restart. Install of a *new* plugin works with the existing
   Reload. Do not discover this at the end and bolt on a modal.
8. **Extend the existing Settings → Custom modules panel; do not add a second surface.**
   `frontend/src/stores/customModules.ts` + the panel in `frontend/src/modules/SettingsModule.vue`
   already show the modules dir, a per-module loaded/error list and a Reload button. Plugins become a
   section there — same page, same store. (Same rule that keeps the shutdown buttons at two:
   `docs/todo/*` history and `docs/UI.md`.)
9. **Uninstall is a directory delete**, and it must be complete: unregister the tasks (the loader
   already prunes on a vanished file — `custom_modules.jl:60-69`), then remove the directory. Nothing
   is written outside the plugin's own directory, which is what makes this safe — hence Decision 1.

   **Refuse the delete while one of the plugin's tasks is running.** Nothing checks this today.
   `_unregister_task!` only drops the registry entries; an in-flight `_run_task` already holds the
   instance, and deleting the directory pulls its `_run.py` out from under a live `run_py` subprocess.
   Uninstall must consult the running-task set and refuse (or offer to cancel) rather than race.

## Phases

- **P1 — layout. ✅ BUILT** (`feat/plugins`). `plugins/<plugin>/` + manifest parse + both API scans
  (Decisions 1–4), the precedence/clash work in `task.jl` (Decision 3) and the plugin-root
  `PYTHONPATH` fix (R2). No network. A plugin installed by hand (`git clone` into the dir) works end
  to end: registers, has a form, has a nav entry — pinned by an API test that asserts exactly that.

  What landed, beyond what this plan first scoped:
  - `app/src/tasks/plugins.jl` (new) — layout constants, manifest, and **`user_task_specs`, the ONE
    enumerator of the user modules tree**. Both API scans consume it instead of each hand-rolling a
    one-level `readdir` with its own copy of the legacy skip list; that duplication was the actual
    mechanism behind the depth asymmetry, so it is now gone rather than worked around.
  - Deduping in that enumerator also fixes an **unreported** pre-existing bug: two clashing custom
    specs used to render two forms on one page while dispatch resolved exactly one of them.
  - `_custom_module_sources` — the loader now loads hand-dropped first, then plugins, each
    path-sorted. Precedence is only meaningful if the order is fixed; `walkdir` returns filesystem
    order. Still fully recursive, so no existing drop-in changes behaviour.
  - `custom_task_clashes()` + `clashes` on the payload — a clash is not a load failure, so it could
    not be reported through the existing `ok`/`error` report.

  - **The module page** (the premise correction above): a plugin ships `plotDefinitions/*.json`,
    `plot_specs()` reads them through `user_plot_specs` (built-ins win on an `id` clash), and
    `CustomModule.vue` gained the `#plots` slot every built-in page already uses. A plugin's new
    category now gets a task form *and* a plot canvas.
  - **A runnable example, loaded by CI**: `docs/examples/plugins/tracktools-example/` — one task on a
    built-in page (`tracking.importCsvTracks`, the lab's actual ask), one in its own new category with
    its own plot spec (`trackTools.cumulativeChange`, the old R tutorial's variant), a shared
    `python/` helper and the column-mapping `templates/`. The `docs/examples/custom-modules/` examples
    were never executed by any test; this one is, so it cannot rot into a plausible file that no
    longer works.

  **Verified against real data** (spleen project `4kS67f`, image `3w4IY5` / M4d — 482 tracked cells in
  17 tracks), run on a COPY in an isolated config dir:
  - `trackTools.cumulativeChange` wrote 3 measures for **431 of 482** cells = 482 − 17×gap(3) exactly,
    so each track loses precisely `gap` leading positions; straightness stayed within 0..1.
  - `tracking.importCsvTracks` recovered the true assignment **exactly (482/482, no false positives)**
    from a synthesised ImageJ-Manual-Tracking-shaped file — `n°` headers, 1-based slices, µm
    coordinates — i.e. through the whole mapping, unit and frame-base path.

  Running it found four real bugs that the test suite could not: reading via `label_props(img;
  value_name)` while writing via `img_label_props_path` (the isfile check passes, the read then
  fails); `view_label_col()`, which is Julia-only; `view_cols()` silently returning just `label` for
  centroids, which live in obs (`view_centroid_cols()` is the accessor); and the Z,Y,X ordering above.
  **The examples in `docs/examples/` should be run, not just loaded** — loading proves they parse.

  **Not built, deliberately: the Settings rendering.** The API serves `plugins` and `clashes` and the
  Pinia store is typed for both, but nothing draws them yet — that is P3, and a clash is currently
  visible only in the payload and the server log.

  **Known boundary:** the resident napari and preview workers pin `PYTHONPATH` to the checkout's
  `python/` only (`app/src/napari.jl:76`, `app/src/preview.jl:105`), so they do not see a plugin's
  shared `python/`. Pre-existing (hand-dropped modules were never visible to them either) and inert
  today, since only built-in segmentation tasks are previewable — but it is the thing that breaks
  first if a plugin task is ever made previewable.
- **P1.5 — the import builder: preview the file, then map its columns.** (Dominik, 2026-08-17.)
  The task half is done; this is the page half for an *input*, which P1 does not cover — P1 gave the
  custom page a plot canvas, which is the right primitive for RESULTS. An import form needs to show
  the data being imported and let the user assign its columns.

  **It stays a plugin: no format-specific or lab-specific knowledge enters cecelia.** The plugin owns
  delimiter sniffing, headers, sample rows, the mapping and the templates. Three of the four pieces
  need no core change at all:

  - **Column chips.** `chipSelect` already exists as a param type with a renderer and validation, and
    dynamic options already exist as a dispatch hook — `_needs_dynamic_options` /
    `_inject_dynamic_options!`, which is how cellpose enumerates checkpoints, kept in sync with
    `validate_params`. A plugin ships arbitrary Julia, so it can overload both. **But plugins are
    locked out today**: `api_task_definitions` resolves via `_fun_name_map()` (built-ins only) and
    skips anything else — resolving through `_task_from_fun_name` instead is roughly a one-line fix
    and is the prerequisite for all of this.
  - **Drawing the tracks.** Reuse `frontend/src/plots/trackPaths.ts` from the correction work, which
    states in its own header that it is "ONE module for this, used by every track-path drawing that
    follows — not a private helper inside the correction view". No new component, no plugin-supplied
    Vue. **Ordering dependency: this waits for `feat/correction-seg-tracks` to merge.** Building a
    second track-path renderer here would be the exact divergence CLAUDE.md warns about.
  - **Parsing.** Any delimited file, in the plugin's own Python/Julia. Core never learns what a CSV is.

  The one genuine core seam is **plumbing, and generic**: a task-declared preview that returns JSON to
  the form, so the plugin can answer "here are the headers" and "here are the paths in this file"
  before anything is imported. That rhymes with the existing `task_previewable` + `/api/preview/run`
  ("run the task's real compute and RETURN the result rather than writing a store") — same shape, JSON
  instead of a mask block. Two sub-problems to settle when building it:
  - Options here depend on **another param's current value** (the file the user just picked), whereas
    cellpose enumerates from the filesystem and needs nothing from the form. So the request has to
    carry current form state; today the spec is fetched once per page load.
  - `trackPaths.ts` takes µm coordinates converted server-side by `scale_centroids!`. The importer's
    own unit/axis handling must go through the same conversion, or the preview and the import will
    disagree — and Z,Y,X vs X,Y,Z has already caused exactly that once (see the Phases note above).

  **Why it is worth doing:** a plugin that contributes a task *and* a real page is the only thing that
  tests the plugin concept end to end. It also retires the standing reservation that `maxDistance` is
  a guess — with a preview you can see whether spots landed on cells instead of inferring it from a
  match count.

- **P2 — install / remove.** Pinned fetch from a URL, install record, uninstall, `/api/plugins/*`
  routes. Confirm dialog with the trust text (Decision 5).
- **P3 — Settings UI.** The plugins section in the existing panel: installed list with version + ref,
  install-by-URL, remove, the restart-needed hint (Decision 7).
- **P4 — the curated few.** The registry list plus the seed plugins.

  **First candidate is the external TRACK importer** (Dominik, 2026-08-17). An earlier draft of this
  plan named cell2location and Xenium instead, "because working old-R code exists to translate" —
  which is a much weaker criterion than *can we tell whether it is right*. Both halves of that
  rationale have since collapsed:
  - There is **no spatial data to hand**, so a cell2location/Xenium plugin could not be verified. This
    plan's own P1 is the argument: four real bugs in the example survived CI *loading* it and died
    only when it was run against real tracks. Shipping an unrunnable seed plugin repeats that.
  - Their design is **not settled for Feijoa**. `importImages` has three tasks and an import runs
    against an already-created `CciaImage`; it does not conjure one from an external file the way old
    R did. So the old code does not translate mechanically — that is design work, not a port.
  - The "without waiting on another lab's files" argument is dead anyway: the importer takes a
    **column mapping** with shipped templates, so no one is blocked on their export.

  Track import has neither problem — real data exists, it is verified end to end, and it is the ask
  that started this plan. Keep cell2location/Xenium on the catalogue above as later candidates, gated
  on spatial data arriving and on the "how does an importer create an image" question being answered.

  **Do not split the example into its own repo before P2 exists.** There is nothing to fetch it with
  until then, and the importer is the intricate half (mapping, units, axis order — where every bug
  was), so moving it out of `docs/examples/` costs it CI coverage for no gain. When P2 lands, extract
  the importer to **`schienstockd/ccia-trackImport`** and leave `cumulativeChange` + its plot spec as
  the in-repo reference; do not keep a copy in both, which is how `docs/examples/custom-modules/`
  rotted.

  **Naming: `ccia-<thing>`, and the installed directory takes the repo name.** So
  `schienstockd/ccia-trackImport` installs to `<config_dir>/modules/plugins/ccia-trackImport/` with
  `"name": "ccia-trackImport"` in its manifest. One-to-one means install, update and remove map onto a
  single directory with no lookup table (Decision 1), and a glance at `plugins/` says where each
  directory came from. The camelCase second half matches the house `fun_name` style
  (`tracking.importCsvTracks`), and a plugin directory name is never parsed as a Julia or Python
  identifier — P1 proved that with a hyphenated name — so it is free to be exactly the repo name.

## Cross-file architecture

| Concern | File |
|---|---|
| Loader (no change expected) | `app/src/tasks/custom_modules.jl` — `load_custom_modules!`, `custom_modules_dir` |
| Registry / precedence | `app/src/tasks/task.jl` — `register_task!`, `_task_from_fun_name` (Decision 3: real work, not a one-liner) |
| Plugin Python on `PYTHONPATH` | `app/src/py_runner.jl` — `_custom_modules_pydirs` (see R2; built) |
| Tarball fetch to reuse in P2 | `api/src/update_api.jl` — staged download on `Downloads` stdlib (see R1) |
| Definitions + categories scans | `api/src/routes.jl:340-362` (`api_task_definitions`), `:464-511` (`api_custom_modules_status`) |
| New: manifest + install/remove | `app/src/tasks/plugins.jl` (new), `api/src/routes.jl` (new `/api/plugins/*`) |
| Frontend store + panel | `frontend/src/stores/customModules.ts`, `frontend/src/modules/SettingsModule.vue` |
| Docs to update on landing | `docs/CUSTOM_MODULES.md` (the distribution section + the reversal), `CUSTOM_MODULES_PLAN.md` (non-goal reversal), `docs/API.md` (routes), `docs/SHIPPING.md` (if install touches the packaged app) |

## Resolved (were open questions; settled against the code 2026-08-17)

**R1. The fetch is a tarball. `git` is not available and never was.** Both installers fetch tarballs
over plain HTTP — `install.sh:85,101` and `install.ps1:76,92` pull
`archive/refs/heads/<branch>.tar.gz` / `releases/download/<tag>/cecelia.tar.gz` via `curl` /
`Invoke-RestMethod`. Stronger still, `_is_installed` (`api/src/update_api.jl:35`) *defines* an
installed app as **"has a `VERSION` file and has no `.git`"** — so "the packaged app may not have git"
is not a risk to check, it is the shipped invariant.

Consequence for P2: **reuse the existing staged-download path in `api/src/update_api.jl`** (Julia's
`Downloads` stdlib, no new dependency, already handles the GitHub release shape). "Which commit"
becomes a manifest convention recorded in `.install.json` (Decision 5), exactly as the tarball
alternative described. This question is closed; do not re-litigate it in P2.

**R2. Co-located siblings already work; a plugin-level shared `python/` dir does not, and cannot.**
Two separate mechanisms, and the plan conflated them:

- A runner's **own directory** is `sys.path[0]` automatically, because `run_py` launches it by
  absolute path (noted at `py_runner.jl:50-52`). So `plugins/<plugin>/tracking/helper.py` imports
  from `plugins/<plugin>/tracking/importX_run.py` today with no change. That half is fine.
- A **plugin-level** `plugins/<plugin>/python/` shared by several of its categories is only reachable
  via the modules root that `_custom_modules_pydirs()` (`py_runner.jl`, formerly the singular
  `_custom_modules_pydir`) adds —
  which spells the import `plugins.<plugin>.python.<mod>`. **`<plugin>` is a directory name, not a
  Python identifier**: the manifest example in Decision 4 is `trackimport-smithlab`, whose hyphen makes
  that import unresolvable no matter what the runner does.

Fix (**built**), in the same helper rather than at any call site: `_custom_modules_pydirs()` contributes
each `plugins/<plugin>/` root to the path alongside the modules root, so a plugin's shared code is
imported as a top-level module and its directory name is free to be any valid path. Do this in **P1** —
it is a few lines, and without it Decision 1's "one directory" cannot hold shared Python at all.

## Open questions

1. **Do plugins need their own Python deps?** Almost certainly yes eventually (a format reader with a
   pip dependency), and `pixi.toml` is the single source of truth for pins. Recommend declaring this
   **out of scope for v1** and saying so in the docs — a plugin may only use what the env already
   ships. Otherwise this quietly becomes an environment-management feature.

   **But note the cost that "use what the env ships" actually carries, because P4 pays it immediately.**
   A store-writing importer (cell2location, Xenium) must go through `staged_store`, `store_compressor`,
   `create_multiscales` and `write_calibration` — the mandated helpers in `python/cecelia/utils/`. Those
   are reachable (the `cecelia` package is on `PYTHONPATH` already), so the seed plugins do work. The
   consequence is that **publishing plugins turns `python/cecelia/utils/*` into a public ABI** that
   can no longer be refactored freely. That is a real commitment and a bigger one than the v1 scoping
   line above; it wants an explicit decision before P4, not after.
2. **Should a plugin be able to ship a suggested view profile?** It composes cleanly — a profile is
   just a list of paths ([`VIEW_PROFILES_PLAN.md`](VIEW_PROFILES_PLAN.md)) — but keep the two systems
   **decoupled**: a plugin may drop a profile file; neither system needs to know about the other.

## Related

- [`CUSTOM_MODULES_PLAN.md`](CUSTOM_MODULES_PLAN.md) — the shipped mechanism this builds on; its
  Decision 2 constrains this plan and its non-goal is what this plan reverses.
- [`../CUSTOM_MODULES.md`](../CUSTOM_MODULES.md) — the user-facing guide to extend.
- [`CORRECTION_PLAN.md`](CORRECTION_PLAN.md) — the same lab's other two asks, which stay in the app.
- [`VIEW_PROFILES_PLAN.md`](VIEW_PROFILES_PLAN.md) — hiding the pages that lab does not use.
