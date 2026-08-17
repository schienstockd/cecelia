# Plugins — distributable custom modules

**Status:** planning, no branch. Written to be picked up cold by another session.
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

So this plan is **distribution only** — install / update / remove a module set from a URL, and host a
few. It is explicitly **not** a plugin framework:
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

**The lab's own two importers are not in the old R checkout.** `inst/modules/sources/tracking/importTracking.R`
is a no-op stub and `inst/app/modules/inputDefinitions/tracking/trackmate.json` is an empty spec with
no `.R` beside it — old R `source()`d the user's `modules/` folder, so their importers almost
certainly lived there, outside the repo. **Ask the lab for those files**; with them this is a
translation job, without them it is a guess.

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

## Decisions

1. **A plugin is one directory: `<config_dir>/modules/plugins/<plugin>/`**, containing a manifest plus
   the same `<category>/<name>.{jl,json,_run.py}` co-located layout as a hand-dropped module. One
   directory = one git repo = one unit to install, update and remove. Hand-dropped modules keep
   working unchanged at `<config_dir>/modules/<category>/...`; `plugins/` is additive.
2. **Category is derived from the directory *below* the plugin root**, never from the plugin name.
   Teach the two API scans about `plugins/*/` explicitly rather than making them blindly recursive —
   an unbounded `walkdir` for categories would turn any stray nested folder into a phantom category.
   The Julia loader needs **no change** (it is already recursive).
3. **Built-ins still win on clash, and a plugin cannot shadow another plugin.** The existing precedence
   rule (`CUSTOM_MODULES_PLAN.md`) extends by one tier: built-in > hand-dropped > plugin, and a
   plugin-vs-plugin `fun_name` collision is a load **error** reported in Settings, not a silent
   last-one-wins.
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
5. **Install is a pinned fetch the user confirms, showing what it will run.** Source = a git URL (or
   release tarball) plus a **commit or tag**, recorded in the manifest's install record. The confirm
   dialog names the repo, the ref, and says plainly that plugin code runs with full access to the
   machine and is not sandboxed. **No auto-update, no background fetch, no install-on-startup.**
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

## Phases

- **P1 — layout.** `plugins/<plugin>/` + manifest parse + the two API scans (Decisions 1–4). No
  network. A plugin installed by hand (`git clone` into the dir) works end to end: registers, has a
  form, has a nav entry. **This is the whole blocker; ship it alone.**
- **P2 — install / remove.** Pinned fetch from a URL, install record, uninstall, `/api/plugins/*`
  routes. Confirm dialog with the trust text (Decision 5).
- **P3 — Settings UI.** The plugins section in the existing panel: installed list with version + ref,
  install-by-URL, remove, the restart-needed hint (Decision 7).
- **P4 — the curated few.** The registry list plus the seed plugins. Strongest first candidates are
  **cell2location** and **Xenium**, because working old-R code exists to translate and they exercise
  the "importer as plugin" shape without waiting on another lab's files.

## Cross-file architecture

| Concern | File |
|---|---|
| Loader (no change expected) | `app/src/tasks/custom_modules.jl` — `load_custom_modules!`, `custom_modules_dir` |
| Registry / precedence | `app/src/tasks/task.jl` — `register_task!`, `_task_from_fun_name` |
| Definitions + categories scans | `api/src/routes.jl:340-362` (`api_task_definitions`), `:464-511` (`api_custom_modules_status`) |
| New: manifest + install/remove | `app/src/tasks/plugins.jl` (new), `api/src/routes.jl` (new `/api/plugins/*`) |
| Frontend store + panel | `frontend/src/stores/customModules.ts`, `frontend/src/modules/SettingsModule.vue` |
| Docs to update on landing | `docs/CUSTOM_MODULES.md` (the distribution section + the reversal), `CUSTOM_MODULES_PLAN.md` (non-goal reversal), `docs/API.md` (routes), `docs/SHIPPING.md` (if install touches the packaged app) |

## Open questions

1. **Does the fetch use `git` or a tarball download?** `git` gives cheap pinning and update but adds a
   binary dependency an installed app may not have; a tarball needs no git but makes "which commit"
   a manifest convention. Check whether the packaged app can assume `git` before designing P2.
2. **Where does a plugin's Python live on `PYTHONPATH`?** `run_py` already resolves absolute paths and
   adds the user modules dir; confirm a `plugins/<plugin>/python/` sibling import works, or require
   the runner to be self-contained.
3. **Do plugins need their own Python deps?** Almost certainly yes eventually (a format reader with a
   pip dependency), and `pixi.toml` is the single source of truth for pins. Recommend declaring this
   **out of scope for v1** and saying so in the docs — a plugin may only use what the env already
   ships. Otherwise this quietly becomes an environment-management feature.
4. **Should a plugin be able to ship a suggested view profile?** It composes cleanly — a profile is
   just a list of paths ([`VIEW_PROFILES_PLAN.md`](VIEW_PROFILES_PLAN.md)) — but keep the two systems
   **decoupled**: a plugin may drop a profile file; neither system needs to know about the other.

## Related

- [`CUSTOM_MODULES_PLAN.md`](CUSTOM_MODULES_PLAN.md) — the shipped mechanism this builds on; its
  Decision 2 constrains this plan and its non-goal is what this plan reverses.
- [`../CUSTOM_MODULES.md`](../CUSTOM_MODULES.md) — the user-facing guide to extend.
- [`CORRECTION_PLAN.md`](CORRECTION_PLAN.md) — the same lab's other two asks, which stay in the app.
- [`VIEW_PROFILES_PLAN.md`](VIEW_PROFILES_PLAN.md) — hiding the pages that lab does not use.
