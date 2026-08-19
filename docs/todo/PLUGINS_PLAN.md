# Plugins — distributable custom modules

**Status:** **P1–P4 built** on `feat/plugins` — layout, both scans, precedence, PYTHONPATH, the module
PAGE, form-driven options, install/remove, the Settings surface and a curated registry. Nothing is
browser-verified. Decisions were verified against the code 2026-08-17: two of the four open questions
are **Resolved** (see that section) and Decision 3's scope was corrected — it was new registry work,
not the one-liner this plan first implied.
**Origin:** a lab that used the old R version wants two format-specific importers back — external
**tracking** and external **segmentation**, in their own format (conference, 2026-08). Neither belongs
in the app: nobody else has that format. Their two other asks are general and are handled in
[`CORRECTION_PLAN.md`](CORRECTION_PLAN.md).

## Open items — the working list

Live checklist for the `feat/plugins` branch. Delete an item when it lands; this is not a changelog.

**Blocking a real import**
- [x] ~~**TrackMate track XML**~~ — read, converted and imported end to end from the real export
      (314 tracks, 4367 detections, 3D, micron). The format has **no columns**: `<particle>` IS the
      track and its id is its ordinal, so the column mapping is bypassed for `.xml`. A DIFFERENT
      export from TrackMate's "Spots in tracks" CSV, which does have columns.
- [x] ~~**Which segmentation does it attach to?**~~ **None — there isn't one** (Dominik). Added a
      *points* mode: each detection becomes a cell, written as a labelProps table and registered, so
      no segmentation is needed. Works because `img_value_names` reads ccid.json's `label_props`, not
      the label store, and `is_tracked` wants only a `track_id` column.
      **Motility only** — no mask means no shape or intensity measures, and napari cannot draw it.
      Track ids are 1-based: `track_props` keeps `track_id > 0`, so the 0-based particle ordinal
      silently dropped a whole track (314 in, 313 out). Pinned by a test.
- [ ] **Does the crop line up?** The export is of a cropped/smoothed OME-TIFF. In points mode the
      tracks carry their own coordinates so nothing has to align — but if they are later compared
      against an image or a segmentation of the UNcropped original, the crop offset shifts everything.
      Only matters once something else is put beside them.

- [ ] **Imported points do not appear in the viewer's MASKS list** (Dominik, on screen). The run
      succeeds and registers the value name, but the chip row still shows only the segmentations that
      have label pixels. Expected in part — a points set has no mask to draw — but it then appears
      NOWHERE in the viewer, which is not the answer either. Being traced; the question is which list
      a tracked-but-maskless value name belongs in.

- [ ] **The contribution model: Decisions 10 and 11 are BUILT, 12–13 are design only** — see *The
      contribution model* below. The `contributions` block, its desugaring, and `views` landed;
      `layers` is next. The component tier is deferred with a named trigger.

**Correctness / cleanup**
- [x] ~~**Split the plugin.**~~ `cumulativeChange` is a track MEASURE and did not belong in a repo
      called `ccia-importTracks` (Dominik). Now two single-purpose example plugins —
      `ccia-importTracks` and `ccia-trackMeasures` — and the published repo carries the importer only.
- [x] ~~**Windows: plugin install could never have worked.**~~ `tar -xzf <absolute path>` — GNU tar
      reads `C:\...` as `host:path` and attempts a REMOTE archive. The repo already had pure command
      builders and a ratchet for exactly this; the installer hand-rolled its own because they covered
      neither gzip nor an explicit destination. Generalised rather than adding a third variant.
- [x] ~~**Windows: a task died on a log line.**~~ A cp1252 stdout raises on `\u2192`, and the
      exception propagates out of `log()` and kills the task. Two SHIPPED built-ins have the same
      latent crash on a success path (`cell_contacts_mesh`, `branching`). Fixed at the sink —
      `script_utils` now reconfigures stdio to UTF-8. Only arrows, \u2264/\u2265 and box rules are
      affected; cp1252 encodes the em dash and \u00b5 fine.
- [x] ~~**A picker labelled "Segmentation" listed image versions.**~~ `valueNameSelection` defaults to
      `filepaths` when `field` is omitted. Three more specs had it, all in files a module author
      copies. Ratcheted: a picker that calls itself a segmentation must read `labels`. Also brought
      `docs/examples/plugins` under `spec_dirs` — no copy ratchet had ever seen a plugin spec.
- [x] ~~**A param can now say "not applicable".**~~ `ParamRenderer` had no conditional visibility at
      all, so an XML with nothing to map still drew five empty dropdowns. `hidden` is set server-side
      by the options hook; resolution ASSIGNS it, or the flag could never be taken back.
- [ ] **The importer exists twice** — the CI-loaded example and the published repo are copies. The
      measures plugin does not (it is in-repo only), so this is now one plugin, not two.
- [ ] **Double tooltips the ratchet cannot see — 52 pre-existing sites, its own piece of work.**
      `nestedTooltips` (`utils/uiCopy.ts`) only knows the `v-tooltip` DIRECTIVE. `SelectionTable` takes
      its tooltip as a PROP (`row-tooltip`) and renders it on the `<tr>`, so a tipped control in one of
      its slots fires two overlapping tooltips and the scan says nothing — which is exactly how one
      shipped into the plugins Settings table before Dominik spotted it on screen.
      Teaching the scanner that `row-tooltip` tips the subtree is a two-line change and finds
      **52 real instances in 6 files**: `ImageTable` 19, `TaskList` 10, `NotebookTable` 9,
      `TasksModule` 6, `MoviesModule` 5, `ProjectPanel` 3.
      NOT done here on purpose: `ALLOWED_NESTED` was deliberately drained to empty and the test fails
      on improvement too, so re-populating it would push a closed ratchet backwards — and fixing 52
      sites across six unrelated files does not belong in a plugins PR. The blind spot is commented at
      the `tipped` line so the next reader finds this entry instead of rediscovering it.
- [x] ~~Blank entry in the tracking function list~~ — `tracking/cell_config.json` (the vendored btrack
      TrackerConfig) was served as a task spec, because the built-in scan had no `fun_name` filter
      while the custom scan did. Both agree now; regression test added.

**UI corrections still open** (all raised on screen, 2026-08-17)
- [x] ~~"Use" button did nothing visible~~ — it filled a field two sections away; registry rows install
      directly now, behind the same confirm.
- [x] ~~Install button rendered BELOW its row~~ — `.save-btn` is `display:flex`, i.e. block-level, in an
      unstyled `.cm-row`. Both lists are `SelectionTable` (THE canonical table) with a single icon
      action per row.
- [x] ~~Content-free "Select this option" tooltip on every row~~ — `SelectionTable`'s fallback; no
      tooltip now unless the caller supplies one.
- [x] ~~Column mapping asked for free text~~ — the columns are `select`s populated from the chosen
      file, and `validate_params` now resolves options against the same form, so a column that is not
      in the file fails validation by name instead of reaching the runner.
- [x] ~~"Write to" invited several rival track_id columns~~ — removed. The import always writes
      `track_id`, the one name `track_props` / `track_measures` / `hmm_transitions` / `is_tracked`
      read. Importing tracks is an ALTERNATIVE to `tracking.bayesian_tracking`, so it writes there.
- [x] ~~A slider to choose between frame 0 and 1~~ — a two-value choice is a select, not a range.
- [x] ~~Track file was a free-text path~~ — `filePath` param type + Browse, reusing `FileBrowser`
      generalised from its `bundle` mode to a `file` mode with an extension filter.

**Unverified**
- [ ] **Nothing in the browser** — the Settings plugins table, the column suggestions, the debounced
      refetch and the custom page's plot canvas are all unrendered by their author.
- [ ] **`maxDistance` default (10 px)** is a guess until an import runs on real data.
- [ ] The ImageJ Manual Tracking and Imaris templates are still inferred, not checked against a real
      export. (TrackMate XML no longer is — there is a real file.)

**Parked**
- [ ] Track preview — blocked on `feat/correction-seg-tracks` (`trackPaths.ts`).

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

## The contribution model — designed, not built

**Status: Decisions 10 and 11 are built; 12–13 are design only.** Written 2026-08-19 after Dominik asked why a
plugin cannot ship a component "the way napari plugins do", and the answer turned out to be more
interesting than "the browser has a build step".

### The wall we hit

`plugin.json` is **metadata only** — `name`, `version`, `description`, `homepage`,
`requiresCecelia`, `categories` (see `read_plugin_manifest`). What a plugin *contributes* is inferred
from where its files sit:

| On disk | Becomes |
|---|---|
| `<category>/<name>.jl` + `.json` | a task, `fun_name = "<category>.<name>"` |
| `plotDefinitions/*.json` | a summary plot on `module: "<category>"`'s page |
| `python/` | importable helpers on `PYTHONPATH` |

Convention over configuration, and it earns its keep: a plugin author writes no manifest
boilerplate, and both example plugins are a handful of files.

**But there is nowhere to declare a KIND of contribution that has no directory.** Every question in
this document that stayed open — "can a plugin draw its output as tracks", "can it add a napari
layer" — is the same question: the plugin has something to say and the manifest has no grammar for
it. Answering each one by inventing another magic folder is how a layout becomes folklore.

### What napari does, precisely

Worth being exact, because the part people remember is not the part doing the work
([building_a_plugin](https://napari.org/stable/plugins/building_a_plugin/index.html)).

Contribution types: `readers`, `writers`, `widgets`, `sample data`, `theme`, `menus`. The manifest
declares them explicitly, with an indirection layer:

```yaml
contributions:
  commands:
    - id: napari-hello.say_hi
      python_name: napari_hello:show_hello_message
  widgets:
    - command: napari-hello.say_hi
      autogenerate: true
```

Three things to take:

1. **`commands` names the callable ONCE**, and contributions reference it by id — so one function can
   be a widget and a menu item without being declared twice.
2. **`autogenerate: true` appears in the first tutorial.** A widget generated from a function
   signature via magicgui is the *canonical* path, not a fallback. Our `params` → `ParamRenderer` is
   the same pattern, arrived at independently — **we already have napari's most-used widget form.**
3. **The central contract is a DATA TUPLE, not a drawing:**

   ```
   LayerData      = (data, [attributes, [layer_type]])
   ReaderFunction = Callable[[PathOrPaths], list[LayerData]]
   ```

   A reader plugin writes no UI at all. It returns `[(arr, {"size": 3}, "points")]` and napari builds
   the layer. `layer_type` is a string; `attributes` are constructor kwargs. **That is a declarative
   description of a layer, passed as data** — and it is the primitive we are missing.

Two things NOT to take:

- **Discovery via pip entry points** (`[project.entry-points."napari.manifest"]`). It requires a
  build backend and a `pip install` before anything works. Our directory drop is genuinely easier for
  a bench scientist; keep it.
- **A copier template.** Our equivalent — "copy `docs/examples/plugins/`" — is loaded *and executed*
  by CI, which a scaffolding template never is.

### The honest scorecard

| napari | cecelia today | gap |
|---|---|---|
| `widgets` + `autogenerate: true` | task spec `params` → `ParamRenderer` | **none** |
| `readers`/`sample data` → `list[LayerData]` | task writes an h5ad; the bridge derives layers from what it already understands | **the layer contract** |
| plots on a plugin's own page | `plotDefinitions/*.json` → `SummaryCanvas`, **plus** `contributions.views` naming a built-in interactive plot | **none** (Decision 11) |
| `widgets` as a hand-written `QWidget` | — | the ABI question, deliberately deferred below |
| `writers` | export tasks | roughly covered |
| `theme` | view profiles | covered, decoupled (Open question 2) |

**A cecelia plugin is not less powerful than a napari plugin in compute.** Both ship executable code
unsandboxed — napari ships Python, we ship Julia plus a Python runner. We are narrower in exactly one
place: *browser rendering*, because a `.vue` needs compiling and an installed app has no Node. That
is a platform fact, not a design choice, and it is why the ABI tier is expensive for us and free for
them.

### Decision 10 — `plugin.json` grows an optional `contributions` block  ✅ BUILT

The filesystem convention **stays as the default and desugars into contributions**, so every plugin
that works today keeps working and writes nothing new. A plugin declares contributions only when it
needs a kind the layout cannot express.

```json
{
  "name": "ccia-trackMeasures",
  "contributions": {
    "tasks":  [{ "funName": "trackTools.cumulativeChange" }],
    "plots":  [{ "spec": "plotDefinitions/cumulative_change.json" }],
    "views":  [{ "module": "trackTools", "view": "trackPaths", "label": "Tracks" }],
    "layers": [{ "fromTask": "trackTools.cumulativeChange",
                 "layerType": "points", "colorBy": "trackTools.cumulativeSpeed" }]
  }
}
```

`tasks` and `plots` are the desugared form of what the directory walk already produces — spelled out
so the grammar is uniform, never required. `views` and `layers` are new.

**No `commands` indirection.** napari needs it because one Python function can be surfaced several
ways; our tasks are already addressable by `fun_name`, which is the same idea with no extra layer.
Adopt it only if a second thing ever needs naming.

### Decision 11 — `views`: a plugin may NAME a built-in view  ✅ BUILT

`frontend/src/components/canvas/interactiveViews.ts` is already a registry keyed by stable id — 8
entries today (`trackPaths`, `trackCorrection`, `trackDiagnostics`, `gatingStrategy`, `filmstrip`,
three flow views) — and its own comment says adding a plot is "add ONE line here". The lookup exists;
it is simply not reachable from a spec.

A plugin names one; `CustomModule.vue` (51 lines, currently `<SummaryCanvas :module="category">`)
renders it. So `ccia-trackMeasures` would show its cumulative measures *and* the track paths those
numbers came from.

**What this makes public: view IDS, not components.** Renaming `trackPaths` breaks installed plugins;
rewriting `TrackPathsView.vue` does not, as long as the id and its data contract hold. That is a far
smaller promise than a component ABI, and it is the whole reason to prefer it.

A named id that does not exist must FAIL LOUDLY — a blank panel is the failure mode this codebase
keeps producing (see the empty column mapping, the wrong-segmentation picker). Ratchet it the way
`showIf` conditions are ratcheted.

**What actually shipped, and the one thing this design missed.** Not "any registered id": a view has
to OPT IN with `pluginPage`, because two of the eight would break if named. `trackCorrection`
**mutates** — a manifest must not be able to put it on a page — and a view asking for a rail
(`clusterPops`, `flowModels`) draws nothing there, since a plugin's page renders the summary canvas's
own population picker and no other. Four are offered today: `trackPaths`, `trackDiagnostics`,
`gatingStrategy`, `filmstrip`, ratcheted to `rail: 'none'`.

`SummaryCanvas` hosts them (the picker gains an **Interactive** optgroup; `InteractivePanel` renders
the panel), so this landed as a capability of THE module-page canvas rather than a custom-page special
case. `CustomModule.vue` passes the category's declared views through; every other host passes none
and is unchanged. An id that does not resolve is reported ON the canvas — "Plot not available here: x
(plugin)" — and the ratchet lives in `interactiveViews.test.ts`, because Julia cannot see the
registry.

### Decision 12 — `layers`: the `LayerDataTuple` equivalent, and where it must differ

A plugin declares what napari should draw from its task's output; the bridge builds the layer. Pure
Python→Python, nothing compiled, and it is the tier that most closely matches what a napari plugin
does.

**This is the gap that made the points import work by accident rather than by design.** Imported
tracks appear in napari only because they registered as a `label_props` value name that the existing
tracks path already understood. A plugin wanting a shapes overlay, a vector field or a mesh has no
route at all — verified: only `napari_bridge.py` calls the `napari_utils.add_*` helpers.

**The one place we cannot copy napari.** A napari plugin runs *inside* the napari process and hands
it a live numpy array. Our task runs in a subprocess and exits. So `data` cannot be an array — it is
a REFERENCE the bridge resolves:

| napari | here |
|---|---|
| `data` = ndarray | a value_name + column selection in the h5ad, or a file the task wrote |
| `attributes` = layer kwargs | the same, restricted to a reviewed allow-list |
| `layer_type` = any napari layer | an allow-list: `points`, `tracks`, `shapes`, `vectors` to start |

The allow-list is the point. `attributes` reaching `viewer.add_*` unchecked is a plugin passing
arbitrary kwargs into napari's constructors, which is an ABI by the back door — the thing Decision 11
was chosen to avoid. Start with what the existing overlays already need and widen on request.

### Decision 13 — the component tier stays deferred, with a named trigger

Shipping a plugin's own renderable component (prebuilt ESM, since a `.vue` cannot be compiled in an
installed app) needs: Vue externalised from the bundle so one instance is shared — it is currently
bundled *inside* `frontend/dist`, checked — a route serving JS from the config dir, a runtime
`import()`, and a version-skew story between a plugin built against Vue 3.5 and an app on 3.6.

Beyond the plumbing it makes props, stores, composables and panel state a **public contract that
cannot be refactored freely**, and unlike code that is not something you can take back once plugins
depend on it.

**Deferred, not rejected. The trigger to revisit: someone wants a picture cecelia genuinely cannot
draw, and Decisions 11–12 cannot express it.** Until then, `views` covers "our plots pointed at their
data", which is what most plugins actually want. Note that napari itself treats the hand-written
widget as the escape hatch and puts `autogenerate` in the tutorial.

### Suggested order

1. **`contributions` block, desugaring only.** ✅ **BUILT** — `Cecelia.plugin_contributions(dir)`
   returns `(; tasks, plots, views, layers, problems)`. The layout half is enumerated by the same
   `_spec_files_in`/`_plot_specs_in` the task and plot scans use, so "which folders are categories"
   has exactly one implementation. A manifest block is CHECKED against it and never restricts: a task
   on disk is a task whether or not the manifest names it, so an author cannot hide their own work by
   adding a block and forgetting a line. `views`/`layers` parse and shape-check (including the
   `layerType` allow-list) but report that nothing acts on them yet, rather than shipping a blank
   panel. Ratchets: every shipped example's declarations must resolve, and at least one example must
   actually declare something or the first ratchet passes vacuously.
2. **`views`.** ✅ **BUILT** — see Decision 11. It settles "a plugin gets a real, non-declarative
   page" without making any component a contract.
3. **`layers`.** Biggest conceptual win, most design left — the reference vocabulary and the
   `attributes` allow-list both want their own pass.

Doing (1) first is the whole point of writing this down: `views` could be built tomorrow as another
special case, and then `layers` would need retrofitting around it.


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

- **P2 — install / remove. ✅ BUILT.** Pinned tarball fetch (`Downloads` in `api/src/plugins_api.jl`),
  verify-then-place in `Cecelia.plugin_unpack!` via `_run_tar`, `.install.json` sidecar, uninstall that
  refuses while one of the plugin's tasks is running, `POST /api/plugins/{install,remove}`. Verified
  live against `github.com/schienstockd/ccia-importTracks`: fetch → install → both tasks register →
  plot spec picked up → registry reports installed → remove unregisters both.
  *(original scope below)*
- **P2 (original) — install / remove.** Pinned fetch from a URL, install record, uninstall, `/api/plugins/*`
  routes. Confirm dialog with the trust text (Decision 5).
- **P3 — Settings UI. ✅ BUILT.** Plugins section in the EXISTING Settings panel (never a second
  surface): install-by-URL behind a confirm that states the code is unsandboxed, installed list with
  version + categories + the advisory version warning, remove, the restart-needed hint for an update,
  the curated list, and the clash list P1 owed. **Unverified in a browser.**
  *(original scope below)*
- **P3 (original) — Settings UI.** The plugins section in the existing panel: installed list with version + ref,
  install-by-URL, remove, the restart-needed hint (Decision 7).
- **P4 — the curated few. ✅ SEEDED.** `app/src/pluginRegistry.json` — shipped with the app, not
  fetched, so an offline install behaves like an online one and the catalogue cannot change under a
  running server; the trade is that adding one needs a release. Seeded with `ccia-importTracks`.

  > **OPEN: the plugin now exists twice.** `docs/examples/plugins/tracktools-example/` (CI loads it)
  > and `schienstockd/ccia-importTracks` (installable) are copies, which is exactly how
  > `docs/examples/custom-modules/` rotted. Resolve once the install flow has been tried by hand:
  > recommended is to cut the example down to `cumulativeChange` + its plot spec as the in-repo
  > REFERENCE, and let the importer live only in its repo. Not done yet because the duplicate is what
  > currently lets both CI and a real install exercise the same code.

  *(original scope below)*
- **P4 (original) — the curated few.** The registry list plus the seed plugins.

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
