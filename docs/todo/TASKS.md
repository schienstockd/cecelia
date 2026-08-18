# Tracked task list — `feat/plugins`

One list, kept current. An item is checked when it is **merged and green**, not when it is written.
Delete a checked item once its PR is merged to main; this is a worklist, not a changelog.

Design plans live elsewhere and are linked per item: `PLUGINS_PLAN.md` for the plugin system,
`docs/ARCHITECTURE.md` for the invariants any of this has to respect.

---

## Now

- [ ] **`showIf` — conditional params in the spec JSON, not in Julia**
      `ParamRenderer` honours a `hidden` flag, but nothing can SET it from a spec: the policy is
      hand-written Julia in each task's `_inject_dynamic_options!`, with the param keys as literals.
      A plugin author ships JSON and a task `.jl`, so today they must write a Julia hook to make a
      param disappear — the highest-friction way to express the thing most tied to the param itself.
      Add `showIf`, evaluated against the form, no Julia:
      ```json
      { "key": "maxDistance", "showIf": { "mode": "attach" } }
      ```
      The boundary is principled, and both sides of it are needed: **can this be decided from the
      form alone?** If yes → `showIf`. If it needs to read a file, the filesystem or Python (the
      importer's "this XML has no columns") → the server hook stays.
      *Moves the importer's three mode rules out of Julia and leaves only `is_xml` in the hook.*

- [ ] **Audit: what else is hardcoded that should be JSON?**
      Same smell, repo-wide — per-task form behaviour living in Vue/TS/Julia instead of the spec.
      Running now; findings land here as their own items, ranked by tasks-affected × how mechanical
      the fix is. Categories: conditional visibility, special-cased param keys, per-task defaults and
      coercion, dynamic-options hooks that did not need to be hooks, task-specific validation,
      components that branch on a task name.

- [ ] **Imported points are invisible in the viewer** (Dominik, on screen)
      ccid.json carries **two independent registries** and the viewer reads the other one:
      `labels` (written by `register_label_files!`, only for a segmentation with a **zarr store**)
      backs the MASKS chips, the segmentation rows and every `field: "labels"` picker;
      `label_props` (an h5ad table) backs `img_value_names`, gating, populations and the observer.
      A points import registers `label_props` only — there are no mask pixels to register — so it has
      no MASKS chip, no segmentation row, and therefore **no per-row tracks toggle**.
      Chosen fix (Dominik): **union the viewer's row list** over `labels` ∪ `label_props`, with a
      maskless row rendering only the toggles that apply — tracks and populations, no show-labels eye.
      Rejected: registering it in `labels` too, which hands napari a store that does not exist.
      *Rendering change — wants his eyes before it ships.*

## Next

- [ ] **The importer exists twice** — the CI-loaded example under `docs/examples/plugins/` and the
      published `schienstockd/ccia-importTracks` are copies that will drift. One has to become the
      source. (`ccia-trackMeasures` is in-repo only, so this is one plugin, not two.)

- [ ] **`ccia-trackMeasures` has no published repo, and would not prove much if it had.**
      Plugins currently ship **no Vue** (`app/src/tasks/plugins.jl:249`) — deliberately, because
      renderable code makes the frontend a plugin ABI. So "custom task AND custom module page" is
      true only in the declarative sense. Reusing `TrackPathsView` from `feat/correction-seg-tracks`
      is the test that settles it, two ways:
      **(a)** widen `SummaryCanvas` so a plot spec can name a BUILT-IN view kind — no ABI, the
      component stays refactorable; **(b)** load plugin ESM — real custom components, and the ABI
      decision reopened. (a) is a day and reversible. Blocked on that branch merging either way.

- [ ] **Does the crop line up?** The real export is of a cropped/smoothed OME-TIFF. In points mode the
      tracks carry their own coordinates so nothing has to align — but put them beside an image or a
      segmentation of the UNcropped original and the crop offset shifts everything. Only bites once
      something else is placed beside them.

## Deferred, with a reason

- [ ] **52 pre-existing double tooltips — its own piece of work, not this PR's.**
      `nestedTooltips` (`utils/uiCopy.ts`) only knows the `v-tooltip` DIRECTIVE. `SelectionTable`
      takes its tooltip as a PROP (`row-tooltip`) and renders it on the `<tr>`, so a tipped control
      in one of its slots fires two overlapping tooltips and the scan says nothing — which is exactly
      how one shipped into the plugins Settings table before Dominik caught it on screen.
      Teaching the scanner that `row-tooltip` tips the subtree is two lines and finds **52 real
      instances in 6 files**: `ImageTable` 19, `TaskList` 10, `NotebookTable` 9, `TasksModule` 6,
      `MoviesModule` 5, `ProjectPanel` 3.
      Not done here on purpose: `ALLOWED_NESTED` was deliberately drained to empty and the test fails
      on improvement too, so re-populating it would push a closed ratchet backwards — and 52 sites
      across six unrelated files do not belong in a plugins PR. The blind spot is commented at the
      `tipped` line so the next reader finds this entry instead of rediscovering it.

- [ ] **`maxDistance` default (10 px) is still a guess** for attach mode. Needs one real dataset with
      both a segmentation and imported tracks to calibrate; there isn't one yet.

- [ ] **ImageJ Manual Tracking and Imaris templates are inferred**, from their documented formats
      rather than from a real export. TrackMate track XML is the only one checked against a real file.

## Standing caveat

Nothing on this branch is browser-verified by me. The chips, the hidden sections, the file chooser
and the Settings plugins table are all unrendered on my side.
