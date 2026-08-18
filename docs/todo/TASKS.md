# Tracked task list — `feat/plugins`

One list, kept current. An item is checked when it is **merged and green**, not when it is written.
Delete a checked item once its PR is merged to main; this is a worklist, not a changelog.

Design plans live elsewhere and are linked per item: `PLUGINS_PLAN.md` for the plugin system,
`docs/ARCHITECTURE.md` for the invariants any of this has to respect.

---

## Done this round (on the branch, not yet merged)

- [x] ~~**`showIf` — conditional params in the spec JSON, not in Julia**~~ Shipped, with the line
      drawn at "is the form enough?": `showIf` for form-decidable conditions, a server hook setting
      `hidden` for anything needing a file read. The importer's three mode rules moved to its JSON.
- [x] ~~**The spec's `default` is the only default.**~~ `run_task` never applied them, so all 215
      handler fallbacks were a second source of truth — and five contradicted their spec
      (clustTracks 1≠5, trainRatio 1.0≠0.8, labelSmoothing 0.0≠0.5, maxContactDist 10≠5,
      forceRecompute false≠true). Only REPL/chain/MCP callers were affected; the GUI always submits
      every param. A ratchet found a sixth the audit missed.
- [x] ~~**`required` actually works.**~~ `Any[] == ""` is false, so it could never express "pick at
      least one" — nine tasks re-implemented it as a post-Run log line. Now an empty collection
      counts, `requiredMessage` carries the useful sentence, and the Run button refuses first.
- [x] ~~**`optionsFrom`**~~ — three tasks each hand-walked the spec to fill a model picker. Now a
      spec field, resolved for every task, with vault options appended to the spec's literal ones.
- [x] ~~**Imported points are invisible in the viewer**~~ — the payload now carries
      `labelPropsNames` beside `labels`, and the viewer unions them: a maskless row keeps the tracks
      toggle, drops the show-labels eye and branches. **Unverified in a browser.**
- [x] ~~**Two pickers read the wrong segmentation**~~ — `resolveColValueName` looked for a literal
      `pops`; `clustPops`/`clustTracks` call theirs `popsToCluster`, so both silently listed the
      first label set's columns. Resolved by param TYPE now.
- [x] ~~**The chain editor blanked every population picker**~~ — its ParamRenderer context omitted
      `projectUid`, so 13 + 4 specs rendered empty there while working in the runner.
- [x] ~~**Anti-drift safeguards**~~ — three ratchets: a spec field must be declared AND documented
      (caught `includeChannels`, read by nothing); a `showIf` must name a param that exists; a
      handler fallback must not contradict its spec default. `docs/MODULES.md` gained a "Fields any
      param may carry" section — nine fields were in use and documented nowhere.

## Now

- [ ] **`cleanupImages.cellposeCorrect` hardcodes its denoise models** — no hook at all, so a
      user-dropped checkpoint is unreachable. One `optionsFrom` away, but it needs a lister for that
      vault first, which is the open half of the known custom-models gap.

- [x] ~~**`importImages.omezarr` ignored the Settings store layout.**~~ `defaultFrom` added, paralleling
      `optionsFrom`: `{"key":"ngffVersion","defaultFrom":"zarr.ngffVersion"}`. The form carried a
      literal `"0.4"` while a comment claimed it pre-filled from `store_layout()` — so choosing zarr
      v3 in Settings and importing from the form silently produced a v2 store. The dead
      `chunkSeparator` param went with it: no `--no-nested`, no `dimension_separator`, nothing read
      it, and its default `"flat"` contradicted `CHUNK_SEPARATOR_DEFAULT = "nested"`.
- [x] ~~**`editImages.cropImage` consumed four undeclared params**~~ — `z0/z1/t0/t1` declared, and
      `CropPanel` reads `resource_pool` from the def instead of hardcoding `'io'`.
- [x] ~~**`propagateValueName` only walked top-level params**~~ — recurses now.
- [x] ~~**Dead type `labelPropsSelection`**~~ — removed.

- [ ] **A dead-PARAM ratchet was tried and rejected — do not re-propose without new signal.**
      "Every declared param key must appear in its task's own `.jl`/`_run.py`" finds **24** hits and
      **23 are false positives**: cellpose and coastal params live inside a `group` handed to Python
      wholesale, so the keys never appear as literals, and others are read through helpers
      (`exclusive` via `correction_utils`, `pops` via `_hmm_pops`). Only `debounce_ms` on a test task
      is genuinely unread. Gating on that needs an allowlist, and an allowlist is what rotted
      `ALLOWED_NESTED`. `chunkSeparator` was found by reading, not by a scan.

- [ ] **`COHORT_STAGES` hardcodes 8 fun_names in TypeScript**, kept in step with Julia by a test, so
      a plugin task can never bank cohort metrics. Either a spec flag or — better, and there is
      precedent — stamped by the definitions route the way `previewable` already is. **Needs a call
      between those two before anyone writes it.**

- [ ] **`showIf` inside a repeatable `group`** is evaluated against the top-level form, so a
      sub-param cannot be gated on its own entry's siblings. Four specs use groups. Does not bite
      today; will the first time someone tries it.


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
