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

- [ ] **`importImages.omezarr` ignores the Settings store layout.** The comment says the import form
      pre-fills `ngffVersion` from `store_layout()`; it does not — no hook, and no frontend code
      touches it, so the Settings choice reaches only REPL/chain runs while the GUI submits the spec
      literal `"0.4"`. Also `advanced.chunkSeparator` is declared, read by NOTHING, and its default
      `"flat"` contradicts `CHUNK_SEPARATOR_DEFAULT = "nested"`. A `defaultFrom` field would parallel
      `optionsFrom`; a live wrong-output path either way.

- [ ] **`editImages.cropImage` consumes four params its spec does not declare** — `z0/z1/t0/t1`,
      supplied by `CropPanel.vue`, which also hardcodes `funName` and `poolName: 'io'` instead of
      reading the `resource_pool` the spec already declares. Undeclared params are invisible to
      validation, to funParams reconciliation, and to anyone reading the spec as the contract.

- [ ] **`COHORT_STAGES` hardcodes 8 fun_names in TypeScript**, kept in step with Julia by a test, so
      a plugin task can never bank cohort metrics. Either a spec flag or — better, and there is
      precedent — stamped by the definitions route the way `previewable` already is. **Needs a call
      between those two before anyone writes it.**

- [ ] **`propagateValueName` only walks top-level params** (`ChainModule.vue:925`), so a
      `valueNameSelection` inside a section is never prefilled from an upstream edge.

- [ ] **`showIf` inside a repeatable `group`** is evaluated against the top-level form, so a
      sub-param cannot be gated on its own entry's siblings. Four specs use groups. Does not bite
      today; will the first time someone tries it.

- [ ] **Dead type `labelPropsSelection`** declared in `types.ts`, used by zero specs.


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
