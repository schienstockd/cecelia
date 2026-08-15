# Value-name input — one primitive for "the name this task writes under"

**Status:** Phases 1 and 2 BUILT; Phase 3 open — tracked in `docs/TODO.md` → *Value-name input —
remaining phases*. Decisions below are locked.

| | phase | state |
|---|---|---|
| — | settle the duplicate "what does this task write" resolvers (`utils/taskOutput.ts`) | ✅ done |
| 1 | the `SuggestInput` primitive + `valueNameInput` param type, on the namespaces that already work | ✅ done |
| 2 | params remembered per output name, restored when you name an existing output | ✅ done |
| 3 | the five namespaces with nothing to suggest from — clusters, regions, stats, models, obsCols | ⬜ open |

Phase 3 in one line: **7 of the 11 output-naming params are still bare text**, because there is no
accessor to list existing names for their namespace. The per-task breakdown and the order to do it in
are in the TODO entry; the design is D5/D6 below.

## The problem, in one screen

Re-running `segment.cellpose` for a second cell type means typing the output name *and* re-entering
every model parameter by hand, because the params you used for `Tcell` are gone — the form shows what
you last ran, which was `Neutrophil`.

Two separate gaps cause that:

1. **The form can't offer the names you already use.** `outputValueName` on cellpose/coastal/branching
   is free text. There is no list, so no recall and no protection against a typo silently creating a
   third label set.
2. **Param memory is keyed by task, not by output.** `meta["funParams"][fun]` (`app/src/model/image.jl`
   → *Per-task param memory*) stores ONE blob per task function. `Tcell` and `Neutrophil` overwrite
   each other.

## Why this is worth a primitive rather than a widget

Value names are how most of this app's storage is addressed, and **eleven task params across six key
spellings** name something into that storage:

| key | tasks | names into | scope |
|---|---|---|---|
| `outputValueName` | `segment.cellpose`, `segment.coastal`, `segment.branching` | label sets | image |
| `valueNameSuffix` | `clustPops`, `clustRegions`, `clustTracks` | cluster/region runs | image × value_name |
| `graphSuffix` | `spatialAnalysis.cellNeighbours` | spatial graphs | image |
| `statsSuffix` | `spatialAnalysis.neighbourStats` | interaction stats | image |
| `colName` | `behaviour.hmmStates`, `behaviour.hmmTransitions` | h5ad obs columns | image × value_name |
| `modelName` | `opticalFlow.train` | flow models | **global vault** |

Six spellings for one UX concept is not merely untidy — **the concept cannot be found by grep today.**
Two separate scoped searches during the design of this plan concluded "3 tasks" and "12 tasks", both
wrong, because each keyed on a name rather than on the behaviour. That is the strongest single
argument for a registry: it makes the concept greppable (`"namespace": "clusters"`).

## Locked decisions

### D1 — Keep the six key names. Add a `namespace` declaration instead.

The keys are NOT synonyms; they name five different storage shapes:

| key | physically |
|---|---|
| `outputValueName` | a versioned-dict key — `labels[name]`, `label_props[name]` |
| `valueNameSuffix` | a column suffix within a family — `clusters.{suffix}` / `regions.{suffix}` |
| `graphSuffix` / `statsSuffix` | a filename stem — `spatialGraph/{suffix}.h5ad` |
| `colName` | an obs column suffix — `live.cell.hmm.state.{colName}` |
| `modelName` | a filename in the global model vault |

Renaming them all to `outputValueName` would make eleven handlers lie — `hmm_states.jl` writes a
COLUMN; calling that a value name is wrong at the point it matters most, in the code that stores it.
**The shared concept is real but lives at the UX layer**, not the storage layer, so it is declared in
the spec (which the form reads) rather than imposed on the key (which the handler reads).

Consequence accepted: nothing on disk changes. Saved chain nodes, `funParams` blobs and `runlog.json`
entries keep their existing keys and keep working, with no migration.

> Optional, separate: `valueNameSuffix` is already slightly wrong — a run suffix inside `clusters.`,
> not a suffix *of* a value name. Renaming that ONE key is a correction, not a convergence. Not part
> of this plan.

### D2 — Suggestions come from DISK; param recall from the params store. *(amended at build time)*

Two different questions that look like one:

- *"What names exist?"* → the namespace's accessor. Authoritative: a name that is offered can be
  re-run onto, and one that was deleted stops being offered. Using the run log here would offer every
  typo forever.
- *"What params did I use for that name?"* → the params store, keyed by name.

> **Amended.** As drafted, this said param recall would read `runlog.json`, since it already records
> `{fun, valueName, status, params, at}` per run. Phase 2 did NOT do that, for three reasons that only
> became clear against the code:
>
> * **The run log is per-IMAGE.** `funParams` resolves image → set, which is what makes the form work
>   when several images are selected. Reading recall from the run log would have meant reimplementing
>   that fallback against a store that has no set-level half.
> * **It records FAILED runs too** — deliberately, so repeated failures are visible. Restoring the
>   params of a run that failed is the opposite of helpful.
> * **It is capped at 200 entries**, so recall would silently stop working on a busy image.
>
> So Phase 2 extended the existing mechanism instead: `meta["funParamsByName"]`, alongside the flat
> `funParams` blob. The run log stays what it says it is — a tuning TRAIL, read by the observer, never
> a source the form restores from. The *shape* of D2 holds; only the store changed.

### D3 — The input is a combobox: type freely, with existing names offered.

Not a dropdown (`measure_labels`'s `valueNameSelection` already proves that shape is too strict for
an output — you can never name a new one) and not bare text (today's cellpose — no recall). The two
already exist in the codebase; this is their union.

**A match against a known name is what triggers param recall.** That resolves the one genuinely
ambiguous case: typing a NEW name must not silently swap the form's other fields out from under you,
and there is no reliable way to know "this is new" from a plain text field mid-typing.

### D4 — A real popover component, not the native `<datalist>`.

**Started native and escalated within the hour, on evidence.** `MoviesModule` already used a
`<datalist>` for movie tags, so it was the obvious first cut — and on screen it is unusable: a
`<datalist>` popup is browser **chrome**, so it renders at the browser's own UI font (~16px), ignores
every `--cc-*` token, and its options come out roughly twice the size of the `0.82rem` input they
belong to. No selector reaches it.

So: `components/SuggestInput.vue`, built on `TeleportPopover` (the canonical popover — teleport,
positioning, theme, outside-click, Escape), owning only the list and the keys. Matching and keyboard
logic is pure in `utils/suggestInput.ts`.

**Named generically on purpose.** A value name is not the only field of this shape — an image
attribute's VALUE and a movie tag are the same problem, and the attribute one matters more than any
value name does, because attribute values are the cohort grouping axis and a typo there invents a
group instead of erroring. `valueNameInput` remains the task-spec param TYPE; the component it renders
is not value-name-specific. See `docs/UI.md` → *Suggesting what you already use*.

Behaviour, chosen deliberately:

- **Filters as you type; opens on typing, never on focus.** An untouched form is not covered by a
  popover nobody asked for, and a name with no matches shows nothing — which is itself the signal
  that you are creating something new.
- **Nothing is highlighted after a keystroke.** You are naming something NEW until an arrow key says
  otherwise, so Enter must never silently accept a suggestion you did not move to.
- **An exact match is still offered.** Seeing the name you just typed in the list is how you know
  this run REPLACES an existing output rather than creating one — the distinction the whole input
  exists to make visible.

### D5 — Namespace accessors belong on the IMAGE.

`_clustfeatures_suffixes` currently lives in `gating/population_manager.jl`. `INVENTORY.md` already
records the rule (*"accessors belong on the image, not buried in gating"*), and every other namespace
already complies (`img_value_names`, `img_spatial_graph_suffixes`, `img_track_value_names`,
`img_branch_value_names`). Unifying is the moment to move it, not a separate tidy-up.

### D6 — Scope is not uniform, and the registry must say so.

Three namespaces are not "a list of names on this image":

- `modelName` → the **global** model vault (`coastal_models_dir`), shared across projects.
- `valueNameSuffix`, `colName` → per **(image, value_name)** — a cluster run belongs to a
  segmentation, not to an image.

A component that assumes image scope is wrong for three of nine. `scope` is therefore part of the
registry entry, not an assumption.

## The registry

One entry per namespace, declared once and consumed by both the form and the param memory.

| namespace | scope | Julia accessor | on the image payload today |
|---|---|---|---|
| `filepaths` | image | `img_value_names(img; field=:filepath)` | yes |
| `labels` | image | `img_value_names(img; field=:labels)` | yes |
| `spatialGraphs` | image | `img_spatial_graph_suffixes` | yes |
| `tracks` | image | `img_track_value_names` | yes |
| `branches` | image | `img_branch_value_names` | no |
| `clusters` | image × value_name | `_clustfeatures_suffixes(; family="clusters")` → **move to image** | no |
| `regions` | image × value_name | `_clustfeatures_suffixes(; family="regions")` → **move to image** | no |
| `stats` | image | **does not exist — to write** | no |
| `models` | global | `coastal_models_dir` listing — **to write** | no |
| `obsCols` | image × value_name | label-props obs columns | no |

Five of ten need no new backend work. The gap is real but bounded: two accessors to write, two to
move, and the payload to extend.

> **Namespace names are not Julia field names.** The `filepaths` namespace reads the `:filepath`
> field (singular); the payload key the frontend already uses is `filepaths` (plural). The registry
> entry is the one place that mapping is written down — do not infer one from the other.

## Build sequence

Each phase is independently shippable and leaves the app working.

**Phase 1 — the primitive, on the namespaces that already work. DONE.**
`valueNameInput` param type + `namespace` in the spec, rendered by `components/SuggestInput.vue`
from the same `availableValueNames` that `valueNameSelection` already computes. Flipped
`cellpose`/`coastal`/`branching` (`labels`) and `cellNeighbours` (`spatialGraphs`). Julia validates
the value (non-empty, no path separator) — the `text` type it replaced validated nothing.

Also, and the precondition for the rest: the DUPLICATION was settled first. `utils/taskOutput.ts` is
now the one answer to "what does this task write"; `previewValueName` and `ChainModule` delegate.
That found two live bugs — `normField` collapsed every non-`labels` namespace to `filepath` (so a
cluster suffix would have been offered as an image version), and `field: "filepaths"` vs
`outputField: "filepath"` never matched when compared raw.

**Phase 2 — param memory keyed by output name. DONE.**
A SEPARATE `meta["funParamsByName"]` rather than nesting inside `funParams[fun]` — nesting would make
that blob ambiguous about whether a key is a param or a name, and would need a migration. Nothing
migrates: a task/name pair with nothing banked reads through to the flat blob, which stays the
most-recent-run record because that is what a NEW name falls back to.

Two things the build added that the plan had not anticipated, both load-bearing:

* **`matched` on the response.** The read has to distinguish "nothing banked for this name" from
  "here is the last run" — the form may only REPLACE what the user is looking at for the first.
  Applying the fallback would stamp the previous run's params over unsaved edits. Hence
  `read_module_fun_params_by_name`, which deliberately does not fall back.
* **Restore on COMMIT, not on change.** `ParamRenderer` emits `commit` when a `valueNameInput` is
  finished (blur, or accepting a suggestion). Per-keystroke would swap every other field mid-word,
  since typing toward `Tcell2` passes through `Tcell`.

`Cecelia.task_output_name` is the Julia twin of `taskOutput`, pinned against the same specs.

**Phase 3 — the missing namespaces.**
Move `_clustfeatures_suffixes` onto the image (D5); write the `stats` and `models` accessors; extend
the image payload with `branches`, `clusters`, `regions`, `stats`, `obsCols`; add the global-scope
path for `models` (D6). Flip the remaining seven task specs.

## Traps

- **`previewValueName`** (`frontend/src/utils/taskPreview.ts`) resolves `outputValueName ?? valueName
  ?? 'default'`, and `ChainModule.vue` carries a second copy of the same rule. Both are correct only
  for the `outputValueName` spelling. The registry is the third place that answers this question —
  **settle on one spelling before writing a third variant**, or this plan adds the exact duplication
  the repo rule forbids.
- **Chains.** A chain node stores params; per-value-name memory must not change what a saved chain
  replays. Chain params are explicit and stay authoritative — the memory only ever populates a form.
- **Set vs image scope.** `funParams` is written to each image AND the set, and read image → set.
  The nested layer must preserve that fallback, or multi-image selections silently lose their default.
- **A name that is offered but no longer exists.** Disk-backed suggestions (D2) make this
  self-correcting; do not "helpfully" merge run-log names into the list.
- **`obsCols` is the one expensive namespace.** Every other accessor reads `ccid.json` or one
  `readdir`; listing obs columns means opening the `.h5ad`. It is also the least valuable of the ten
  (two `behaviour` tasks). If Phase 3 gets tight, drop `obsCols` rather than paying an HDF5 open on
  every form render — and if it is kept, the list belongs behind the same lazy fetch the pop pickers
  use, not on the image payload.

## Out of scope

- Renaming any param key (D1).
- Replacing `valueNameSelection` — the strict dropdown is correct where the input MUST already exist
  (`measure_labels`, tracking).
- A styled combobox component (D4) — revisit on evidence.
