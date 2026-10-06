# Function groups — sub-headings in the module function picker

**Status:** in-progress (2026-10-06) on `feat/fun-dividers`.

## Goal

A module page's **Function** select lists every task in that module as one flat, alphabetised block.
On Segment that is twelve rows where segmenters, measurers, correction runs and correction
housekeeping (snapshot / restore obs, report stale downstream) are interleaved by spelling — the
user cannot tell at a glance which rows they need. Put the rows under headings that say what each
kind of function is *for*.

## Reference — what the old R version did

`old-R-shiny-version/inst/app/modules/inputDefinitions/<module>/<fun>.json` carried a per-function
`fun.<name>.category`, rendered as Shiny `selectInput` optgroups. It was a **lifecycle** split, not a
purpose split: 54 of 76 functions were `"Module functions"`, the rest `"Preview functions"`,
`"Synchronise data"`, `"Preparation functions"`, `"Manage images"`, `"Workflow outputs"`. It never
needed purpose headings because corrections lived in their own module (`cleanupImages`) rather than
inside `segment`. We port the *mechanism* (a field on the function spec → optgroup), not its values.

## Decisions (2026-10-06)

1. **New optional spec field `group`, not a reuse of `category`.** Feijoa's `category` already means
   "the module" and is read by the lab-log digest (`app/src/lab_log_context.jl` `_category_of_fun`,
   ordered by `_CATEGORY_ORDER`) and plugin discovery (`app/src/tasks/plugins.jl`). Repurposing it
   would regroup the digest. `group` is presentational only — no backend reads it.
2. **Group names are purpose verbs/nouns, chosen per module** (table below). The same word means the
   same thing everywhere it appears (`Measure` on Segment and on Tracking).
3. **Group order is one global list in the frontend helper** (`utils/taskGroups.ts` `GROUP_ORDER`),
   workflow-ordered; a group not on the list goes after the known ones, alphabetically. Mirrors
   `_CATEGORY_ORDER` and `popGroups.ts`'s `ORDER`. Only a page's own groups ever render, so the list
   only has to be consistent pairwise within a module.
4. **Within a group: alphabetical** (as today's flat sort).
5. **Headers only when they help.** A picker whose defs carry fewer than two distinct groups renders
   flat — no lone header on a one- or two-function module. If some defs are grouped and others are
   not (a hand-dropped plugin task without `group`), the ungrouped ones land under **Other**, last.
6. **Labels stay as they are.** The heading would make the `Correct labels —` prefix redundant *in
   the picker*, but `label` is also the run's name in Task Manager history, chain nodes and the lab
   log, where there is no heading — and `Report stale downstream` exists in both Segment and Tracking,
   so shortened labels would collide there. Considered and dropped: a second `menuLabel` field (one
   more thing to keep in sync for three rows).
7. **One helper, every picker.** `groupTaskDefs(defs)` feeds the module-page `TaskRunner` select
   (`<optgroup>`) and the chain whiteboard palette (sub-headings inside each module section, same
   ≥2-groups rule). The MCP `get_module_params` trim passes `group` through so Claude sees the same
   structure.

## Assignments

| Module (dir) | Group → functions |
|---|---|
| `importImages` + `exportImages` (Manage images) | **Import** → Convert to OME-ZARR, Migrate legacy image · **Export** → Export as OME-TIFF |
| `editImages` (Preprocessing) | **Reshape** → Crop, Bin, Resample Z, Flip · **Project** → Z-projection, T-projection · **Convert** → Bit depth · **Register** → Register images (staining cycles) |
| `cleanupImages` | **Autofluorescence** → AF correction · **Align** → Drift correction, Within-stack alignment, Flow-based registration · **Denoise** → Denoising (SUPPORT), Smoothing |
| `segment` | **Segment** → Cellpose ±measure, Optical flow ±measure, Ridge · **Measure** → Measure labels, Branching (skeleton) analysis · **Correct** → Correct labels ±measures · **Correction tools** → snapshot obs, restore obs, report stale downstream |
| `tracking` | **Track** → Bayesian tracking ±measures · **Measure** → Track measures · **Correct** → Correct tracks ±measures · **Correction tools** → report stale downstream |
| `spatialAnalysis` | **Contacts** → Cell contacts (points / meshes) · **Aggregates** → Detect aggregates, Detect aggregates (meshes) · **Neighbourhood** → Neighbour graph, Interaction matrix |
| `behaviour` | **HMM** → HMM states, HMM transitions, HMM (states + transitions) · **Motifs** → Motif discovery |
| `opticalFlow`, `clustPops`, `clustTracks`, `clustRegions`, `testTasks` | no group — ≤2 functions, renders flat |

Hidden tasks (`copyImage`, `importImages.remove`) get a group anyway so the chain palette places them.

## Phases

- **P1 — mechanism + data** (this branch): `group` on `TaskDef`; `utils/taskGroups.ts` + test;
  `TaskRunner` optgroups; chain palette sub-headings; MCP passthrough; `group` on every spec above;
  `docs/MODULES.md` field section; inventory line.
- **P2 — promote**: once merged, `docs/MODULES.md` is the permanent home; delete this plan.
