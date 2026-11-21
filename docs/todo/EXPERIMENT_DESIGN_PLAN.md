# Experiment design — plan

**Status:** planning (2026-11-21). Nothing built. Task 1 of the archived brief
[`../archive/NATURE_PROTOCOLS_VALIDATION_PROMPT.md`](../archive/NATURE_PROTOCOLS_VALIDATION_PROMPT.md),
drawn out as its own plan. The brief's validation tasks (2–8) come after this, as a short update on top of
[`CECELIA_STRUCTURE_PLAN.md`](CECELIA_STRUCTURE_PLAN.md). They are not a fresh audit.

## Why

Today Cecelia's structure is a record of what happened. One scheduler logs every run, raw files are
fingerprinted, and gates keep their history. Checks then read that record after the fact. But nothing
states what the experiment was for: the question, which images are compared, and what counts as a
replicate. Without that, the framework is a toolbox. Images go in, and the structure gets reconstructed
at the stats step, or never.

A design turns that around. **Each set starts with a design**: the question, the groups, and the
replicate. The images are then added into that design. Or the user says up front that the set is
exploratory. Plots, statistics and checks read the design. When the data drift from it, they warn.
The user can change the design at any time, and the record keeps every version.

This is the same idea as GraphPad Prism, where you declare the table layout and then enter the data.

## Not one technology

The design works the same way for every kind of data, in line with the microscope-profile rule (no
technology is the default). It names *attributes* and *roles*. It never names a modality. Three sets
show the range:

| | Intravital 2P — RFtn9f / lfGUDg | PhenoCycler — mjphFQ / xxHqjQ | Fixed confocal (typical) |
|---|---|---|---|
| Images | 16 movies from 8 mice over 4 days (file name `M1-2` = mouse 1, position 2; date = the original folder) | 8 tissue cores cut from one scan | fields of view from tissue sections |
| Attributes today | `Test` = A/B (the panel) only | none | — |
| Question | do naive P14 / OT-I / gBT move differently between the two label panels? (null expected) | does cell-type composition differ between tissues? | does treatment change infiltration? |
| Group | Panel | Tissue | Treatment |
| Unit (replicate) | Movie | Core (or Donor, if cores share a donor) | Mouse |
| Parent | Date × Mouse (mouse numbers restart each day) | Donor / Slide | — |
| Below the unit (never the n) | track, cell, timepoint | cell, region | field of view, cell |
| Acquisition | differs between panels by design; each panel imaged on its own days | one scan: one acquisition batch for all cores | per-section sessions |

What the grid shows for lfGUDg, read from the original paths (2026-11-21):

| Panel | Date | Mice | Movies |
|---|---|---|---|
| A (P14-ubTomato) | 2021-11-25 | 3 | 5 |
| A | 2021-11-28 | 2 | 4 |
| B (P14-CTDR) | 2021-12-01 | 2 | 5 — three of them are mouse 2, position 1 |
| B | 2021-12-02 | 1 | 2 |

Panels were imaged in different mice, so mouse nests in panel: 5 mice against 3. Panel is also fully
confounded with acquisition day (A in November, B in December). A difference between panels cannot be
told apart from a day effect. That is exactly the reservation the grid has to raise, whatever the
technology. Three panel-B movies are all named mouse 2, position 1, but their run suffixes differ
(`_0001`, `_0003`, `_0005`). The maintainer sometimes forgot to rename between positions, so these may be
three positions under one stale name. A name is evidence, not truth: a design suggested from names stays
*inferred* until the user confirms it, and the run suffix is shown beside the name so a stale name can be
caught at confirmation.

Two things to note:
- **The unit can sit above the image.** With several fields per mouse, the image is below the unit
  and the n is the mouse count. With one movie per site, the image *is* the unit. The design must allow
  both. Today stats only offer `individual` or `image` as the summary level.
- **Neither example set can be checked yet.** Mouse, panel, donor and tissue have not been entered as
  attributes. Generating a design from the images (below) has to start from exactly this state: names,
  channel lists and metadata, with nothing annotated.

## What exists today (read at 3897df8a7)

| Piece | Where | What it gives the design |
|---|---|---|
| Image attributes, per set | `img.attr`; MCP `get_image_attributes` | The grouping values. The design records *roles* over these attributes and stores no grouping of its own |
| Set metadata | `CciaSet.meta`, `app/src/model/set.jl:3` (already holds `recipe`, `app/src/model/project.jl:255`) | A place to store the design without a new store |
| Plot grouping | `groupAttr` per summary slot, `frontend/src/components/canvas/SummaryPanel.vue:66` | Grouping is already chosen per plot by hand, and is the default the design can fill in |
| Summary level | `statUnit` `individual`/`image`, `frontend/src/utils/statUnitState.ts`; default `individual` | Only two levels. No level above the image. The default counts cells or tracks as the n |
| Claim scope | `ClaimScope.unit_attrs`, `app/src/claim_checks.jl:24`; S2 rules `few_units`, `no_unit`, `images_per_unit` | A unit taken from the claim. **No group attribute**, so a group confounded with a parent cannot be checked |
| Acquisition check | `app/src/acq_check.jl`; plan stored as a Blackboard entry (`acqCheck` meta) | Already holds a per-set "plan" sentence. Groups movies into label designs *inferred* from channel lists |
| Blackboard profile | pinned entry `profile`, `frontend/src/utils/blackboardFilters.ts:18` | Per project: what the project is. Free text |

## Decisions (proposed)

1. **One design per set.** A project holds several sets, each with its own design.
2. **Roles, not new grouping.** Grouping stays as image attributes. The design records which attribute
   is the **group**, the **unit**, and the **parents** the units nest in. Anything below the unit is
   never the n.
3. **Two modes, chosen explicitly:**
   - *designed* is the default;
   - *exploratory* is for a test set with no structure yet.

   A set with images that is neither is in the **no-design** state. The page says so ("No design for
   this set") and offers to generate one or to declare the set exploratory. An exploratory set can be
   promoted to designed later, and the history keeps the step.
4. **The question is part of the design.** One sentence, plus the expectation: not sure, no
   difference, or higher / lower in one group (a null is a valid design, and differs from not sure). The acquisition check's plan sentence reads it instead of keeping its own.
5. **The design is set up once the metadata is in, from the Design check** (the maintainer, 2026-11-21;
   replaces "the dialog opens after register"). A design is built from attributes, and right after
   register most sets have none yet: names and original paths, nothing else. So no dialog pops up when
   files land. The entry is the **Design check** in the Checks cluster (beside Acquisition and QC, on
   every page with an image table, the Metadata page included): amber while the set has no design or
   an unconfirmed one ("Add the metadata, then set up the design"), and amber with a count when images
   sit outside a confirmed design — added after it was set up, or missing a design attribute. What the
   dialog showed at confirm (confounding, few replicates) is not raised again. A first version put a
   section on the Metadata page; it was dropped because a design set up once there is forgotten when
   the set grows (the maintainer, 2026-11-21). The dialog still suggests from
   attributes, file names and original paths, and a suggested design stays *inferred* until confirmed.
   - *Set already designed, files added later*: a short "place these images" step (later PR). Each
     image goes into a grid cell or Unassigned, and the grid's reservations update.

   After import, channel lists and acquisition settings arrive. They never change a confirmed design.
   Where they disagree with it (a "panel A" movie with panel B's channel list), that is a reservation.
6. **Warn, never block.** No step refuses to run. A design differing from the data is a reservation,
   not an error.
7. **A claim is taken from the plot, not typed.** The claim check already takes a structured scope
   (set, populations, measure, unit attributes; `ClaimScope`, `app/src/claim_checks.jl:24`) started
   from the plot. The design adds the group. A sentence is only the claim's label, so the check never
   has to parse wording. One question per set; several claims against it are allowed. The readiness
   view counts the comparisons checked on the set and shows them. It does not correct alpha (the
   claim-recital decision: show rounds, don't charge them).
8. **Overrides are free and recorded.** Grouping by date to look for a batch effect is allowed. The
   plot and its export record "grouped by Date; design group is Panel", and the claim check reads that.
   An override is *exploration* until it is the grouping a claim rests on.
9. **The design is versioned like gate history.** A stats run and an exported figure freeze the
   design version they used. A later change marks them stale, and earlier runs keep their design.
10. **Blackboard profile stays separate.** It is per project and holds aims and free text. The design
   is per set and is data that plots and checks read. They only overlap in the question, and the
   question moves to the design. No migration needed: existing projects keep their profile, and their
   sets start in the no-design state.

## The design record

The current design is stored on the set (`set.meta["design"]`, written through `commit_state!` like `recipe`).
Its history uses the gate-history machinery, not a new versions list. Each change is appended to a
`*.history.jsonl` beside the set, written with `append_jsonl!` and read with `read_jsonl` (as in
`app/src/gating/popmanager/history.jl`), and keyed by a content hash. That is what `gate_file_edits`
(`app/src/analysis_export_provenance.jl:52`) already uses to mark an export stale, so step 5 reuses it.
Gate history belongs to an image and value name, and this one belongs to a set, so the helper may
need a set-level path. That fit has not been checked yet.

```
question     one sentence + expectation (unsure | same | higher | lower) + expectIn (the group value)
mode         designed | exploratory
status       inferred | confirmed
group        attribute(s)            e.g. ["Panel"]
unit         attribute(s)            e.g. ["Movie"]   ([] = the image is the unit)
parents      attribute(s), outermost first   e.g. ["Mouse"]
planned      optional n per group (and per parent)
(history)    one jsonl row per change: {hash, at, by (author stamp), change, mode, status}
```

The grid (group × parent, n units per cell) is computed from attributes. It is never stored.
`mode` and `status` each take a fixed set of values, so they use the `@enum` pattern (as in `TaskStatus`).

**Naming.** "design" already means *label design* in `app/src/acq_check.jl` (`acq_design_key`,
`acq_suggest_designs`): movies that share a channel list. In code, this record is the *experiment
design* (`experiment_design`), so the two never share a name.

## Reservations along the way

In a designed set, a reservation appears at the step where it arises, not only at the stats step.
Each one says what it threatens for the question. All of them gather in one **readiness view** per
set: what the data can and cannot support yet.

| Reservation | Raised at | Exists today? |
|---|---|---|
| Unassigned images; empty or unbalanced grid cells; planned vs actual n | import, attribute edit | no — new, from the grid |
| Group in only one parent (confounded) | import, attribute edit | built in the grid (`confounded`): an outer parent or other attribute whose every value falls in one group; the innermost parent (the replicate) is nested by design and left out |
| Paired / blocked design: every identity at a parent level holds every group (each mouse both conditions, each day every group) | design confirm | built in the grid (`paired`), info line: linked replicates need a paired test or a two-way ANOVA with the replicate as a factor; testing them as independent is too conservative (Lord et al. 2020, doi:10.1083/jcb.202001064; Zweifach 2024, doi:10.1083/jcb.202401074). The claim check's S2 already decides paired vs unpaired for populations sharing units (`app/src/claim_checks.jl`, `ev["design"]`), with a looser rule: any overlap is paired, n = the shared units. Step 3 makes the two one rule |
| Too few units per group for a test; too few parents | import, then stats | partly — S2 `few_units` at claim time only |
| Stats on cells or tracks as the n (below the unit) | plot / stats | partly — `statUnit` exists, defaults to `individual` |
| Acquisition differs between groups | import | partly — acquisition check, but against inferred label designs, not the declared group |
| Processing differs between groups (parameters, versions, dims) | after each task | partly — S1 at claim time only |
| Calibration missing or differing | import | partly — claim S2 `uncalibrated` |
| Unit not declared; no design; inferred design unconfirmed; exploratory set used for a confirmatory claim | everywhere the design is read | no |
| Design changed after a stats run | stats, export | no |
| A confound that can't be checked: no attribute that could reveal it (no date, no parent) | design confirm | no — say what cannot be checked rather than stay silent |
| Imported metadata disagrees with the design (channel list, acquisition settings vs the declared group) | after import | no |
| Shared acquisition claimed but not in the record ("one scan") | design confirm | no — derive it from a common source file or run; otherwise word it as unverified |
| Comparisons checked on this set | claim | no — a count, not a correction |
| Cells per unit: how precise each unit's mean is | claim / stats check (it depends on the measure, so not the grid) | no. The CV of a unit's mean, SD_cells / √n / mean, as a band: about 5% unpaired, about 2.5% paired. Cells needed = ceil((SD_target / SD_cells)²), with the upper 95% CI of the SD when it is uncertain (Zweifach 2024). Also flag very unbalanced cells per unit (Lord et al. 2020: keep observations per sample similar). Zweifach assumes a whole-cell property at one time point, so the wording for track and time measures needs care. Validate against his Data S1 calculator when built |

The wording comes from the design. It does not depend on the technology. For few units nested in
few parents, the claim check must say so outright: for example "Movie-level, from 2 mice:
descriptive, not confirmatory."

## Target UX

[`experiment-design/mock.html`](experiment-design/mock.html) is the layout spec: one frame per moment,
from registering files to the claim check. A static mock: the T cell set's names, runs, folders, panels
and mice are real; plot values, parameters and history entries are placeholders.

## Where the design takes effect

A design nobody reads is a form to fill in. Today nothing outside the dialog reads it (the one
exception: an attribute rename updates it). Each place below chooses group or unit on its own; each
row is what the design fills in there, and what the user sees change. Ordered by how soon a user
notices. A build step is done when its rows are.

| # | Surface (where it chooses today) | Design fills in / checks | The user sees |
|---|---|---|---|
| T1 | **Plot compare row**: `useSummaryData.ts` `compareMode` (default `per_image`) + `compareAttr` (default first attribute); the same row in `SummaryCanvas.vue` and `LayoutCanvas.vue`; `trackGroups.ts`; `image_metrics.jl` (region, proximity, spatial); UMAP `facetBy`/`colourBy` attribute (`ClusterMapView.vue`) | `by_attr` on the design's group, ordered as declared; facet/colour by group | A new plot is already grouped by Panel, not per image or by the first attribute |
| T2 | **Plot stats unit**: "Datapoint" `individual` (`statUnitState.ts`, `plotting_api.jl:245`); collapse goes to the image at most (`plot_data.jl:669,731`); `stats_qc_findings` assumes the image is the replicate | Datapoint = the replicate (Mouse): a new collapse level above the image, per parent; SuperPlot dots coloured by replicate; the QC findings count replicates | The amber "Test counts each cell or track as a replicate" lines go; n reads "5 mice" |
| T3 | **Claim check**: replicate picker default by name regex (`claimApi.ts` `defaultUnitAttr`); compared groups are always populations (`claimScopeOf`); S1b/S2 infer pairing and confounding themselves | Replicate from the design; attribute groups as the compared groups; S1b/S2 read `confounded`/`paired` from the grid; new: unconfirmed design, exploratory set, override | The picker is filled in; the claim check says what the dialog said |
| T4 | **Kiwi / MCP**: `guidance.py` tells the model to size groups itself and pass `unit_attrs`/`compare_by`; `add_analysis_board(compare_by="")`; board summaries and `kiwi_refs` omit the compare attribute | `get_experiment_design` first (#2416); `compare_by`, `unit_attrs`, `statUnit` default to the design; summaries carry group + unit | Kiwi's boards and claims match the design without being told |
| T5 | **Acquisition plan**: `AcqPlanForm.vue` free-text "Expected"; `planSentence` | The design's question and expectation (Decision 4); the check compares against the declared group | One question, typed once |
| T6 | **Export**: stats CSV (`plotStatsToCsv`) has no unit or group attribute; PDF/SVG provenance (`boardProvenance.ts`) carries neither; captions blank | Group, replicate, n of each, design version; the caption (step 5) | A figure says how it was computed |
| T7 | **Tables**: image table and Movies show attribute columns, no group counts | Images outside the grid marked (Unassigned) | The Design check counts images outside the design (built); per-row marks in the table are still open |
| T8 | **Notebooks**: `CeceliaNb` pools cells, never reads `img.attr` | A per-replicate summary helper | Low — notebook users choose their own unit |

T1 and T2 are built (branch `feat/experiment-design-dialog`, uncommitted): a canvas on the untouched
compare default switches once to the design's group, a bullseye in the compare row says so, and a
panel with no saved Datapoint reads as the replicate ("mouse"). Open in T2: the SuperPlot layer (cell
dots under the replicate means), and the per-image metric plots (region, proximity, spatial) still
take the image as the unit. Kiwi boards with no `compare_by` / `statUnit` get both through the same
defaults; board specs may name `statUnit: "replicate"`. The compare row's bullseye shows whenever the
set has a design: plain when the row follows it, amber when it differs (one click switches back).

T5 is built too: the question and expectation live in the design only. The acquisition plan form
asks them through the same fields as the dialog (`DesignQuestionFields`) and saves them into the
design (a set with none gets an inferred design holding just the question); the plan sentence and the
model review read them from there. A plan's old `expectation` text is read only when the design says
nothing.

Pairing: for intravital, each mouse is imaged in one group only (transferred with one panel's
cells), so a replicate never spans groups and an unpaired test is right. `paired` matters only for
designs where one subject appears in several groups.

## Road forward (the maintainer, 2026-11-21)

The Checks row is the workflow's status surface. The design adds one check to it and otherwise works
quietly through the plots and the claim check. No second place says the same thing: the mock's
readiness panel (frame 4), the design pill and question line in the set bar are **not built** — the
Design check and the Acquisition check cover them. In order:

1. Merge the record (#2415), MCP tools (#2416) and dialog + plots (#2417).
2. **T3 — the claim check reads the design** (build step 3).
3. **Plot n + replicate dots**: the n drawn ("5 vs 3 mice") in the plot's footer — built
   (`unitCountLabel`), and the SuperPlot layer — built: with the replicate as the unit, the box and
   strip charts draw each replicate's mean over its cells, both in the replicate's colour
   (`plots/superplot.ts`, `_superplot!`).
4. **Freeze and stale (T6)** (build step 5).

Parked: the design-first planned grid (frame 2b; only for a design set up before imaging), a separate
"place these images" step (frame 3b; the Design check counts them and opens the dialog), per-row marks
in the image table (T7), a notebook helper (T8).

## Build order

1. **Record + dialog.** The design record and versions; the dialog behind the Design check (suggest,
   confirm); the grid; the
   no-design state; the exploratory declaration. Suggesting a design from names, channel lists and
   metadata comes with it, because neither example set has attributes yet. The suggestion reuses the
   Metadata panel's extraction (`frontend/src/utils/regexBuilder.ts`: file name or original path,
   split-and-take and look-around builders). It does not add a second parser.
2. **Plots and stats read the design (T1, T2; T4's board defaults with them).** Grouping and unit default to the design. The plot header
   shows group, unit, n and parents. The summary plot is a SuperPlot (Lord et al. 2020,
   doi:10.1083/jcb.202001064): the cell or track dots underneath, coloured by unit or parent, the unit
   means on top, and the test run on the means. Unit dots alone would hide the spread between cells.
   Once a set has a design, its unit is the default `statUnit` (today's default `individual` counts
   cells as the n; both papers treat cells as not independent replicates and the sample mean as the
   n). Overrides recorded.
3. **Claim check reads the design (T3, T4's claim defaults).** Unit and group come from the design. New checks: group confounded
   with parent, too few parents, unconfirmed design, exploratory set used for a claim, override noted.
   One rule each, shared with the grid: paired vs unpaired (S2's `ev["design"]` and the grid's
   `paired` disagree on a partial overlap today), and confounded (S1's `claim.confounded` over record
   variables and the grid's `confounded` over design attributes — one finding code, two subjects).
4. **Reservations as work progresses (T5).** No readiness view: the Checks row is it. The acquisition check compares against
   the declared group. Processing consistency (S1) runs after each task, not only at claim time.
5. **Freeze and stale (T6).** Stats runs and exported figures carry the design version and are marked
   stale when the design changes. An exported figure's caption carries the n of units and parents and
   how the means and the test were computed (Lord et al. 2020). Open: whether summary error bars are
   the SEM across units or the SD across cells — not checked.
6. **Two worked examples, different technologies.** lfGUDg (intravital) and xxHqjQ (PhenoCycler),
   designed from their current state. Then the Nature Protocols validation as an update to
   `CECELIA_STRUCTURE_PLAN.md`.

## Out of scope

- New statistics (mixed models, power calculation). The design states the n. It does not compute power.
  The cells-per-unit band (reservations) sits just inside this line: it says how precise each unit's
  mean is from the cells already measured, and does not plan a sample size for a test.
- Designs that span sets. A comparison lives inside one set, and a set can hold every group.
- Blocking anything.

## README

The README claims claim checks ask "whether … the statistical unit is right". Today a plot's summary
level defaults to cells or tracks, and the claim check only knows a unit when the claim names one.
Step 2 makes the claim true. Proposed opening, ahead of the scheduler sentence:

> Every set starts with a design: the question, the groups, and what counts as a replicate. Images are
> added into that design, or the set is declared exploratory. Plots, statistics and checks read the
> design and warn when the data drift from it.

## Open decisions

1. **Where the design lives.** Proposed: `set.meta`, with history in the gate-history style (above),
   because plots and checks read it as data and it travels with the set. The alternative is a Blackboard entry with rounds, like the acquisition plan
   and claims. That would be consistent with those, but plots would then depend on a notes store.
   Whether project export carries both stores is not yet checked.
2. **Summary levels.** Should stats offer any attribute as the unit, or only the design's unit and
   parents?
3. **New sets.** Should the dialog open on every new set, or only when the first image arrives?
4. **The example sets' attributes.** lfGUDg: answered. `Test` is the panel, and date and mouse come
   from the original path and file name. Position is not reliable from the name (see above), so the unit
   is the movie. xxHqjQ: which tissue and donor is each core?
