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
5. **The dialog opens after register, before import** (the maintainer, 2026-11-21). A new set is empty,
   so there is nothing to suggest from. Adding images already happens in two steps
   (`frontend/src/modules/ManageImagesModule.vue`, `submitRegister`):
   - *register* (`POST /api/images/register`) creates image records with name and original path, and
     converts nothing;
   - *import* (the `importImages` task) converts later.

   The dialog sits between the two, when names and original paths exist and nothing has been
   processed. One record, three moments:
   - *Empty set*: no dialog. The set bar offers "Set up a design" for anyone who wants to design first
     (declare the grid and planned n, then images land in it).
   - *First files registered into a set with no design*: the dialog opens with a suggestion (question,
     mode, layout, confirm). Cecelia suggests from attributes, file names and original paths. A
     suggested design stays *inferred* until confirmed, and the record says "inferred, then confirmed".
   - *Files registered into a designed set*: a short "place these images" step. Each image goes into a
     grid cell or Unassigned, and the grid's reservations update.

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

## Build order

1. **Record + dialog.** The design record and versions; the dialog after register (suggest,
   confirm), "Set up a design" on an empty set, and the "place these images" step; the grid; the
   no-design state; the exploratory declaration. Suggesting a design from names, channel lists and
   metadata comes with it, because neither example set has attributes yet. The suggestion reuses the
   Metadata panel's extraction (`frontend/src/utils/regexBuilder.ts`: file name or original path,
   split-and-take and look-around builders). It does not add a second parser.
2. **Plots and stats read the design.** Grouping and unit default to the design. The plot header
   shows group, unit, n and parents. The summary plot is a SuperPlot (Lord et al. 2020,
   doi:10.1083/jcb.202001064): the cell or track dots underneath, coloured by unit or parent, the unit
   means on top, and the test run on the means. Unit dots alone would hide the spread between cells.
   Once a set has a design, its unit is the default `statUnit` (today's default `individual` counts
   cells as the n; both papers treat cells as not independent replicates and the sample mean as the
   n). Overrides recorded.
3. **Claim check reads the design.** Unit and group come from the design. New checks: group confounded
   with parent, too few parents, unconfirmed design, exploratory set used for a claim, override noted.
   One rule each, shared with the grid: paired vs unpaired (S2's `ev["design"]` and the grid's
   `paired` disagree on a partial overlap today), and confounded (S1's `claim.confounded` over record
   variables and the grid's `confounded` over design attributes — one finding code, two subjects).
4. **Reservations as work progresses + the readiness view.** The acquisition check compares against
   the declared group. Processing consistency (S1) runs after each task, not only at claim time.
5. **Freeze and stale.** Stats runs and exported figures carry the design version and are marked
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
