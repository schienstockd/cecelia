> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run.
>
> **Outcome:** task 1 (experimental design) became [`../todo/EXPERIMENT_DESIGN_PLAN.md`](../todo/EXPERIMENT_DESIGN_PLAN.md) (2026-11-21), written for every technology, not intravital only. Tasks 2–8 are not run; most were already audited in [`../todo/CECELIA_STRUCTURE_PLAN.md`](../todo/CECELIA_STRUCTURE_PLAN.md) and [`../todo/IMAGE_ANALYSIS_RECITAL_PLAN.md`](../todo/IMAGE_ANALYSIS_RECITAL_PLAN.md), which this brief does not cite: start from those. Premises found wrong: the worked set (lfGUDg) is one set of 16 movies from 8 mice, not 6–7 movies from 2 mice; panel is a group attribute (`Test`), panels are in different mice, and each panel was imaged on its own days; drift was removed from the acquisition check on 2026-11-11.

# NATURE_PROTOCOLS_VALIDATION_PROMPT

Audit Cecelia Feijoa (Julia + Python + Vue) and design the validation for a Nature Protocols paper. Read the actual code. Where the README, docs or this prompt disagree with the code, the code wins.

## Goal

Show that Cecelia is a structured platform, not a box of tools. Two parts:

1. **Experimental design as a first-class object** (task 1). Like GraphPad Prism: the user declares the layout (groups, replicates, parents), then adds the data. Plots, stats and checks read the design.
   The structural principle: no image enters Cecelia as a loose file in a toolbox. Every image belongs to an experimental set that is either **designed** or **explicitly declared exploratory** from the start. Designed sets flag reservations about the claim the researcher wants to make, along the way.
2. **Evidence the structure is enforced and detectable** (tasks 2 to 8): invariants, bypass audit, planted errors, replay, handoff.

## Constraints

- The design system is in scope and is a core deliverable. Other new features are out of scope unless needed to make a claim true. Prefer the smallest change, and say what you leave out.
- Cite file and line. Mark every statement VERIFIED (read in code or run) or INFERRED. INFERRED statements cannot support a paper claim.
- If a claim in the README is false or weaker than stated, say so and give replacement wording.
- Checks warn, never block.

## Worked dataset

- Intravital: unpublished naive T cells in lymph node, 6 to 7 movies from 2 mice, 3 T cell types per movie, two acquisition panels.
- Groups = the two panels. Hypothesis: no difference in movement (null).
- Unit = movie (6 to 7 points), nested in mouse (n = 2). Accepted for a baseline comparison, but the nesting must be declared and shown.
- Panels differ in acquisition by design, so "treated the same?" should surface that. The protocol frames it as the check showing what differs, never as equivalence.
- Unknown: how panels are spread across the 2 mice. Find out whether project metadata can hold mouse, panel and movie.
- PhenoCycler: public data for a second example. Out of scope here except where it changes the invariants.

## Tasks

### 1. Experimental design system

**Audit first.** What attributes exist (mouse, panel, movie, condition, date, batch)? How are they set? Can they express a hierarchy (mouse > movie > track > timepoint)? Where does the stats path get its unit and n (declared, inferred, defaulted)? What does the claim check read? What is an "experimental set" in the code today (the unit images are grouped into for one comparison)? Is there an existing object a design could live in?

**Specify.**
- **Roles over attributes.** Grouping stays stored as image attributes. The design records which attribute is the group, the unit (replicate), the parents (what units nest in), and the subunits (track, cell, timepoint; never the n). **One design per experimental set**, not per project; a project can hold several sets, each with its own design. Say if this cannot be done without new storage.
- **Setup dialog, two paths, one record.** Design first: declare the layout, then images are assigned to design cells at import, or flagged unassigned. Images first: add images, and the design is generated from the imaged sets (attributes, metadata, names) and shown for confirmation. A generated design is INFERRED until confirmed. Confirmation shows the grid and n per cell, and the record says "inferred, then confirmed".
- **Two explicit modes per set.** *Designed* (the default) and *exploratory*. Exploratory is for test sets with no upfront structure, and the user chooses it explicitly when creating the set. A set with images that is neither designed nor declared exploratory is the "no design" state: say so where the user works ("You don't have a design"), and offer to generate one or declare the set exploratory. Exploratory sets are labelled as such in provenance and figures, and claim checks say they cannot support a confirmatory comparison. A set can be promoted from exploratory to designed later, and the record keeps that history.
- **Reservations along the way.** In a designed set, flag critical points as work progresses, not only at the stats step: sample size per group (vs plan and vs what a test needs), acquisition consistency across groups (settings, calibration, panel, saturation, drift, date), processing consistency across groups, tracking and count sanity, and confounds from the grid. Each flag says what it threatens for the claim. Specify a per-set "claim readiness" view that collects open reservations in one place, so the researcher sees what the data can and cannot support before making the claim. Audit which of these flags already exist, at which step, and which must be added. Flags warn, never block.
- **Grid.** Group x parent with n units per cell: empty cells, imbalance, unassigned images, a group present in only one parent. If the design came first, compare planned vs actual counts.
- **Prefill.** Comparison plots and stats default grouping and unit from the design. Show the active design (group, unit, n, parents) in the plot or stats header. Say which entry points already take a grouping and which need changing.
- **Overrides.** Never blocked (e.g. grouping by date to look for batch effects). When a comparison's grouping differs from the design, provenance records it ("grouped by X, design group is Y") and the claim check reads it. Define when an override is exploration vs a comparison.
- **Design history.** Like gate history: a design change after a stats run keeps earlier runs under the design they used, and a stale-design warning appears when a figure no longer matches.
- **Blackboard project description.** The blackboard already has a project description entry. Decide, with evidence from the code and how it is used (by Kiwi, checks, exports, docs), whether it should be integrated into the design entry, superseded by it, or kept separate. Cover: what fields it holds and who reads them; what overlaps with the design (groups, conditions, animals, acquisition) and what does not (aims, hypothesis, free text); whether the design should live on the blackboard or elsewhere; whether the description becomes per-set or stays per-project; and the migration for existing projects. Recommend one option and give the cost of the others.
- **Design is frozen into the stats run** and carried into the exported figure.

**Required warnings:**
- test run on a subunit (pooled tracks or cells as n)
- n per group too small for a test; units nested in too few parents
- group confounded with parent (a group in one parent only)
- unit not declared; no design and no exploratory declaration; unconfirmed inferred design; exploratory set used for a confirmatory-style comparison
- For 6 to 7 movies from 2 mice the output must say movie-level, 2 mice, descriptive or exploratory, never confirmatory
- Parent-level view: per-parent means beside unit-level points (movie points coloured by mouse). Say if this falls out of current plotting or needs work.

Report current behaviour per item (VERIFIED or INFERRED), the gap, and the smallest change. List what you propose to build, in order.

### 2. Invariants and bypass audit

Write 6 to 10 falsifiable invariants that define "structure" (e.g. every exported figure resolves to run records, fingerprints and parameters; no task changes data outside the scheduler; replay reproduces recorded settings; everything works with AI off; every image belongs to a set that is designed or explicitly exploratory). For each: how code enforces it and how a test would break it.

For each invariant list every route that could violate it, and try it: napari edits, manual ROIs, direct file writes, moved or edited raw files, custom modules and plugins, Kiwi / Claude Code actions, chains and replay, anything writing to project storage unrecorded. Table: record written (y/n), who or what is logged (including AI-initiated steps), what the fingerprint covers (raw, converted zarr, path and metadata), and what happens on move or change. Gaps are findings.

### 3. Claim checks

Which checks exist (saturation, calibration, drift, counts and tracks, gates, statistics, claim checks) and what each reads from the record. What "treated the same" can and cannot establish (parameters and versions yes; batch, date, biology no). How design from task 1 feeds them.

### 4. Planted-error benchmark

A small versioned set of deliberate errors on copies of the worked dataset. For each: which check should fire, whether it does, what it says. Report detections, misses and false alarms. Run the same errors through a plain scripts-and-notebooks version as the contrast, and say what that contrast can and cannot show. Include errors we did not design checks for.

- mismatched segmentation or tracking parameters between panels
- tracks or cells pooled as the unit
- no design and no exploratory declaration; unit undeclared; inferred design used unconfirmed
- exploratory set presented as a confirmatory result
- acquisition settings differing between groups with no flag raised at import or later
- too few units per group with no flag before the stats step
- group present in only one mouse
- movies from one mouse counted as independent parents
- comparison on a grouping that differs from the design; design changed after a stats run
- changed calibration on one movie; saturation or drift
- raw file moved or edited after import; gate changed after the fact
- replay with a changed model or library version

### 5. Replay vs re-run

Same movies and recorded settings, run twice and replayed. What is identical (parameters, record) and what can differ (outputs: GPU nondeterminism, seeds, model and library versions). Give exact wording for what "exact settings" guarantees.

### 6. Handoff test

Someone who did not run the analysis answers from the record alone: what was done, with which parameters and versions, what was compared, whether treatment was the same, what the design was. List what the record must contain for each answer and which answers it cannot yet support.

### 7. Coverage table and schematic

Table: for each step (acquire, import, segment, track, populations/gating, analyse, export): recorded, checked, not covered. Schematic brief: the design and the scheduler-plus-record as one layer every step passes through, tools attached to it.

### 8. Protocol reproducibility

Is the install pinned (tagged release, lockfile)? What downloads on first launch (Cellpose checkpoints, bioformats2raw), and are versions and hashes recorded? Runtime and resources for the worked example. What a cold install on a clean machine hits.

## Output

Write `docs/todo/NATURE_PROTOCOLS_VALIDATION_PLAN.md`:

1. Verdict per invariant: holds / holds with caveats / does not hold
2. Design system: audit, spec, blackboard project description decision (integrate / supersede / keep separate) with migration, build list in order
3. Invariants and bypass table
4. Claim-check findings
5. Benchmark spec and any results
6. Replay vs re-run findings and wording
7. Handoff test spec
8. Coverage table and schematic brief
9. README claims to soften, with replacement wording
10. Fixes before submission, ranked by threat to a claim
11. Open decisions for the maintainer

Be blunt. A protocol that states what it guarantees and what it does not is the goal.
