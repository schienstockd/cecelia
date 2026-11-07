> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.
>
> **Outcome:** not run as of archiving (2026-11-07, `origin/main` c7f0af6d); no
> `docs/todo/CECELIA_STRUCTURE_AUDIT.md` exists. Read against main, these premises had moved:
>
> 1. **"Seg/track params stored only as flat last-run values."** True of the P0 project's July runs,
>    not of the code now. `meta["funParamsByName"]` banks params per output name, and
>    `run_log_params_for_output` (`app/src/run_log.jl`) answers older names from `runlog.json`
>    (`docs/OBJECTMODEL.md` → *`funParamsByName`*). The remaining break is history: runs made before
>    the run log existed.
> 2. **"Motion dims chosen without being logged."** They were logged, without saying which
>    population got which (`IMAGE_ANALYSIS_RECITAL_PLAN.md` → *P0 results*). Since #1958 the log line
>    names the output (`track_measures.jl`) and `motionDims` is a cohort QC metric (`qc_cohort.jl`)
>    that flags a set mixing 2D and 3D. A B-vs-T claim within one image still has no check.
> 3. **"Use `docs/todo/AI_SURFACES_AUDIT.md` if it exists."** It does not: its prompt was archived
>    unrun the same day (`AI_SURFACES_AUDIT_PROMPT.md`).
> 4. **Maturity tiers.** The scale is Coming soon · Experimental · Beta · Tested · Stable
>    (`docs/todo/FEATURE_MATURITY_PLAN.md`), not "planned". Starting tiers are a draft
>    (`FEATURE_MATURITY_TIERS.md`), and `frontend/src/lib/guides/maturityRecords.ts` gives no
>    feature Tested (14 Beta, 9 Experimental, 1 exempt). A schematic coloured by verified tier would
>    have no Tested boxes.
> 5. **Output location.** It asked for the audit in `docs/todo/`, which holds plans. An audit record
>    belongs in `docs/archive/`; only a resulting plan goes in `docs/todo/` (with a row in its README).

# CECELIA_STRUCTURE_AUDIT_PROMPT

Audit and plan only. No code changes. Outputs: `docs/todo/CECELIA_STRUCTURE_AUDIT.md`, plus the schematic source (Mermaid or SVG) under `docs/`. Self-contained: do not assume other context. Audit the Julia + Vue version (the Nature Protocols target, "feijoa"). Touch the R/Shiny version only where needed to explain a difference.

## Why

The working headline for the protocol paper is that Cecelia is a **structured platform for image analysis**: analysis you can hand off and check afterwards (to a student, an agent, an overnight run), with the record and the checks telling you what was done and whether it holds up. If that is the punchline, it must be true, concrete and clear. AI features then stay a thin, experimental, more freeform layer on top of that structure.

This audit does three things:
1. Says what the structure actually is, in the code, and tests it against the claim.
2. Produces one clean schematic that a reader understands in a minute.
3. Lists gaps, flaws and aspects worth expanding, honestly, so the claim is made only where it holds.

## The claim, as properties to test (do not assume them)

1. **Traceability:** every result traces back to its inputs: images, parameters, versions, gates, population definitions, run history.
2. **Consistency:** what was compared was treated the same, and this can be checked.
3. **Honest record:** gaps show as gaps (not recorded / measured / inferred), not as silent defaults.
4. **QC at each stage:** acquisition, import, segmentation, tracking, cleanup, populations and gating, statistics.
5. **Replay:** a recorded analysis can be re-run on a batch, deterministically.
6. **Review:** a claim can be checked against the record by mechanical rules.
7. **Hand-off:** a second person can see what was done from the record alone.
8. **Works without AI:** every property above holds with AI features off.

## Audit method

**A. Inventory the structure.** Layers, objects and contracts: project, image set, image, population, task, output, run, gate, blackboard entry, chain; where each is stored and in what format; the pipeline stages and what each records (inputs, outputs, params); the UI surfaces over them; the API and MCP layer; the extension points (how a task or population type is added). For each: purpose, owning file or module, and maturity tier (Tested / Beta / Experimental / planned).

**B. Provenance walk (the key test).** Pick two or three real results, for example a population speed comparison on a real project, a gated population, and an exported figure. Trace each backwards to the raw images, hop by hop. Report what can be reconstructed and where the record breaks. Known breaks from the claim-review P0 to confirm or refute: segmentation and tracking params stored only as flat last-run values per task, a run log that starts after the first runs, motion dims chosen automatically per image and population without being logged, set members silently excluded. Add any others you find.

**C. Contracts and invariants.** What is enforced in code and what only by convention: units, coordinate systems, 2D vs 3D, time interval, naming, population definitions. Where can a stage silently change behaviour between images or versions?

**D. Replay and hand-off tests.** Can a recorded chain reproduce a result on the same data? On a new image? Can someone else understand what was done from the record alone? Say what you tried and what happened.

**E. Coverage map of QC and checks.** Per pipeline stage: which run-time QC, cohort QC, cleanup facts, acquisition panels (light path, data path), and claim-review rules exist, and which stages have none.

**F. Where AI sits.** List the AI surfaces (use `docs/todo/AI_SURFACES_AUDIT.md` if it exists) and which structural facility each one relies on (record, blackboard, MCP tools, QC). Show that AI sits on top of the structure and reads from it, and say what it would lose with a weaker structure. Do not propose new AI features.

## Schematic requirements

- One page, readable at paper-figure size and on a slide. Plain labels, no codenames.
- **Horizontal flow:** acquire, import, process (segment, track, clean), populations and gating, analyse and plot, export.
- **Vertical layers across the flow:** the record (provenance), QC and checks, replay (chains), and an optional AI layer on top.
- Colour or mark each element by verified maturity (Tested / Beta / Experimental / gap). Do not draw planned or aspirational elements as if they exist: show them as gaps.
- Every element maps to a verified file or module, listed in a legend table beside the schematic.
- Deliver two versions: a full one for the docs, and a simplified one for a paper figure (about ten boxes). Include a two-sentence caption for each.
- Mermaid source, plus an SVG or PNG render if feasible.

## Gaps, flaws and expansion

For each item: what is missing or wrong, which property (1-8) it breaks, evidence, severity for the headline claim, a proposed fix with size, and whether it must be fixed before the protocol or can be stated as a limitation. Group them as:

- **Gaps:** things the claim needs that do not exist.
- **Flaws:** things that exist but break a property (silent defaults, last-run-only params, inconsistent contracts).
- **Worth expanding:** structure that is strong and underused, or small additions that would make the story clearer.
- **Worth cutting or hiding:** parts that dilute the story.

## Output requirements

- A table per property (1-8): ready to claim / claim with caveat / do not claim yet, with evidence and file references.
- The inventory, the provenance walk results, the contracts and invariants findings, the replay and hand-off results, the coverage map, the AI dependency map.
- The schematics and their legends.
- The gap list, prioritised, with a recommended order of work that fits the hardening phase.
- **Punchline test:** can the headline be stated truthfully in two sentences today? Write the truthful version, and what would have to change to make the stronger version true.
- Separate verified facts (from the repo, with references) from assumptions and open questions for the maintainer.
- No speculation presented as fact. Say what you could not test.
