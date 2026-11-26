> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.
>
> **Outcome (2026-11-26):** not run. Read against `origin/main` (`e6028b0f`), the verdict was not to
> act on it as written:
>
> 1. **Wrong population for demand.** image.sc and the napari/QuPath/Fiji trackers skew to 2D
>    generalist users (the brief says so itself). Cecelia's users are intravital/confocal immunology
>    labs, whose asks already arrive directly (the old-R lab's four asks, `report_issue`, live sessions).
> 2. **Phase 4 is costly at this size.** The app and viewer can be driven headless (`scripts/docshots/`,
>    `scripts/gui_smoke.mjs`), but twelve driven tasks on top of ~300 signals spend the seat quota
>    before a pilot has shown the method finds anything.
> 3. **The recall check can't pass.** The named control (blebbing dying cells → Find ROIs) came from
>    the maintainer's own data, not any forum thread, so a miss would say nothing about the method.
> 4. **Overlap.** The "met elsewhere" diff already exists tool by tool (~54 audits in `docs/archive/`:
>    Scope2Screen, SPACEc, TACIT, Tribus, BEHAV3D, radiatR, Galaxy, …).
> 5. **Two unsafe steps.** Caching fetched third-party pages under `docs/todo/` commits them to the
>    repo, and "delete candidates no signal asks for" treats missing forum demand as evidence.
>
> **Pilot (2026-11-26), PhenoCycler only:** 13 open-access papers (image.sc refused automated
> fetches; 15 search snippets only) → 10 needs; 2 driven headless on a copy of set xxHqjQ (~$5.3).
> Recall: the autofluorescence need surfaced independently (shipped as `segment.afCells`). The
> driving, not the mining, produced the findings: the MCP has no cluster-population stale flag (the
> GUI computes it in `utils/clusterMeasure.ts`), and artefact QC (folds, blur, debris) has no
> measure, so the agent did it by eye. Pilot design flaw: a processed copy hands the agent the
> maintainer's own answers.
>
> **Parked (2026-11-27).** The aim was agents suggesting new features; the pilot found bugs instead,
> the same result as the abandoned overnight agent runs. The bugs were fixed in the PR that recorded
> this: `stale` on cluster populations in the MCP's definitions and `gate_stats`; a whole-section
> figure read every channel as float64 (66.7 GiB) to set its display range; the figure renderer's
> errors never reached the caller; a clearer empty boolean-population error.

# GAP_DISCOVERY_PROMPT

You are auditing Cecelia (this repo) to find what it is missing, without being told. Work from evidence in the field, then test each candidate gap by driving the app with its existing features. Output is a ranked, evidence-backed gap report. **Do not modify application code.** Write only under `docs/todo/gap-discovery/`.

This is an experiment. If the method turns out not to work, say so plainly in the report. That is a valid result.

## Goal

Produce a short list of real gaps, each classified as one of:

- **Discoverability/docs:** the capability exists but an agent (or user) could not find it.
- **Workflow/UX:** solvable, but awkward (many steps, hacks).
- **Method/parameter:** result is only approximate with current knobs.
- **Feature gap:** impossible with current features.

## Non-goals

- No feature implementation, no PRs, no plan for implementation. Proposals only.
- No claims without a link. No link, no claim.
- Do not assume gaps from general knowledge of other tools. Everything comes from retrieved sources.

## Phase 0: Capability map

Build `docs/todo/gap-discovery/CAPABILITY_MAP.md` from code, docs, in-app guides, and the existing `docs/todo/` ideas and audits. For each capability: name, where it lives (page/module), what it does, known limits, and the knobs exposed. Also record **which parts an agent can drive** (co-driving API, headless Chrome, CLI) and which it cannot (e.g. canvas drawing, ROI sketching). Read existing `docs/todo/*PROMPT.md` and idea files so you do not re-propose ideas already planned or rejected; list them as "already tracked."

## Phase 1: Demand mining

Sources (public only, respect rate limits and ToS):

- **forum.image.sc:** use the Discourse JSON API (`/search.json`, `/t/<id>.json`, `/tag/<tag>.json`). Focus tags and topics: tracking, intravital, 4D, 3D segmentation, napari, QuPath, TrackMate, Fiji, ilastik, cellpose, stardist, multiplex/spatial, plus anything related to immune cells and cell migration.
- **GitHub issues/discussions** of napari, QuPath, TrackMate, Fiji, ilastik, CellProfiler, Imaris-adjacent open tools, and related repos you discover.
- **Papers:** methods and limitations sections of recent intravital, spatial/multiplex, and cell-tracking papers; bioRxiv and PMC full text where available.
- **Talks/preprints/docs** only if transcripts are accessible as text.
- Time window: last 24 months unless a classic thread is highly relevant.

Extract signals such as:

- "is there a tool that...", "how do I...", unanswered or unresolved threads
- "I wrote a custom script to...", "we manually annotated/curated..."
- Limitations stated by authors, workarounds, reviewer-style complaints

For every signal store: quote (under 15 words, otherwise paraphrase), URL, date, author type if visible (user, developer, reviewer), and tags. Save raw signals to `docs/todo/gap-discovery/SIGNALS.jsonl`.

## Phase 2: Cluster and diff

1. Cluster signals into needs (one sentence each).
2. For each need, check against `CAPABILITY_MAP.md` and against other tools: **unmet everywhere / met elsewhere not in Cecelia / already covered in Cecelia**.
3. Drop needs already covered or already tracked, but list them in an appendix with the reason.
4. Tag needs for relevance to Cecelia's users: intravital immunology, confocal, multiplex/spatial. Down-weight generic 2D needs unless they clearly apply.

## Phase 3: Verification pass

Spawn a separate verifying pass (different prompt or subagent) that re-opens every cited URL and confirms the quote and the claim. Remove any need whose evidence does not hold. Record what was removed and why.

## Phase 4: Drive the app (the delta test)

For the top 8 to 12 surviving needs, write a task each:

- Goal in biological terms, a dataset to use (prefer existing benchmark/example data in the repo; list which), and a rubric for a good answer.
- Attempt it **using only existing features**. Log every step, workaround, dead end, and the point where you gave up.
- If it fails, retry once with a hint from `docs`/guides. Count only failures that persist as deltas.
- Classify the outcome (see Goal). Note any step blocked by agent-interface limits and mark it "interface-limited", not "impossible."

Include **2 or 3 control tasks** from gaps already known to the maintainer, for example missed blebbing dying-cell events (led to Find ROIs) and the items in recent `docs/todo/` ideas. Report whether the method independently surfaced them. This is the recall check.

Keep a cost log: tasks attempted, steps, and rough effort per task.

## Phase 5: Report

Write `docs/todo/gap-discovery/GAP_DISCOVERY_REPORT.md`:

1. **Verdict on the method** (3 to 5 lines): did it surface real, new gaps? Recall on control tasks? Cost? Would you run it again?
2. **Ranked gaps** (top 10 max). For each: need, classification, evidence links with dates and counts, what the agent tried and where it stopped, which similar tools solve it and how (links, licenses), rough scope (S/M/L), and a one-line success metric a rerun of the same task could check.
3. **Already covered / already tracked** appendix.
4. **Removed after verification** appendix.
5. **Method failures:** selection bias, hallucination catches, interface limits, anything that made results unreliable.
6. **Delete candidates:** things in Cecelia that no signal asks for and that could be cut or simplified. Include only if you have evidence.

## Rules

- Every claim cites a URL. Quotes under 15 words. Paraphrase otherwise.
- Do not treat frequency as importance. Say who is asking and whether they match Cecelia's users.
- Forum posters skew toward 2D, napari, and Fiji users. State this bias where it matters.
- Do not invent scientific validity. For any proposed analysis, flag whether its statistical or biological validity needs a domain reviewer.
- If a source is inaccessible, say so and move on. Do not fill the gap from memory.
- Respect rate limits. Cache fetched pages under `docs/todo/gap-discovery/cache/`.
- Stop when the report is written, or if two consecutive phases produce nothing usable. Then write the verdict explaining why.

## Budget

Keep it bounded: at most about 300 signals read in depth, 12 driven tasks, one verification pass. If you hit a limit, stop and report what you have.
