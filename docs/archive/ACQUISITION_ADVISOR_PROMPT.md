> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.
>
> **Outcome:** not run. Archived 2026-11-04; no `docs/todo/ACQUISITION_ADVISOR_PLAN.md` exists.
> Checking it against `origin/main` (fbcfb4d8) showed premises that don't hold:
>
> 1. **The Blackboard can't hold a facility knowledge base (decision 5).** It is per-project —
>    `<proj>/blackboard/`, carried by `.ccbundle`, and `docs/ARCHITECTURE.md` → *Project memory*
>    states no store exists outside `<proj>/`. Entries carry only `open | resolved | parked` plus
>    snapshots: no provenance tiers, review queue or version tags. A per-scope manual KB is a new
>    store, not a Blackboard use.
> 2. **No project-less path.** `pixi run chat` always attaches to a project (`chat.py` docstring);
>    Milestone 1's "saved CZI/OIR file in, advice out" assumes an entry point that doesn't exist.
> 3. **No acquisition settings are parsed.** `python/cecelia/utils/ome_xml_utils.py` reads dims,
>    pixel size and time increment only — no laser power, gain, pinhole or scan speed. CZI/OIR come
>    in via Bio-Formats (`api/src/routes/image.jl`), so those fields would come from its OME-XML;
>    completeness per scope is unverified.
> 4. **"Advisor profile" collides with Cecelia's user profiles** (owners in `project.json`); call
>    it an advisor persona.

# ACQUISITION_ADVISOR_PROMPT

Plan-only task. Do not implement. Output: `docs/todo/ACQUISITION_ADVISOR_PLAN.md`.

## Goal

Evaluate and plan an **acquisition advisor** in Cecelia: Claude helps people at the microscope avoid simple, common acquisition mistakes (saturation, undersampling, wrong pinhole, bleaching, bleed-through, wrong z-range or frame rate for the question). It advises. It does not drive hardware.

Target scopes: Zeiss LSM 780 (ZEN black), Zeiss LSM 980 (ZEN blue), Evident FV4000.

## Settled design decisions (challenge only with evidence)

1. **Advisor first, no hardware control.** Hardware actuation (Model Hardware Standard, vendor APIs, macros) is an optional later tool source behind the same interface. Do not design around MHS: its spec is not public.
2. **Inputs, in order of trust:** image metadata (CZI/OIR headers) > computed image metrics > user note on sample/fluorophores/question > screenshots of settings panels (via existing shared captures).
3. **Metrics are computed by code, interpreted by Claude.** No eyeballing saturation, SNR or bleaching from pixels.
4. **Pilot analysis:** optionally run a saved test field through the real Segment/Cleanup/track steps and report whether they survive, with likely acquisition causes.
5. **Knowledge base on the blackboard:** per-scope, per-software-version entries for where each setting lives in the UI, what it does, and typical ranges, built by preprocessing the facility's manuals. Entries carry provenance (vendor-manual unverified / facility-verified / user-contributed) and version tags. Facility staff verify through a review queue.
6. **Lands as Experimental** under the maturity tiers. No new headline surface during hardening.
7. **Offline first.** Build and validate the advisory machinery on saved files before any live-session or microscope-UI coupling. The live session is a later milestone, gated on the offline stop/go criterion.

## Audit first (read the repo, report facts with file/PR references)

- What `pixi run chat` can do today: can it open an arbitrary CZI/OIR outside a project and run QC measures on it? If not, what is entangled?
- Existing per-task QC measures and the Segment/Cleanup QC work: which metrics are reusable headlessly for acquisition QC?
- Metadata readers: what CZI/OIR fields are parsed today? Compare against what the advisor needs (laser power, gain, pinhole, pixel size, z-step, scan speed, frame interval, tracks/channels). Note per-scope gaps (780, 980, FV4000).
- Shared captures: how a capture reaches chat, and what an advisor offshoot needs.
- Blackboard: schema, write/read paths, versioning, provenance support, who can edit.
- Claude co-driving/MCP layer: how an advisor profile would plug in; what guardrails and feedback knobs exist.

## Plan the following

1. **Advisor profile:** system prompt and rules, the error-mode list (acquisition errors and their downstream effects on analysis), and what it must echo back before advising ("I read 561 at 2.0%, gain 650, pinhole 1.0 AU: correct?").
2. **Headless QC entry point:** minimal interface (path in, metrics out) and the metrics list: saturation fraction, SNR, Nyquist sampling vs. objective/pixel size, bleaching across timepoints, drift, channel bleed-through. State what each needs from metadata.
3. **Pilot analysis:** how to run a small crop fast enough to be useful at the scope (give a target time), and pass/fail plus cause reporting. Define advisory-only behaviour.
4. **Manual ingestion pipeline:** PDF to structured blackboard entries (setting, function, UI location, range, caution, software version, source page). Include the review queue, provenance upgrade path, correction flow and staleness handling. Note the licensing position for manuals and derived text.
5. **Screenshot lane:** the minimal offshoot of shared captures; how the advisor reads panel values and what it cannot trust (display-scaled, 8-bit, LUT-dependent).
6. **Driver-agnostic interface:** define from advisor needs only ("current settings", "test image", later "apply bounded change"). Say how a future MHS/MCP tool source would slot in, and where hard bounds would live (in the adapter, not the prompt).
7. **Evaluation:** how to tell whether advice is useful. Propose a small set of real bad-acquisition cases per scope, expected diagnoses, and a pass criterion. Include a check that the advisor admits uncertainty on unverified UI directions.
8. **Phasing (offline first):**
   - **Milestone 1, offline advisory core:** works on saved CZI/OIR files plus a short sample/question note, with no scope, no screenshots and no live session. Includes the headless QC entry point, advisor profile, and a small evaluation set of real acquisitions (good and bad: saturated, undersampled, bleached, bled through) with expected diagnoses. Define a stop/go criterion (e.g. diagnosis accuracy on the set, correct uncertainty handling, staff agreement) that must be met before Milestone 2 starts. This milestone must also work as a demonstrable proof of principle for people not involved in the build: give a short demo script (input files, expected advice).
   - **Milestone 2, knowledge base:** manuals-to-blackboard ingestion with the review queue.
   - **Milestone 3, live session coupling:** save/transfer/load loop at the scope, capture-share lane for panel context, test-frame workflow. Treat adoption friction (speed, locked-down acquisition PCs) as its own test with a few real users.
   - **Later, optional:** hardware actuation via MHS, vendor APIs or macros, only after the advisor has proven useful.

   For each milestone give effort estimate, risks, and what can start during the hardening phase vs. what waits.

## Prior art and citation audit (do this before finalising the plan)

Search the web and the literature. Cite every source (authors, year, venue, link). Report what you could not find or access. Do not cite from memory: every citation must be checked against a retrieved page.

**A. Guides and papers on acquisition issues.** Find the best existing guidance on common acquisition errors and how to avoid them: saturation, sampling/Nyquist, pinhole, bleaching/phototoxicity, bleed-through/crosstalk, detector settings, z-step and frame-rate choices for tracking, intravital-specific issues. Include image-quality-metrics work (CellProfiler's image quality measures and the related papers are a known starting point), community quality and reproducibility efforts (QUAREP-LiMi is a likely one), confocal tutorials and best-practice protocols, and metadata/reporting standards. Treat these seeds as leads to verify, not as facts. For each source: what it covers, how actionable it is, and whether its rules can be turned into checks or knowledge-base entries. Build a table mapping each acquisition error mode to the best source(s) and a candidate metric.

**B. Existing tools and frameworks that already give advice.** Find software that diagnoses images or recommends acquisition settings: image-QC tools and modules, microscope-side assistants and adaptive/smart acquisition, LLM or agent-based microscopy assistants, and any facility-level guidance systems. For each: what it advises on, what input it needs, whether it is open source, and how it validated its advice. Say plainly where something already solves part of this and what is left.

**C. Actuation layer (for later, brief).** Micro-Manager/pymmcore-plus, ZEN Developer Toolkit and FluoView automation options: what is documented, licensed and verified, and what is not.

**Conclusion required:** what to reuse, what to cite in the knowledge base and docs, what to build, and what existing work the advisor must be benchmarked against. Flag anything that makes the proposed advisor redundant.

## Output requirements

- Start with a 10-line summary and a recommended first slice.
- Separate **verified facts** (from the repo) from **assumptions** and **open questions for facility staff / vendors**.
- List what you could not verify, e.g. UI locations by software version, metadata completeness per scope, ZEN Developer Toolkit licensing on the 980, and any FV4000 external interface.
- No new features proposed beyond this scope. No speculation presented as fact.
