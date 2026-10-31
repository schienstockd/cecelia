> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> AI-assist design lives in [`docs/todo/KIWI_ASSISTANT_PLAN.md`](../todo/KIWI_ASSISTANT_PLAN.md).
>
> **Outcome:** not run as of archiving (2026-10-31); no `BEHAV3D_AUDIT_REPORT.md` exists. Premise
> drift at archiving: `OPUS_AI_INTEGRATION_BRAINSTORM` does not exist in this repo; the brief calls
> Kiwi a "semi-autonomous co-driver", but `KIWI_ASSISTANT_PLAN.md` is a structured duck (claims +
> refs, no recommendation kind) — co-driving is the separate MCP observer arc
> ([`OPUS_AUDIT_CLAUDE_CO_DRIVING_REPORT.md`](OPUS_AUDIT_CLAUDE_CO_DRIVING_REPORT.md)).

# BEHAV3D-Explorer audit: adopt, learn, or keep trucking?

You are auditing an external repository on behalf of the Cecelia maintainer. The goal is a decision, not a survey. **"Keep trucking, nothing worth taking" is a valid and fully acceptable outcome.** Do not manufacture adoption candidates to look useful.

- Repo under audit: https://github.com/imAIgene-Dream3D/BEHAV3D-Explorer (Apache-2.0, ~820 commits on `main`)
- Our repo: `schienstockd/cecelia` (R/Shiny legacy, Julia + Vue rewrite "pineapple" / Cecelia.jl, with Kiwi as the AI layer)

## 0. Setup

1. Clone BEHAV3D-Explorer to a scratch directory outside our repo. Pin the commit SHA you audit and record it in the report.
2. Read our own relevant docs first so you judge against what we actually have and intend, not a generic baseline: `KIWI_ASSISTANT_PLAN`, `OPUS_AI_INTEGRATION_BRAINSTORM`, the guide-run harness, and the recital checks. If any are missing, say so and continue.
3. Everything below marked **[README claim]** comes from BEHAV3D's READMEs and has **not** been verified against code. Treat it as a hypothesis to confirm or refute.

## 1. What a previous reader already saw (unverified)

**Processing and analysis [README claim]**
- napari plugin plus Jupyter notebook: data prep, segmentation, tracking, feature extraction, filtering, analysis tabs.
- Segmentation: Cellpose 3D with pretrained T cell and organoid models (Zenodo), napari-convpaint, retraining notebook.
- Tracking: btrack with two preset motion configs (`cell_config.json`, `particle_config.json`); outputs CSV plus 4D zarr labels.
- Analysis: contact (distance threshold, T cell-organoid and T cell-T cell), killing, death dynamics, invasion, per-cell readouts.
- Schema is built around a T cell + organoid co-culture (`tcell_channel`, `live_channel`, `dead_channel`, `organoid_line`), so the "any 3D data" claim is doubtful.
- Inputs: czi, lif, tiff, zarr. Windows/macOS one-click installers, Colab demo.

**In-product assistant "QueenB" [from `chatbot/README.md`, more detailed]**
- napari chat dock, CPU-only Modal service, DeepSeek (`deepseek-v4-flash` default), native tool calling over SSE.
- Live UI state serialized into every request (active tab, metadata incl. unsaved draft, validation state, visible controls, queue, logs, result files, analysis state, experiment references).
- Model can only act through `set_ui_value` against a live control registry (stable IDs, unit, choices, visibility, step/method/cell-type scope, `CONTROL_CONTRACT_VERSION`). Tool calls are proposals; the client validates, detects no-ops, auto-applies only structural/blank-field changes, and shows confirmation cards for existing or sensitive values. LLM-proposed metadata always requires confirmation.
- Server decision order: intent clarification, deterministic actions (EDT/min-size, tracking radius from interval, metadata conversion), preflight guards that ask one focused question when evidence is missing, then guidance cards + top-6 RAG (BGE-small, flat cosine) + DeepSeek.
- Rules: one user turn = one request, no hidden continuation; topic words are not commands (explaining a plot must not navigate); a saved config is not evidence a step ran; biological names never select methods or values; deterministic templates stay experiment-neutral.
- Process artifacts: transcript-derived regression fixtures, a deployed-endpoint smoke suite, pure-logic tests without Modal or network.
- Gaps noted so far: no result interpretation, no self-correction loop, no multi-step autonomy; many hand-written handlers (the README itself warns about handler-order interactions); public endpoint with no auth that sends live context to a third-party model.

**AI-assisted development [README claim]**
- States use of Codex/ChatGPT, Claude (Sonnet, Haiku, Opus), Cursor, Gemini, with human supervision and testing. No workflow is described. Anything beyond that is inference and must be labelled as such.

## 2. Task A: analysis and processing capabilities

Read the code under `behav3d/` (especially `analysis/`, `preprocessing/`, `napari/`), not just the docs.

1. Inventory what is actually implemented vs only documented. For each analysis (contact, killing, death dynamics, invasion, track counts/QC, anything else): inputs, algorithm in one paragraph, parameters, outputs, assumptions, `file:line` evidence.
2. Compare against Cecelia's equivalents (tracking, behaviour clustering, contact/interaction analysis, population gating). Be specific: what do they do that we don't, what do we do that they don't, where do the two produce different answers on the same question.
3. Identify methodological content worth knowing even if we never use the code: e.g. how contact is defined in 3D (surface vs centroid distance, thresholds, unit handling), how killing events are called, how dead-channel signal is turned into an event time, how EDT-based size recommendations are derived.
4. Flag correctness or reproducibility problems you find (hardcoded assumptions, silent unit handling, thresholds without justification, untested paths).
5. Check portability: Cecelia is moving to Julia + Vue. Separate (a) algorithms/ideas we can reimplement, (b) code we could call as an external tool, (c) things that only make sense inside napari.

## 3. Task B: how AI was used, and what transfers

Two separate questions. Do not conflate them.

**B1. AI inside the product (QueenB).** Evaluate against Kiwi's direction, which is semi-autonomous Claude co-driving the app and viewer with controls, guardrails and feedback knobs for the user. We have already moved away from fully autonomous runs.
- Which of their mechanisms (control registry + contract version, deterministic-first routing, preflight evidence guards, confirmation policy, one-request-per-turn, provenance precedence, topic-word-is-not-command rule) map onto a Kiwi gap we actually have? Name the gap, the mechanism, and the smallest change in our code.
- Which are specific to a form-filling parameter assistant and don't carry to a co-driver that drives a viewer and interprets results?
- What do they do about wrong answers: how would their design detect each of our known error modes (fanout, convention, and the image-analysis/quantification error modes we want agents to self-correct)? If it doesn't, say so.
- Cost/ops: Modal + DeepSeek vs our Claude-based setup. Anything worth copying (health route with provider probe, status row, retry-only-before-output)?

**B2. AI used to build the repo.** From git history, commit messages, test layout, docs structure, CI, and any agent-instruction files (`CLAUDE.md`, `AGENTS.md`, `.cursor/`, etc.):
- What evidence is there of an agent-driven workflow vs ad hoc assistance? Quote file paths and commit patterns, and state your confidence.
- What quality controls did they build around AI-generated code (regression fixtures from user transcripts, smoke suites, contract versions)? Which are worth adopting in our process?
- What failure modes are visible in the history (reverted AI changes, handler-order bugs, repeated fixes of the same class)? Compare with our own recital checks.
- Do not speculate about private practices. If the repo doesn't show it, write "not visible".

## 4. Decision framework

Classify every candidate item into exactly one bucket, with evidence and an effort estimate (S/M/L) for ours:

| Bucket | Meaning |
|---|---|
| **Adopt** | Take the idea or code now. State exactly what, where it lands in Cecelia, and the attribution/licence handling (Apache-2.0: preserve notices, state changes, credit in docs and the relevant PR). |
| **Learn** | Change how we think or test something, no code taken. State the concrete change to our plan/docs/tests. |
| **Watch** | Not now; state the trigger that would make it relevant. |
| **Ignore** | Not useful, or worse than what we have. One line why. |

Rules:
- Prefer fewer, higher-value items. More than ~8 Adopt/Learn items in total needs justification.
- Every claim about their code cites `path:line` at the pinned SHA. README-only claims are labelled as such.
- Every Adopt/Learn item names the specific Cecelia file, plan section, or error mode it improves. "Could be useful someday" goes in Watch or Ignore.
- Do not recommend rewriting working Cecelia components to match their structure.
- Do not recommend dependencies without checking licence, maintenance state, and whether it fits Julia + Vue.
- Flag anything in their design we should explicitly avoid (e.g. unauthenticated endpoint, live-context exfiltration, model-driven routing for deterministic questions).

## 5. Output

Write `BEHAV3D_AUDIT_REPORT.md` with these sections, in this order:

1. **Verdict** (3 to 6 lines): adopt / learn / keep trucking, and the single most valuable takeaway. Written so it can be read alone.
2. **Audit basis**: commit SHA, what you read, what you did not read, confidence per section.
3. **Task A findings**: capability inventory table, comparison vs Cecelia, methodological notes, problems found.
4. **Task B findings**: B1 and B2 separately.
5. **Decision table**: all items by bucket with evidence, target in Cecelia, effort.
6. **If Adopt/Learn is non-empty**: a proposed ordered work list (max 5 items), each small enough for one PR, with acceptance checks. Include attribution text to use.
7. **Open questions** for the maintainer: only ones that change the verdict.

Style: terse and direct, no filler, no praise. Tables and short bullets over prose. Do not restate this prompt. If a section has nothing, write "nothing found" rather than padding.
