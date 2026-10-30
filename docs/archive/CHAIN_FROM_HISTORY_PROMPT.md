> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> chain design lives in [`docs/SCHEDULER.md`](../SCHEDULER.md) and the plan below.
>
> **Outcome:** not run as written. Archived 2026-10-30. The design was worked out in conversation and
> tested against two real projects instead: [`docs/todo/CHAIN_FROM_HISTORY_PLAN.md`](../todo/CHAIN_FROM_HISTORY_PLAN.md)
> (adds a manual chain step for gating). Checked against the code on archiving, several premises are
> out of date:
> - **The run boundary (item 7) is already built.** `mcp/cecelia_mcp/autonomous.py` +
>   `autonomous_server.py` start chains (`run_chain`) and tasks, behind a project lock. That server is
>   only given to unattended or headless sessions; the observer still cannot start anything.
> - **One template already targets many images.** `chain:run` takes `imageUids`, skips excluded
>   images and can resume a run (`api/src/sockets.jl`), so "per-image fan-out?" is answered.
> - **History records effective params.** The run log stores `job.params` after
>   `_apply_spec_defaults` (`app/src/tasks/scheduler/run.jl`, `jobs.jl`). Statuses are
>   running/done/failed/cancelled/interrupted. "Superseded" is not recorded; a later run with the same
>   `fun`+`valueName` is the only sign.
> - **History is lossy for this job.** `/api/tasks/history` rows carry no set uid, task id or input
>   version. The list defaults to 100 rows and each image keeps only its last 200 runs
>   (`RUN_LOG_CAP`). Edges are easier than item 3 assumes: a chain edge only orders nodes and
>   propagates failure (`app/src/tasks/chain/validate.jl`, `execute.jl`); data flows by the output
>   names in each node's params, which the run log already holds.
> - **Deliverable location.** Plans go in `docs/todo/*_PLAN.md`, not the repo root.
> - **Missing from the brief: gating.** No task applies gates, so neither a mechanical builder nor
>   Claude can produce a chain past the first gated population.

# CHAIN_FROM_HISTORY: audit and plan

You are Opus, working in `schienstockd/cecelia`. Read-only investigation first, then write a plan. Do not implement.

## The scenario

A user processes a few images and experiments: tries tasks, tweaks params, reruns, discards some attempts. Then they say:

> "I have another 29 images that are similar. Process them the same way: build a chain and run it."

Today the chain whiteboard is authored by hand, or by Claude through `POST /api/chains/create` (MCP `create_chain`). The task history already records what was actually done.

## The question

Can "history on N exemplar images → chain template → run on the rest" be **mechanical (no LLM)**, with Claude optional on top? If so, what is the minimal, correct design?

## Investigate (cite file:line for every claim)

1. **History shape.** What does `GET /api/tasks/history` return (api/src/routes.jl and its backing code)? Per run: fun_name, params as submitted, status, timestamps, image/set uid, output valueName, version? Is the *effective* param set recorded (after spec defaults), or only what the UI sent? Are failed, cancelled and superseded runs distinguishable?
2. **Chain template shape.** Schema of a chain template (nodes, edges, startTargets), what `/api/chains/create` validates, and how the whiteboard merges spec defaults into sparse params.
3. **Wiring.** How can edges be inferred? Candidates: `outputValueName` / `writes`, selection params (`field`, `accepts`, `popScope`), and the versioned value names a later task reads. Is history order alone ever sufficient? Where is it ambiguous (fan-out after segmentation, two segmentations of one image, corrected vs uncorrected branches)?
4. **Exemplar selection.** Given "the few images I played with", how do we pick which runs count? Options: explicit user choice of images, a "last successful run per task per image" rule, the latest lineage (`/api/analysis/lineage`). Evaluate each for noise handling.
5. **Param reconciliation.** When exemplars disagree (different thresholds per image), what rule is defensible: last-wins, modal, refuse and ask? What must never be silently guessed?
6. **Execution.** How does a chain run on a set of images today (`chain:run` WebSocket message, `startTargets`)? Can one template target 29 images at once, or does it need per-image fan-out? What are the failure and retry semantics, and the resource-pool implications?
7. **The run boundary.** `mcp/cecelia_mcp/client.py` deliberately has no way to start a chain: "Claude designs, the user runs" is a property of the transport. Check whether an "autonomous" client exists (see the `http_json` docstring) and what it is allowed to do. Propose how a batch run should be triggered without weakening the observer guarantee, e.g. a GUI button ("Apply to N images"), a confirmation step showing the diff, or a separate guarded route.
8. **Where it lives.** Julia endpoint (e.g. `GET /api/chains/suggest`) vs a Python module reusing `http_json`. Which gives the whiteboard GUI the feature with no Claude in the loop? Which reuses server-side validation?

## Mechanical vs Claude

Produce an explicit split:

- **Mechanical:** extraction, filtering failed runs, edge inference, param reconciliation, validation, template creation, batch fan-out.
- **Needs a human:** ambiguity the algorithm cannot resolve (branching, conflicting params). Design this as a *question surfaced in the UI*, not a guess.
- **Optional Claude:** naming, explaining the proposed chain in plain language, flagging suspicious params. Must be removable without breaking the feature.

## Failure modes to address

- Exemplar images are not actually similar to the remaining 29 (different channels, dimensions, timepoints). What pre-flight check catches this before 29 runs start?
- Params tuned per image (e.g. a threshold chosen for a dim image) get replayed on bright ones.
- History includes an experiment the user abandoned; chain bakes it in.
- A task's output name collides with an existing version on a target image (overwrite vs new version).
- Partial failure midway through 29 images: resume, skip, or roll back?
- Mutation guarantees: what exactly can the batch run overwrite, and is it recoverable?

## Deliverable

Write `CHAIN_FROM_HISTORY_PLAN.md` at repo root with:

1. **Verdict** (one paragraph): is mechanical feasible, and what is the hard part.
2. **Algorithm**: numbered steps, with data structures.
3. **Decisions** (D1, D2, ...): each with options, a recommendation, and the reason.
4. **Pre-flight checks** before a batch run.
5. **Run boundary design** (item 7).
6. **Phased implementation plan**: smallest useful PR first, then increments. Name files to touch.
7. **Open questions** for the maintainer, max 5.

Be terse. No restating of this prompt. Prefer tables where comparing options.
