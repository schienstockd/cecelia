**ARCHIVED — audit record.** This file is not authoritative; it is the running log of findings
from the 2026-09-15 comment/contract maintainability audit (source prompt:
[`comment-audit-prompt.md`](comment-audit-prompt.md)). The durable outputs are
[`docs/MAINTAINABILITY.md`](../MAINTAINABILITY.md) (the standard) and [`docs/MAP.md`](../MAP.md)
(where things live). If a grep leads you here, go find the current answer there or in the file
itself; this record is frozen once the audit closes.

# Comment/contract audit — findings log

Scope, framing and running deliverables: see [`comment-audit-prompt.md`](comment-audit-prompt.md).

Format per file:

```
### path/to/file
- [pattern 1|2|3|contract|monolith|nav] <location> — <what's wrong> → <proposed fix (P1/P2 only)>
Verdict: <normal pass | protected — concurrency-critical>
```

Registers (patterns 4 and 5) also collate at the bottom of this file as they accumulate.

---

## Phase 0 — anchor calibration (2026-09-15)

Three files the prompt names as reference cases. Read to calibrate the sweep-grep patterns and the
durable-standard template. No file edits yet.

### `app/src/behaviour/hmm.jl`
- [pattern 1] header L3–4 — `Port of behaviourAnalysis hmmStates + hmmTransitions (R/Shiny cecelia). The original used depmixS4…` — cites a repo nobody here can open → **Fix:** replace with a Julia-native statement of what this file guarantees; drop the biographical narrative. Suggested: `Gaussian HMM over per-track measurements. One sequence per track, diagonal-Gaussian emissions with an owned weighted M-step (HiddenMarkovModels.jl has no suffstats for a diagonal MvNormal). Pure functions over a pooled per-cell DataFrame — no I/O, no HTTP.`
- [pattern 1] header L12–16 — `matching the R drop_na/filter(!is.infinite) then left-join behaviour, including its quirk of treating a mid-track drop as if the surrounding cells were contiguous` — the quirk matters, the R attribution doesn't → **Fix:** `Rows with a missing/non-finite value in any measure (track-start cells with no speed/angle) are excluded from the fit and returned as missing on the joined output. A mid-track drop is treated as if its neighbours were contiguous — no state boundary is introduced across the gap.`
- [pattern 1] L75 — `an approximation of R caTools::runmean(endrule)` → **Fix:** `Edge windows shrink to the available samples (no padding).`
- [pattern 1] L96–102 — `Mirrors the R normMeasurements / scaleMeasurements steps (scale(center = FALSE))` → **Fix:** `Divide each measure by a chosen summary (min/max/median/mean), then optionally divide by its uncentered std. Applied in place across all observations, per-measurement.`
- [pattern 1] L131–133 — `Ports the R postFiltering/postIterations step (DescTools::Mode over a frollapply window, take-first on ties)` (prompt's own example) → **Fix:** `Windowed-mode smoothing of a per-track state sequence. Ties broken by smallest state ID for reproducibility.`
- [pattern 1] L160–163 — `so the R seed param is unnecessary for our path; accepted but unused at the engine level` → **Fix:** delete the R clause; keep `Deterministic init → reproducible fit; a seed argument is accepted but unused.`
- [pattern 1] L203 — `Preprocessing order mirrors the R port: …` → **Fix:** drop the "mirrors the R port" clause; the ordered list that follows is the actual doc.
- [pattern 1] L294 — `Faithful to the R hmmTransitions truth table (includeStart × includeSelfTransitions)` (prompt's own example) → **Fix:** delete; the bullet list above it *is* the truth table.
- [contract] `pop_df` shape at L205–208 — expects `uID`, `value_name`, `track_id`, `time_col` + each named measure; no schema check. Upstream rename fails late with `KeyError`. Enforce via `_require_cols(df, cols)` at entry or a typed view struct.
- [contract] L307–309 — `state_cols may arrive as Ints (fresh fit) OR Strings (categorical obs read-back)` handled with `_present`/`_state_str`; the shape choice lives entirely in the reader. Normalise at the read boundary, or type intermediate columns as `Union{Int,Missing}`.
- [nav] "where does the HMM state fit / transitions truth table live" — file is discoverably named; `hmm_fit_states` and `hmm_transitions` are the two entry points. Good.
- Verdict: **normal pass**.

### `app/src/tasks/cleanupImages/af_correct.jl`
- [pattern 2] `af_qc_findings` docstring, L69–119 — poster child. Dataset IDs (`kSUFux`, `WIaUjL/p6t4mC`), specific measurements (`0.001% to 0.018%`, `0.113 from CH3 into CH2`, `The first run reported 0.0248`, `735–3576 of 65536 levels (1.1–5.5%)`, `co-positive retention 5.6–7.4% → 82–83%`), and the "This task used to be QC-exempt…" narrative are all incident history. The **decisions** encoded in it (why `af-low-range` is deleted; why no invented suppression finding) are real and worth keeping — just not here. → **Fix:** trim the docstring to what the function does and returns (2–4 lines + a per-finding one-liner). Move rejected-alternatives + rationale to `docs/todo/AF_CORRECTION_AUDIT.md` **Locked decisions** section. Move the dataset-ID / numeric-history block to the AF-correction PR body or `CHANGELOG.md` if kept at all.
- [pattern 2] L131–134 inline — `used to hand-roll the finding with a detail STRING and no long at all, which the QC panel rendered as "Channel N saturated → undefined" — visible in the GUI from the day AF QC shipped` — incident report. → **Fix:** replace with the invariant it defends: `Every finding goes through qc_finding + QC_TEXT so short/long/detail rendering stays consistent — no ad-hoc detail strings.`
- [pattern 3] L20–23 (target dropped from own competitor list) — mathematically load-bearing (naming the target twice halves the channel's output). **Keep.**
- [pattern 3] L36–38 (why `unique`), L47 (target competes with others, never itself) — same invariant restated at call site. **Keep.**
- [pattern 3] L184 (shared with preview — cannot disagree), L189–192 (log RESOLVED sets, not just count) — load-bearing. **Keep.**
- [contract] `params["afCombinations"]` shape (L25–54): a dict of dicts with keys `competingChannels`, `targetChannel` — no schema. A frontend key rename ships silently. Enforce with a typed constructor at the JSON boundary (`AfCombinationSpec`) or explicit validation from `validate_params`.
- [contract] `af_qc_findings(per_channel)` (L120): `Dict{String,Any}` with per-channel nested dicts of `saturatedFrac`, `levelsUsed`, `levelsAvailable`, `bleedthrough` — no schema. A Python-side key rename produces zero findings, silently. Enforce via a small `AfChannelStats` struct read at JSON parse.
- [monolith] Single file carries (a) frontend→Python param translation, (b) QC scoring, (c) run orchestration. New AF work will land here by gravity. Cleaner seams: `af/translate.jl`, `af/qc.jl`, `af/run.jl`. Prompt calls this out by name.
- [nav] "where does AF-correction QC live" — `*_qc_findings` grep works; MAP entry added because a new contributor won't know that suffix is a convention.
- Verdict: **normal pass**.

### `app/src/tasks/scheduler.jl`
- [pattern 1] L136 — `Analogue of R's mcparallel / mccollect(wait=TRUE)…` → **Fix:** drop the R clause; keep `run_task submits a job and blocks on take!(done_ch) until it finishes — synchronous from the caller. Blocking on a Channel/Condition yields the OS thread to Julia's scheduler (no spin-wait), so blocked submitters don't exhaust it.`
- [pattern 3] `list_tasks` L16–43 — dense per-field justification block; some restatements of "record dies on completion" (L302, L315, L325, L330) each attach to a *different* field's rationale. **Keep as-is.**
- [pattern 3] `pool_status` L87–92 — lock-ordering invariant (`never nest _TASKS_LOCK inside _POOLS_LOCK`). **Keep.**
- [pattern 3] `cancel_chain_run!` L111–121 — cancellation race commentary. **Keep.**
- [pattern 3] Resource-pools block L128–144 — pool concurrency semantics + resize-safety proof. **Keep.**
- [pattern 3] `_start_pool!` L196–219 — silent-throw-in-`@spawn` backstops with per-try justification. **Keep** — the "why two `try`s" reasoning is the file's most important commentary.
- [pattern 3] `TaskRecord` L293–334 — every field carries its own justification (`@atomic proc`, terminal-status handling, published `params` for Re-run). **Keep.**
- [pattern 3] `_execute_job!` L425–537 — one-and-only-one post contract; entire function is race-critical. **Keep, do not consolidate.**
- [contract] `TaskRecord.status::Symbol` (L312) — state machine (`:queued|:running|:done|:failed|:cancelled`) enforced by convention + a terminal-final check. A new call site can set an invalid symbol silently. Enforce via `@enum TaskStatus` + typed field, or a `set_status!(rec, ::TaskStatus)` wrapper.
- [contract] `TaskJob.imgs::Union{Nothing,Vector{CciaImage}}` (L422) — set-scope vs single-image discriminated by `nothing`-vs-vector; every branch has to remember the check. Turn into a sum type or a `job_target(job)` helper that never leaks the union.
- [contract] `job.done` is a size-1 `Channel{Any}` posted exactly once — submitter-liveness argument depends on it. Not typed. Minimum: `@assert job.done.sz_max == 1` in `_execute_job!` entry.
- [contract] `_publishable_params` whitelist (L74–85) — reasoning airtight *for current JSON3 behaviour*. If JSON3's behaviour changes, whitelist silently under-includes (the safe direction). **Not a risk-register item; leave.**
- [monolith] 733 lines, five responsibilities: (a) chain-cancel registry, (b) resource pools + dispatcher, (c) task registry + `TaskRecord`, (d) job execution, (e) four `run_task`/`run_tasks` overloads. Cleaner seams: `scheduler/pools.jl`, `scheduler/registry.jl`, `scheduler/execute.jl`, `scheduler/run_task.jl`, `scheduler/cancellation.jl`. **Do not split unadvisedly** — lock-ordering invariant crosses (b)↔(c); a split without preserving `never nest _TASKS_LOCK inside _POOLS_LOCK` is a race bug. Flag as slow, explicitly-invariant-preserving refactor.
- [nav] "where does a Python subprocess get killed when a task is cancelled" → **hidden** inside `_execute_job!`'s `on_process` closure L473–482. MAP entry added.
- [nav] "how does chain cancellation propagate to in-flight subprocesses" → `cancel_chain_run!` + `cancel_task!` + the race guard above. MAP entry added.
- [nav] "why does a task disappear from `list_tasks()` on completion, and where do I find the outcome" → header L21–24 answers it. MAP entry added: task snapshot is live only; terminal outcomes in `recent_tasks()` (`tasks/task_outcomes.jl`).
- Verdict: **protected — concurrency-critical** (add the one-line file-header note in a follow-up edit).

### Phase 0 — frontend sanity-check (not full findings, scope-decision evidence only)

Read to test the "keep frontend out of the sweep" call. Result: refined to a narrow P2-only pass
on frontend.

- `frontend/src/components/ViewerPanel.vue` (1131 lines, 266 comment lines, 20 commits in 3 weeks) — **~20–30 lines out of 266 (~10%)** are P2 cleanup candidates (dated authorship notes, quoted user reports, dataset IDs, phase codes, SHA cross-refs). Full audit lands in Phase 3.
- `frontend/src/tasks/smoothVis.ts` (688 lines, 261 comment lines) — this file is a **design-doc-in-code**; nearly every comment is load-bearing rationale defending a schematic constant against drift. Provenance IS the argument for the number. Two comments cite a dataset ID (`WIaUjL/p6t4mC`) and a PR (`#554`) but they defend `GAP_WORTH_PAYING_FOR = 0.12` and `GATED_SEC_PER_PLANE = 0.12`. Classifies as **pattern 3 (protected)**, not pattern 2. Marked as an exception in [`MAINTAINABILITY.md`](../MAINTAINABILITY.md) → *Frontend specifics*.

---

## Risk register — implicit cross-module contracts (pattern 4)

Ranked informally by how silently they'd fail. Populated as the audit progresses; anchor entries
follow.

**High — silently produces zero output on a shape change**
- `af_qc_findings(per_channel)` in `app/src/tasks/cleanupImages/af_correct.jl` — `Dict{String,Any}` with per-channel keys from the Python side (`saturatedFrac`, `levelsUsed`, `levelsAvailable`, `bleedthrough`). A Python-side key rename produces **zero findings, silently**. **Fix:** typed `AfChannelStats` struct read at JSON parse.
- `af_combinations_for_python(params, raw)` in the same file — `params["afCombinations"]` schema (`competingChannels`, `targetChannel`). A frontend rename produces empty combinations, silently. **Fix:** typed `AfCombinationSpec` at the JSON boundary.

**Medium — fails late with a KeyError or wrong result**
- `hmm_fit_states(df, measures; …)` in `app/src/behaviour/hmm.jl` — expects `df` from `pop_df` with `uID`, `value_name`, `track_id`, `time_col` + each named measure. Upstream `pop_df` rename fails late. **Fix:** `_require_cols(df, cols)` at entry.
- `hmm_transitions` L307–309 — `state_cols` may arrive as Int (fresh fit) OR String (categorical obs read-back). The reader normalises defensively; nothing at the producer says which. **Fix:** normalise at the read boundary.

**Medium — type violation permitted by convention**
- `TaskRecord.status::Symbol` (`app/src/tasks/scheduler.jl` L312). State machine documented by convention; a new call site can set an invalid symbol silently. **Fix:** `@enum TaskStatus` + typed setter.
- `TaskJob.imgs::Union{Nothing,Vector{CciaImage}}` (L422). Every branch remembers the check. **Fix:** sum type or `job_target(job)` helper.

**Low — documented in the code, safe direction on failure**
- `_publishable_params` whitelist (`scheduler.jl` L74–85). Airtight for current JSON3 behaviour; if that changes, silently under-publishes (safe). **Leave.**
- `job.done` channel size + single-post (`scheduler.jl` L438). Whole submitter-liveness argument depends on it, documented. **Minimum:** `@assert job.done.sz_max == 1` in `_execute_job!` entry.

---

## Structure register — monolith pressure (pattern 5)

Places where the next feature will land by gravity unless a seam is opened.

- `app/src/tasks/cleanupImages/af_correct.jl` (243 lines). Three responsibilities: run orchestration + QC scoring + frontend→Python param translation. Cleaner seams: `af/run.jl`, `af/qc.jl`, `af/translate.jl`. Non-invariant; safe to split.
- `app/src/tasks/scheduler.jl` (733 lines). Five responsibilities: chain-cancel registry, resource pools + dispatcher, task registry + `TaskRecord`, job execution, four `run_task`/`run_tasks` overloads. **Splitting requires explicit lock-ordering-invariant proof** — the `never nest _TASKS_LOCK inside _POOLS_LOCK` invariant crosses would-be seams. Not a routine cleanup.

---

## Phase 2 — backend sweep (2026-09-15, LANDED)

294 raw grep hits triaged into 7 buckets. **Net: 30 rewrites across 22 files.** 264 raw hits were
false positives (path fragments, route strings, test-set descriptions), legitimate citations
(celltrackR ports, R-migration files, reference implementations), or measurement provenance
defending hardcoded constants (smoothVis-class — protected).

**Bucket A — anchor rewrites** (11 edits, 3 files, per Phase 0 findings above):
- `app/src/behaviour/hmm.jl` — 8 P1 rewrites; header block trimmed.
- `app/src/tasks/cleanupImages/af_correct.jl` — docstring trimmed to 4 lines + per-finding one-liner + pointer to `docs/todo/AF_CORRECTION_AUDIT.md`; inline QC-panel-bug narrative collapsed to invariant.
- `app/src/tasks/scheduler.jl` — L135 R-analogue clause dropped; file-header `# concurrency-critical` note added.

**Bucket D — narrative trims** (11 edits, 11 files). 6 of the 17 initial candidates reclassified to protected on re-read (the "used to be" narrative WAS the load-bearing argument — measurement provenance or silent-failure contract):
- Applied: `api/dev.jl:37`, `api/src/preview_api.jl:29`, `api/src/routes.jl:434`, `api/src/notebooks_api.jl:131`, `app/src/ai/measures.jl:36`, `api/src/gating_api.jl:520`, `app/src/preview.jl:179`, `app/src/tasks/spatialAnalysis/detectAggregates.jl:52`, `python/cecelia/utils/movie_io.py:109`, `python/cecelia/utils/zarr_utils.py:327`, `python/cecelia/utils/zarr_utils.py:397`.
- Reclassified to protected on re-read: `api/src/server.jl:137` (route-table 42s→11s boot measurement), `app/src/run_log.jl:10` (cancel-logging argument), `app/src/runner/client.jl:265` (silent-retry pitfall), `app/src/runner/server.jl:472` (port-then-state race), `python/cecelia/utils/segmentation_utils.py:401` (perf argument for outer-loop placement), `python/cecelia/utils/store_sweep.py:195` (name-vs-structure silent-failure).
- Skipped as legitimate reference: `api/src/routes.jl:1693` (route describes a legacy-migration import).

**Bucket E — dated signatures** (7 edits, 6 files): `(Dominik, YYYY-MM-DD)` stripped in place across `api/src/image_geometry.jl:14`, `api/src/image_render.jl:7`, `api/src/movie_helpers.jl:114`, `app/src/config.jl:1173`, `app/src/tasks/task.jl:181`, `python/cecelia/utils/title_card.py:288`, `python/cecelia/utils/zarr_utils.py:144`.

**Standard-doc addendum:** [`docs/MAINTAINABILITY.md`](../MAINTAINABILITY.md) → *No cross-references*: added a paragraph naming the whole-file exception (legacy migrators, canonical algorithm ports, reference implementations).

## Phase 3+ — pending

- Phase 3 (task #5): narrow frontend P2 pass — candidate list pending.
- Phase 4 (task #4): registers extended from the sweep. Current registers (in `docs/MAP.md` + the tables above) already cover the anchor-file findings; sweep did not surface further pattern-4 or pattern-5 seams beyond what Phase 0 named.
