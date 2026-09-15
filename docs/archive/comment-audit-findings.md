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

Ranked by how silently they'd fail. Anchor entries + Phase 4 boundary-file extension (2026-09-15).

**A systemic pattern.** Six Tier-1 entries below are the same shape: a `String` or `Symbol` field
with a documented-in-comment enum of legal values, no compile-time check. **One `@enum` pass**
across `TaskRecord.status`, `ChainNode.scope`, `ChainNode.barrier_policy`, `ImageNodeState.status`,
`CciaImage.status`, and `Population.pop_type` (+ `popType` in `gating_api.jl`) would resolve six
entries in one PR. Fix template: `@enum X ...` + typed field + `set_X!(rec, ::X)` setter.

### Tier 1 — High (silently produces zero output or wrong result on a shape drift)

**Boundaries — `Dict{String,Any}` at a JSON edge**
- `af_qc_findings(per_channel)` in `app/src/tasks/cleanupImages/af_correct.jl` — `Dict{String,Any}` with per-channel keys from the Python side (`saturatedFrac`, `levelsUsed`, `levelsAvailable`, `bleedthrough`). A Python-side key rename produces **zero findings, silently**. **Fix:** typed `AfChannelStats` struct read at JSON parse.
- `af_combinations_for_python(params, raw)` in the same file — `params["afCombinations"]` schema (`competingChannels`, `targetChannel`). A frontend rename produces empty combinations, silently. **Fix:** typed `AfCombinationSpec` at the JSON boundary.
- `api/src/sockets.jl:104–120` — `handle_message` dispatches on `type = get(data, :type, "")` and every branch unpacks fields via `get(data, :key, default)` with no schema. A frontend rename silently defaults. **Fix:** one typed struct per WS message (`TaskRunMsg`, `MovieRunMsg`, `MovieBatchMsg`) via a `parse_ws_message` dispatcher; the whole `_wstr`/`_ov_look_str`/`_ov_look_int` family becomes redundant.
- `api/src/sockets.jl:304–312, 449–451` — `movie_config::Dict{String,Any}` assembled with 15+ hand-written keys. A downstream reader (`movies_api.jl`) missing one silently records without it. **Fix:** `MovieConfig` struct — schema becomes compile-checkable Julia-side and JSON-schema-checkable at the boundary.
- `api/src/routes.jl:344, 434, 611, 1255, 1365` — every POST handler pattern: `body = JSON3.read(req.body); field = string(get(body, "key", ""))`. Same drift risk as sockets, × ~50 routes. **Fix:** one request-body struct per route family (chains/, boards/, images/, movies/, tasks/, viewer/) parsed at router entry; handlers take the typed value. `test_ws_body_shapes.jl` already pins some shapes — extend to HTTP.
- `app/src/tasks/chain.jl:23` — `ChainNode.params::Dict{String,Any}` is the on-node params bag; every task consumes it differently. Same boundary as scheduler/af_correct — the fix is per-task typed constructors, not a chain-level fix.
- `app/src/model/image.jl:22–23` — `im_channel_names::Dict{String,Any}`. Every consumer expects `value_name → Vector{String}`; `ccid_channel_names` silently returns `String[]` on shape drift, so every task on the affected image runs with "(none registered)" and no error. **Fix:** `Dict{String,Vector{String}}` — the type IS the schema.
- `app/src/model/image.jl:23` — `img.meta::Dict{String,Any}`. 20+ readers assume specific keys (`"SizeC"`, `"SizeT"`, `"PhysicalSizeX"`, …). **Fix:** typed `ImageMeta` struct + one converter at load; higher fix cost, high risk.
- `app/src/label_props.jl:718–740` — `write_categorical_obs(columns::AbstractVector; …)` expects a vector of `(; name, labels, values)` NamedTuples; a differently-named field crashes deep in the Python writer. **Fix:** `struct CategoricalObsColumn` + typed vector.
- `app/src/label_props.jl:598–606` — `add_obs(lp, df)` requires a `label` column (error-checked) but silently assumes every other column is `Float64`-convertible; a `String` column throws inside `Float64(v)` after the guard. **Fix:** `_assert_float_convertible(pend)` at entry, or an `add_obs_numeric`/`add_obs_categorical` split.

**State machines — string-typed with an enum-in-a-comment**
- `TaskRecord.status::Symbol` (`scheduler.jl` L312). 5 states, convention-enforced. **Fix:** `@enum TaskStatus`.
- `ChainNode.scope::String`, `ChainNode.barrier_policy::String` (`chain.jl:20–33`). `"image | set | incremental"` and `"all | require_all | successful_only"`. Typo in `barrier_policy` silently defaults to `"all"`. **Fix:** `@enum ChainScope`, `@enum BarrierPolicy`.
- `ImageNodeState.status::Symbol` (`chain.jl:73–78`). 7 states. Same class as `TaskRecord.status`. **Fix:** `@enum ChainNodeStatus` + `set_state!(rec, ::ChainNodeStatus)`.
- `CciaImage.status::String` (`model/image.jl:11`). 4 states. **Fix:** `@enum ImageStatus`.
- `Population.pop_type::String` (`population_manager.jl:61–68`). 5 values, referenced from `accepts` allow-lists, `pop_df`, palette, popScope. A typo yields a pop nothing routes to. **Fix:** `@enum PopType`; allow-list checks become type checks.
- `popType` in `gating_api.jl:1033–1036` — repeats `Population.pop_type` on the API side. Same enum resolves both.

**Variant types — `Union` with `Nothing` as discriminant**
- `TaskJob.imgs::Union{Nothing,Vector{CciaImage}}` (`scheduler.jl` L422). Every branch remembers the check. **Fix:** sum type or `job_target(job)` helper.
- `population_manager.jl:65–73` — filter-pop shape: `filter_measure::Union{String,Nothing}`, `filter_fun::Union{String,Nothing}` (enum-in-comment: `"gt|gte|lt|lte|eq|neq|in"`), `filter_values::Any`, `filter_conditions::Union{Vector,Nothing}` of implicit-shape NamedTuples. A malformed persisted condition reads back into a runtime error deep in `recompute!`. **Fix:** `struct FilterCondition` + `@enum FilterFun`; sidecar's shape becomes a schema at load time.
- `population_manager.jl:82–88` — `boolean_op::Union{String,Nothing}` (`"and" | "or"`), `boolean_pops/_not::Union{Vector{String},Nothing}` — three-fields-for-one-concept, silent-null combinations. **Fix:** sum type `BoolMembership = None | And{pops, not} | Or{pops, not}`.
- `population_manager.jl:87–89` — `explicit_labels::Union{Vector,Nothing}` is `Vector` (any), not `Vector{Int}`. Floats resolve via `_maybe_int` deep in `label_props`. **Fix:** `Vector{Int}` annotation.

### Tier 2 — Medium (fails late with a KeyError or wrong result)

- `hmm_fit_states(df, measures; …)` in `app/src/behaviour/hmm.jl` — expects `df` from `pop_df` with `uID`, `value_name`, `track_id`, `time_col` + each named measure. Upstream rename fails late. **Fix:** `_require_cols(df, cols)` at entry.
- `hmm_transitions` L307–309 — `state_cols` may arrive as Int (fresh fit) OR String (categorical obs read-back). The reader normalises defensively; nothing at the producer says which. **Fix:** normalise at the read boundary.
- `app/src/tasks/task.jl:1039–1250` — composite-task step resolution reads `spec["composite"]` and each step's `params` as untyped bags; a step key rename silently drops that step. **Fix:** `CompositeStepSpec` struct + validator at spec-load time.
- `app/src/tasks/task.jl:1252–1378` — repeatable-group ordering reads `<group>Order` as an implicit array of ids matched to a group name. Silent skip on a typo. **Fix:** `RepeatableGroupOrder` typed accessor + entry check.
- `api/src/gating_api.jl:343, 363` — `body["projectUid"]`, `body["imageUid"]` — bare `KeyError` on missing keys rather than a friendly 400. Every other handler uses `get(body, …, "")` + emptiness check. **Fix:** shared `_require_ids(body)` returning `(pu, iu)` or 400.
- `api/src/notebooks_api.jl:20–46` — `_nb_proc_ref`, `_nb_lock`, `_nb_starting`, `_nb_error` — four coordinated refs guarded by one lock; the write path can drift. **Fix:** bundle into `struct PlutoServerState` + one atomic-transition function.

### Tier 3 — Low (well-defended already, or safe direction on failure)

- `_publishable_params` whitelist (`scheduler.jl` L74–85). Airtight for current JSON3 behaviour; silently under-publishes if it drifts (safe). **Leave.**
- `job.done` channel size + single-post (`scheduler.jl` L438). Documented; whole submitter-liveness argument depends on it. **Minimum:** `@assert job.done.sz_max == 1` in `_execute_job!` entry.
- **Verified well-defended (Phase 4 boundary-file audit — no new entry needed):**
  - `app/src/py_runner.jl` — `PY_CONTRACT_VERSION` env-var contract; tests enforce parity + progress format + BLAS env.
  - `python/cecelia/utils/label_props_utils.py` — `_check_centroid_names` raises loudly on any pre-migration file; axis-name convention pinned both sides.
  - `python/cecelia/utils/zarr_utils.py` — `IMAGE_COMPRESSOR_CHOICES` + `staged_store` + structural `is_zarr_store` enforce the contract; convention-test-enforced.
  - `app/src/model/project.jl` — small, well-typed.
  - `app/src/qc.jl` — `QC_TEXT` catalog IS a schema; `qc_finding` builds `Dict{String,Any}` with hardcoded consistent keys; render side validates codes.
  - `app/src/qc_cohort.jl` — `COHORT_METRICS` is a typed constant.

---

## Structure register — monolith pressure (pattern 5)

Places where the next feature will land by gravity unless a seam is opened. Anchor entries +
Phase 4 hot-file extension (2026-09-15).

### Tier 1 — highest ROI (biggest / most-mixed / most-churned)

- **`app/src/tasks/task.jl`** (1657L, 14 section headers, 45 commits/6mo). Responsibilities: (a) spec loading, (b) live outputs, (c) previewable, (d) custom-task registry, (e) fun_name precedence resolution, (f) param validation, (g) run output-name resolution, (h) applicability gating, (i) internal dispatch, (j) composite task orchestration, (k) composite step resolution, (l) repeatable groups, (m) per-param image-gating, (n) fun_name dispatch. **Cleaner seams:** `task/spec.jl` (a, e), `task/validate.jl` (f, m), `task/composite.jl` (j, k, l), `task/register.jl` (d), `task/preview.jl` (b, c), `task/dispatch.jl` (h, i, n, g). Non-invariant; safe to split.
- **`app/src/gating/population_manager.jl`** (2412L, 16 section headers, 36 commits/6mo). Responsibilities: (a) `Population` + `PopulationMap` structs, (b) path helpers, (c) boolean pops, (d) mutations (add/rename/move/del), (e) tree (de)serialisation, (f) sidecar persistence, (g) co-clustered segmentation share, (h) categorical colour palette, (i) `pop_df` accessor, (j) derived pops, (k) summary-canvas picker logic, (l) `popScope`, (m) `accepts` allow-list, (n) mixed-type resolution. **Cleaner seams:** `population_manager/model.jl` (a, b, c), `mutations.jl` (d), `persist.jl` (e, f), `pop_df.jl` (i, j), `scope.jl` (k–n), `cluster_share.jl` (g), `palette.jl` (h). **Splitting requires preserving the `uid_index` sync invariant** (kept in step by every mutation) and the `retired_uids` set — moving mutations to a separate file makes the sync a cross-file contract a new call site could break silently. Extract with tests pinning uid_index/pops parity.
- **`app/src/tasks/chain.jl`** (1415L, 19 section headers). Responsibilities: (a) `ChainNode`/`ChainEdge`/`ChainTemplate` model, (b) `ChainRun` + `ImageNodeState`, (c) fs helpers, (d) template I/O, (e) template validation, (f) content cache, (g) run-record I/O, (h) topo sort, (i) barrier primitive, (j) per-node progress, (k) per-image execution, (l) set-scope node runner, (m) incremental plot runner, (n) resume helpers, (o) forward references, (p) `run_chain` public, (q) REPL helpers. **Cleaner seams:** `chain/model.jl`, `template.jl`, `run_record.jl`, `schedule.jl`, `barriers.jl`, `execute.jl`, `resume.jl`, `run_chain.jl`, `repl.jl`. **Splitting requires preserving the `ChainRun._lock` + `_barriers` + `_barriers_done` invariants** — same class of caveat as `scheduler.jl`, though less severe (process-local, not multi-thread hot path).
- **`app/src/config.jl`** (1488L, 9 section headers, 49 commits/6mo — **top-churned file in the repo**). Responsibilities: (a) user-supplied model checkpoints, (b) model-vault helpers, (c) coastal (optical-flow) models, (d) denoise (SUPPORT) models, (e) CPU counting, (f) linearly-scaling stage widen guard, (g) detached task runner config, (h) image store compression, (i) store layout defaults. Four distinct domains: model catalogs, CPU/scheduling, runner config, storage. **Cleaner seams:** `config/base.jl`, `models.jl` (a–d), `cpu.jl` (e, f), `runner.jl` (g), `storage.jl` (h, i). Non-invariant. **Highest bang-per-buck** of the P5 candidates because it's the most-churned file and its four domains are already visibly separate in the section headers.

### Tier 2 — worthwhile splits, smaller wins

- **`app/src/tasks/importImages/omezarr.jl`** (1074L). Responsibilities: (a) OME-ZARR metadata reader (`_delta_t_fallback`, XML scraping), (b) ccid.json helpers, (c) the import task. The metadata reader is arguably a reusable utility. **Cleaner seams:** `importImages/ome_metadata.jl` (a), `ccid_helpers.jl` (b), `omezarr.jl` (c). Non-invariant.
- **`app/src/qc.jl`** (1015L, 41 commits/6mo). Responsibilities: (a) `QC_TEXT` copy catalog (~250 lines of Dict entries + placeholders), (b) read-time rendering, (c) image calibration QC, (d) count metrics banking. The catalog is separable and would shrink the "logic-and-writing mixed" surface the file's own header warns about. **Cleaner seams:** `qc/copy.jl` (a) + `qc.jl` (b, c, d). Non-invariant.
- **`api/src/routes.jl`** (3053L). One file for the route table AND every handler without its own `_api.jl`. Splitting by route family → 6–8 files of ~300–500L each. **Cleaner seams:** `api/src/routes/chains.jl`, `boards.jl`, `images.jl`, `movies.jl`, `profiles.jl`, `misc.jl`; the routing TABLE stays in `server.jl` (already the case). Non-invariant. High yield.

### Tier 3 — anchor entries (already scoped in Phase 0)

- `app/src/tasks/cleanupImages/af_correct.jl` (243 lines). Three responsibilities: run orchestration + QC scoring + frontend→Python param translation. Cleaner seams: `af/run.jl`, `af/qc.jl`, `af/translate.jl`. Non-invariant; safe to split.
- `app/src/tasks/scheduler.jl` (733 lines). Five responsibilities: chain-cancel registry, resource pools + dispatcher, task registry + `TaskRecord`, job execution, four `run_task`/`run_tasks` overloads. **Splitting requires explicit lock-ordering-invariant proof** — the `never nest _TASKS_LOCK inside _POOLS_LOCK` invariant crosses would-be seams. Not a routine cleanup.

### Verified coherent — NOT monoliths (Phase 4 audit)

- `app/src/tasks/plugins.jl` (810L) — plugin layout + contribution model + install/update/remove. Coherent.
- `app/src/qc_cohort.jl` (433L) — single-domain.
- `app/src/tasks/opticalFlow/train.jl` (362L) — coherent task file.
- `api/src/movie_rail.jl` (972L) — the movie-recording pipeline, single-domain.
- `api/src/notebooks_api.jl` (739L) — notebook lifecycle + list, coherent.

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

## Phase 3 — frontend name-elimination + narrow P2 (2026-09-15, LANDED)

Mid-audit, Dominik widened the scope from the narrow P2 sweep to a **whole-codebase name-elimination pass** (`grep just for Dominik. this shouldn't really be in any code file`). Docs (.md) deliberately excluded — archived audit records legitimately name the audit-scope user.

- **Parenthetical signature strips** (mechanical, single-line): 66 files, 167 strips. `(Dominik, YYYY-MM-DD…)` removed with leading whitespace. `[^)\n]` regex constraint so multi-line parentheticals were left for per-case handling.
- **Reworded attributions + ownership refs**: 61 files, ~150 rewrites in two rounds. `Dominik's <thing>` → `the <thing>`; `reported by Dominik` → `reported`; `Dominik hit` → `observed`; quote-attributions kept the quote and dropped the name; multi-line parentheticals + one-off attributions per-case.
- **`/home/dominik` path literal** in `frontend/src/utils/fsPath.test.ts` → `/home/alice`. Test is pure string-parsing (`fsBreadcrumbs` doesn't touch the filesystem); semantics preserved.
- **Frontend narrow-P2 categories** (post name-elimination): 5 commit SHA cross-refs removed; 25 orphan phase codes dropped or reworded (the 19 that anchor a `docs/todo/*_PLAN.md` section by path are kept); 1 quoted user report converted to invariant.
- **Corruption fixes**: my round-2 substitution script used Python's `\s` which spans newlines, collapsing a handful of multi-line parentheticals into `//. Continuation` junk. Caught via a targeted `^\s*(#|//|\*)\.\s*[A-Z]` grep; fixed at all 9 sites (`ViewerWindow.vue:2960 & 3691`, `brickVolumeRenderer.ts`, `volumeRenderer.ts`, `viewerCrashGuard.ts`, `debouncedLatest.ts`, `CollapsiblePanel.vue`, `BatchMoviesPanel.vue`, `CorrectionCockpit.vue`, `cell_cards_api.jl`).

Final state: **0 `[Dd]ominik` mentions in any source file**; **0 orphan-period comment lines**; **0 orphan commit SHAs / quoted reports / orphan phase codes** (retained references all anchor a plan doc by path).

## Phase 4 — register extension on hot/boundary files (2026-09-15, LANDED)

Rubric per user: boundary files for P4 (Julia↔Python runners + API handlers + label_props + zarr_utils + image.jl), size+churn for P5 (task modules). Fork-driven read across 14 boundary files + 9 monolith candidates. **Findings-only, no code edits.**

- **P4 additions:** 17 Tier-1 entries (10 boundary bags, 5 string-typed state machines, 4 variant types), 6 Tier-2 entries, 6 Tier-3 verifications (well-defended). Dominant systemic pattern surfaced: **six string-typed state machines want one `@enum` pass** — see the risk register header.
- **P5 additions:** 4 Tier-1 (task.jl, population_manager.jl, chain.jl, config.jl — top-churned file), 3 Tier-2 (omezarr.jl, qc.jl, routes.jl), 5 verified coherent. `population_manager.jl` and `chain.jl` splits carry invariant-preservation caveats (uid_index sync; `_lock`/`_barriers`) — same class as `scheduler.jl`.
- **Richest sources:** `population_manager.jl` (4 P4 entries + Tier-1 P5), `task.jl` (2 P4 + Tier-1 P5), `chain.jl` (3 P4 + Tier-1 P5), `model/image.jl` (3 P4), `sockets.jl` (2 P4).

## Audit — closed

All five audit phases (0 anchor → 4 registers) landed on branch `docs/comment-audit-prompt` in commits `9da86408..HEAD`. The registers above are the actionable outputs — priority list for the next round of typed-boundary + monolith-split PRs.
