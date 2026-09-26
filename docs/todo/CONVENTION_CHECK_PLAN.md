# Convention-check reviewer — the pre-commit anti-duplication check

Status: **DRAFT (2026-09-26)** — plan only, nothing built. Reviewer prompt is a draft; inventory scope is under audit (see [Open decisions](#open-decisions--dominiks-call)). Companion to [`EFFECTIVENESS_LOG_PLAN.md`](EFFECTIVENESS_LOG_PLAN.md); parallel in shape to [`docs/ai-assist/SIBLING_CALL_AUDIT.md`](../ai-assist/SIBLING_CALL_AUDIT.md).

## Goal

Catch **convention drift** — when an implementing agent writes a new helper, component, or endpoint that already exists under another name, or a new implementation that skips an existing canonical framework (e.g. building bespoke gating logic instead of reusing `population_utils`, hand-rolling a button instead of using `AppButton`, adding a new zarr accessor instead of going through `zarr_utils`).

Complementary — not overlapping — with the [sibling-call audit](../ai-assist/SIBLING_CALL_AUDIT.md):

| | Sibling-call audit | Convention check (this plan) |
|---|---|---|
| Fires on | fix-shaped hunks | addition-shaped hunks |
| Search shape | grep same symbol at other call sites | grep functional near-equivalents by domain + inventory |
| Catches | fix drift outward | duplication inward |
| Ground truth | the diff itself | `docs/inventory/*.md` + repo grep |

Different failure classes, different search shapes, different reviewer prompts. This plan is the second reviewer.

## Origin

- 2026-09-26 chat with Dominik. He observed: "every time I point out to Opus — hey this button looks hand rolled, did you check app convention — it normally gets it and says, oh, yeah, I didn't actually check. Wouldn't all we need is a tick on the PR requiring the agent to say yes I checked app conventions XYZ. And couple this to a git commit hook."
- Immediate refinement: a self-attested tick is gameable — the cheapest way to satisfy "yes I checked" is to say yes. What actually forces the check when Dominik prompts after the fact is *specificity* — naming what the agent should have looked at. That specificity plus a fresh subagent that has to produce evidence, not just an assertion, is the mechanism.
- Direct precedent: [`SIBLING_CALL_AUDIT.md`](../ai-assist/SIBLING_CALL_AUDIT.md) — same shape (fresh subagent, tail line, evidence fold), different job.

## Why not just self-attestation

The gap between "self-tick" and "reviewer subagent":

- **Self-tick** — the implementing agent says "I checked convention." Text output; no verifier. Falls to Goodhart: cheapest satisfying path is to write "yes" without checking. Same failure mode Sonnet warned about in the reward-system discussion.
- **Reviewer subagent** — fresh agent, fresh context, cannot see or edit code, must produce evidence (which inventory files read, which greps run) before the mechanism-visibility tail line fires. Missing evidence fold = check silently didn't run = pre-commit hook fails.

The point isn't the tick. It's the fresh reviewer holding evidence.

## How it runs

Parallel to the sibling-call audit. At the pre-commit reservations step, the implementing agent spawns a second subagent:

```
Agent(
  subagent_type: "general-purpose",
  model: "sonnet",
  prompt: <contents of the "Reviewer prompt" section below>
          + "\n\n---\n\n" + <git diff --staged>
)
```

The subagent's reply is used in the recital the same way the sibling's is:

1. **Woven into the reservations list** — `**should reuse**` findings become reservation items. `**potential duplicate**` folds in with hedging.
2. **Printed verbatim as evidence** under a `_Convention check (evidence):_` fold — the inventory files consulted, the greps run, per-addition disposition.
3. **Tail line** — `_Convention check: run_` (or a skip line). Missing tail = mechanism went dark.

**Model = sonnet.** Same reasoning as sibling — read diff → identify additions → grep inventory + repo. Many small independent reads, not one deep reasoning chain.

Latency: adds ~30s per commit on top of sibling-audit's ~30s. Total pre-commit reviewer cost ~60s. Not free; not prohibitive.

## Escape valves — skip the subagent when

- Diff is docs-only (only `docs/**`, `*.md`, `CLAUDE.md`). Tail: `_Convention check: skipped — docs-only diff_`.
- Diff has no additions (only modifications / deletions / renames). Tail: `_Convention check: skipped — no additions_`.
- Diff is test-only (only `test_**`, `tests/**`, `*.test.*`, `*_test.jl`). Tests are allowed to hand-roll fixtures. Tail: `_Convention check: skipped — tests-only_`.

Otherwise spawn. The prompt's own short-circuit handles the "additions present but all trivial" case (tail: `_no convention check needed_`).

## Reviewer prompt (draft)

*(Everything below the horizontal rule is passed verbatim as the subagent's prompt, followed by the staged diff.)*

---

You have full read access to the repo (do not modify). `git diff --staged` follows the `---`.

**Job:** catch **convention drift** — a new helper, component, endpoint, or accessor that duplicates an existing canonical implementation, or that skips an existing framework. You surface candidates; you do not review code.

**Per addition-shaped hunk:**

1. Name the symbol added (`file:line`) — function, class, Vue component, endpoint handler, MCP tool, exported helper. Also count as additions: new dataframe accessors, new h5ad readers/writers, new JSON readers/writers, new zarr accessors.
2. Identify its **domain**: backend Julia (`.jl` under `app/src/` or `api/src/`), backend Python (`python/cecelia/**`), frontend (`frontend/src/**/*.vue`, `frontend/src/**/*.ts`), API surface (REPL / MCP / HTTP handler), plots / analysis-board panel, task-authoring.
3. **Read the docs for that domain** — the ground-truth set is broader than `docs/inventory/`:
   - Frontend additions: `docs/inventory/FRONTEND.md` AND `docs/ui/PRIMITIVES.md` AND `docs/ui/COPY.md` AND `frontend/CLAUDE.md`. All four. A bespoke button that skips `PRIMITIVES.md` is the exact failure mode this reviewer exists to catch.
   - Plot / analysis-board panels: `docs/PLOTS.md` — the "don't hand-roll a panel, register via `INTERACTIVE_VIEWS` or `CLUSTER_PANELS`" doc. Not in inventory today.
   - Backend Julia additions: `docs/inventory/JULIA_API.md`, `JULIA_APP.md`, `FLOWS.md`, `fingerprint_extractors.md`, plus root `CLAUDE.md` and `app/CLAUDE.md`.
   - Backend Python additions: `docs/inventory/PYTHON.md`, `DATA_ACCESS.md`, plus root `CLAUDE.md`.
   - Task-authoring: `docs/MODULES.md`.
   - API surface: `docs/inventory/MCP.md`, `JULIA_API.md`.
4. `grep` the repo for functional near-equivalents. Inventory is a floor, not a ceiling — a canonical public helper may exist in code but not yet be catalogued. Grep source directly before claiming no equivalent. Use synonyms, not just the exact name. A new `IconToggle` greps `Toggle`, `Switch`, `IconButton`, `AppToggle`; a new population accessor greps `pop_df`, `PopUtils`, `pop_type`; a new h5ad reader greps `read_h5ad`, `anndata`, `label_props_utils`; a new JSON reader greps `atomic_io`, `read_json`, `load_json`. Do NOT hunt for cloned private helpers with opaque names (`_catkey`, `_impl`) — that class is out of scope; see §Not in scope.
5. If a canonical equivalent exists, flag as **should reuse**. If none exists but shape is suspicious, flag as **potential duplicate** (say what would confirm).

**Addition-shaped** = new named entity contributing to the public or intra-module API. Not: pure renames, moves without semantic change, refactors, test fixtures, generated code, formatting, docs.

**Output** — one line per finding, most severe first, ≤300 words total:

```
- **file:line** — added `symbol`, closest canonical `existing.symbol` (path), why potentially duplicate [**should reuse** | **potential duplicate**]
```

- **should reuse** — you read the canonical and it fits the addition's purpose.
- **potential duplicate** — same domain, similar shape, fit unverified or ambiguous.

**Evidence to include** — for every finding AND for every addition that got a clean bill of health:

- Which inventory files you read (paths).
- Which grep terms you tried.
- Which candidate near-equivalents you looked at and rejected (path, one-line reason).

Empty evidence fold on a "no findings" reply = failed check. Fail loud rather than silently pass.

**Short-circuit**: if the diff contains no addition-shaped hunks, reply exactly `_no convention check needed_` — no evidence fold.

---

## Inventory scope

Audited 2026-09-26 (companion research pass). Inventory alone is **not** sufficient — three high-value doc touchpoints live outside `docs/inventory/*.md` and must be part of the reviewer's context, or the reviewer misses exactly the cases this plan is meant to catch (see [Case studies](#case-studies)).

**Per-domain reviewer context**:

| Domain | Reviewer reads |
|---|---|
| Backend Julia | `docs/inventory/JULIA_API.md`, `JULIA_APP.md`, `FLOWS.md`, `fingerprint_extractors.md` · root `CLAUDE.md` · `app/CLAUDE.md` |
| Backend Python | `docs/inventory/PYTHON.md`, `DATA_ACCESS.md` · root `CLAUDE.md` |
| Frontend | `docs/inventory/FRONTEND.md` · **`docs/ui/PRIMITIVES.md`** · **`docs/ui/COPY.md`** · `frontend/CLAUDE.md` |
| API surface | `docs/inventory/MCP.md`, `docs/inventory/JULIA_API.md` |
| Plots / analysis-board panels | **`docs/PLOTS.md`** (58 KB — the "don't hand-roll a panel" doc; cited from zero inventory files today) |
| Tasks / task-authoring | `docs/MODULES.md` (JSON spec shape, QC-required, param advisories) |

The three **bold** entries are the audit's critical finding. They are load-bearing — `frontend/CLAUDE.md` explicitly makes `PRIMITIVES.md` and `COPY.md` mandatory reads, and `PLOTS.md` catalogues the plot-registry axis (`INTERACTIVE_VIEWS` / `CLUSTER_PANELS`), but none are folded into `docs/inventory/`. A reviewer that stops at inventory silently misses them.

### Grep the source, not just inventory

Inventory is a floor, not a ceiling. A canonical public helper may exist in the codebase but not yet be catalogued in the relevant inventory file (drift between code and doc). Reviewer must grep source before claiming no equivalent exists — inventory alone will miss uninventoried-but-public canonicals.

**Explicitly out of scope: cross-module private-helper cloning.** The `ddaeb0b2` audit spot-check surfaces `_catkey` cloned from `plot_data.jl` into a new endpoint handler. That case is a different failure class from convention drift — private helpers with opaque names (`_catkey`, `_impl`, `_key`) do not respond to synonym greps, do not belong in inventory (which is for canonical public things), and detecting them reliably would require a semantic-similarity index. This reviewer does not catch them. See §Not in scope.

### Inventory gaps worth closing

No domain has zero inventory — nothing structurally missing — but three small write-ups would materially help:

1. A short plot-registry entry in `docs/inventory/` pointing at `docs/PLOTS.md` and naming `INTERACTIVE_VIEWS` / `CLUSTER_PANELS`.
2. A note in each inventory file that private helpers are NOT inventoried, so the reviewer must grep source before saying "no equivalent."
3. **Codify the module-shell convention** in `docs/ui/PRIMITIVES.md` or `frontend/CLAUDE.md`: "New module pages match `ChainModule`'s full-height flex-column shape (toolbar top, content below) — use `ModulePage layout="fill"` for standalone pages, or match `ChainModule` directly for pipeline surfaces. No bordered `surface-1` box scaffolding." Named as gap by the 2026-09-26 dry-run — Finding #4 was reachable but weakest, and a less-thorough reviewer run plausibly misses it without this line. **Close this one before wiring the reviewer unattended.**

Items 1 and 2 are not blockers on shipping the reviewer. Item 3 is the pre-wire cleanup surfaced by the dry-run.

## Case studies — real drift the reviewer must catch

Concrete cases from the repo's own history. The reviewer prompt is calibrated so that if it is set loose on these diffs, each is flagged. If any of them slip through, the prompt or the doc list is wrong.

### Case A — the Blackboard Vue page (**extreme**)

**Ground truth**: PR #1070 landed as commit `19738976` (2026-09-20): a new `/blackboard` module. The diff added:

- A bespoke `<ul>`/`<li>` entry list with hand-rolled CSS (~40 LOC).
- An inline `ConfirmButton` slot pattern for deletes.
- A fixed `width: 22rem` list pane with no draggable divider.
- A raw PNG attachment strip with no mark compositing.
- A bespoke root shell with bordered `surface-1` boxes instead of the pipeline flex-column shape.

**All four** had canonical equivalents in the codebase:

| Bespoke thing | Canonical it should have reused |
|---|---|
| Hand-rolled `<ul>` list | `SelectionTable` (used by `NotebookTable`) |
| Inline `ConfirmButton` slot | `ConfirmDeleteButton` |
| Fixed-width list pane | `usePanelResize` composable (used by `TasksModule`, `ChainModule`) |
| Bordered `surface-1` boxes | `ChainModule` flex-column shape |
| Raw PNG thumbnails | `composeImageWithOverlay` in `overlayCompose.ts` (Kiwi PR A pattern) |

**What actually happened**: Dominik reviewed, filed feedback, and the fix landed as **three separate follow-up commits** — `81b6d2e3` (canonical primitives), `7df00c55` (canonical divider), `20b11c7f` (collapsed one-mechanism-not-two). Fix commit message states verbatim: *"I hand-rolled a list + bespoke layout + non-canonical buttons + attachment thumbnails that didn't show the annotations. Rewritten against the frontend inventory."*

**Reviewer expectation**: On the initial `19738976` diff, the reviewer must produce **at least four `should reuse` findings** citing `SelectionTable`, `ConfirmDeleteButton`, `usePanelResize`, `ChainModule` layout, and — likely a fifth — `composeImageWithOverlay`. All five canonicals are named in `docs/ui/PRIMITIVES.md` and/or `docs/inventory/FRONTEND.md`. Missing this case = reviewer is broken.

**Dry-run result (2026-09-26)**: **5/5 CAUGHT.** A fresh sonnet subagent given only the reviewer prompt (§Reviewer prompt above) and `git show 19738976` produced all five `should reuse` findings with direct doc citations (PRIMITIVES.md:27 for SelectionTable, PRIMITIVES.md:41 for ConfirmDeleteButton, PRIMITIVES.md:52-53 for usePanelResize) plus grep-surfaced hits for `composeImageWithOverlay`. **Finding #4 (module shell shape) was the weakest** — no doc line pre-existed naming "new module pages must match `ChainModule`'s toolbar+split shape"; the reviewer inferred it from `ModulePage layout="fill"` consumers. A less-thorough run could miss #4. Recommend closing that gap before wiring the reviewer unattended (see §Inventory gaps worth closing).

**Why the audit's inventory finding matters here**: `SelectionTable` and `ConfirmDeleteButton` are catalogued in `docs/ui/PRIMITIVES.md`, which is currently **not** referenced by any inventory file. If the reviewer prompt says "read inventory" and stops, it misses this case. Hence the requirement (§Reviewer prompt step 3) to read `PRIMITIVES.md` and `COPY.md` explicitly.

### Case B — `pop_df` bypass

**Canonical**: `PopUtils.pop_df` at `python/cecelia/utils/pop_utils.py:22` (Python) and Julia's `pop_df` in `app/src/` — image-owned population dataframe accessor. Docs: `docs/POPULATION.md`, referenced from `python/cecelia/utils/pop_utils.py:5`.

**Failure mode**: an agent writes a new "get all cells for pop X" helper by hand-reading label props and filtering, instead of calling `pop_df`. Common because the reader plausibly doesn't know `pop_df` handles the pooling / dedup rules.

**Reviewer expectation**: any diff adding a function whose body reads label props + filters by pop path/type must flag `pop_df` as the canonical. Grep terms: `pop_df`, `PopUtils`, `pop_type`, `pop_path`. `DATA_ACCESS.md` covers this.

### Case C — hand-rolled h5ad access

**Canonical**: `label_props_utils` and the h5ad helpers in `python/cecelia/utils/atomic_io.py`. Cecelia's rule (per memory + `CLAUDE.md`): all h5ad reads/writes go through the canonical helpers — atomic writes, schema-versioned columns, correct index handling.

**Failure mode**: an agent uses `anndata.read_h5ad` directly, or writes a partial file without atomic-io. Silently breaks concurrent-access invariants.

**Reviewer expectation**: any diff introducing a bare `anndata.read_h5ad(...)` / `.write(...)` call outside `python/cecelia/utils/` must flag the canonical helper. Grep terms: `read_h5ad`, `anndata.AnnData(`, `atomic_io`, `label_props_utils`. `DATA_ACCESS.md` covers this.

### Case D — hand-rolled JSON I/O

**Canonical**: `atomic_io` for reads/writes of sidecar JSON (gating, populations, chains, blackboard entries). Handles atomic write, tmp+rename, schema versioning.

**Failure mode**: an agent uses `json.load(open(...))` / `json.dump(..., open(...))`. Silently breaks the atomic-write invariant and can corrupt sidecars on concurrent access.

**Reviewer expectation**: any diff introducing a bare `json.load` / `json.dump` for a project sidecar path must flag `atomic_io`. Grep terms: `atomic_io`, `read_json`, `write_json`, `json.dump`, `json.load`.

### Spot-check verdicts from the inventory audit

The audit ran the current inventory against three recent commits, giving a preview of reviewer coverage today (before this plan ships):

- `84fc964f` (new panel-resize helper) — **caught** by inventory alone; `usePanelResize` was named.
- `ddaeb0b2` (`POST /api/tracks/by_category`, cloned private `_catkey`) — **out of scope**; private-helper cloning with opaque names, not a public-convention miss. See §Not in scope.
- `d24e18b9` (KiwiRef resolver, first-of-its-kind) — correctly "no equivalent"; inventory upkeep worked in the same commit that shipped it.

Scoped honestly, that's 2-of-2 in-scope + one out-of-scope case named as such. Adding `docs/ui/PRIMITIVES.md` and the case-study calibration extends coverage to the blackboard case.

## Load-bearing dependencies

Two things the plan makes newly load-bearing. Worth stating.

### Ground-truth doc currency

Today `docs/inventory/*.md`, `docs/ui/PRIMITIVES.md`, `docs/ui/COPY.md`, and `docs/PLOTS.md` are norms — nice-to-have, occasionally consulted. Once this reviewer cites them as ground truth for pre-commit gating, stale docs silently miss duplications. The convention check **promotes this set to first-class artifacts**:

- Adding a new canonical helper without updating the relevant inventory file, or a new primitive without updating `PRIMITIVES.md`, becomes its own convention violation. (Not enforced by this reviewer at v1; a future extension could grep whether a new file marked `# Canonical:` in a comment appears in the corresponding doc.)
- The reviewer itself surfaces stale-doc candidates — if it flags a "should reuse" that Dominik overrides because the canonical is stale, that's a doc-update signal.
- The [Case studies](#case-studies) — blackboard, `pop_df`, h5ad, JSON — should stay in the plan doc as a regression-test set. Any change to the reviewer prompt or the doc list must not regress these.

### Pre-commit latency

Sibling-audit ~30s. Convention check ~30s. Total ~60s per commit. If both routinely take longer on real diffs (large multi-file changes), that number climbs. Worth measuring after 5–10 real commits; if latency crosses a pain threshold, options are:

- Skip convention check for diffs under N added lines (small changes are less likely to add duplicated frameworks).
- Run both reviewers in parallel rather than sequentially.
- Fold the addition-hunk pass into the sibling reviewer (single prompt, two search shapes). Costs prompt clarity.

### Cost-gradient check — unaudited

Raised in review (2026-09-26): sibling-audit's case for existence was earned from concrete evidence — three confirmed case-F pairs (#816→#822, #828→#839, #1101→#1151) plus a 3-month PR-trail audit that measured value against overhead. This plan justifies a *second* reviewer largely by **structural analogy** to sibling-audit — same fresh-subagent shape, similar failure class, therefore similar cost/return. That analogy is unaudited: the case-F cost-gradient evidence does not automatically transfer to convention drift.

What that means honestly:

- The four case studies in this plan (blackboard, `pop_df`, h5ad, JSON) are *acceptance criteria* — the reviewer must catch these — but they are not *cost-gradient evidence*. They say "worth catching," not "worth catching at this overhead."
- **The effectiveness log is the answer to this critique.** Once the reviewer runs at N=5–10 real commits, the log rows (catch rate, false-positive rate, `outcome=fixed_pre_commit` fraction, per-run duration) let a similar retrospective audit compute return-per-overhead against a real denominator. That's what earned sibling-audit its keep; the same discipline earns or unearns this reviewer's.
- **Ship-order implication**: this argues for wiring the log BEFORE (or at least alongside) wiring the reviewer, so the first commits under the new reviewer are captured with zero backfill. Aligns with §Open decisions #2.

Not resolved in this plan. Named so it isn't waved through on momentum.

## Wiring

Same shape as sibling-audit — four touchpoints:

1. **This reviewer prompt** — lives at `docs/ai-assist/CONVENTION_CHECK.md` once shipped (parallel to `SIBLING_CALL_AUDIT.md`).
2. **CLAUDE.md §*Git & commits*** — the reservations section already requires the sibling tail line; add convention-check tail line + evidence fold with parallel enforcement.
3. **Pre-commit hook (local)** — verifies both tail lines and the evidence folds. Missing either = commit fails with a message directing the agent to run the reviewer.
4. **PR CI check (remote)** — same verification, applied to the reservations section of the PR body or the tip-of-branch commit message. GitHub Actions workflow; required check for merge.

Attribution: as the effectiveness log plan notes, if this reviewer's output eventually appears on a public GitHub page (as data), the attribution question ("Claude ran this reviewer") should be handled the same as other public Claude-authored artifacts.

## Effectiveness log integration

New event type in the effectiveness log schema (see [`EFFECTIVENESS_LOG_PLAN.md`](EFFECTIVENESS_LOG_PLAN.md) §Event taxonomy):

| `event` | When emitted | Payload fields |
|---|---|---|
| `convention_check_run` | Every time reviewer runs | `additions_reviewed: int`, `duration_s: float`, `escape_valve: null \| "docs_only" \| "no_additions" \| "tests_only" \| "no_additions_worth_checking"` |
| `convention_check_finding` | Per finding | `verdict: "should_reuse" \| "potential_duplicate"`, `file: str`, `line: int`, `added_symbol: str`, `canonical_symbol: str \| null`, `outcome: <closed list from EFFECTIVENESS>` |

Same outcome vocabulary as sibling findings (`fixed_pre_commit`, `shipped_with_finding`, `false_positive`, `dropped_no_action`).

This closes the gap flagged in the effectiveness plan's [Ceiling](EFFECTIVENESS_LOG_PLAN.md#ceiling--what-this-cannot-measure): sibling covers case-F fix drift only, so a broader "we catch drift" claim is currently unenforced. With this reviewer wired up + logged, the broader claim earns its name.

## Open decisions — Dominik's call

1. ~~**Inventory scope**~~: answered by the companion audit (2026-09-26). Confirmed doc list is in §Inventory scope; ground-truth set spans `docs/inventory/*.md` + `docs/ui/PRIMITIVES.md` + `docs/ui/COPY.md` + `docs/PLOTS.md` + module `CLAUDE.md` files.
2. ~~**Ship order**~~: **log first (or alongside).** The cost-gradient section makes the argument — without the log wired, the first N reviewer runs go unmeasured and the "worth 60s/commit" claim can't be earned back with data.
3. ~~**Escape valve for tests-only diffs**~~: **as drafted** — skip pure `test_**` / `tests/**` / `*_test.jl` diffs; fixtures are legitimately local. Watch for the specific failure of a fixture graduating into shipped code — if it happens once, tighten.
4. ~~**PR CI check severity**~~: **advisory first, hard-required once measured.** Reasoning: an unmeasured reviewer produces calibration false positives; if those block merges from day one, the fix-up habit becomes "just override," which mutes the guard. Advisory keeps findings in the reservations recital + log while calibration runs; flip to required when the log shows FP rate is tolerable. **The local pre-commit hook that forces the reviewer to RUN stays hard from day one — different teeth: mechanism vs verdict.**
5. ~~**Reviewer prompt v1 — dry-run against the blackboard case first?**~~ **Done 2026-09-26: dry-run passed 5/5.** Sonnet subagent given the prompt verbatim + `git show 19738976` produced all five predicted `should reuse` findings with direct doc citations. Finding #4 (module shell shape) is the one thin place, addressed by §Inventory gaps worth closing item 3.
6. ~~**Latency threshold**~~: **90 seconds combined pre-commit** as the pain floor. Under 90s = tolerable; over = investigate a mitigation from §Pre-commit latency.
7. ~~**Close the two inventory gaps first?**~~ **Yes — do them before wiring.** Both are one-liners (§Inventory gaps worth closing items 1 and 2). Item 3 (module-shell convention) is being closed on branch `docs/module-shell-convention` (#1245).

## Not in scope (this plan)

- **Cross-module private-helper cloning** (e.g. the `_catkey` spot-check from the inventory audit). Different failure class: opaque names, no synonym-grep signal, module-internal by intent. Would need semantic-similarity indexing to detect reliably. Named on the effectiveness-log ceiling as an unmeasured miss class.
- Wiring inventory-currency enforcement (a reviewer that checks whether new canonical helpers land in inventory). Follow-up.
- Cross-language conventions (a Julia helper and a Python helper doing the same job in two languages). Reviewer runs per-domain; cross-domain duplication is a harder search shape.
- Retrofit onto existing code. This is a going-forward pre-commit gate, not a codebase-wide sweep.

## Prior art in this repo

- [`docs/ai-assist/SIBLING_CALL_AUDIT.md`](../ai-assist/SIBLING_CALL_AUDIT.md) — the fix-drift reviewer this plan parallels.
- [`docs/todo/EFFECTIVENESS_LOG_PLAN.md`](EFFECTIVENESS_LOG_PLAN.md) — the measurement layer both reviewers feed.
- [`docs/inventory/*.md`](../inventory/) — the ground truth this reviewer cites.
- `CLAUDE.md` §*Git & commits* — where the reservations discipline and tail-line enforcement live.
