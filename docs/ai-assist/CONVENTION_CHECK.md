# Convention-check — the pre-commit anti-duplication reviewer

**What this file is:** the exact prompt the pre-commit convention-check reviewer subagent is spawned with. Also the mechanism note and the escape valves. Cited from [`CLAUDE.md`](../../CLAUDE.md) → *Git & commits*. Companion to [`SIBLING_CALL_AUDIT.md`](SIBLING_CALL_AUDIT.md) — same shape, different job.

**Why this exists:** catch **convention drift** — a new helper, component, endpoint, or accessor that duplicates an existing canonical implementation, or that skips an existing framework. The two real cases the plan calibrated on: [PR #1070](https://github.com/schienstockd/cecelia/pull/1070)'s Blackboard page hand-rolled `<ul>` / bespoke buttons / fixed-width pane instead of `SelectionTable` / `ConfirmDeleteButton` / `usePanelResize` — three follow-up fix commits rewrote it. Sibling-call audit does not catch this class (it hunts fix drift outward from a hunk, not addition-drift inward from inventory). Design record: [`../todo/CONVENTION_CHECK_PLAN.md`](../todo/CONVENTION_CHECK_PLAN.md).

## How it runs

At the pre-commit reservations step, the implementing agent spawns a fresh subagent — parallel to the sibling-call audit, same shape:

```
Agent(
  subagent_type: "general-purpose",
  model: "sonnet",
  prompt: <contents of the "Reviewer prompt" section below>
          + "\n\n---\n\n" + <git diff --staged>
)
```

The subagent's reply is used the same way the sibling's is:

1. **Woven into the reservations list** — `**should reuse**` findings become reservation items ranked by how much they matter; `**potential duplicate**` folds in with hedging.
2. **Printed verbatim as evidence** under a `_Convention check (evidence):_` fold after the list — so each woven item cites its source and the user can verify the finding wasn't dropped or misrepresented. Missing evidence fold = the check silently didn't run.
3. **Tail line** — `_Convention check: run_` (or a skip line) — the leading indicator that the check happened. Missing tail = mechanism went dark.

**Per-finding outcome tag — enforced by a pre-commit hook.** Every `**should reuse**` convention finding must carry an outcome from the closed vocabulary at the end of the line, in square brackets: `[fixed_pre_commit]` / `[shipped_with_finding: <reason>]` / `[false_positive: <reason>]` / `[dropped_no_action: <reason>]`. `.claude/hooks/check_commit_recital.py` grep-checks the commit message and blocks if any finding lacks an outcome. See root [`CLAUDE.md`](../../CLAUDE.md) → *Git & commits* for the full rationale.

**Model = sonnet, not Opus.** Same reasoning as sibling — many small independent reads (diff → identify additions → grep inventory + repo), not one deep reasoning chain.

**Log emission** — done automatically by `pixi run recital` (`python/cecelia/effectiveness/recital.py`) which is the standard invocation path (see root [`CLAUDE.md`](../../CLAUDE.md) → *Git & commits*). Recital emits a `convention_check_run` event with `duration_s` (and `error` on failure) atomically with the reviewer spawn, so the parent agent can't silently skip. The full payload documented in [`EFFECTIVENESS_METHODOLOGY.md`](EFFECTIVENESS_METHODOLOGY.md) (`additions_reviewed`, `escape_valve`, `cited_doc_refs`) is not populated in v1 — parsing the reviewer output for those fields is deferred. Findings emission (`convention_check_finding`) also deferred — needs outcome-resolution design (a finding only carries meaningful signal once labelled `fixed_pre_commit` / `false_positive` / etc.). Rows accumulate in `~/.cecelia-effectiveness/events.jsonl` for later rollup and usage-weighted spot-check.

Manual emission via `python scripts/log_event.py` remains available for ad-hoc / retrospective rows.

## Escape valves — skip the subagent when

- Diff is docs-only (only `docs/**`, `*.md`, `CLAUDE.md` files touched). Tail: `_Convention check: skipped — docs-only diff_`.
- Diff has no addition-shaped hunks (only modifications / deletions / renames). Tail: `_Convention check: skipped — no additions_`.
- Diff is tests-only (only `test_**`, `tests/**`, `*.test.*`, `*_test.jl`). Tests are allowed to hand-roll fixtures. Tail: `_Convention check: skipped — tests-only_`.

Otherwise spawn. The prompt's own short-circuit handles the "additions present but all trivial" case (tail becomes `_no convention check needed_`, no evidence fold).

## Reviewer prompt

*(Everything below the horizontal rule is passed verbatim as the subagent's prompt, followed by the staged diff.)*

---

You have full read access to the repo (do not modify). `git diff --staged` follows the `---`.

**Job:** catch **convention drift** — a new helper, component, endpoint, or accessor that duplicates an existing canonical implementation, or that skips an existing framework. You surface candidates; you do not review code.

**Per addition-shaped hunk:**

1. Name the symbol added (`file:line`) — function, class, Vue component, endpoint handler, MCP tool, exported helper. Also count as additions: new dataframe accessors, new h5ad readers/writers, new JSON readers/writers, new zarr accessors.
2. Identify its **domain**: backend Julia (`.jl` under `app/src/` or `api/src/`), backend Python (`python/cecelia/**`), frontend (`frontend/src/**/*.vue`, `frontend/src/**/*.ts`), API surface (REPL / MCP / HTTP handler), plots / analysis-board panel, task-authoring.
3. **Read the docs for that domain** — the ground-truth set is broader than `docs/inventory/`:
   - Frontend additions: `docs/inventory/FRONTEND.md` AND `docs/ui/PRIMITIVES.md` AND `docs/ui/COPY.md` AND `frontend/CLAUDE.md`. All four. A bespoke button that skips `PRIMITIVES.md` is the exact failure mode this reviewer exists to catch.
   - Plot / analysis-board panels: `docs/PLOTS.md` (the "don't hand-roll a panel, register via `INTERACTIVE_VIEWS` or `CLUSTER_PANELS`" doc) + `docs/inventory/PLOTS.md`.
   - Backend Julia additions: `docs/inventory/JULIA_API.md`, `JULIA_APP.md`, `FLOWS.md`, `fingerprint_extractors.md`, plus root `CLAUDE.md` and `app/CLAUDE.md`.
   - Backend Python additions: `docs/inventory/PYTHON.md`, `DATA_ACCESS.md`, plus root `CLAUDE.md`.
   - Task-authoring: `docs/MODULES.md`.
   - API surface: `docs/inventory/MCP.md`, `JULIA_API.md`.
4. `grep` the repo for functional near-equivalents. Inventory is a floor, not a ceiling — a canonical public helper may exist in code but not yet be catalogued. Grep source directly before claiming no equivalent. Use synonyms, not just the exact name. A new `IconToggle` greps `Toggle`, `Switch`, `IconButton`, `AppToggle`; a new population accessor greps `pop_df`, `PopUtils`, `pop_type`; a new h5ad reader greps `read_h5ad`, `anndata`, `label_props_utils`; a new JSON reader greps `atomic_io`, `read_json`, `load_json`. Do NOT hunt for cloned private helpers with opaque names (`_catkey`, `_impl`) — that class is out of scope.
5. If a canonical equivalent exists, flag as **should reuse**. If none exists but shape is suspicious, flag as **potential duplicate** (say what would confirm).

**Addition-shaped** = new named entity contributing to the public or intra-module API. Not: pure renames, moves without semantic change, refactors, test fixtures, generated code, formatting, docs.

**Output** — one line per finding, most severe first, ≤300 words total:

```
- **file:line** — added `symbol`, closest canonical `existing.symbol` (path), why potentially duplicate [**should reuse** | **potential duplicate**]
```

- **should reuse** — you read the canonical and it fits the addition's purpose.
- **potential duplicate** — same domain, similar shape, fit unverified or ambiguous.

**Evidence to include** — for every finding AND for every addition that got a clean bill of health:
- Which inventory / ground-truth files you read (paths, and section names or line numbers where relevant — these become `cited_doc_refs` in the effectiveness log).
- Which grep terms you tried.
- Which candidate near-equivalents you looked at and rejected (path, one-line reason).

Empty evidence fold on a "no findings" reply = failed check. Fail loud rather than silently pass.

**Short-circuit**: if the diff contains no addition-shaped hunks, reply exactly `_no convention check needed_` — no evidence fold.

**Don't:** suggest fixes; re-review the diff for its own bugs; flag cross-module private-helper cloning (out of scope); fabricate a canonical (say so if grep is empty).
