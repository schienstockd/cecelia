# CLAUDE.md compliance eval — plugin-eval port plan

**Status:** P0 spike + P1 catalog port SHIPPED (2026-09-28) on
`feat/plugin-eval-port-spike`. P2 (`discovery-first`), P2.5 (indirect-cue variants),
P3 (ablation task), P4 (sunset) pending. Supersedes the "tool-log inspection scorer"
deferral note in [`CLAUDE_MD_EVAL_PLAN.md`](CLAUDE_MD_EVAL_PLAN.md).

## Goal

Port the bespoke eval runner (`scripts/claude_md_eval/run_prompt.py` + `run_suite.py`) to
Anthropic's built-in `claude plugin eval` harness. Two reasons:

1. **Unblocks `discovery-first`** — the one prompt deferred from the P1 catalog because its
   rule is only visible in the tool log, not the diff. Plugin-eval ships a `tool_order` grader
   that implements this directly (zero custom code).
2. **Unlocks ablation** — `claude plugin eval --ablation with-without` re-runs each prompt
   with CLAUDE.md stripped and reports the delta. Answers "does CLAUDE.md's compliance text
   actually change agent behaviour?" — the whole premise of the compliance-eval project, and
   something no amount of bespoke plumbing gives us for free.

Everything else — the 9 shipped prompts, the effectiveness-log emission, the recital/rollup
pipeline — stays.

## Why not the smaller bolt-on

Alternative considered: add a transcript-jsonl reader for `discovery-first` alone (grade
whether a `Grep`/`Read` on `docs/inventory/*` fired before the first `Write`). Same yes/no
per run for that one prompt.

Rejected because: (a) two eval frameworks maintained side-by-side, (b) Claude's transcript
jsonl is an internal format with no compatibility promise, (c) any future rule shape (soft
LLM grade, file-existence check, regression baseline) needs its own bolt-on, (d) ablation
is a real new insight class the bolt-on cannot deliver at any cost.

## Locked decisions

**D1 — one framework, not two.** Retire `run_prompt.py` and `run_suite.py` once every prompt
is ported. No hybrid.

**D2 — keep the effectiveness-log emission shape.** Wrap `claude plugin eval --json` in a
thin adapter that reshapes plugin-eval's output into the existing `claude_md_eval_run` /
`claude_md_eval_pass` / `claude_md_eval_suite` event schema. Nothing downstream of
`~/.cecelia-effectiveness/events.jsonl` changes.

**D3 — spike first.** Port `h5ad-read` alone (strongest baseline, 3/3 compliant, minimal
moving parts) and verify the adapter round-trip end-to-end before touching the other 8. If
sandbox / worktree / `.env` behaviour mismatches, learn cheap.

**D4 — `discovery-first` is the acceptance test for the port.** Added only after all 9
existing prompts are ported. If plugin-eval's `tool_order` grader can't express the rule
cleanly, the port is broken — rethink, don't ship the port and defer discovery-first again.

**D5 — ablation via scaffold-swap, NOT plugin-eval's `--ablation with-without`.**
Plugin-eval's built-in ablation flips *the plugin*, not CLAUDE.md — CLAUDE.md is
classified as project-level and excluded from both arms by design (confirmed by
`claude-code-guide` subagent, 2026-09-28). To ablate CLAUDE.md, we'd have to carve
its compliance sections out into a plugin skill, which (a) downgrades those sections
from always-loaded prose to advertised-may-invoke skills — different rule shape, and
(b) is a substantial CLAUDE.md restructure that shouldn't ride the eval port.

Instead: our scaffold script (`setup.sh`) reads `EVAL_CLAUDE_MD_ARM=with|without` from
the sandbox env, copies the full CLAUDE.md into the sandbox for `with`, copies nothing
for `without`. `pixi run claude-md-eval-ablation` fires two suite runs with the two env
values and computes the delta ourselves. Emits `claude_md_eval_ablation` rows to the
effectiveness log.

**D6 — `tool_used: Skill` deferred.** Only meaningful if `discovery-first` ever becomes a
skill (open question — see [`CLAUDE_MD_EVAL_PLAN.md`](CLAUDE_MD_EVAL_PLAN.md)). Wire the
grader shape when a skill lands, not preemptively.

**D7 — the 2026-09-28 utf-8-json-write scorer widening carries.** The regex now recognises
three compliant idioms (inline `encoding="utf-8"`, explicit `.encode("utf-8")`, canonical
helper family). Port those alternatives as-is into the plugin-eval `regex` grader.

**D8 — the `without` ablation arm copies no CLAUDE.md at all — binary, not truncated.**
Full-or-empty avoids the fuzz of "which sections do we strip?" Every stripped variant
would need an explicit accounting of what's kept vs cut (e.g. the doc-index table points
at `docs/inventory/*.md` and routes some discovery-first behaviour even if the "mandatory
discovery step" section is stripped). One boolean is self-explanatory; deltas attribute
to CLAUDE.md as a whole. Per-section stripping is a follow-up instrument gated on the
broad ablation showing non-trivial delta.

**D9 — fixture scope: whole-repo `git clone` (spike-resolved).** `git clone` from the
source repo populates a 30MB working tree in ~0.7s per run (via `file://` prefix to
force `--depth=1` — `git clone --local` silently ignores shallow-clone). Whole-repo
clone avoids predict-and-prune (which would game discovery-first) and cost is
negligible. Full ablation suite = 9 cases × 3 runs × 2 arms × ~30MB = ~1.6GB of tmp
churn; acceptable.

**D10 — pass `--ablation none` explicitly (spike-learned).** Plugin-eval defaults to
`--ablation with-without` whenever a `plugin.json` resolves — it runs both arms
automatically (with-plugin-loaded vs without-plugin-loaded, flipping the plugin, NOT
CLAUDE.md). Our scaffold-swap ablation (D5) is orthogonal, and the two would run
compounded (4 arms per case) if we don't suppress plugin-eval's built-in ablation. The
adapter passes `--ablation none` for every invocation.

**D11 — only the `CLAUDE_CODE_*` env-var prefix passes into the sandbox
(spike-learned).** Docs suggested `EVAL_*` also passes through; measurement disagrees.
Our scaffold reads `CLAUDE_CODE_EVAL_CLAUDE_MD_ARM` and
`CLAUDE_CODE_EVAL_SOURCE_REPO`. The `_SOURCE_REPO` var has a fallback: the scaffold
resolves its own on-disk path via `readlink -f "${BASH_SOURCE[0]}"` and walks up 2
dirs to the plugin root, so out-of-tree invocations still work without setting the
env var.

**D12 — N≥3 runs minimum before quoting any Δ anywhere (2026-09-28 Sonnet-flagged).**
One-run signals like the P0 spike's Δ=1 (with-arm compliant, without-arm noncompliant)
are smoke-test evidence *that the mechanism works*, not evidence that a specific rule
pulls weight. The port's default `--runs 3` matches the bespoke runner; ablation runs
report per-arm mean + N.

**D13 — prompt-cue discipline: direct-cue and indirect-cue variants
(2026-09-28 Sonnet-flagged, follow-up).** The P1 prompts inherit the bespoke catalog's
domain-loaded phrasing (`.h5ad`, `label`, `live.cell.speed` for h5ad-read; `_kill_tree`
absent but `kill process tree` in the ask). Casual/indirect-cue prompts ("I want to
display cell speeds in a notebook") test whether the rule survives when the domain
term isn't handed to the agent. Not shipped in the port — deferred as **P2.5**: for
each rule, ship an indirect-cue variant alongside the direct one, tag them
(`tags: [direct-cue]` vs `[indirect-cue]`), report compliance separately. Load-bearing
enough that any Δ claim about a specific rule's effectiveness should split direct vs
indirect.

## Phases

- **P0 spike SHIPPED** (2026-09-28) — ported `h5ad-read`, wrote adapter
  (`scripts/claude_md_eval/run_plugin_eval.py`), ran end-to-end via
  `claude plugin eval`. Sandbox self-populates via scaffold (git-clone the source
  repo, ~0.7s per run). Adapter smoke-test on captured JSON emits the same
  `claude_md_eval_run` + `_pass` event schema as the bespoke runner. Ablation-swap
  produces visible behavioural delta (with=compliant, without=noncompliant on 1 run).
  Gate passed at N=1; N=3 confirmation deferred to first suite pass.

- **P1 port the other 8 SHIPPED** (2026-09-28) — `cite-algorithm`, `dir-size`,
  `h5ad-write`, `kill-process-tree`, `spawn-python`, `utf-8-json-write` (with D7
  widening), `zarr-read`, `zarr-write`. Case files under `evals/<id>/`; shared
  scaffold at `evals/_shared/setup.sh` symlinked from every case. Suite driver at
  `scripts/claude_md_eval/run_plugin_eval_suite.py`, `pixi run claude-md-eval-plugin`.
  Bespoke runner (`run_prompt.py` / `run_suite.py`) intact — P4 sunset once parity
  confirmed on a live pass.

- **P2 add `discovery-first` (~1 hr)** — new eval with
  `tool_order: {before: {tool: Grep, input_match: "inventory"}, after: {tool: Write}}`.
  **Gate: manually verify at least one baseline run passes (agent actually does the
  discovery) and at least one adversarial run fails (agent that writes-first is
  flagged).**

- **P2.5 indirect-cue variants (~half day)** — D13 follow-up. For each rule ship an
  indirect-cue variant alongside the direct-cue port; tag with
  `[direct-cue]` / `[indirect-cue]`; report separately. Only reason a "does the h5ad
  section pull weight?" claim survives Sonnet's critique — the P1 direct-cue prompts
  overstate compliance by handing the agent the domain terms.

- **P2.6 rollup rendering (open — needs decision at build time)** — the general
  effectiveness rollup (`python/cecelia/effectiveness/rollup.py` →
  `docs/ai-assist/EFFECTIVENESS.md`) already exists but doesn't render
  `claude_md_eval_*` events. Two shape decisions to make together:
  (a) **rendering target** — dedicated `docs/ai-assist/CLAUDE_MD_EVAL.md` page (parent
  plan's original design, pre-dates general rollup existing) vs section-within-
  EFFECTIVENESS.md (unified rollup, may now be cleaner);
  (b) **source segmentation** — add `source: "eval"` field on emitted rows so
  cost-summing can separate real-work API spend from eval-run API spend. Otherwise
  `cost_usd` totals mix mechanisms once real-work events start carrying cost.
  Both are cheap adds. Neither belonged in the port PR (they're consumer decisions);
  land them together when the eval-events consumer is built.

- **P3 ablation as separate pixi task (~1 hr)** — `pixi run claude-md-eval-ablation`
  fires `claude-md-eval-plugin --arm with` and `--arm without` back-to-back, computes
  per-prompt delta, emits `claude_md_eval_ablation` rows to
  `~/.cecelia-effectiveness/events.jsonl`. **Gate: first pass produces per-prompt
  with/without deltas; interpret with the user before committing to any rollup shape.
  Δ quoted only at N≥3 per arm (D12).**

- **P4 sunset (~1 hr)** — delete `run_prompt.py`, `run_suite.py`,
  `scripts/claude_md_eval/prompts/*.md`,
  `python/cecelia/tests/test_claude_md_eval*.py`. Rename
  `claude-md-eval-plugin` → `claude-md-eval`. Update `CLAUDE_MD_EVAL_PLAN.md` status
  ("superseded, see PORT plan"). Update `pixi.toml` tasks. Update
  `docs/todo/README.md` row for the parent plan; add sunset note here. **Gate: at
  least one live suite pass on plugin-eval + one live pass on the bespoke runner on
  the same CLAUDE.md blob within noise of each other — parity check.**

Total: ~1.5 days of clean-slate coding for P0–P1 (delivered 2026-09-28); ~1 more day
across P2 + P3 + P4.

## What we're keeping

- **The 9 prompts** — text stays, relocated to `evals/<id>/prompt.md`.
- **The regex scorer signals** — reshape as plugin-eval `regex` graders. The 2026-09-28
  utf-8 widening carries.
- **The effectiveness-log schema** — `claude_md_eval_run`, `_pass`, `_suite` unchanged.
  Adapter reshapes plugin-eval's output into that schema.
- **The CLAUDE.md blob-SHA anchoring** — still the row-level `commit` field.
- **Branch capture** — same helper (`git_context.current_branch`), applied in the adapter.

## What we're retiring

- `scripts/claude_md_eval/run_prompt.py` (~330 lines).
- `scripts/claude_md_eval/run_suite.py` (~180 lines).
- `python/cecelia/tests/test_claude_md_eval.py`.
- `python/cecelia/tests/test_claude_md_eval_suite.py`.
- Bespoke worktree management (plugin-eval handles its own sandbox).
- Bespoke prompt-frontmatter parser (`_FRONTMATTER_RE`).

## Risks

- **Sandbox mismatch** — plugin-eval's sandbox model may not copy `.env`, may re-pair the
  running Cecelia project, or may not respect `git worktree add --detach`. Load-bearing;
  P0 spike gates before any further port work.
- **Event-schema adapter drift** — plugin-eval's `--json` output shape may change between
  Claude Code versions. Mitigation: pin the adapter to a documented output field set, add
  a smoke test that parses a fixture.
- **`tool_order` semantics different from what we want** — plugin-eval matches on tool-call
  names + `input_match` regex, not on directory globs. `docs/inventory/*.md` check needs
  `input_match: "inventory"` (approximate). If wrong, D4 fires — port is broken.
- **Ablation cost** — doubles wall clock per run. Mitigation: separate task, not run by
  default (D5).

## References

- `claude plugin eval` added Claude Code v2.1.269. Grader types: `regex`, `tool_used`,
  `tool_order`, `file_exists`, `llm`, `baseline`. Confirmed by a `claude-code-guide`
  subagent lookup, 2026-09-28.
- Supersedes [`CLAUDE_MD_EVAL_PLAN.md`](CLAUDE_MD_EVAL_PLAN.md) → *One prompt still
  deferred: discovery-first* (delete that section on sunset).
- Companion to [`CLAUDE_MD_ENFORCEMENT_PLAN.md`](CLAUDE_MD_ENFORCEMENT_PLAN.md) (the
  reason the eval exists).
