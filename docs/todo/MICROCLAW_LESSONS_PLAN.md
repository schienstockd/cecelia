# MicroClaw lessons — what our Claude integration takes from it, and what it doesn't

**Status:** **P1–P3 built** (2026-10-27, branch `docs/microclaw-lessons`); **O1, O2 parked**. Of eight
candidate lessons, five were checked against our code and real runs and are **not needed**. The three
small changes they left (P1–P3) are built. The two open checks (O1, O2) cost quota and wait on
Dominik's call.

## Goal

MicroClaw ([Marin et al., bioRxiv 10.64898/2026.09.15.751723](https://doi.org/10.64898/2026.09.15.751723);
code at [Micro-Claw/microclaw](https://github.com/Micro-Claw/microclaw)) is a Claude agent that drives
microscopes through Micro-Manager. It shares our problems: an LLM working inside a scientific tool,
sourcing its claims, using lab knowledge, and leaving something repeatable behind. This doc records
which of its patterns we checked, what we found in our own code and runs, and the few that are worth
building.

## Provenance — read before citing anything here

- **Who read MicroClaw.** Claude agents only: five Opus agents split the repo by area, at
  `bb3e123` (2026-10-19). Claude spot-checked the key claims against the source. Nobody ran
  MicroClaw, and Dominik has not read it. Everything here about MicroClaw is **an agent's reading**.
  Don't repeat it as a fact about their software.
- **Who checked Cecelia.** One Sonnet agent per item, reading `origin/main` at `5565e179`, the 15 app-tier
  agent runs in `~/.cecelia-effectiveness/app-runs/` (2026-10-04 → 10-09), and 3,002 Claude Code
  transcripts under `~/.claude/projects/` (for tool-result sizes). Claims marked ✔ were re-checked by
  the coordinating session against the code. Unmarked ones are the agent's reading.
- **Not in scope.** Bugs in MicroClaw itself. Dominik's call: we don't report them, because he would only be
  relaying an agent's reading of code he hasn't seen.

## Verdicts

| # | MicroClaw pattern | Cecelia today | Verdict |
|---|---|---|---|
| 1 | Inject the knowledge base into every prompt; add a gap-driven "ask about unanswered setup" block | `get_session_briefing` already pushes `knowledge` entries **first** (✔ `mcp/cecelia_mcp/server.py:1107-1116`, titles only, max 8). In the 3 knowledge-seeded runs the agent read all 3 entries right after the briefing (NIGHT2_NOTES.md). `newProject` guidance already asks about missing Subject/Goal (`guidance.py:355-364`) | **Not needed** for MCP or autonomous runs. **Kiwi unknown → O1** |
| 2 | Error hints that classify the failure and say whether a retry can help | Errors reach the model flagged `is_error` (`server.py:120-133`). Across 15 runs there were **0** identical repeated failing calls; every repeated error class was fixed in one step. Many routes already list valid alternatives | **Not needed** as an envelope. **P2** covers one gap |
| 3 | Results that say when they were cut short | Most list tools already return `total`/`truncated`. **Two silent caps:** `get_task_history` cuts to `limit` and reports only `count` (✔ `api/src/routes/image.jl:535-536`); `get_task_log` returns the whole log file (✔ `server.py:300-303`, no tail). The only observed overflow (203k `get_measure_summary`) is already fixed | **Partial → P1** |
| 4 | Always include "unknown" rather than omitting a field; stamp numbers with what they're comparable within | Physical sizes are sent as nullable, not defaulted (✔ `api/src/routes/helpers.jl:213-218`). `img_is_calibrated` separates "1 µm/px" from "uncalibrated". Measure rows carry population, valueName, kind and n (`app/src/ai/measures.jl:81-110`). Kiwi claims carry typed refs checked in code. No observed case of an invented value | **Not needed** |
| 5 | Routing hints ("load guide X first") in the description of the tool where the guide applies | Guides are announced in the briefing only. `run_task`/`run_chain`/`create_chain` don't mention `get_guide`. **No unprompted run has ever had `get_guide`**: the tool shipped after the unprompted nights, and the guide runs were told to use a guide | **Can't tell → O2** |
| 6 | Export a session to a deterministic artifact (their script) — our version would be "run → chain" | Repeat is already covered: Copy-params per task (`CopyFunParamsModal.vue`), chain-over-any-image-set, resume by `params_hash`. Autonomous runs are chain-first (`AGENT_OVERNIGHT_PLAN.md:217-226`). No evidence anyone rebuilt a chain by hand from what they ran | **Not needed** |
| 7 | Process: (a) a new test counts only after you watch it fail on the pre-fix tree; (b) PASS / FAIL / NOT EXERCISED; (c) fakes copied from the dependency's real output | (a) only as a memory rule for guards. (b) already handled where it bit (judge usage-limit fixes; recital `skipped —` tails). (c) Root `CLAUDE.md` wants golden values from reference implementations, but the board-authoring fixture was invented to match the parser **three times** | (a) **P3** · (b) **not needed** · (c) **P3** |
| 8 | Replay tests that assert tool-call order | The briefing was call 1–2 in 15/15 runs. The real failures (NIGHT3_NOTES.md) were about content, not order. Traces already keep call order (`scripts/agent_eval/trace_view.py`), so an offline scorer would be about 20 lines at $0 if ever wanted | **Not needed** |

**Correction to the first read-through.** The first summary said Kiwi and the briefing only *pull* knowledge
and that this was "a likely reason guides go unused". The briefing half was wrong: knowledge is pushed into
the first tool result. The guides half was never evidenced (see row 5).

## Decisions (proposed, Dominik's call)

1. **Don't port MicroClaw's knowledge injection, error envelope, blanket "unknown" fields, session export
   or replay harness.** Each was checked above and either already covered or has no observed failure behind it.
2. **Port only what fixes a hole we can point at:** the two silent result caps (P1) and the
   population-not-found 404 (P2).
3. **CLAUDE.md lines (P3) are a process change** and are his call. They are not part of P1/P2.
4. **The open checks cost quota (O1, O2), so they run only on his say-so**, smallest first, one at a time.

## Phases (each ships alone)

**Built 2026-10-27.** P1: `/api/tasks/history` returns `total` + `truncated`, and `get_task_history` passes
them through as `{count, total, truncated, history}`. `/api/images/tasklog` takes `maxBytes`
(`_tasklog_tail` keeps whole lines) and returns `totalBytes` + `truncated`. `get_task_log` sends 20 000 by
default and says so in a first line when it cut. P2: `_pop_not_found` in `api/src/gating_api.jl` replaces all
10 sites. P3: two lines under *Testing* in the root `CLAUDE.md`. The P2 test was watched failing on
the pre-fix routes. The P1 tests assert fields the old routes don't return.

**P1 — say when a result was cut short.** Two tools:
- `get_task_history`: add `total` and `truncated` to the reply, and one line in the tool docstring.
  Touches `api/src/routes/image.jl` (≈3 lines) and `mcp/cecelia_mcp/server.py`.
- `get_task_log`: add an optional tail (`maxChars` or `tailLines`, with a default), reply `truncated` + `totalChars`.
  Touches `api/src/routes/image.jl`, `mcp/cecelia_mcp/client.py`, `server.py`, and `guidance.py` if the tool
  is mentioned there.
- Observed sizes are small today (max 18.7k chars for the log, 16.4k for history), so this guards against
  growth rather than fixing breakage. Test: a Julia route test with more rows or lines than the cap.

**P2 — the population 404 points somewhere.** "Population not found: $pop" appears at 10 sites in
`api/src/gating_api.jl` + `api/src/gating_views_api.jl` and names no alternatives. Route them through one helper
that adds "see get_populations" and up to 8 nearest valid paths. It's the same enrichment already applied to
`gate_plot` and `get_cohort_qc`. Seen once in the traces (`/P14qc/_tracked` then `/_tracked`, run
20261006T031851). Risk: tests that assert the exact string.

**P3 — two CLAUDE.md Testing lines** (≈1 line each):
- "For a bug fix, run the new test against the pre-fix tree and confirm it fails for the stated reason."
- "A fixture or fake for a parser, API or library is copied from the dependency's real output, not written
  from the caller's assumptions."

No recital check. A reviewer can't cheaply run the old tree, and spotting a hand-written fixture isn't
mechanical.

## Open checks

- **O1 — does Kiwi ever see lab knowledge?** Kiwi replaces the system prompt
  (✔ `api/src/kiwi_turn.jl:540`, `replace_system_prompt = true`, `strict_mcp = true`). Its prompt
  (`kiwi_turn.jl:93-154`) never mentions the briefing or the blackboard. `get_session_briefing` is only on the
  allow-list (`:25`). Unverified: whether the MCP server's "call the briefing first" instruction survives a
  replaced system prompt. No Kiwi engine transcripts were found on disk to check. **Decides it:** one Kiwi turn
  on a project with a `knowledge` entry, then read its trace for a briefing or blackboard call (≈1 turn of quota).
  If it never looks, the fix is one sentence in Kiwi's prompt, not an injection channel.
- **O2 — does an unprompted agent find the guides?** **Decides it:** one unprompted run ("track the cells…")
  with `get_guide` available, then check whether the trace calls it before `run_task`/`run_chain`. This is held by
  the guide-runs park ([`GUIDE_RUNS_PLAN.md`](GUIDE_RUNS_PLAN.md)) until Cleanup facts P1–P3 ship. If it skips,
  the smallest fix is one generic sentence ("task parameters and pipeline recipes: get_guide") in those
  docstrings. It must never name a guide or a data type: NO BREADCRUMBS.

## Worth reading in MicroClaw anyway

The design notes are the best part of the repo. Each is a postmortem of one real session that quotes the
transcript by message number, checks whether the previous fix held, and names the general failure behind
the specific one:

- [design/20 — tables that look sourced](https://github.com/Micro-Claw/microclaw/blob/main/design/20-tables-that-look-sourced.md)
  (written in [#16](https://github.com/Micro-Claw/microclaw/pull/16), built in [#17](https://github.com/Micro-Claw/microclaw/pull/17))
- [design/21 — sourced, and still wrong](https://github.com/Micro-Claw/microclaw/blob/main/design/21-sourced-and-still-wrong.md)
  (written in #17, built in [#18](https://github.com/Micro-Claw/microclaw/pull/18))
- [design/22 — correction is not instruction](https://github.com/Micro-Claw/microclaw/blob/main/design/22-correction-is-not-instruction.md)
  (inside #18)
- [design/23 — redundant imaging and tool-bounded thinking](https://github.com/Micro-Claw/microclaw/blob/main/design/23-redundant-imaging-and-tool-bounded-thinking.md)
  (in [#19](https://github.com/Micro-Claw/microclaw/pull/19))

Their conclusions match rules we already hold:
- design/21: a prompt rule had "a measured failure rate of one for one", so fix what the agent is *able* to
  conclude. That is our rule that evals fix the framework, not the prompt, and it is how Kiwi's claims work.
- design/22: "the schema is advice, the gate is the guarantee". Put the rule in the tool description up front, so
  a fresh session doesn't relearn it from a rejection every time. We had already done this, and gone further,
  for the two errors that recurred once per run in our traces: `create_chain` now repairs bad names
  (✔ `server.py:1623-1625`), and `get_cohort_qc` answers composites per step (✔ `server.py:260-263`).
