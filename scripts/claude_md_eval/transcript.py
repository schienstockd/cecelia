"""Parse `claude -p --output-format=stream-json --verbose` stdout.

Design: docs/todo/CLAUDE_MD_EVAL_PLAN.md → *Tool-log signals (2026-09-28 additions)*.

Bespoke-runner counterpart to plugin-eval's `tool_order`/`tool_used` graders. Reads the
jsonl event stream Claude emits on stdout and exposes the two things a compliance eval
needs from it:

- **Ordered tool-call list** — every `tool_use` block from every assistant message, in
  the order Claude emitted them. Powers `tool_order_passes(...)` (was the agent's first
  `Grep(inventory)` before its first `Write`? — the `discovery-first` rule).
- **Terminal `result` event** — final `total_cost_usd` + `num_turns` + `result` text.
  Feeds cost/turns fields on emitted rows so the effectiveness log can render
  "compliance costs 60% more than the bypass" — the pattern Sonnet noted in the
  plugin-eval spike ($0.16 without-arm vs $0.26 with-arm on h5ad-read, N=1).

Not doing the more-general plugin-eval grader menu (`file_exists`, `llm`, `baseline`,
`tool_used`) here — those weren't asked for. Add when a real prompt needs them.
"""
from __future__ import annotations

import json
import re


class TranscriptSignals:
    """Everything a scorer needs from one `claude -p --output-format=stream-json` run.

    Plain class (not a dataclass) — this module is spec-loaded from disk by the runner
    (`importlib.util.spec_from_file_location`), which sets `__module__` to a name that
    isn't in `sys.modules`. `@dataclass` decoration then trips on type resolution
    (`sys.modules.get(cls.__module__).__dict__` returns None).
    """

    def __init__(self, tool_calls: list[dict] | None = None,
                 cost_usd: float = 0.0, turns: int = 0,
                 final_message: str = "", parse_errors: int = 0):
        self.tool_calls: list[dict] = tool_calls if tool_calls is not None else []
        self.cost_usd = cost_usd
        self.turns = turns
        self.final_message = final_message
        self.parse_errors = parse_errors

    def tool_order_passes(self, before_tool: "str | list[str]",
                          before_arg_match: str,
                          after_tool: "str | list[str]") -> bool:
        """True iff ANY `before_tool` call whose args regex-match `before_arg_match`
        appears before the FIRST call to any tool in `after_tool` in the tool-call
        sequence.

        `before_tool` / `after_tool` accept either a single tool name or a list of
        alternatives. Widened for the indirect-prompt tier (2026-09-28+): a real
        agent chasing a bug may Read `INVENTORY.md` instead of Grepping it, or reach
        for `Edit` / `MultiEdit` instead of `Write` when patching existing files —
        the discovery-first rule is satisfied by any inventory-touching read tool
        before any write-shaped tool. The singular string form stays valid for
        prompts that want an exact match.

        Semantics still match CLAUDE.md → *Before implementing anything*: the first
        write-shaped call is the point-of-no-return; a Grep AFTER it doesn't count.

        Returns False if no `after_tool` ever fires (rule vacuous — treat as failed
        so the run scores noncompliant rather than trivially compliant).
        """
        before_set = {before_tool} if isinstance(before_tool, str) else set(before_tool)
        after_set = {after_tool} if isinstance(after_tool, str) else set(after_tool)
        after_idx = next(
            (i for i, c in enumerate(self.tool_calls) if c["tool"] in after_set),
            None,
        )
        if after_idx is None:
            return False
        pat = re.compile(before_arg_match) if before_arg_match else None
        for c in self.tool_calls[:after_idx]:
            if c["tool"] not in before_set:
                continue
            if pat is None:
                return True
            if pat.search(json.dumps(c.get("input", {}))):
                return True
        return False


def parse_stream_json(stdout: str) -> TranscriptSignals:
    """Parse the stream-json stdout of one `claude -p --verbose` invocation.

    Each line is one JSON event. Assistant messages carry `content: [{type: "tool_use",
    name, input}, ...]`; the terminal `result` event carries `total_cost_usd`,
    `num_turns`, `result`. Silently skips malformed lines (counted in `parse_errors`);
    a stray non-JSON line shouldn't wedge the reader.
    """
    sig = TranscriptSignals()
    for line in stdout.splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            ev = json.loads(line)
        except json.JSONDecodeError:
            sig.parse_errors += 1
            continue
        if not isinstance(ev, dict):
            continue
        msg = ev.get("message")
        if isinstance(msg, dict):
            for c in msg.get("content", []) or []:
                if isinstance(c, dict) and c.get("type") == "tool_use":
                    sig.tool_calls.append({
                        "tool": c.get("name", ""),
                        "input": c.get("input", {}) or {},
                    })
        if ev.get("type") == "result":
            sig.cost_usd = float(ev.get("total_cost_usd") or 0.0)
            sig.turns = int(ev.get("num_turns") or 0)
            r = ev.get("result")
            if isinstance(r, str):
                sig.final_message = r
    return sig
