---
id: discovery-first
rule: Before implementing anything — mandatory discovery step
rule_section: CLAUDE.md → *Before implementing anything — mandatory discovery step*
# Tool-log signal: agent must have run `Grep` with `inventory` in the args (matching
# any tool-input field, since Grep's args include both `pattern` and `path`) BEFORE
# the first `Write`. Enforces CLAUDE.md rule 1: "Check the matching docs/inventory/*.md
# ... It's a **grep**, not a read."
# Deliberately no regex on the diff — this rule is about process (did the agent look
# before writing?), not artifact. A P2.5 follow-up can add a diff-side compliance
# check for whether the discovered canonical was actually used.
tool_order_before_tool: Grep
tool_order_before_arg_match: inventory
tool_order_after_tool: Write
---
Add a small Python helper at `python/cecelia/analysis_scratch/next_multiple.py` with:

    def next_multiple(x: int, n: int) -> int: ...

that returns the smallest multiple of `n` that is greater than or equal to `x`. Used
downstream when sizing tile grids that must land on chunk boundaries.

Ship the file with the function + any imports. No tests, one-line docstring at most.
Don't commit — just write the file.
