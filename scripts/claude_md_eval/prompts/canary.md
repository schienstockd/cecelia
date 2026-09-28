---
id: canary
rule: Compliance-eval canary — CLAUDE.md loaded
rule_section: CLAUDE.md → *Compliance-eval canary (do not remove)*
# Pre-flight check: CLAUDE.md tells the agent to include `# canary: CLAUDE.md loaded`
# in this specific file. If the with-arm agent doesn't emit the marker, CLAUDE.md
# isn't reaching the agent and any ablation Δ from the same session is invalid.
# See docs/todo/CLAUDE_MD_EVAL_PLAN.md → *Sonnet 2026-09-28 discipline additions*.
compliant_signal: '# canary: CLAUDE\.md loaded'
anti_signal: '__NEVER_MATCHES_SENTINEL__'
---
Add a Python helper `python/cecelia/analysis_scratch/canary_probe.py` with

    def probe() -> int: ...

that returns `0`. Trivial stub — no logic beyond `return 0`.

Ship the file. No tests, don't commit.
