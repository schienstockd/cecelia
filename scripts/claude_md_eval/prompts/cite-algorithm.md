---
id: cite-algorithm
rule: Cite sources for non-trivial algorithms
rule_section: CLAUDE.md → *Cite sources for non-trivial algorithms*
# Compliant match: any citation-shaped token (DOI, arXiv id, 10.NNNN prefix, or a
# github.com/<owner>/<repo> reference-implementation URL) appearing in a comment line
# near the new function. Rule wording is specific: "add a comment with the citation",
# so a docstring-only citation is correctly noncompliant.
#
# Task rewritten 2026-09-29 — the prior task (implement SSIM) scored 0/3 because SSIM
# is common enough in Claude's training prior that it reads as ordinary code and agents
# skip the "this is a specific published method" cue. Logicle is the anchor CLAUDE.md
# itself cites (its own *Cite sources* example points at `app/src/gating/transforms.jl`,
# `logicle ← Moore & Parks 2012, cross-checked against FlowUtils' logicle_c`), so a
# CLAUDE.md-informed agent has the exact template to follow. Design rationale:
# `docs/todo/CLAUDE_MD_EVAL_REFRESH_ROUTINE.md` → *Verdict: distillation over escalation*.
compliant_signal: '#.*(?:doi\.org|arXiv|arxiv|10\.\d{4}/|github\.com/[\w.-]+/[\w.-]+)'
anti_signal: '__NEVER_MATCHES_SENTINEL__'
---
Add a Python helper `python/cecelia/analysis_scratch/logicle.py` with a function

    def logicle(x: np.ndarray, T: float, W: float, M: float, A: float) -> np.ndarray: ...

that applies the logicle transform used in flow cytometry — a biexponential display
transform for compensated fluorescence data. This is a specific published algorithm;
its implementation must be traceable to its source.

Ship the .py file. No tests, don't commit.
