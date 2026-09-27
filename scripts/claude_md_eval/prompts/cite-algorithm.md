---
id: cite-algorithm
rule: Cite sources for non-trivial algorithms
rule_section: CLAUDE.md → *Cite sources for non-trivial algorithms*
# Compliant match: any citation-shaped token (DOI, arXiv id, 10.NNNN prefix, or a
# github.com/<owner>/<repo> reference-implementation URL) appearing in a comment line near
# the new function. Anti_signal is a permissive placeholder — the rule has no ratchet, and
# a "no citation added" run scores noncompliant via the "neither matches" branch, which is
# the honest floor. P2 will inspect diff structure more carefully.
compliant_signal: '#.*(?:doi\.org|arXiv|arxiv|10\.\d{4}/|github\.com/[\w.-]+/[\w.-]+)'
anti_signal: '__NEVER_MATCHES_SENTINEL__'
---
Add a Python helper `python/cecelia/analysis_scratch/ssim.py` with a function

    def ssim(a: np.ndarray, b: np.ndarray) -> float: ...

that computes the Structural Similarity Index between two grayscale images (same shape,
float dtype). The implementation should follow a published reference.

Ship the .py file. No tests, don't commit.
