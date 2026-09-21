# P5.2b — fuzzy fingerprint bucket matching (follow-up to #1164)

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of a follow-up brainstorm
> written against PR #1164 (Phase 5.2 exact-match guardrail retrieval). The exact-match reservation
> it targets is real; whether to act on it depends on measured fragmentation, not on the prompt.
> Read the current `docs/todo/PROJECT_MEMORY_PLAN.md` (Phase 5 section) for the authoritative
> design, not this file.

## Context

#1164 shipped guardrail mining with **exact** bucket matching:
`_fingerprint_bucket_key` canonicalises a fingerprint and `_mine_guardrails`
groups `bad`-tagged entries by exact bucket equality. Self-flagged
reservation in that PR: `stain_classes=["membrane","nucleus"]` does not
cluster with `["membrane","nucleus","reporter"]`, even though these are
plainly the same imaging context for guardrail purposes.

This matters because the original plan (D4) called for **scored/overlap**
matching specifically to avoid this. Exact matching raises the effective
bar for hitting the N=3 recurrence threshold (D5) — near-duplicate
fingerprints fragment into separate buckets that each individually never
reach 3, even though the *combined* signal would. Net effect: guardrails
stay empty longer than the underlying data actually supports, and it'll
look like "the feature doesn't work" when the real cause is bucket
fragmentation.

## Goal

Replace (or supplement) exact bucket-key matching in `_mine_guardrails`
with overlap-scored matching, per the original D4 intent, without
reintroducing the false-clustering problem the v2 fingerprint's
absent-on-unknown design was built to prevent.

## Decisions

- **D1 — `pipeline_stage` stays exact.** Not up for fuzzing. A
  segmentation guardrail is not relevant during gating; don't cluster
  across stages regardless of overlap elsewhere.
- **D2 — everything else scores by overlap, not exact equality.**
  Two entries are candidates for the same cluster if their fingerprints
  share enough fields/values in common, not only if every field matches.
  Concretely: score = (# matching fields, weighted or unweighted TBD by
  Opus) / (# fields present in either). `stain_classes` specifically
  should use set-overlap (Jaccard or subset match), not list equality —
  that's the exact case called out in #1164's reservation.
- **D3 — absent fields stay absent, don't count as a match or a
  mismatch.** Preserves the v2 "unknown reads as no signal" design.
  Two entries that are both missing `tissue_context` shouldn't score
  higher *or* lower on that dimension than if only one had it set —
  absence is excluded from the scoring denominator, not treated as a
  wildcard match.
- **D4 — overlap threshold is a separate tunable from the D5 recurrence
  count.** Something like `MIN_BUCKET_OVERLAP` (start at a value Opus
  proposes — e.g. requiring `pipeline_stage` exact + ≥1 other matching
  field, or a proportion like ≥0.6 overlap) that determines whether two
  entries can join the same cluster at all, separate from N=3 which still
  determines whether a cluster is big enough to surface. Don't collapse
  these into one number.
- **D5 — clustering becomes a proper grouping problem, not pairwise.**
  With exact keys, "bucket" was just a dict key — trivial. With overlap
  scoring, entries need to be grouped transitively (something like
  single-linkage clustering on the overlap score) so A-similar-to-B and
  B-similar-to-C can end up in one cluster even if A and C alone
  wouldn't meet threshold. Flag if this risks over-merging (a long chain
  of loosely-related entries collapsing into one bucket) — if so, prefer
  a stricter same-cluster requirement (e.g. must match the cluster's
  centroid/representative fingerprint, not just any prior member) over
  pure transitive chaining.
- **D6 — backward compatible with existing `bad`-tagged entries and
  fingerprint v1/v2 dispatch.** No re-tagging or migration required.
- **D7 — keep the "empty when nothing recurs" behavior.** Fuzzier
  matching should surface guardrails *sooner*, not surface noisier ones.
  If overlap-based clustering makes low-quality/coincidental clusters
  appear, tighten D4's threshold rather than shipping something that
  proposes weak guardrails.

## Not in scope

- No change to how guardrails are surfaced in briefing (#1164's
  `BRIEFING_GUIDANCE` / "HOW TO OPEN" wiring stays as-is).
- No cross-project federation (still deferred, per #1164's D8 /
  `IMMUNEMAP_IMPORT_PLAN.md`).
- No change to the v2 fingerprint extractors themselves (modality,
  tissue_context) — this is purely about how buckets are compared once
  fingerprints exist.

## Test plan (extend #1164's existing suite, don't replace it)

- [ ] Regression: existing `MineGuardrailsTest` cases (exact-match
      scenarios) still produce the same clusters under the new scoring
      (an exact match is a special case of high overlap, should still
      cluster).
- [ ] New: `stain_classes=["membrane","nucleus"]` and
      `["membrane","nucleus","reporter"]` (same modality/tissue/stage)
      now cluster together.
- [ ] New: two entries differing in `pipeline_stage` only never cluster,
      regardless of overlap elsewhere (D1).
- [ ] New: two entries both missing `tissue_context` don't get an
      inflated overlap score from the shared absence (D3).
- [ ] New: a chain of loosely-related entries (A~B~C, A≁C) — verify
      whichever grouping strategy was chosen (transitive vs.
      centroid-based) behaves as decided in D5, with a test that would
      catch accidental over-merging.
- [ ] `pixi run test-api` and `pixi run test-py` both pass.

## Suggested first step

Before writing clustering logic: pull a sample of current `bad`-tagged
entries (even if none hit N=3 yet) and manually compute what buckets
*should* form under overlap scoring vs. what #1164's exact matching
currently produces. Confirms the fragmentation problem is real and
sized correctly before committing to a specific overlap formula.
