# Audit prompt: rubber-duck framing vs. shipped v0.2.6

> **ARCHIVED — one-off audit prompt, not authoritative.** Audit ran 2026-09-23. Outcome parked
> as [`docs/todo/RUBBER_DUCK_FIT_PLAN.md`](../todo/RUBBER_DUCK_FIT_PLAN.md). Of the 7 gaps the
> prompt asks about, only 4 are real and actionable: #1b (uncaptured-target addressing —
> partial), #4 (provisional/observed at claim level), #5 (required-note friction, unmeasured),
> and #1 residual (position-without-ID cross-plot linking, parked). #2 was closed by shipped
> `LANDSCAPE_COMPLEMENTARY_PLAN` phases 1–5. #3 is a deliberate v1 scope call
> (server.py:820). #6 is roadmap and nothing depends on it. Part 2's drift concerns are
> confirmed real; fix is a framing companion note (Decision 2 of the plan), not a behaviour
> change — the shipped curation/warning behaviours solve `PROJECT_MEMORY_PLAN`'s payoff
> problem. Do not act on this file as a spec — the plan is the durable design surface.

## Context

`docs/archive/kiwi-purpose-and-framing.md` (landed via #1187) frames Kiwi
as a documentation helper / structured rubber duck: the value is a
faithful record of what was observed and tried — dead ends included —
not the correctness of Claude's replies. v0.2.6 shipped the Blackboard
arc (outcome tags, guardrail retrieval, fingerprints), bidirectional
plot point-out across ten families, behaviour cards, and landscape
Z-awareness/slim response. Audit both directions: where the shipped
implementation falls short of the framing, and whether the framing
itself still fits what's been built.

## Part 1 — Gaps against the framing

1. **Cross-plot reasoning is a prerequisite, not built.** `list_plots`
   (MCP registry, per-family axis meta) is noted as prerequisite for
   cross-plot reasoning; `point_at_ui` + pick/trackHighlight across ten
   families is single-plot highlight-on-pick. The `linkedTo`/data-identity
   cross-referencing designed for module-page multi-panel capture
   (mark in panel A ↔ point in panel B, by stable data ref surviving
   layout changes) does not appear to exist yet. Confirm, and if
   correct, this is the largest gap against the "cross-reference across
   multiple images/panels" half of the original design.

1b. **No way to point outward to an uncaptured target.** Distinct from
   gap 1 — cross-plot reasoning is about linking captures that already
   exist; this is Claude referencing something the user hasn't captured
   at all (e.g. "check the viewer around t=40, that region looks off,"
   or "pull up the collagen density plot for this treatment") and
   having that be actionable rather than just prose. This is squarely
   in-scope for the duck framing (it's "prompt the next question," the
   role Claude was always meant to have) but nothing in the addressing
   model supports it — everything shipped requires a capture to exist
   before Claude can reason about or reference it. Needs: (a) an
   addressing scheme for not-yet-captured targets — a plot known via
   `list_plots` with no capture yet, a viewer position by t/z with no
   capture — that Claude can name in a reply even without an image
   behind it, and (b) a UI affordance for the user to act on that
   reference (jump to the plot, seek the viewer to that t/z). Treat as
   its own workstream, not a sub-case of gap 1.

2. **Landscape is still category-only.** Shipped: Z-awareness, slim
   response, on-demand tile subset (`get_capture_landscape_tiles`). Not
   shipped, per the complementary-stats audit: per-tile channel
   mean/SNR, `segCount`, populations, tracks+props (incl. HMM states,
   motifs). Landscape tiles still mostly duplicate what's visible in
   the RGB composite rather than adding non-visual ground truth.

3. **Guardrail retrieval is fingerprint-scoped, not similarity-scoped.**
   Content fingerprint (schema v1) detects *repeat* sessions and pins
   prior context — good for "you're back on the exact same problem."
   Does not appear to cover "different image, same underlying failure
   mode as three weeks ago" — the harder, more valuable case for a
   documentation tool meant to stop people re-discovering the same dead
   end. Check whether this is intentional scoping for v1 or an actual
   gap.

4. **No visible provisional/observed distinction at the claim level.**
   Outcome tags (good/bad + required note) mark whether an
   *investigation* succeeded — a different thing from marking whether a
   specific statement in a Kiwi reply is Claude's inference vs. a
   direct readout of the data. Confirm whether anything in the
   Blackboard/ClaudeOverviewDialog UI currently makes that distinction
   visible, or whether replies read as uniformly authoritative.

5. **Outcome tagging requires a note — check this against the
   friction goal.** The framing doc's design implication was "lower
   friction to record" as the priority over conversational polish. A
   *required* note on every outcome tag is exactly the kind of overhead
   that caused the original problem (nobody logs failure modes). Pull
   actual usage data if available — tag completion rate, abandonment at
   the note-entry step — before assuming this is fine.

6. **No export/writeup path.** Unchanged since flagged: nothing turns a
   tagged chain into a methods-ready summary. Still roadmap, not a v1
   gap, but re-confirm it hasn't quietly become load-bearing for anyone
   expecting it.

## Part 2 — Does "rubber duck" still fit what's shipped?

The original rubber-duck pitch: value comes from the *person* narrating
and marking up what they see; Claude notices/organizes/prompts the next
question, doesn't conclude on their behalf. Check whether the shipped
mechanics still match that, or whether the system has drifted toward
something with more active opinion than a duck implies:

- **Guardrail retrieval surfacing past bad-outcome entries unprompted**
  is Claude proactively injecting judgment ("this failed before") into
  a session, not passively reflecting what the user said. Is that still
  duck-shaped, or is retrieval-on-fingerprint-match closer to an
  assistant *warning* someone — which is a step toward the
  discussion-helper framing that was deliberately ruled out?
- **Outcome tags (good/bad) are evaluative**, even if the person
  assigns them. Combined with bias-ordered retrieval (bad > good >
  untagged), the system is making a judgment call about which past
  entries matter more — worth naming explicitly whether that's the
  duck organizing the person's own record, or the system starting to
  rank/curate in a way a duck wouldn't.
- **Behaviour cards and cluster/HMM medoid rendering** are Kiwi (or
  adjacent Analysis-board tooling) producing a synthesized
  representation, not just reflecting a raw mark back. Confirm whether
  these are presented as Claude's interpretation (provisional) or as
  neutral display — same concern as gap #4, but specifically worth
  checking here since medoid selection is itself an inference.

## Ask

1. Confirm/deny each Part 1 gap against actual code state (not just
   changelog wording) and give effort estimate for closing the
   highest-value one.
2. For Part 2: either confirm the shipped mechanics still fit the
   rubber-duck framing, or flag which specific features have drifted
   toward discussion-helper territory and need either a framing update
   or a behavior change to stay consistent with
   `docs/archive/kiwi-purpose-and-framing.md`.
