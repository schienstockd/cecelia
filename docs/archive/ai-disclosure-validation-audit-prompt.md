# AI-development disclosure & validation — landscape survey + honest self-audit

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

## Why

The README already discloses that Cecelia was written almost entirely by
Claude Code under Dominik's direction, and states that scientific/visual
validation was done by the human. That disclosure is good but generic —
it asserts "validated correctness" without the specific, falsifiable detail
that makes a claim like that credible to a skeptical reader. A concrete
example already exists: coastal produced optimization numbers that looked
fine but didn't confirm whether the segmentation actually captured the
signal Dominik wanted — automated metrics alone were insufficient, and
visual inspection against real images was what caught it. That's worth
documenting precisely, and there may be other cases like it that haven't
been written down anywhere.

This is not a "make rewrites.bio's principles official" task. rewrites.bio
is one company's proposal, written for a different situation (rewriting
existing published tools for speed), with real gaps of its own (no
governance, no sunset/maintenance-lapse plan, silent on whether AI-assisted
work should seek publication). Treat it as one data point among several,
not a standard to adopt wholesale.

## Part 1: Landscape survey — what does the field actually expect?

Nobody currently knows what other researchers/reviewers actually expect
from AI-assisted scientific software, because this is genuinely unsettled.
Find out what's actually out there rather than assuming:

1. **AI-assisted rewrites/tools already surveyed in this conversation** —
   revisit and go deeper than the summaries already gathered: RustQC
   (Seqera), rewrites.bio itself, Rob Patro's SSHash/piscem Rust rewrite
   (combine-lab.github.io blog post, Feb 2026), Fulcrum Genomics' fgumi.
   For each: how do they disclose AI's role, what validation do they claim
   and how specifically, do they address publication at all, what's their
   actual governance/maintenance story now (not just at launch).
2. **The MorPhiC/STAR-Flex case** (arxiv 2512.11993) — read the full paper,
   not just the excerpt already seen. How does an NIH-funded, presumably
   peer-review-adjacent effort describe its "human-led AI team" methodology
   in formal academic writing? This is the closest precedent to an actual
   published disclosure norm — treat it as more load-bearing than blog
   posts for that reason.
3. **Search specifically for**: any journal or preprint server's stated
   policy on AI-authorship/AI-assisted-development disclosure for
   *software* specifically (not text-generation policies, which are a
   different, more publicized debate — Nature/Science/etc. have those, but
   this needs the software-development angle specifically). Check Nature
   Portfolio, PLOS, eLife, F1000Research, JOSS (Journal of Open Source
   Software — likely the most relevant venue given it publishes software
   specifically and may already have a stated position).
4. **Search for community reaction/discussion**, not just vendor
   announcements — Biostars, Hacker News, r/bioinformatics, Twitter/X
   threads from working bioinformaticians reacting to RustQC or similar
   releases. What do people actually push back on? What reassures them?
   This is where the *real* expectations bar lives, not in a company blog
   post's stated principles.
5. **Existing precedent already gathered in this conversation, worth
   citing as contrast**: Napari (no primary paper, Zenodo DOI only, never
   made an AI-disclosure claim because it predates this question), QuPath
   (traditional peer-reviewed primary paper, also predates this question).
   Neither is an AI-disclosure precedent, but both are precedent for "how
   does a tool earn trust without one particular kind of validation," which
   is the same underlying question.

Output for Part 1: a short synthesis — not a list of links, an actual
answer to "what do people expect, where do they agree, where is it
genuinely unresolved." Flag disagreement or open questions honestly rather
than picking one convention and presenting it as consensus.

## Part 2: Honest self-audit of Cecelia's own validation practices

Separately from the survey, audit what actually happened during
development — this needs to come from Dominik's memory and the repo
history, not be inferred or invented.

1. **Catalog every place validation happened, at the level of specificity
   the coastal example set**: which subsystems were validated by
   golden-value/fixture tests only (name them — the citation convention
   in CLAUDE.md, e.g. the logicle transform cross-checked against
   FlowUtils, is one concrete instance to start from), which were
   validated by eyeballing real images/output because a numeric metric
   alone wasn't sufficient (coastal is the one instance we have — ask
   Dominik directly whether there are others: segmentation quality checks,
   tracking sanity checks, gating population plausibility checks, anything
   in the napari-bridge/viewer work), and which — if any — haven't been
   validated against real data at all yet and should be flagged as such
   rather than implied to be covered.
2. **Name the actual failure mode explicitly**: automated optimization/fit
   metrics can look acceptable while not confirming that the intended
   biological signal was captured — the metric measures fit to *an*
   objective, not necessarily the objective the scientist actually cares
   about. This is a real, useful methodological point on its own, worth
   stating plainly rather than just implying "human looked at it."
3. **Check for gaps between what CLAUDE.md's testing rules formally
   require and what real-data visual validation actually covers** — the
   four `pixi run test-*` categories are about code correctness (does the
   function do what it's supposed to, does the pipeline run without
   crashing); they are not the same thing as "does this segment the right
   cells on real intravital data." Make that distinction explicit
   somewhere durable, since right now it's implicit and easy for a reader
   to conflate "tests pass" with "scientifically validated."

## Part 2.5: Goalposts going forward — this was made up as we went

Be direct about this in the writeup rather than glossing over it:
validation up to now was ad hoc — Dominik used judgment case by case,
without a stated rule for when a fixture/golden-value test is sufficient
versus when real-data visual inspection is required. That's not a failure
to hide, but it means there's currently no standing convention a future
contributor (human or AI) could follow to know which bar applies to new
work. Two separate things are needed here, and they're different kinds of
work:

**A. Retrospective clean-up** — of what Part 2's audit finds:
- Any subsystem the audit surfaces as validated only by fixture/golden
  tests, where the coastal precedent suggests a numeric-metric-only check
  may not be sufficient (segmentation quality, tracking correctness,
  gating plausibility are the obvious candidates — confirm which ones
  actually need it, don't assume all of them do), gets flagged as an open
  item — either scheduled for real-data validation, or explicitly accepted
  as fixture-only with a stated reason (e.g. the coastal case was
  specifically about signal capture, which may not apply to, say, a pure
  data-transform function with no visual/biological interpretation).
- File genuine gaps in `docs/TODO.md` per its existing convention (open
  work only, delete when done) rather than inventing a new tracking
  mechanism.

**B. A forward-looking convention** — a stated rule for future work, added
to `CLAUDE.md` alongside the existing "Cite sources for non-trivial
algorithms" and testing sections (same spirit, same place, not a separate
process document floating elsewhere):
- Define what triggers the extra bar: work whose correctness can't be
  fully judged from a numeric fit/optimization metric alone because the
  metric doesn't capture whether the *intended biological signal* was
  captured — segmentation, tracking, gating thresholds, anything where
  "the loss went down" and "this is scientifically right" can diverge.
  Draft this trigger condition from the coastal case, but state it
  generally enough to apply to future subsystems, not just as a rule
  about coastal specifically.
- State plainly what satisfies the bar when it's triggered: real-data
  visual inspection by Dominik (or whoever has the domain expertise),
  documented as such — doesn't need to be elaborate, but should exist
  somewhere durable (a test comment, a doc note, a PR description) rather
  than living only in memory.
- This is a convention for a two-person-scale project with one domain
  expert — don't over-engineer it into a formal sign-off process or
  committee-style review that doesn't match how this project actually
  works. Match the weight of CLAUDE.md's existing conventions (a
  paragraph, a clear trigger, a clear bar) rather than inventing new
  process machinery.

## Part 3: Write it up

Once Part 1 and Part 2 are both done, propose (don't just write
unilaterally — this is Dominik's disclosure to make, in his own words
where it matters) concrete updates:

- A strengthened "human role" / validation section in `README.md`,
  replacing the generic "reviewed all output and validated scientific and
  visual correctness" line with the specific coastal-style example(s)
  found in Part 2, and the automated-metric-insufficiency point named
  plainly.
- A new validation-convention paragraph added to `CLAUDE.md` per Part 2.5B
  — the trigger condition and the bar, stated once, durably, so future
  work (Dominik's or an AI agent's) has a rule to follow instead of
  re-deriving judgment calls each time.
- Any retrospective clean-up items from Part 2.5A filed in `docs/TODO.md`
  per its existing convention.
- Whether a standalone `docs/PROVENANCE.md` (or similar) is warranted to
  hold the fuller version of this — the landscape context from Part 1,
  the full validation catalog from Part 2, cross-referenced from the
  README rather than duplicated in it. Recommend this only if the content
  genuinely doesn't fit in a README section; don't create a file for its
  own sake.
- Explicitly do NOT frame any of this as adopting rewrites.bio's
  principles as a standard, or as claiming a settled community consensus
  that doesn't exist per Part 1's findings. The tone should be: here is
  what we did, here is why, here is what we found when we looked at what
  others are doing, here is what's still genuinely unresolved in the
  field. Confidence should track what Part 1 actually establishes, not
  overstate it.

## What not to do

Don't treat this as a task to make the README sound more reassuring.
The goal is accuracy and specificity, not marketing. If Part 2 surfaces a
subsystem that genuinely hasn't been validated against real data yet, say
so in the writeup rather than smoothing over it — that's exactly the kind
of honest gap disclosure that builds credibility with a skeptical reader,
and burying it is the failure mode this whole exercise exists to avoid.
