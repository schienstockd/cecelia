# Kiwi: what it is and why

> **ARCHIVED — framing note, not authoritative.** A discussion-derived reframe of Kiwi as a
> **documentation helper** (not a discussion helper). The `ClaudeOverviewDialog` copy already
> reflects it (dead ends first-class, provisional/observed distinction, capture chain as the
> durable trail); citations in `frontend/src/lib/claudeOverview.ts` +
> `frontend/src/components/ClaudeOverviewDialog.vue` point back here so the framing behind the copy
> stays discoverable. Do not act on this file as a spec — the durable design lives in the modal
> itself.

## The one-liner

Nobody logs their failure modes when they're heads-down in Fiji or a
notebook. Kiwi is what's left behind when they didn't have to.

## What it is

Kiwi is Cecelia's in-app assistant. It watches what you're looking at —
viewer frame, plot panel, landscape tile — and turns "hey, look at this"
into a persisted, evidence-linked record instead of a screenshot that
never gets found again.

## The actual problem it's solving

Image analysis in practice: people start, poke around, hit something
odd, either chase it or drop it, and move on. No log entry either way.
When something doesn't work, it's just abandoned — not written down as
"tried X, didn't work, here's why." At writeup time, the only trail is
whatever's left in Fiji history or scattered notebook cells, and nobody
remembers which one mattered.

Microscopy departments/core facilities also can't respond to every
in-the-moment query — so the person hits a wall, gets no answer, and
either guesses or drops it silently.

## Framing: documentation helper, not discussion helper

This distinction is load-bearing, not cosmetic:

- A **discussion helper** implicitly promises answers. Kiwi has no
  ground truth for domain-specific gotchas (e.g. "that's not a
  segmentation bug, that's known autofluorescence at that laser power")
  that a human core-facility person would catch. A confidently wrong
  reassurance in exactly a failure-mode moment is the worst outcome —
  same shape as the mistake, just silent instead of loud.
- A **documentation helper** only promises an accurate record of what
  was observed and what was tried. It doesn't need to be *right* about
  the interpretation to be useful — it needs to be a faithful trail.
  Much lower correctness bar, much less exposure when Claude gets a
  domain call wrong.
- Conversation with Claude still happens inside Kiwi — but it's in
  service of producing a better record, not the product itself.
  Anything that reads as a conclusion or interpretation should be
  visibly provisional/unverified, distinguishable from "what the
  pixels/data actually show."

## Mechanics (already landed on main)

- **Capture** — mark up what you're seeing: freehand on an image
  frame, a point on a plot, a tile on the landscape overlay.
- **Chain** — each follow-up capture links back to its source via
  `previousCaptureId`. A whole investigation — including dead ends —
  stays one traceable thread instead of scattered screenshots.
- **Cross-reference** — marks resolve by data identity (`dataRef`), not
  screen position or panel ID, so a link between two plots still means
  something after the layout changes, panels close, or the session
  ends.
- **Discuss** — Claude reasons over what's marked. The point isn't the
  answer; it's that the question, the evidence, and the attempt get
  written down as a side effect of doing the analysis, not as a
  separate chore nobody does.

## Design implications that follow from "documentation helper"

- **Dead ends are first-class.** A tool that only records successes is
  a highlight reel, not documentation. Nothing in the capture/chain
  model should bias toward surfacing only positive findings.
- **Passive capture > active Q&A as the optimization target.** Lower
  the friction to *record* ("circle it, one sentence, done") over
  optimizing conversational quality.
- **Provisional vs. observed needs a visible distinction** in however
  captures/threads get displayed later — don't let a database presence
  read as more rigorous than a notebook scrawl just because it's
  structured.
- **Export/writeup path is a real roadmap item.** The pitch is "no more
  flinging through notebooks at writeup time" — a path from capture
  chain to a methods-section-ready summary is the natural endpoint,
  even if v1 doesn't build it.

## Note on prior planning docs

Earlier prompt files (multi-panel capture, landscape complementary
stats) used "discuss graphs and results" language — discussion-first
framing. This doc supersedes that framing: the mechanics don't change,
but anything still to be built (esp. UI copy, conclusion display, any
Kiwi-authored summary text) should read as documentation-first, not
conversation-first.

## Reframe: structured rubber duck (2026-09-22)

Later Sonnet chat surfaced a sharper positioning: **Kiwi is a
structured rubber duck.** "Documentation helper" describes the artifact
(what Kiwi leaves behind). "Structured rubber duck" describes the
user's activity (articulate, get a provocation back). The two are
compatible: the blackboard, outcome tags and guardrail retrieval are
what makes the rubber ducking *survive* to writeup time — that is what
"structured" is doing.

Every shipped mechanic reads cleanly through the rubber-duck lens:

- **Marks** — "I articulated what I saw."
- **Chains** — "the shape of my thinking over time."
- **Blackboard entries with outcome tags** — "the ones I want to
  remember, tagged with how they turned out."
- **Claude's replies** — "the noise back that helps me hear myself"
  (a real duck can't; a Claude can, imperfectly).

Why it's the better positioning:

- Names the *user's* activity, not the tool's category — same move as
  Copilot's "pair programmer." Positions Kiwi against what an
  immunologist actually does at the screen, not against a class of
  software.
- Sidesteps the "was Kiwi right about the biology" bar. A rubber duck
  cannot be wrong; a structured one holds notes and lets you ask
  someone else later.
- Matches what LLMs are actually good at — being a provocation to your
  own thinking — without pretending they're domain oracles.

"Rubber duck" is a technical term from another discipline (software
engineering, `rubberduckdebugging.com`); dragging it into immunology is
fine — flow cytometry did the same with "gate," "compensation" and
"backgating." Fair game for the modal lede + FAQ; keep the modal title
plain-language ("What Kiwi does here") so a first-time reader can still
enter without knowing the term.
