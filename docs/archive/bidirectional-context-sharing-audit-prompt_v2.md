> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

# Bidirectional Context Sharing — Audit & Design — Cecelia

## Background

Cecelia has moved fully to a Vue + Julia frontend/backend, with a WebGPU volume
viewer (see `frontend/src/utils/viewerWindow.ts` for how a viewer window is
addressed: `projectUid` + `imageUid` + optional `valueName`). There is no napari
anymore, so any prior assumption about a Python-side viewer with an inspectable
scene graph no longer applies.

There is already a working MCP server (`mcp/`, see `mcp/README.md`) that gives
Claude Code **read** access to a running Cecelia project — project state, images,
task logs, QC, lineage, populations, measures, lab log — plus a small,
deliberately **additive-only** set of writes (lab-log entries, Pluto notebooks, a
whiteboard chain template). It also runs a session monitor over the backend's
WebSocket event stream, surfacing patterns like repeated-failed-attempts through
a pull tool (`poll_observations`) without the user narrating anything.

**The gap:** none of this lets the person and Claude share a common frame of
reference for what's actually on screen. Right now, using Claude Code on this
project means opening a terminal and describing — in words — what image, what
channel, what timepoint, what looks wrong. For imaging work this is a real
cost: segmentation errors, track jumps, channel artifacts, and spatial patterns
are things a person can point at instantly and would take paragraphs to
describe accurately, and the description itself introduces error before Claude
even sees the problem.

**The concept: Bidirectional Context Sharing.** The Vue app should function as
a shared workspace both parties bring context to and read context from, not a
one-way display the person narrates and Claude never sees. Three parts to this,
not two separate features plus an unrelated add-on:
- **Share-in** — the person shares their current viewer context (state +
  pixels, maybe a short clip) into whatever Claude Code session they have
  running, on demand — and, critically, can **draw on the captured frame**
  before sharing it, not just hand over a plain screenshot. See "the shared
  drawing layer" below — this is not a separate feature from point-out, it's
  the same mechanism run in the other direction.
- **Point-out** — Claude shares its own understanding back into the app, by
  pointing at or marking something (a cell, a track, a panel, a control)
  rather than only describing a location in words — using the same drawing
  layer as share-in.
- **Blackboard** — a persistent, versioned space for concepts and ideas the
  person and Claude develop together — diagrams, notes, open questions, and
  optionally a particularly useful annotated frame worth keeping — kept
  deliberately separate from the chain whiteboard. Detailed in Part 4 below.

## The shared drawing layer (not a screenshot tool, not a live feed)

Two mechanisms were considered and rejected before landing on this one, worth
stating explicitly so the reasoning isn't re-litigated later:
- **Plain screenshot/screencast** — solves nothing new. It's a slower version
  of what already happens today (context-switch, capture, paste into a
  terminal); it doesn't reduce the actual cost, which is disambiguating *what
  part of the image* matters.
- **A continuous "watch over my shoulder" feed** — actively wrong for this
  design. It requires an always-open channel, constant token cost, and an
  agent continuously deciding what's worth noticing with no explicit trigger
  — the same embedded-always-on-agent shape already ruled out for the
  chat-panel question, just relocated to vision. It also reintroduces the
  same ungated-judgment risk flagged for autonomous ROI selection: no trigger,
  no ground truth, drift risk with nothing to catch it.

**What this should be instead: an explicit-trigger paint/annotation layer on a
captured frame.** The person (or Claude) triggers a capture, the view freezes,
a drawing surface appears over it — circle, arrow, underline — and *that*
markup, together with the underlying state + pixels, is what gets shared. A
drawn mark is the actual disambiguation that currently costs a paragraph:
"there's a weird blob near the left edge" becomes a circle on exactly the
blob.

**This must be one shared primitive, used from both directions, not two.**
Share-in (Part 2, the person marks a frame for Claude) and point-out (Part 3,
Claude marks something for the person) are the same capability run backwards
— one drawable annotation layer over an addressed frame/entity, with two
different triggers. Building this twice would be exactly the "no new
component is an island" failure the cross-cutting constraint above already
warns against. Part 1 should check whether the correction cockpit's
highlight machinery or the existing guide/pointer system already has anything
resembling a drawing/markup primitive to extend, before assuming this is
built from scratch.

**Persistence is optional, and belongs to the Blackboard, not to share-in/
point-out themselves.** Most annotated frames are transient — ephemeral by
default, per Part 3's existing requirement. But an annotated frame that turns
out to matter (the circled blob that was the actual answer to a long-running
question) should be *promotable* to a Blackboard entry — the same "kept, not
throwaway" space already scoped for diagrams and notes — rather than either
being lost when the session ends or bloating the default ephemeral path with
permanent storage nobody asked for.

Both directions are in scope for this design pass, and should be designed
together as one coherent capability, not bolted on separately. There is
apparently already an in-app guide/pointer system (used for onboarding or
tutorials) that highlights or points at something in the UI — find it in Part 1
and treat it as the first candidate to reuse or extend for the point-out half,
rather than building a second pointing mechanism from scratch.

**What we do NOT want:** a full embedded chat panel like Claude Code's own UI
or Claude Science's workbench. Claude Code is already rich in terminal/editor
affordances; duplicating a chat UI inside the Vue app is not the goal. This is
a channel for shared context between two existing surfaces (the terminal and
the app), not a new place to have a conversation.

## Prerequisite: this builds on the maintainability audit, not around it

A separate audit pass on this codebase produced (or will produce)
`docs/MAP.md` (a plain-language "where things live" index) and
`docs/MAINTAINABILITY.md` (a short, living checklist covering comment/docstring
rules, cross-module contract rules, and structure rules for where new
functionality belongs). Read both before starting Part 1, if they exist yet.

This matters concretely, not just as a formality:
- Any new file, module, or seam this design creates should follow
  `docs/MAINTAINABILITY.md`'s rules from the outset — new MCP tools, new
  backend endpoints, and new frontend capture/highlight code are exactly the
  kind of addition that audit exists to keep legible. Don't reproduce the
  patterns that audit flagged as problems (comments that narrate debugging
  history, docstrings referencing context that won't be in the repo, a new
  responsibility bolted onto an already-mixed file).
- Once this design is implemented, **update `docs/MAP.md`** to include it — a
  person should be able to find "where does the viewer-share/point-out
  machinery live" the same way they'd find anything else in the map.
- If `docs/MAP.md` or `docs/MAINTAINABILITY.md` don't exist yet when this runs,
  say so plainly and proceed using the same principles described above from
  first principles, rather than skipping this section.

## Cross-cutting constraint: no new component is an island

This design introduces several new pieces — a viewer capture mechanism, a
point-out/highlight tool, and a Blackboard — each explicitly told to reuse
something that already exists (the correction cockpit's highlight machinery,
the guide/pointer system, the Pluto notebook versioning system, the lab-log,
the `*_PLAN.md` convention). That's deliberate: the goal is one connected set
of mechanisms with clear links between them, not four or five independent
subsystems that each happen to touch "sharing context" but don't know about
each other.

Concretely, before finishing this design, check it against this failure mode
directly:

- **Every new entity should be referenceable from the others it's related to.**
  A Blackboard entry that led to a decision should be linkable from that
  decision's `*_PLAN.md` entry. A point-out marker placed because of something
  found in a Blackboard diagram should be able to reference that diagram's id.
  A lab-log entry logging a Blackboard update should reference the entry id,
  not just describe it in prose. If two of these new pieces can't reference
  each other at all, that's a sign they were designed as separate features
  bolted on side by side rather than one coherent capability.
- **Don't give the same kind of data two different addressing schemes.** If an
  "entity id" means one thing in the correction cockpit and something
  differently-shaped in a new point-out tool, that's exactly the kind of
  implicit-contract drift the maintainability audit's pattern 4 flags —
  applied here to the new code, not just the old.
- **State explicitly, in the final report, how the new pieces connect** — not
  just how each was designed in isolation. A short diagram or table showing
  "Blackboard entry → referenced by → lab-log entry / `*_PLAN.md` decision /
  point-out marker" is worth including even if it feels redundant with the
  per-part descriptions above.
- **If something genuinely can't be linked** (e.g. a viewer snapshot is
  ephemeral by design and shouldn't be permanently cross-referenced), say so
  explicitly and why, rather than leaving it ambiguous whether the omission was
  a decision or an oversight.

## Cross-cutting constraint: implement as multiple, reviewable PRs — not one

This design is large — share-in, point-out (three anchor types plus freeform
marks), a shared drawing layer, capture-mode splitting, and the Blackboard.
Treating the whole thing as one implementation PR would repeat a mistake
already made and corrected elsewhere in this codebase: PR #915 tried to close
an entire remaining arc of task-param typing in one 89-task PR, breaking the
one-family-per-PR cadence that made the earlier PRs in that arc reviewable,
and had to be split back apart. Don't reproduce that here on a design that's
larger still.

Concretely:
- **Design everything together (this document), implement it in separate
  PRs**, roughly along the Part boundaries below — e.g. (1) the shared
  drawing layer + capture modes on their own, since Parts 2 and 3 both depend
  on it, (2) share-in / Part 2 built on top of it, (3) point-out / Part 3's
  data and UI anchors, (4) grid/landscape anchors as their own PR given the
  k-means-vs-SAM evaluation and standalone-feature framing involved, (5) the
  Blackboard, including the notebook-versioning extraction. Propose your own
  breakdown if a different split is more natural given what Part 1 actually
  finds — the point is *a* sequence of small, reviewable units, not this
  specific one.
- **Each PR should be self-contained and independently mergeable**, the same
  way each of #906–#914 shipped a working, tested slice rather than a
  half-finished piece waiting on a later PR to be meaningful.
- **State the proposed PR sequence explicitly in your final report** — not
  just "here's the design," but "here's the order I'd implement and land
  this in, and why," so review capacity can be planned against it rather than
  discovered as one large diff.
- This constraint governs implementation, not the design work itself — Parts
  1–4 below should still be designed as one coherent, cross-referenced
  capability (per "no new component is an island" above); only the shipping
  sequence needs to be broken apart.

## Cross-cutting constraint: MCP is the only channel, in both directions

This applies to both the share-in and point-out directions, and it's
non-negotiable for the design, not just a nice-to-have:

**Claude must only ever act through the MCP contract — never by reading or
inferring from the actual frontend/backend source.** Claude Code currently has
local repo access during development, which makes it easy to accidentally design
something that "happens to work" because Claude quietly cross-checked against
the Vue source or inferred a DOM structure by reading `viewerWindow.ts` directly,
rather than because the MCP tool actually exposed what was needed. That's a
trap: Cecelia is also deployed on remote servers where Claude Code has no code
access at all, only the MCP connection. Anything that works today because
Claude peeked at the source will silently break there.

Concretely, this means:
- Every piece of information Claude needs to identify an anchor, a location, or
  a UI element (image/track/cell/ROI ids, screen regions, panel/control
  identifiers) must be retrievable through an MCP tool call, not inferred from
  reading the codebase.
- The audit and design in Part 1–3 below should be evaluated *as if Claude only
  has MCP access* — if a design only works because Claude Code happens to also
  have the repo open, it's not a valid design for this feature, it's a
  development-time shortcut that needs to be fixed or flagged.
- Any test/validation of this feature should be run in a way that would also
  hold on a remote deployment with no code access — MCP tool calls and
  responses only.

## The bigger frame this sits inside

The actual ambition here is something like "Claude Imaging" — a domain-tuned
agentic stack for bioimage analysis, in the same spirit as Claude Science for
molecular/genomic work. But imaging is not "Claude Science pointed at a
different database." Claude Science's primitives — database connectors,
skills encoding known analysis idioms, citation trails from data to conclusion
— all depend on the underlying data already being legible to a model: tabular,
queryable, embeddable. A z-stack, a multichannel time series, or a track set is
not legible in that way until *after* a segmentation/tracking pipeline has
already made judgment calls on it.

So what imaging needs that Science's current architecture doesn't have to solve
is a **visual grounding layer**: a way for the model to actually perceive
spatial/temporal structure — exact state plus pixels/clips — rather than only
receiving a table. Without that layer, none of the imaging-specific skills you'd
eventually want (segmentation QC, track-jump detection, channel-bleedthrough
diagnosis, spatial-stats interpretation — much of which already exists
informally as QC logic in tasks like `af_correct.jl`) have anything to be built
on top of.

**This means Bidirectional Context Sharing is not a side convenience
feature — it is the foundational primitive for that whole direction.** Design
it as step one of an imaging-grounding stack, not as an isolated one-off: the
state+pixels payload shape, the addressing scheme (project/image/frame/
channel/selection), and the audit-trail habit (a conclusion should trace back
to the exact frame/state that produced it, the same way Science ties a
conclusion back to its source data) should be able to carry the weight of
future skills built on top, not just today's "look at this" use case.

**Context added 2026-09-17 — MHS is upstream, not competing.** Anthropic
previewed the Model Hardware Standard (MHS) on 2026-08-27, an equipment-control
specification with HHMI Janelia as the anchor microscopy partner. MHS lets an
agent drive acquisition hardware (microscopes, liquid handlers, robotic arms)
through standardized drivers, with device-level safety limits enforced below
the model. Two things this design should take from that:
- **The Claude Imaging framing above is no longer speculative.** MHS is
  Anthropic itself moving on imaging-specific primitives; bidirectional
  context-sharing is the analysis-side counterpart to what MHS does upstream.
  The verdict section's question about whether the framing overreaches should
  weigh this evidence honestly rather than defaulting to skepticism.
- **The addressing/provenance shape should be MHS-adjacent, not MHS-specific.**
  If an agent one day acquires via MHS and analyses via Cecelia, the trail
  should connect — an acquisition run citable the same way a Cecelia measure
  is citable. The prompt already asks for stable, citable addressing; add the
  constraint that the scheme should be *expressible* in a form a future MHS
  state-dictionary could reference, without designing to MHS today (it's
  invite-only preview, no spec published yet). Do not couple to MHS's data
  shapes; do not assume acquisition metadata will arrive via MHS.

(Where on the executor↔advisor spectrum this design lands is deliberately not
listed here as a separate MHS bullet — Part 3's grid/landscape / Micropilot
discussion already covers the same axis in sharper, design-relevant terms.)

## Task

Two parts. Do the audit first; let it inform the design, not the other way
around.

### Part 1 — Audit what exists

Read through the relevant surface area before proposing anything:
- `mcp/` in full (server, client, guidance, monitor, wsclient, tests) — not just
  the README. Understand the no-mutation guarantee, the additive-write pattern,
  and exactly how `guidance.py` briefs a session so a new tool would be
  discoverable the same way.
- The Vue viewer and its surrounding state — how a viewer window knows what
  image/version it has open (`viewerWindow.ts` is the entry point; find and
  read whatever owns the actual WebGPU canvas, current z/t, channel/LUT state,
  camera, and any current selection — track, cell, ROI).
- The WebSocket event stream the MCP server's monitor already consumes
  (`ws://…/ws`, `api/src/sockets.jl` per the README) — is viewer state already
  flowing through it in any form, or is this monitor scoped only to task
  events?
- `docs/ai-assist/OBSERVER.md` and `docs/ai-assist/OBSERVER-SETUP.md` — the
  stated phased plan (Phase 1 observe, Phase 2 actionable assist, `submit_task`/
  `adjust_params`/`acknowledge_flag` deliberately unwired). Where would viewer
  context-sharing sit in that arc, and does it change the arc's logic at all?
- Any existing screenshot/export capability already in the viewer (image/movie
  export for figures, if one exists) — reuse it rather than building a second
  capture path if one is already there.
- **The existing in-app guide/pointer system** — find wherever the app already
  highlights, points at, or annotates something on screen (onboarding tours,
  tutorials, tooltips). Understand exactly how it addresses its targets: by DOM
  element id/selector, by screen pixel coordinates, by scene/image coordinates,
  or something else. This matters a lot for Part 3 below — an addressing scheme
  based on stable identifiers (DOM ids, image-space coordinates) will survive
  panning/zooming/window resizing; one based on raw screen pixels won't.
- **The correction cockpit work already started** — a system for flagging
  segmentation artifacts and tracks that need attention, where flagged objects
  can also be highlighted in the viewer and shown on gating plots. Find it and
  answer directly: is "an object is highlighted in the viewer" and "an object
  is highlighted in the cockpit / gating plot" driven by **one shared
  selection/highlight implementation**, or are these separate, parallel
  implementations that happen to look similar? This is a load-bearing question
  for Part 3 — if it's already unified, that's very likely the exact mechanism
  the point-out pathway should hook into (an object id in, highlighted
  everywhere the app already knows how to highlight it, for free). If it's
  duplicated, say so plainly and treat unifying it as a prerequisite worth
  doing before adding a third caller (Claude) on top of two already-diverging
  implementations — bolting a new pathway onto whichever one happens to be
  touched first would just add a third variant to reconcile later.
- **Any existing freeform drawing/markup capability**, anywhere in the app —
  an annotation tool on figures in the analysis board, a draw-a-ROI tool if
  one exists, anything letting a user mark up an image with shapes rather
  than just select existing entities. This is the primary reuse candidate for
  the shared drawing layer (Parts 2–3) — check before assuming a canvas-based
  drawing surface needs to be built from nothing.
- **The Pluto notebook versioning system** — find how notebook versions are
  stored, diffed/listed, and surfaced in the Vue app today. This is the
  primary reuse candidate for Part 4's Blackboard versioning — the goal is to
  extract it into a shared component, not build a second version-history
  mechanism.
- **The notebook pruning mechanism** — notebooks hit a version-accumulation
  problem and a pruning policy was introduced to address it. Find it and
  understand its actual policy (age-based? count-based? something else) —
  it's the reuse candidate for Part 4's question about whether Blackboard
  versions need the same treatment.
- **The Claude Code setup check on the settings page** — find how the app
  already detects whether Claude Code is installed/configured on the machine.
  Part 4's Blackboard page should be gated on this exact check.

For each, note explicitly: what's reusable as-is, what needs extending, and
what would be a genuinely new piece.

### Part 2 — Design: how would this fit best

Using what you find in Part 1, propose a concrete design for **on-demand viewer
context-sharing into an MCP tool call**. Constraints:

- **Additive, not architectural surgery.** This should follow the same pattern
  as the lab-log/notebook/chain writes: a new capture, stored somewhere
  recoverable, read back through a new MCP tool — not a rework of the viewer or
  the transport layer.
- **Pull, not push, to match the existing MCP model.** The README is explicit
  that MCP is client-pull and unsolicited server→push is a later slice
  (`poll_observations` is a pull tool fed by a background listener). A new
  capability should follow that shape: the user triggers a capture in the Vue
  app, and Claude picks it up on request in the terminal ("look at what I just
  shared") — not a push notification into an already-running session.
- **State + pixels together, not pixels alone.** A screenshot without
  project/image/valueName/z/t/channel/selection context is not more useful
  than what the user could type. The captured payload needs to carry enough
  that Claude doesn't have to ask "which image is this."
- **Two explicit capture modes, auto-detected with a manual override — not a
  menu the person has to choose from every time.**
  - **UI mode** — capturing the interface itself, addressed by DOM element
    ids (feeds Part 3's UI anchors). No grid/landscape prep runs in this
    mode — nothing there needs it, and it must stay off by default rather
    than becoming an always-on cost.
  - **Viewer/image mode** — capturing the viewer canvas, addressed by
    image-space coordinates/entities. This is the only mode where the
    grid overlay (and, on request, the landscape/k-means pass) from Part 3
    runs.
  - **Hybrid** — not a third parallel mode; a toggle on viewer mode ("also
    include surrounding UI context") for the narrow real case of "the viewer
    shows X but the panel controlling it shows Y." Keep the two real modes
    clean rather than building every consumer a three-way branch.
  - Mode should default to whichever surface (viewer canvas vs. a UI panel)
    last had focus/interaction when the capture was triggered, with the
    hybrid toggle as the manual escape hatch when auto-detection guesses
    wrong — the one-click trigger shouldn't gain a mode-selection step for
    the common case.
- **Actual image content, not a description.** If MCP tool results can carry
  image content blocks Claude Code renders directly, prefer that over a
  file-path handoff the user has to separately open.
- **The captured payload includes the drawing/markup layer, not just the raw
  frame.** Per "the shared drawing layer" above — the person should be able to
  circle/arrow/underline on the frozen frame before it's shared, and the
  markup travels with the capture as part of the same payload (either baked
  into the image, or as structured overlay data alongside it — decide which
  and say why). This is not optional polish; it's the actual disambiguation
  value of this feature, not an add-on to a plain screenshot.
- **Built on the same primitive as Part 3, not a separate one.** The
  drawing/markup surface used here should be the same component Part 3 uses
  for Claude's point-out markers — implemented once, triggered from both
  directions. Design this jointly with Part 3 rather than finishing Part 2's
  capture mechanism first and bolting annotation on separately.
- **Consider whether a short clip (not just a single frame) is worth it** for
  motion-related problems (a track jump, a segmentation flicker across frames) —
  and if so, what's cheap to capture vs. what's scope creep for a first version.
- **Fits the no-mutation/additive philosophy.** Should read the same way the
  README frames lab-log/notebook/board writes: create-only, recoverable, listed
  explicitly in the allow-list, with a test pinning it the way `test_client.py`
  pins the write set today.

Propose:
1. What gets captured, and by what trigger (button, shortcut, or both) in the
   Vue app — including how the person enters "annotate" mode on the frozen
   frame before sharing.
2. What new backend endpoint(s) this needs, and where it fits alongside the
   existing `/api/lablog`, `/api/notebooks`, `/api/chains/create` pattern.
3. The new MCP tool(s): name, inputs, what it returns, and how `guidance.py` /
   the session briefing would need to change so Claude actually knows to reach
   for it (per the README's rule: "a new tool must be named there or the
   assistant never offers it").
4. What's explicitly out of scope for a first version vs. worth flagging for
   later (e.g., start with single-frame + full state; clip capture and
   selection-region capture as a fast-follow).
5. Where this sits relative to the OBSERVER.md phased plan — is it a new phase,
   or does it belong inside Phase 2 (actionable assist)?
6. Whether the payload/addressing shape you propose would hold up as the base
   layer for later imaging-specific skills (segmentation QC, track review,
   channel diagnosis) — or whether it's being designed too narrowly for just
   the "share this one screenshot" case and would need rework once a second
   use case shows up.
7. How an annotated capture gets promoted to a Blackboard entry when it's
   worth keeping (per "the shared drawing layer" above) — a deliberate,
   separate action from sharing it, not automatic persistence of every
   capture.

### Part 3 — Design: the reverse direction (Claude points at something)

The mirror problem: Claude sometimes needs to point at something in the app
rather than the user sharing context with Claude — e.g. "the segmentation
error is here, on frame 40, at this location," or "click this control to fix
the parameter," or annotating a specific track in the viewer as suspicious.
Right now Claude can only describe a location in words, which is exactly as
lossy in this direction as it is in the other.

Two different kinds of targets to design for, and they likely need different
mechanisms — don't collapse them into one:

- **Data anchors** — a specific cell, track, ROI, or measurement that already
  has coordinates/identifiers in the backend (from segmentation, tracking, or a
  measures table). Claude already gets these ids through existing read tools;
  the new piece is a way to say "highlight/mark entity X" and have the Vue
  viewer render that, live, using whatever transport already pushes state to
  the frontend (the WebSocket stream the MCP monitor already reads from is the
  natural candidate to also carry this, if it doesn't already). **The
  correction cockpit's highlight machinery (Part 1) is the primary reuse
  candidate here** — it already flags objects and shows them highlighted in
  both the viewer and gating plots, which is precisely "given an entity id,
  highlight it everywhere the app knows how" already built. If Part 1 found it
  unified, the new MCP tool likely just needs to feed an entity id into that
  same mechanism, gaining viewer *and* gating-plot highlighting for free rather
  than building viewer-only highlighting from scratch. If Part 1 found it
  split, propose unifying it as part of this design rather than picking one
  implementation to extend and leaving the other to drift further.
- **UI anchors** — pointing at an interface element itself (a button, a panel,
  a specific control) to guide the user through the app, not through the image
  data. This is much closer to what the existing guide/pointer system already
  does for onboarding — the design question is whether that system's targeting
  mechanism (from Part 1) can be driven by an MCP tool call instead of only by
  hardcoded tutorial steps.
- **Freeform marks** — not an entity id or a UI element, but an arbitrary
  region/shape (a circle around an area with no track id, an arrow, an
  underline) — the same shape a person draws on a captured frame in Part 2,
  used by Claude instead. This is the actual point of "the shared drawing
  layer" above: it must be the *same* drawing primitive as Part 2's
  annotation layer, not a third mechanism alongside data anchors and UI
  anchors. Design this jointly with Part 2, not as an afterthought once
  Part 2's capture mechanism is finished.
- **Grid/landscape anchors — for images with no existing segmentation to
  anchor to.** Freehand pixel-level localization on raw, unsegmented image
  content is a known weak point for vision-language models generally, not
  something to trust as a one-shot capability (see the freeform-marks caution
  below). The fix is not to make Claude better at guessing coordinates — it's
  to give it a coordinate system to point *within*, the same way an address
  or a map grid lets you name a location without visually estimating lat/long.
  Two layers, worth building both, cheap-to-expensive:
  1. **A coarse grid overlay** — divide the frame into a fixed set of labeled
     cells (spreadsheet-style, e.g. tile B3), computed from geometry alone, no
     image analysis required. Always available, always cheap. This turns
     "point at the exact pixel" into "name the cell it's roughly in" — a much
     easier and more verifiable claim. **This mechanism has direct published
     precedent — pick one, don't re-derive:** SCAFFOLD (Lei et al., arXiv
     2402.12058) overlays a uniform labelled dot matrix and extends to 3D
     (t,x,y); Marked-Grid Scaffold (2024 GUI-grounding follow-up) uses an
     8×8 / 9×9 labelled grid, discretising coordinate prediction into
     classification-over-grid-IDs; Grid-Augmented Vision (Chen 2024, arXiv
     2411.18270) is a plain 9×9 grid overlay, no training, improves
     localization on GUI / robotics / medical images. See
     `docs/archive/landscape-anchor-prior-art-audit.md` for the comparison.
  2. **A cheap "landscape resolution" pass** — not full segmentation, a fast,
     rough semantic layer over the grid: k-means-style clustering (already
     tried informally, worth formalizing) rather than full linear unmixing,
     to get an approximate semantic map per tile — what kind of signal
     dominates each region — plus optionally a heatmap/texture layer
     (intensity, local variance, or another cheap per-tile statistic) as an
     alternative or complement to hard clustering. The output is a short list
     of *discrete, labeled candidates* per grid cell, not a pixel-accurate
     mask. This turns "freehand point at something" into the same
     discrete-selection problem data anchors already solve reliably — picking
     among a small labeled set — instead of continuous localization from
     nothing.
  This pass must stay genuinely cheap (on-demand, any image, not a
  full analysis pipeline) or it defeats the purpose of handling the
  no-segmentation case at all — if it needs real compute time, that's a sign
  it's turned into "just run the real pipeline," which is a different,
  already-covered case (data anchors), not this one. **This pass only ever
  runs in viewer/image capture mode (Part 2) — never triggered by a UI-only
  capture, and never automatic on every viewer capture either; it should be
  something Claude (or the person) requests explicitly when it's actually
  needed, not a default cost on every share-in.**

  **Precedent, and a real alternative worth weighing, not just validating the
  chosen approach:** Set-of-Mark prompting (Yang et al., Microsoft Research,
  2023, arXiv 2310.11441 — overlay a segmentation model's regions as
  speakable marks; GPT-4V+SoM beat a fully-finetuned referring-segmentation
  model zero-shot on RefCOCOg) is direct precedent for "discrete labeled
  marks beat asking a multimodal model for raw coordinates" — worth taking as
  inspiration for the *shape* of this mechanism, not ported wholesale (SoM
  was validated on natural photos; fluorescence/intravital microscopy is a
  real distribution shift, and a "semantically meaningful region" from a
  general segmentation model may not correspond to a biologically meaningful
  one — this needs its own validation, not an assumption it transfers).
  **No published SoM evaluation on fluorescence / intravital microscopy
  surfaced in a targeted 2026-09-18 search** (see
  `docs/archive/landscape-anchor-prior-art-audit.md`) — treat as a real
  unknown. The design must include a small internal evaluation on Cecelia
  data (a handful of frames from `zolIMa` / `jFWePN` with expert-marked
  ground-truth ROIs, compared across SoM+SAM, SoM+μSAM, SoM+Cellpose-SAM,
  and the plain grid overlay) *before* locking the region source, not after.

  Concretely for the region source, evaluate at least three candidates rather
  than defaulting to raw SAM: (a) a fast SAM variant — MobileSAM, FastSAM,
  EfficientSAM, or SAM 2 for temporal — as the visual-region baseline; (b)
  **μSAM / MicroSAM** (Archit et al., *Nature Methods* 22, 579–591, 2025) —
  the canonical microscopy-prompted segmentation model, LM+EM, napari plugin
  shipping; (c) **Cellpose-SAM** (Pachitariu lab, bioRxiv 2025) — SAM
  backbone inside the Cellpose stack that Cecelia already runs. For
  fluorescence/intravital data, raw SAM regions are usually contrast blobs
  that split cells; μSAM and Cellpose-SAM produce biologically-meaningful
  regions and should be the default, not the fallback. Reserve the plain SAM
  variant for cases where you truly want *visual* over-segmentation to feed
  the marks scheme rather than semantic regions. Marks should be "speakable"
  (numbers/letters), not arbitrary ids, so Claude can reference them
  naturally in text.

  Separately: **automated ROI-targeting in microscopy without any LLM is
  already a mature field, and shipping commercially today** — Micropilot
  (Conrad et al., *Nature Methods* 2011, doi:10.1038/nmeth.1558) is the
  citable early example (real-time classifier-driven acquisition changes);
  SBEMimage (Titze et al., *Front Neural Circuits* 2018) covers volume-EM
  acquisition; ZEISS ZEN Smart Acquisition Toolkit and Nikon NIS.ai are the
  current commercial state, both shipping DL-driven event/target adaptive
  acquisition; see the smart-microscopy roadmap review (*npj Imaging* 2026)
  for the current landscape. This is evidence that *detection itself* is
  often already a solved classical-CV problem; the open question for using Claude here at all is whether
  natural-language-specified criteria (flexibility) is worth trading against
  the reliability of a purpose-built classical detector for well-specified,
  repeated criteria. Don't treat "route ROI-finding through Claude" as the
  default just because this design exists — say plainly in your report where
  a classical detector would likely outperform this mechanism, and where
  language-flexibility genuinely earns its keep.

  **This should be built as a standalone, generally useful capability, not
  Claude-exclusive scaffolding.** If the grid overlay and landscape/candidate
  pass are implemented as a real feature — a person can turn it on to get a
  quick semantic overview of an unsegmented image, or use candidate regions
  as a starting point for manual review, independent of any Claude session —
  that's a genuine win on its own, and it also means the MCP tool is just one
  more consumer of an already-useful feature rather than a special AI-only
  code path. Design it this way from the start: the Vue-side rendering of the
  grid/landscape should be a real, user-facing feature, with the MCP tool
  exposing the same underlying computation, not a hidden backend-only pass
  that only Claude can trigger.

Constraints, following the same shape as Part 2:
- **Additive, one new write path, same allow-list discipline** as the
  lab-log/notebook pattern — a marker/annotation is a small created record, not
  a mutation of viewer or app state.
- **MCP-only addressing** (see the cross-cutting constraint above) — the tool
  Claude calls should accept ids/coordinates it already has from other MCP
  tools, never something it had to infer by reading source.
- **Reuse over reinvention, and one drawing primitive, not four.** If the
  existing guide system's targeting mechanism is stable-identifier-based (not
  raw pixel coordinates), extend it. Data anchors, UI anchors, grid/landscape
  anchors, and freeform marks should share one underlying rendering/markup
  component wherever possible — differing only in what identifies the target
  (an entity id, a DOM id, a grid cell/candidate id, or raw coordinates) —
  not four separate implementations that happen to all draw on the same
  canvas.
- **Ephemeral by default** — a marker Claude places should be easy to dismiss/
  expire; this is for drawing attention in the moment, not permanently
  annotating the dataset (that's what lab-log/notebook writes are already for).

Propose:
1. The new MCP tool(s) for placing (and clearing) a marker/annotation — name,
   inputs (what identifies the target: entity id vs. UI element id vs. grid
   cell/candidate id vs. image-space coordinates), and how it reaches the
   frontend (WebSocket push into the already-open viewer, vs. something the
   user has to pull).
2. Whether data anchors, UI anchors, and grid/landscape anchors should be one
   tool with a target-type parameter, or separate tools — and why.
3. How this interacts with `guidance.py`/the session briefing, same as Part 2
   — including whether Claude should be told explicitly to prefer data
   anchors over grid/landscape anchors whenever segmentation data already
   exists, since the former is the more reliable claim.
4. What's out of scope for a first version (e.g., start with data anchors in
   the viewer only; grid/landscape anchors and UI-anchor guidance as
   fast-follows, roughly in that order of value vs. effort).
5. For grid/landscape anchors specifically: what the k-means-style clustering
   pass should run on (raw channel intensities? a cheap embedding?), how
   many grid cells/candidates is a reasonable default, and whether this reuses
   any existing coastal/segmentation-adjacent code rather than being written
   from scratch. Compare explicitly against a lightweight pretrained
   segmentation model (per the SoM precedent above) as an alternative
   candidate-generation method.
6. Whether the grid/landscape mechanism should ship as a user-facing Vue
   feature in its own right (a toggle in the viewer, independent of any
   Claude session) with the MCP tool as one consumer of it — or whether
   there's a real reason to keep it backend-only. Default to building it as a
   standalone feature unless there's a concrete reason not to.

### Part 4 — Design: the Blackboard (versioned, kept — not throwaway)

Share-in and point-out (Parts 2–3) are both about a specific moment: this
frame, this track, right now. The Blackboard is different on purpose — a place
for concepts, diagrams, and open questions the person and Claude develop
together over time, meant to be **kept, not thrown away**, and explicitly
**not executable** — the direct counterpart to the chain whiteboard, which is
executable and never speculative.

Contrast to hold onto throughout this design:

| | Chain whiteboard | Blackboard |
|---|---|---|
| Content | Real tasks, real params | Diagrams, notes, ideas |
| Validated? | Yes — must be runnable | No — can be wrong, unfinished, speculative |
| Can it execute? | Yes, that's the point | Never |
| Lifecycle | Template → run → frozen snapshot | Accumulates, versioned, edited over time |

Requirements:
- **Mermaid as the default diagram format.** It renders natively in Vue with no
  new rendering engine, and Claude already produces Mermaid directly — so a
  diagram Claude proposes in a Claude Code session can be pasted straight into
  a Blackboard entry and rendered, no conversion step.
- **Versioning: reuse the Pluto notebook versioning system, don't build a new
  one.** Notebooks already have a working version history mechanism. Find it
  in Part 1, pull the versioning logic out into a shared component both
  notebooks and the Blackboard use, and extend it only if the Blackboard's
  needs (e.g. mixed text+diagram content vs. notebook cells) genuinely don't
  fit as-is. This is the same "reuse over reinvention" principle as the
  correction-cockpit highlight machinery in Part 3 — don't stand up a second,
  parallel versioning implementation next to one that already works.
- **Additive at the storage layer**, same allow-list discipline as lab-log/
  notebooks — a new Blackboard entry or version is a created record, never a
  destructive edit to an existing one.
- **Addressable and citable**, same as everything else in this design — an
  entry should have a stable id so it can be referenced from a lab-log note, a
  chain, or a future point-out marker ("see Blackboard entry X for why this
  node is structured this way").
- **Mixed content, not diagram-only.** Should support plain notes and open
  questions alongside Mermaid blocks — sometimes the shared idea is text, not a
  diagram.
- **Conditional visibility, piggybacking on the existing Claude Code check.**
  The settings page already has a check for whether Claude Code is set up on
  the machine (find it in Part 1). The Blackboard page should be conditional on
  that same check — visible only when there's actually a Claude Code session
  this feature is useful for, rather than a page that appears for every user
  regardless of setup. Reuse that existing check directly; don't build a second
  detection mechanism.

Propose:
1. What the Pluto notebook versioning mechanism actually is (found in Part 1),
   what shared component it should be extracted into, and what — if anything —
   needs to change to also fit Blackboard entries (mixed text/diagram content
   rather than notebook cells).
2. Where Blackboard entries live on disk (parallel to
   `<project>/settings/chains/` seems natural — e.g.
   `<project>/settings/blackboard/`) and what a version history looks like on
   disk, using the extracted versioning component from (1).
3. The new MCP tool(s): create an entry, add a new version to an existing
   entry, read an entry (latest or a specific version), list entries. Same
   additive-only shape as the lab-log/notebook tools.
4. How a Blackboard entry surfaces in the Vue app — a dedicated view/panel
   rendering Mermaid + notes, gated by the existing Claude Code setup check
   (found in Part 1), and whether it needs live-update (WebSocket) or a
   simple pull-on-open is sufficient, given entries aren't time-sensitive the
   way a viewer marker is.
5. Whether this belongs in `guidance.py`/the session briefing the same way
   other MCP tools do (Part 2/3's rule applies here too: unlisted tools don't
   get used).
6. **How this links to the lab-log and to `docs/todo/*_PLAN.md`'s "Locked
   decisions" convention** (not a new ADR folder — see the maintainability
   audit's revised guidance): a plausible boundary is —
   - Lab-log stays purely chronological ("what happened," including Claude's
     QC/notes entries) — unchanged by this design.
   - Each new Blackboard diagram version writes a companion lab-log entry with
     the reasoning for that update, so the "why did this change" narrative is
     preserved chronologically even though the diagram itself is versioned
     separately.
   - A Blackboard idea that settles into an actual decision graduates into the
     relevant feature's `*_PLAN.md` "Locked decisions" section — the Blackboard
     entry stays as the working history, the plan doc gets the settled
     decision.
   Don't take this boundary as fixed — propose it, or a better one, and say
   why, rather than letting Blackboard/lab-log/plan-doc overlap with unclear
   edges.
7. **Whether Blackboard versions need pruning, like notebooks did.** The
   project already hit an accumulation problem with notebook versions and
   introduced a pruning mechanism (find it in Part 1). Mermaid + text entries
   are almost certainly far lighter per-version than notebooks, so the
   calculus may be different — but check the actual numbers (typical entry
   size, expected versioning frequency if every diagram update also logs to
   the lab-log per point 6) rather than assuming "text is small enough" is
   sufficient. If pruning is warranted, reuse the existing notebook pruning
   mechanism/policy rather than designing a second one; if it isn't, say so
   with the reasoning, not just "probably fine."

## Your own verdict

After the audit and design, give an honest, unhedged take. The first four
questions matter most — answer these with real conviction, not just listed
alongside everything else:

- Is "on-demand context-sharing via a new MCP tool" actually the right shape
  for this, or is there a simpler mechanism already close at hand (e.g. a
  clipboard-image paste workflow) that would get 80% of the value with far
  less new surface area?
- Did the shared drawing layer (Parts 2–3) actually end up as one primitive
  used from all four anchor types and both directions, or did some of them
  (entity ids, DOM ids, grid cells, raw coordinates) turn out different
  enough that forcing one component was more friction than it was worth? Say
  so plainly if two or three related-but-separate implementations is the
  honest answer.
- Given classical, non-LLM automated ROI-targeting is already mature and
  already shipping commercially (Micropilot 2011; ZEISS ZEN Smart
  Acquisition Toolkit and Nikon NIS.ai today): for Cecelia's actual use
  cases, is
  routing ROI-finding through Claude likely to add real value over a
  purpose-built classical detector, or is the honest answer "only for ad hoc,
  language-specified, one-off criteria — build the classical path too, and
  make it the default for anything repeated"? Give a clear recommendation.
- If you were prioritizing this against the maintainability/navigability
  audit (comment quality, implicit contracts, monolith pressure,
  `docs/MAP.md`) done separately on this codebase, where does this rank? Is
  this the more urgent problem, or is it visible/exciting but lower-impact
  than the quieter structural issues?

The rest, still worth answering plainly, lower-stakes individually:

- Anything in the existing MCP/observer architecture that actively resists
  this addition and would need to change first?
- Does "this is the foundational primitive for a Claude Imaging direction"
  framing actually hold up, or is that overreaching from a single feature?
  Weigh Anthropic's own 2026-08-27 MHS launch as evidence here — an adjacent
  upstream primitive from Anthropic itself lowers the "overreach" bar, but
  don't inflate the framing beyond what the concrete design here actually
  delivers.
- Does anything in the proposed design tacitly assume MHS-shaped upstream
  acquisition metadata will be available, or fail to work if it isn't? The
  design should stand on its own with Cecelia's own state as the only ground
  truth, and be *compatible with* future MHS interop without depending on it.
- Is the existing guide/pointer system a good fit to extend for Part 3, or
  does its design make it awkward to drive from an MCP tool call rather than
  a hardcoded tutorial step?
- Is the correction cockpit's viewer/gating-plot highlighting one shared
  implementation or two that happen to look alike? If split, is unifying it
  in scope here or genuinely separate work?
- Does the Blackboard need to be a new subsystem, or does it overlap enough
  with the `*_PLAN.md` "Locked decisions" convention that maintaining both
  would just create a second place to look for the same content?
- Is the lab-log/Blackboard/`*_PLAN.md` boundary proposed in Part 4 actually
  workable day-to-day, or does it create busywork? Propose a lighter rule if
  so.
- Did the final design end up as one connected set of mechanisms, or did some
  pieces end up isolated despite the intent? Point at any place where two new
  components can't reference each other and should be able to.
- Is the grid/landscape anchor idea a good tradeoff, or does the k-means-style
  pass introduce its own false-confidence risk? Should its output be
  presented with an explicit "coarse/approximate" framing in how it's
  rendered, not just documented?
- Does the UI/viewer/hybrid mode split (Part 2) cover the real cases cleanly,
  or is there a capture scenario that doesn't fit any of the three? Does
  auto-detection-by-last-focus work reliably given how the Vue app is
  structured?
- Does the Pluto notebook versioning mechanism actually generalize cleanly to
  Blackboard entries, or does notebook-specific structure make the "shared
  component" idea harder than it sounds?
- Anywhere in your own proposed design that would only work because Claude
  Code currently has local repo access — anything that quietly assumes
  code-reading as a fallback and would break on a remote deployment. Worth
  stating explicitly even if the answer is "none found."
- Is the proposed PR sequence (per the cross-cutting constraint above)
  actually independently mergeable step by step, or does it secretly require
  an earlier PR to land before a later one is even reviewable in isolation?

Don't hedge any of this to match the tone of the rest of the report — this is
the part meant to be read first.
