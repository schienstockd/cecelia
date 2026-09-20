> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/todo/PROJECT_MEMORY_PLAN.md`, which reuses the shipped
> Blackboard (from `docs/todo/BIDIR_CONTEXT_PLAN.md`) rather than the parallel index this prompt
> assumed. The prompt's "narrative vs configuration-artifact" split still holds; its precondition
> ("wait until Blackboard has shipped") was met — Blackboard shipped 2026-09-20.

# Project Memory: A Queryable Findings & Configuration Index — Cecelia

## Prerequisite — do not run this until the bidirectional-context-sharing
## design has actually shipped, not just been designed

This prompt references the Blackboard, capture entries, and lab-log
conventions from `bidirectional-context-sharing-audit-prompt.md` as things it
indexes over. As of writing this, only share-in (`#1040`) has actually
landed — point-out, the shared drawing layer, grid/landscape anchors, and the
Blackboard itself (Part 4 of that design) are still unbuilt specs, not real
storage shapes. Designing an index against a spec that might still change
during implementation risks building against the wrong interface and having
to redo it.

**Wait until at least the Blackboard has shipped** (and ideally point-out too,
since findings/configuration artifacts may end up referenced from point-out
markers) before running this prompt. When you do run it, treat the *actual
shipped* storage shapes as ground truth over anything this document assumes
about them — re-verify Part 1's audit against what was really built, not
against this document's guesses about what the design would produce.

## Background

Claude cannot be trained or fine-tuned per project — the model is fixed. The
only lever for "does Claude know we've dealt with this before in this
project" is **what's retrievable into context at the start of a session**,
not anything learned. That makes documentation quality a much higher bar than
notes for a person: it has to substitute for memory, not just supplement it.

This is broader than phenotypes/behavior. In practice it covers at least four
different kinds of "have we seen this before":
- **Findings** — a phenotype, a QC anomaly, a pattern in the data. Narrative;
  what you'd want *described back to you* with context.
- **Parameters** — the odd, easy-to-forget value that works for a specific
  case (a segmentation diameter for an unusual cell type, a normalization
  factor for a particular imaging condition).
- **Gating definitions** — the specific shape drawn to define a population on
  a scatter/gating plot for a particular cell type or condition.
- **Viewer settings** — a specific LUT/channel/camera configuration that
  reveals a structure well for a given image type.

The first is genuinely narrative — you want it recalled and *read*, with
reasoning attached. The other three are **configuration artifacts** — you want
them recalled and *reapplied*, exactly as they were, not re-derived from a
description. That's a different retrieval need, and the design below should
not collapse it into one mechanism. In particular: **do not fold this into the
Blackboard.** The Blackboard (versioned Mermaid diagrams + notes) is for
things actively being thought through and revised — a gating shape or a
viewer preset isn't something you reconsider, it's something you want back
byte-for-byte. Keep them conceptually and mechanically separate.

Today's actual retrieval surface, per `mcp/README.md`, is thin:
- `get_session_briefing` surfaces lab-log entries from **the last 7 days
  only**.
- `read_lab_log` returns the **entire** lab-log as one unbounded markdown
  blob — the only way to look further back, and it doesn't scale as a
  project's log grows. "Have we seen this before" currently means "dump
  everything and hope Claude notices the right line," which isn't retrieval,
  it's luck.
- There is no tool to search project history by phenotype, finding type,
  parameter, gating definition, or any other structured key. Everything is
  chronological-only.
- It's not yet established whether gating definitions, viewer presets, or
  per-run parameters even lack storage — see Part 1. The likely gap is not
  "these things aren't saved anywhere," it's "nothing lets you *find* the
  right one later."

This connects to two other things already in motion and should be designed
alongside them, not independently:
- **The Blackboard** (from the bidirectional-context-sharing design) — kept
  separate, per above. Narrative findings worth keeping may still live there
  or in the lab-log; configuration artifacts should not.
- **Behavior-readout standardization** — "have we seen phenotype X before" is
  only answerable if phenotype/finding descriptions are comparable across
  entries in the first place. A repeated finding described in three different
  ad hoc ways is invisible to search even if a search tool exists. This
  applies to findings specifically, not to the configuration-artifact side.

## Task

### Part 1 — Audit what's actually there

- Read the lab-log implementation in full: how an entry is structured on
  disk/in the API (`GET /api/lablog`, `POST /api/lablog/append`) — is it pure
  markdown text, or does it have any structured fields (date, source, tags)
  already that a search could key on?
- Read how QC findings, populations, and other per-image structured data
  (`get_qc_metrics`, `get_populations`, `get_measure_summary`,
  `get_behaviour_summary`) are already stored — is there an existing
  structured record of "finding" that this could build on, or would a new
  finding type need to be introduced?
- **Find out where gating definitions actually live today.** Population/
  gating definitions almost certainly already have some persistent
  representation (however a user currently draws and saves a gate on a
  scatter plot) — locate it. The question isn't whether to build gating
  storage; it's whether the existing storage is already named/described well
  enough to search, or needs metadata added.
- **Find out whether viewer presets/settings have any save mechanism today**
  (a LUT/channel/camera configuration a user can name and reload). If none
  exists at all, that's a real gap distinct from this prompt's actual scope —
  note it plainly rather than scope-creeping a new preset-save feature into
  this design.
- **Find out where per-run parameters are recorded** — the scheduler already
  tracks `params` per task run (content-hashed chain history, per earlier
  findings). Check whether "the diameter that worked for this weird case" is
  already sitting in that provenance, just not searchable by "weird case" as
  a concept.
- Check whether anything like search/indexing already exists anywhere in the
  codebase (even unrelated to lab-log) that this could reuse rather than
  building a new indexing mechanism from scratch.

### Part 2 — Design a queryable index, split by what's actually being recalled

Goal: an MCP-accessible way to answer "has something like X come up in this
project before, and what did we do about it" — for findings (narrative) and
separately, "what configuration did we use for case Y, give it back to me" —
for parameters/gating/viewer settings (artifact recall).

Requirements, split explicitly:

**For findings (narrative recall):**
- **Keyed by something structured, not full-text guessing** once the
  behavior-readout standardization work exists — a phenotype label, a
  motility descriptor range, a QC finding type. Full-text search over
  free-form lab-log prose is a fallback, not the primary mechanism.
- **Built on what already exists, not a parallel history.** Index over
  lab-log/Blackboard entries, don't create a third writing surface.

**For configuration artifacts (parameters, gating shapes, viewer settings):**
- **Index over wherever Part 1 found these actually live** (task-run
  provenance, gating-definition storage, viewer-preset storage if it exists)
  — don't duplicate the artifact itself into a new store. The index should
  hold a reference (id/path) plus enough tagged context to search by ("this
  diameter was used for elongated dendritic-cell-like morphology"), and the
  actual value/blob stays in its existing home.
- **Retrieval should return something directly reusable**, not just a
  description — e.g. a gating-definition id the app can load back into the
  gating UI, a param value that can be pasted into a new chain node, a viewer
  preset that can be reapplied to the current session — not just "yes, we
  used something like that once, here's a paragraph about it."
- **If no persistent storage exists yet for one of these** (per Part 1 — e.g.
  viewer presets might not be saveable at all today), say so explicitly and
  scope that as separate, prerequisite work rather than silently building it
  as a side effect of this design.

**Shared across both:**
- **Project-scoped, travels with the project** — not a cross-project or
  field-wide index (that's the Immunemap-scale problem from the imaging pitch
  doc, out of scope here).
- **Additive, same allow-list discipline as everything else in this design.**
  A new MCP read tool is easy — read-only, no new write surface needed if
  tagging happens via metadata on existing writes/artifacts.
- **Cheap to query at session start.** `get_session_briefing`'s 7-day window
  exists for a reason (token cost) — this should be something Claude calls
  deliberately when it has a specific thing to check, not something that
  dumps a large result into every session's opening context.

Propose:
1. What structured metadata a lab-log/Blackboard entry, a gating definition,
   a viewer preset, and a task run need to carry to be indexable — likely
   different tag shapes for narrative findings vs. configuration artifacts;
   don't force one schema to fit both.
2. The new MCP tool(s): one for "has this finding come up before" and
   (separately, since the return shape differs) one for "what configuration
   did we use for case Y" — or one tool with a mode parameter, if that's
   genuinely cleaner. Name, inputs, and what each returns.
3. Whether tagging happens automatically (at write/save time) or requires an
   explicit step, and how to avoid this becoming one more convention that's
   stated but not enforced — same failure mode as the zarr-writer and CSS
   findings from earlier in this project. If tagging is Claude's own
   responsibility at write time, what would actually verify it's happening,
   rather than trusting it will?
4. How this surfaces in `guidance.py` — when should Claude actually check
   history/prior configuration before proceeding, versus proceeding without
   checking? Don't make every session query reflexively; define when it's
   actually warranted (e.g. before proposing a segmentation parameter for an
   unusual case, before writing a new QC finding, before proposing a
   phenotype label).
5. How this relates to `docs/MAINTAINABILITY.md`/`docs/MAP.md` — this is a
   new standing mechanism and should be recorded there once built.

## Your own verdict

- Is a structured tag vocabulary actually necessary for findings, or would a
  good full-text/semantic search over the existing lab-log + Blackboard
  content get most of the value with far less new machinery? Give a clear
  recommendation.
- For configuration artifacts specifically: does Part 1 actually find
  existing storage for all three (gating, viewer presets, params), or is one
  of them missing storage entirely — meaning this design has a genuine
  prerequisite gap, not just an indexing gap? Say so plainly.
- What's the actual failure mode if tagging is inconsistent — does a missed
  tag mean a real prior finding or configuration silently doesn't surface
  (dangerous, since Claude would confidently proceed as if it were novel),
  and if so, what mitigates that risk given tagging can't be perfectly
  enforced any more than "reuse, don't reinvent" was?
- Does this need the behavior-readout standardization work to exist first for
  the findings half, or can a useful first version ship before that's done,
  using looser matching in the meantime? Does the configuration-artifact half
  depend on it at all, or is it independent? Say plainly which order actually
  makes sense.

Don't hedge this section to match the tone of the rest of the report — this is
the part meant to be read first.
