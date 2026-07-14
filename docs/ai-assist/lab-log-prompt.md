# Phase: Lab Log — Shared Project Memory

Design phase for Opus. Read `mcp-readonly-prompt.md` and `qc-process-prompt.md` alongside this. The lab log is the shared artefact that makes Claude useful across sessions. Without it, every session starts blind. With it, Claude accumulates the non-obvious knowledge that isn't inferable from data or framework alone.

---

## What the lab log is

A per-project, append-only, human-and-AI-writable file that lives in the project directory. It is the persistent memory for AI-assisted analysis sessions.

It is NOT:
- Documentation (that's `docs/`)
- A task log (that's the per-image task log the scheduler writes)
- A results file (that's the analysis output)
- Image notes (those are per-image, written by the user in Cecelia)

It IS: knowledge that would otherwise be lost between sessions. The methodology. The why. The edge cases. The corrections.

**Opus: propose the best filename.** Working title `lab-log.md`. Consider: lives in the project directory, written by human and AI, append-only, purpose is persistent cross-session AI context. The name should be intuitive to a biologist opening the project directory for the first time.

---

## Relationship to image notes and task logs

The lab log complements, not duplicates:

- **Image notes** (per-image, user-written in Cecelia UI) — quick observations about a specific image. "This section has a fold." "Channel 2 autofluorescence high."
- **Task logs** (per-image-per-task, written by the scheduler) — what ran, parameters, output paths, errors. Machine-written.
- **Lab log** (per-project, human and Claude) — cross-image knowledge, methodology rationale, pattern recognition, corrections. What image notes and task logs don't capture.

Claude reads all three. The lab log is the only one Claude writes to.

---

## What goes in it

**Yes:**
- Gating decisions not derivable from data ("CD4+ gate runs loose on spleen, channel 2 lower boundary ~0.3 too low for this tissue prep")
- Image exclusions with quantitative justification
- Parameter choices that worked for this cohort and why
- Cross-experiment patterns Claude recognised
- Corrections to Claude's prior suggestions
- User's stated rationale when Claude asked "why did you do that"
- QC flags that were acknowledged and what action was taken

**No — inferable from framework or data:**
- Task completion status (task manager)
- Image dimensions, channel names (MCP tools)
- Population counts (MCP tools)
- What module functions exist (CLAUDE.md)

---

## Format

Append-only, dated, author-tagged entries. Never edit old entries — add a correction entry instead.

```markdown
## 2026-07-14 [Claude]
- Image 7 flagged: gate produced 23 cells (cohort mean 187). User excluded image.
- Repeat attempt pattern: cellpose on image 12 run 8 times. User confirmed tissue folding 
  is the issue — cannot be recovered. Excluded.

## 2026-07-14 [User]
- CD4+ gate: lower boundary channel 2 should be ~0.25 for this tissue prep, not 0.3 default.
  Spleen samples in this batch have higher background than thymus.
- Image 3 state 1 dominance is real biology — early-stage infection. Keep, note in analysis.

## 2026-07-14 [User — correction]
- Corrects 2026-07-12 [Claude]: Image 5 speed distribution is NOT an outlier. This mouse 
  received a different treatment — the higher motility is expected. Do not flag this pattern
  for treated cohort.
```

Author tag `[Claude]` or `[User]` on every entry block. Correction entries reference the original by date and author. Claude reads corrections and adjusts reasoning within the session.

---

## MCP tools

```
read_lab_log       → full log content, returned newest-first for efficient context loading
append_lab_log     → append a dated [Claude] entry, never edits, enforces append-only
```

User writes directly to the file or via the Vue panel. Claude writes only via `append_lab_log`. This keeps Claude entries identifiable and prevents overwrites.

---

## Vue panel

The lab log GUI is what determines whether anyone actually uses it. The file is the backend. The panel is the product. Design for zero friction between "I just made a decision" and "it's recorded."

**Placement**: persistent slide-out panel accessible from any module page without navigation. Not a separate page. The user makes a gating decision, glances right, types one line, continues. If they have to navigate somewhere to log it, they won't.

**Always-visible entry field**: a single text input at the top of the panel, always focused, date and author pre-filled. One sentence minimum. Submit with Enter. No form, no fields, no save button to click.

**Context-aware prefill**: when the user is on a module page and something changes — threshold adjusted, image excluded, task re-run — a "note this" prompt appears inline on that page, pre-filled with the context (image UID, stage, what changed). The user adds the reason and submits. The worst version is an empty box. The best version already knows what happened and just needs the why.

Example prefill from a gating action:
```
[2026-07-14] CD4+ gate threshold changed 0.3 → 0.25 on image KDIeEm — [your note here]
```

**Project open summary**: when a project is opened that has lab log entries, the most recent 3-5 entries surface automatically in a banner or sidebar without the user having to open the panel. If you haven't touched this project in three months, the first thing you see is what you were doing and why.

**Entry display**: chronological, newest first. Clear visual distinction between `[Claude]` and `[User]` entries — different background colour or left border colour, not just a tag. New Claude entries since last session are badged so the user is prompted to review them.

**Correction**: one-click on any entry opens a pre-filled correction: `[correction to YYYY-MM-DD User/Claude]: ` — user completes it and submits. No editing of old entries, only appending corrections.

**Read-only for Claude entries in the UI**: Claude entries cannot be edited from the panel. Append-only enforced both in the UI and in the MCP write tool.

Note: voice input is explicitly out of scope — it solves a problem that doesn't exist for this workflow and adds unnecessary complexity.

---

## Versioning

The lab log is per-project and per-version. On version bump, prior lab log is copied with a version boundary header:

```markdown
## [Version boundary: v1 → v2, 2026-07-15]
Entries below are from v1. Context may still be relevant.
---
```

Institutional memory does not get lost on version bump.

---

## Claude session behaviour

On session start: Claude reads the full lab log. Weights recent entries more heavily. Explicitly notes correction entries and adjusts reasoning — a corrected pattern should not be flagged again.

Claude appends to the lab log during the session when:
- A QC flag fires and the user acknowledges it
- A why question is answered by the user
- A cross-experiment pattern is recognised
- A parameter adjustment is accepted or rejected

Claude does NOT append for routine completions, ambient observations that don't reach flag threshold, or anything that duplicates what image notes or task logs already capture.

---

## Implementation plan

Opus: produce a numbered step plan covering:

1. File format spec — exact markdown structure, required fields, author tag format, correction entry format
2. Filename decision — propose and justify
3. MCP read tool — full content return, newest-first ordering, any summarisation for very long logs
4. MCP write tool — append-only enforcement, date injection, author tag injection, format validation
5. Vue panel — placement in UI, entry display, user input, correction template, visual distinction
6. Version copy behaviour — trigger, header format, how old entries are marked
7. Claude weighting — how recent vs. old entries are handled, how corrections gate future flags
8. Integration with image notes and task logs — how Claude uses all three together without duplicating
9. Dependencies on QC process (QC-triggered appends) and MCP server — build order
