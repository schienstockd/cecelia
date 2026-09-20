# Project memory — plan

**Status:** planning (2026-09-20). Design lives on `docs/project-memory-plan`. Written to be picked
up cold; sits on the shipped Blackboard, captures and lab-log surfaces from
[`BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md) (parts 2 + 4 + 5 + 6 + 7 — all landed) rather than
defining new storage. Companion reading: [`docs/archive/project-memory-index-prompt.md`](../archive/project-memory-index-prompt.md)
(the ask, written before Blackboard shipped — its "narrative vs artifact" split still holds; its
storage assumptions were overtaken by shipping) and [`docs/archive/claude-imaging-pitch.md`](../archive/claude-imaging-pitch.md)
(what this connects to at the field level).

## Goal

A per-project, structured, retrievable memory Claude Code can lean on across sessions — so a fresh
session doesn't rediscover the project from zero, and prior findings / decisions / configurations
surface when they're relevant.

Concretely: at the start of a session Claude knows what this project *is* (subject, cohort, goal,
key channels), what threads are currently open, and can search prior narrative when it needs to
check "have we seen this before" — without pulling the entire lab-log into every context window.

The memory **travels with the project**: it lives in the project directory, exports through
`.ccbundle` by construction (see [`PROJECT_IO_PLAN.md`](PROJECT_IO_PLAN.md)), and is portable across
machines. It is **not** in `~/.claude/` — that is per-workstation, per-user, and does not survive a
project hand-off.

## What this is NOT

- Not a cross-project index. One project's memory does not surface in another's briefing.
  Federation across projects (Immunemap-scale) is out of scope — see
  [`IMMUNEMAP_IMPORT_PLAN.md`](IMMUNEMAP_IMPORT_PLAN.md).
- Not a config-artifact recall system. Reapplying "the diameter/gate/LUT we used for that case"
  is a real want (Decision 6 defers it) but a different problem shape — it needs tags on writes,
  not narrative search. Revisit once narrative search is proven in flight.
- Not a phenotype vocabulary. Standardising behaviour readouts across images is
  [`BEHAVIOUR_READOUT_PLAN.md`](BEHAVIOUR_READOUT_PLAN.md); this plan does not depend on it.
- Not a new writing surface. See Decision 1.

## Cross-cutting constraints

- **Reuse the shipped surfaces.** Blackboard is the narrative store, captures are the pixel-state
  citation, lab-log is the chronological archive. No parallel store. If a want doesn't fit one
  of these three, it belongs in a follow-up plan, not here.
- **Portable by construction.** Every byte lives under `<proj>/`, never under `<config_dir>/` or
  `~/.claude/`. `.ccbundle` export/import already carries `<proj>/blackboard/`,
  `<proj>/captures/`, `<proj>/settings/`, `<proj>/lab-log.md` — do not introduce a memory path
  outside those.
- **MCP-only at Claude's end.** Every read is through an allow-listed MCP tool. Every write
  Claude does goes through an existing tool (`create_blackboard_entry`, `revise_blackboard_entry`,
  `append_lab_log`) — this plan does not add a Claude-authored write path.
- **Additive-write discipline.** Same as [`BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md): no
  mutations of viewer state, gates, populations, or analysis data. Status changes on a Blackboard
  entry are additive (a new snapshot); no field is silently overwritten.
- **Cheap at session start.** The briefing must stay small — profile body + open-entry titles +
  last N captures. Deep lookups happen on demand via targeted MCP calls, never in the opening
  payload. Same reasoning as the existing 7-day lab-log slice, applied to a durable surface.

## Locked decisions

Numbered so code and other docs can cite them (`Decision 5`).

1. **Reuse Blackboard as the narrative memory surface. Do not build a parallel store.**
   Blackboard already ships per-project markdown with versioning, capture references, and MCP
   read/write. A second markdown-backed store for "memory" would duplicate authoring UI, storage
   layout, and MCP surface. One canonical helper per job (see `CLAUDE.md` → *Watch for divergent
   re-implementation*).

2. **Project profile = Blackboard entry with reserved id `profile`.** Auto-created empty on first
   project open. Not a new artifact class. Free-form markdown with a schema-lite header
   (subject, modality, cohort/groups, key channels, current goal) that Claude revises as
   understanding deepens. Session briefing loads its full body every session. Rationale: no new
   editor, no new storage, no new MCP tool needed to author it — `revise_blackboard_entry`
   already exists. The reserved id is a light semantic overload of Blackboard entries (a `profile`
   is a "kind"); mitigated by pinning it to the top of the module's list.

3. **Add `status` field to blackboard entry `meta.json`.** Enum `open | resolved | parked`.
   Default `open`. Missing (pre-migration) backfilled as `open` on read. Status transitions are
   additive — set via an optional `status` param on `revise_blackboard_entry`, recorded in the
   entry's snapshot history like any other revision. This is the "what's currently on the table"
   signal the briefing keys on.

4. **Search is plain substring in v1.** `search_blackboard(query, status?)` scans entry titles +
   bodies server-side, returns id + title + snippet + status, newest-match first. Blackboard
   bodies are already capped at 100 KiB per entry, so a full scan is cheap at the sizes this
   store reaches. Semantic search is documented as a future gap in `docs/FUTURE.md`; revisit if
   substring stops surfacing what a real session needs.

5. **Session-briefing composition.** `get_session_briefing` returns, in order: (a) the `profile`
   entry body in full; (b) up to 8 open blackboard entries (title + one-line snippet + updated-at);
   (c) the last 5 captures (id + surface + one-line notes preview if present). The 7-day lab-log
   slice drops out of the briefing default — `read_lab_log` is still there for chronological
   reading when explicitly wanted. Rationale: Blackboard is the durable "what we're thinking
   about" surface; the lab-log is a chronological record whose value is post-hoc. The briefing
   should key on the former.

6. **Config-artifact recall is deferred.** Reapplying a stored gate / diameter / viewer preset
   by intent is a real want but a different problem shape (needs tags on writes, retrieval
   returns something directly reusable, storage is per-image not per-project). Not built in this
   plan. Recorded in *Future work* below and revisited after v1 lands.

7. **Named viewer presets as a first-class artifact are deferred.** Today a viewer state only
   exists inside a capture envelope's `viewStateSnapshot`; there is no `settings/viewer_presets.json`.
   Introducing one is real work with its own UI and MCP surface, and is a prerequisite for
   Decision 6, not for the narrative half. Called out as a genuine gap so a future reader isn't
   surprised.

8. **The `KIWI_PLAN.md` status row in [`docs/todo/README.md`](README.md) is left alone by this
   plan.** Noted only as a housekeeping item found during the audit.

## Phases

Each phase is independently shippable and reviewable. Phases 2 and 3 may ship in one PR at the
maintainer's discretion; keep P1 separate.

### Phase 1 — Status field + reserved `profile` entry

**Goal.** Every blackboard entry carries a status; the project profile exists as a well-known
Blackboard entry from the moment the project opens.

- `api/src/blackboard_api.jl` — extend `meta.json` schema with `status`. Backfill missing as
  `open` on read. Extend `create_blackboard_entry` / `revise_blackboard_entry` to accept optional
  `status`. New helper: on project open, ensure `blackboard/profile/` exists (empty entry,
  reserved title `Project profile`, pinned).
- `frontend/src/stores/blackboard.ts` — plumb the `status` field through the store; add a
  `setStatus(bbId, status)` action that calls `revise_blackboard_entry`.
- `frontend/src/modules/BlackboardModule.vue` — status chip per row (open/resolved/parked);
  filter chip in the list view; pin the `profile` row to the top with a distinct affordance.
- `mcp/cecelia_mcp/server.py` — `list_blackboard_entries` returns `status`; `create/revise` accept
  it. Docstrings updated so a fresh model knows when to set which value.
- `mcp/cecelia_mcp/guidance.py` — guidance rule: when a Blackboard entry's topic has been
  resolved in this session (a decision was locked, a bug was fixed, a finding was acted on),
  transition it to `resolved`; when it's set aside deliberately, `parked`. Do not blanket-close
  entries as noise.
- Tests: `mcp/tests/test_server.py::GuidanceTest` pins the tool arguments; Julia round-trip test
  on `meta.json` including status; frontend store test on the new action.

### Phase 2 — Blackboard search

**Goal.** One MCP tool that answers "has this come up before in this project."

- `api/src/blackboard_api.jl` — `POST /api/blackboard/search` (body: `{query, status?, limit?}`).
  Server-side case-insensitive substring over `title` + entry body. Returns id + title + snippet
  (±40 chars around the hit) + status + updatedAt, newest-match first, capped `limit ≤ 50`,
  default 10.
- `mcp/cecelia_mcp/server.py` — `search_blackboard(query, status_filter=None, limit=10)`.
  Read-only, allow-listed. Explicit docstring on *when* to call it.
- `mcp/cecelia_mcp/guidance.py` — call this before: proposing a phenotype label that sounds
  familiar; suggesting a processing step for an unfamiliar image; writing a new finding that
  might restate an existing one. Not reflexively on every session.
- Tests: round-trip search over a fixture project; empty-result and long-query cases;
  guidance test pins the tool.

### Phase 3 — Session-briefing rewrite

**Goal.** Session start returns durable project context, not a chronological slice.

- `mcp/cecelia_mcp/server.py` — `get_session_briefing` returns the shape locked in Decision 5.
  `recentLabLog` drops from the default payload; `profile`, `openBlackboardEntries`,
  `recentCaptures` take its place. Fixture-backed regression test asserts the shape.
- `mcp/cecelia_mcp/guidance.py` — top-of-session guidance: read the profile in full, scan the
  open entries, only reach for `read_lab_log` when a chronological question comes up.
- Docs: `docs/ARCHITECTURE.md` gains a *Project memory* subsection pointing at Blackboard as
  the store and this plan as the *why*.

### Migration

- Existing blackboard entries pre-Phase 1 have no `status` on disk. Read path assigns `open` on
  load; no separate migration task. A user's first `revise_blackboard_entry` on an old entry
  writes the field.
- Projects with no `blackboard/profile/` get one created on next open. Empty body; the user
  or Claude fills it as work happens.
- `.ccbundle` (see [`PROJECT_IO_PLAN.md`](PROJECT_IO_PLAN.md)) already includes
  `blackboard/**` — no export/import change needed.

## Storage inventory this plan touches

Reference for future readers — nothing new, only additions to existing shapes:

| Artifact | Path (relative to `<proj>/`) | This plan's change |
|---|---|---|
| Blackboard entry | `blackboard/<bb-id>/{entry.md, meta.json, .snapshots/entry@v<N>.md}` | `+ meta.status: open\|resolved\|parked` |
| Blackboard registry | `settings/blackboard.json` | (no change) |
| Reserved profile entry | `blackboard/profile/…` (Decision 2) | new well-known id, otherwise a regular Blackboard entry |
| Captures | `captures/<cap-id>/{meta.json, frame.png}` | (no change; referenced in briefing) |
| Lab-log | `lab-log.md` | (no change; still readable via `read_lab_log`, drops from briefing default) |

## Future work

Surfaced here so a follow-up isn't invented from scratch. Each is its own plan when it's time.

- **Config-artifact recall by intent** (Decision 6). "Reapply the diameter/gate/LUT from that
  case." Different problem shape from narrative search. Would need: tags on the writers that
  produce these artifacts (`gating/{vn}.json`, `runlog.json`, viewer presets — if Decision 7 is
  built), plus a retrieval that returns a directly-reusable reference (id + path) rather than a
  description. Revisit after v1 lands and we know whether the "suggest processing steps" ask is
  actually served by Blackboard search or genuinely needs artifact retrieval.
- **Named viewer presets** (Decision 7). Independent `settings/viewer_presets.json` + MCP
  list/apply. Real prerequisite gap for config-artifact recall.
- **Semantic search** (Decision 4). Replace substring with embedding-backed search over
  Blackboard bodies. Only worth building if substring measurably fails on real usage.
- **Cross-project federation.** Explicitly out — see
  [`IMMUNEMAP_IMPORT_PLAN.md`](IMMUNEMAP_IMPORT_PLAN.md).

## Open questions

- **Profile schema-lite header — mandatory fields?** Decision 2 names subject, modality,
  cohort/groups, key channels, current goal. Should any of these be required for the entry to
  count as "filled in," and does the UI nudge on empty fields? Deferred to Phase 1 review — a
  strict schema is easy to add later, hard to remove.
- **Briefing when the project is fresh.** A brand-new project has an empty profile, no open
  entries, no captures. The briefing should say so plainly rather than return three empty
  arrays.
- **Retirement of an entry.** Is `parked` distinct from `resolved` in a way the briefing acts
  on, or is the split only for the human reader? Default: both drop out of the briefing's
  `openBlackboardEntries` list; the difference is documentary.
