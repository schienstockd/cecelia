# Project memory — plan

**Status:** Phases 1–3 SHIPPED 2026-09-20/21 (PR #1103, single commit `87706bad` after rebase onto
the split-suite refactor storm). Phase 4 (outcome tagging) in planning as of 2026-09-21. Design
lives on `docs/project-memory-plan-p4` (was `docs/project-memory-plan` up to P3). Written to be
picked up cold; sits on the shipped Blackboard, captures and lab-log surfaces from
[`BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md) (parts 2 + 4 + 5 + 6 + 7 — all landed) rather than
defining new storage. Companion reading: [`docs/archive/project-memory-index-prompt.md`](../archive/project-memory-index-prompt.md)
(the ask, written before Blackboard shipped — its "narrative vs artifact" split still holds; its
storage assumptions were overtaken by shipping); [`docs/archive/claude-imaging-pitch.md`](../archive/claude-imaging-pitch.md)
(what this connects to at the field level); [`docs/archive/blackboard-outcome-tagging-prompt.md`](../archive/blackboard-outcome-tagging-prompt.md)
(the P4 + P5 ask; P4 folds in here as Phase 4, P5 is deferred to *Future work* pending the P5.0
metadata audit).

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
   Default `open`. Missing (pre-migration) backfilled as `open` on read. This is the "what's
   currently on the table" signal the briefing keys on.

   **Refinement at ship time:** transitions land through a dedicated `POST /api/blackboard/status`
   endpoint + `set_blackboard_status` MCP tool rather than an optional param on
   `revise_blackboard_entry`. Rationale: flipping open→resolved is a *state* transition on the
   whole entry, not a content revision; keeping it out of `revise` means it doesn't consume a
   snapshot and doesn't need to load or diff the entry body.

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

   **Empty-briefing case (Decision 9).** On a brand-new project — no profile, no open entries, no
   captures — the payload carries `newProject: true` and guidance instructs Claude to greet + ask
   the user to describe subject/goal before proposing anything. See Decision 9.

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

9. **Profile schema-lite: `subject` and `goal` required, everything else free.** The empty-profile
   entry ships with all five suggested headings (subject, modality, cohort/groups, key channels,
   current goal), but only `subject` and `goal` are enforced by the briefing: with either missing,
   the payload carries `newProject: true` and Claude greets + asks the user to fill them before
   proposing anything. Rationale: without knowing WHAT the project is and WHAT you're trying to
   do, the briefing has nothing useful to lean on. Modality etc. are useful but come later; a
   strict schema is easy to add, hard to remove.

10. **`parked` and `resolved` are documentary only in v1.** Both drop from the briefing's
    `openBlackboardEntries`; the chip is for the human reader; the memory slice treats them
    identically. Different guidance for the two states is a v2 concern once we see how the field
    gets used. Keeps the code path narrow.

11. **Phase 4 — outcome tagging (D1–D6 from the archived prompt, D6 corrected).**
    - **D1 binary, not scored.** `outcome.verdict: good | bad`. No numeric rating.
    - **D2 note is required.** A verdict without a note is rejected (400). Reason: the note is
      what a future Claude session actually reads; a number nobody explains is noise.
    - **D3 entry-level.** `outcome` attaches to the whole Blackboard entry, same shape as `status`.
      Per-suggestion granularity is a v2 problem.
    - **D4 no neutral state.** `good | bad` only; absent = "no signal", not "neutral".
    - **D5 tap-fast in the UI.** Inline verdict control + inline note input; not a full form page.
      "One tap" softens to *tap → type note → confirm* (D5↔D2 tension acknowledged: the note is
      the required part, so friction lives there).
    - **D6 filter chips in the entry list, both status and outcome.** The archived prompt's D6
      assumed `status` was already filterable — it isn't. This phase adds both filter chips
      together, not just outcome. `outcome` filter options: `all | untagged | good | bad`.

12. **Retrieval bias on outcome (Phase 4.3).** `search_blackboard` gains an outcome-aware
    tiebreak: on equal match strength, `bad` beats `good` beats untagged (a `bad` verdict is a
    known trap; leading with it is the whole point of the tag). Session briefing surfaces
    `outcome` on the open-entries slice and promotes a `bad`-tagged entry above an untagged one
    when both are open. Semantic topic matching is deferred (Decision 4 is still substring).

## Phases

Each phase is independently shippable and reviewable. Phases 2 and 3 may ship in one PR at the
maintainer's discretion; keep P1 separate.

### Phase 1 — Status field + reserved `profile` entry — SHIPPED 2026-09-20 (PR #1103)

**Goal.** Every blackboard entry carries a status; the project profile exists as a well-known
Blackboard entry from the moment the project opens.

Backend + MCP landed in `87706bad`:
- `api/src/blackboard_api.jl` — `meta.status` (open/resolved/parked; backfilled as `open`);
  reserved `profile` entry auto-created on first `list`; `_ensure_profile_entry!` idempotent.
- `POST /api/blackboard/status` (Decision 3 refinement — separate endpoint, no snapshot).
- MCP `set_blackboard_status`, `list_blackboard_entries` surfaces status.
- Tests: `api/test/suite/bidir_blackboard.jl` — status + reserved-profile testset (MEMORY P1);
  BIDIR Part 4 CRUD updated to filter the auto-created profile row.
- Guidance rule in `guidance.py` — transition to `resolved` when a topic settles, `parked` when
  set aside; do not blanket-close entries as noise.

**DEFERRED (frontend UI pass; see Phase 4 — bundled).**
- `frontend/src/stores/blackboard.ts` — plumb `status` through the store; `setStatus(bbId, status)`.
- `frontend/src/modules/BlackboardModule.vue` — status chip per row; status filter chip; pin the
  `profile` row to the top with a distinct affordance.

### Phase 2 — Blackboard search — SHIPPED 2026-09-20 (PR #1103)

**Goal.** One MCP tool that answers "has this come up before in this project."

- `api/src/blackboard_api.jl` — `POST /api/blackboard/search` (body: `{query, status?, limit?}`).
  Case-insensitive substring over `title` + entry body. Returns id + title + snippet (±40 chars
  around the hit) + status + matchType + updatedAt, title-hits first, capped `limit ≤ 50`,
  default 10.
- MCP `search_blackboard(query, status_filter=None, limit=10)`, read-only + allow-listed.
- Guidance: call before proposing a phenotype label that sounds familiar / suggesting a step
  for an unfamiliar image / writing a finding that might restate an existing one. Not reflexive.
- Tests: `api/test/suite/bidir_blackboard.jl` — search testset (MEMORY P2), covers title-vs-body
  ordering, case-insensitivity, status filter, snippet shape, limit clamp.

### Phase 3 — Session-briefing rewrite — SHIPPED 2026-09-21 (PR #1103)

**Goal.** Session start returns durable project context, not a chronological slice.

- `mcp/cecelia_mcp/server.py::_memory_briefing_slice` composes Decision 5 shape in Python
  (avoids coupling Julia `session_briefing` to blackboard code). Each upstream call wrapped in
  try/except so a failed upstream degrades gracefully.
- `get_session_briefing` returns `profile` (full body), `openBlackboardEntries` (title + snippet
  + updatedAt, status=open, profile excluded), `recentCaptures` (last 5, slim shape).
  `recentLabLog` dropped from the default payload; `read_lab_log` still there on demand.
- Guidance rewritten: read profile first, scan open entries, reach for `read_lab_log` only for
  chronological questions.
- Test: `mcp/tests/test_server.py::test_the_briefing_ships_the_guidance` asserts the merged
  shape (patched client — no live server).

**DEFERRED.**
- **Eyeball on a real session** (session-flow-visible change; can't be automated).
- **`newProject: true` empty-briefing case (Decision 9)** — server currently returns three empty
  arrays; the greet-and-ask flow needs adding to the memory slice + guidance. Small follow-up.
- **`docs/ARCHITECTURE.md` gains a *Project memory* subsection** — doc-only cleanup, tail work.

### Phase 4 — Outcome tagging + deferred P1 frontend UI — PLANNING (2026-09-21)

**Goal.** Every Blackboard entry can be tagged `good`/`bad` with a required note. Bundles the
deferred P1 frontend chips so BlackboardModule.vue takes ONE UI pass, not two.

**Phase 4.1 — schema + API (backend).** Mirror the P1 status shape:
- `api/src/blackboard_api.jl` — `outcome: {verdict: "good"|"bad", note: str, tagged_at: iso}` on
  `meta.json`; validator (verdict requires non-empty note; verdict absent + note present → 400);
  additive, no snapshot fired on tag change.
- `POST /api/blackboard/outcome` handler (same discipline as `/status` — new state on the whole
  entry, doesn't consume a snapshot). Register in `api/src/server.jl`; bump POST_ROUTES to 138
  in `api/test/suite/e2e_sysimage_router.jl`.
- MCP `set_blackboard_outcome(project_uid, entry_id, verdict, note)` — client + server tool +
  test_client.py allowlist entry.
- Testset in `api/test/suite/bidir_blackboard.jl` (Phase 4 section — same sector as P1/P2).
- Guidance: when to tag good vs bad; note is required and is what future sessions read.

**Phase 4.2 — Frontend UI pass (BlackboardModule.vue + store).** One visit to the file for
both P1 chips (deferred) AND P4 outcome control:
- `frontend/src/stores/blackboard.ts` — plumb `status` + `outcome`; `setStatus(id, status)`,
  `setOutcome(id, verdict, note)` actions.
- Status chip per row (P1 deferred — open/resolved/parked).
- Profile-pin at top with distinct affordance (P1 deferred).
- Outcome tag control: inline good/bad + note-required inline input; re-tagging allowed.
- Both filter chips (P4.D6): status (all/open/resolved/parked), outcome (all/untagged/good/bad).
- Extract testable logic into `frontend/src/utils/*.ts`; test.
- Rendering half needs Dominik's eyeball per `feedback_reservation_is_not_management`.

**Phase 4.3 — Retrieval bias.**
- `api/src/blackboard_api.jl::api_blackboard_search` — outcome tiebreak on equal match strength:
  `bad` > `good` > untagged. Expose `outcome` in result rows.
- `mcp/cecelia_mcp/server.py::_memory_briefing_slice` — surface `outcome` on open-entries slice;
  promote `bad`-tagged entries above untagged when both open. Extend the P3 test.
- Guidance: on session open, if a `bad`-tagged entry surfaces, lead with it.

**Phase 4 non-goals.** No numeric rating, no per-suggestion granularity, no third `inconclusive`
state, no auto-scoring by Claude. These come back only if a real case demands them.

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
| Blackboard entry | `blackboard/<bb-id>/{entry.md, meta.json, .snapshots/entry@v<N>.md}` | P1: `+ meta.status: open\|resolved\|parked`. P4: `+ meta.outcome: {verdict: good\|bad, note, tagged_at}` (absent = untagged). P5.1: `+ meta.fingerprint: {v, channel_count, stain_classes[], pipeline_stage}` (absent on entries created without image context or before P5.1). |
| Blackboard registry | `settings/blackboard.json` | P1: mirrors `status` per entry. P4: mirrors `outcome.verdict` per entry (for cheap filter without loading meta). |
| Reserved profile entry | `blackboard/profile/…` (Decision 2) | new well-known id, otherwise a regular Blackboard entry. `subject` + `goal` enforced by briefing (Decision 9). |
| Captures | `captures/<cap-id>/{meta.json, frame.png}` | (no change; referenced in briefing) |
| Lab-log | `lab-log.md` | (no change; still readable via `read_lab_log`, drops from briefing default) |

## Future work

Surfaced here so a follow-up isn't invented from scratch. Each is its own plan when it's time.

- **P5.1 — entry fingerprint (writer) — SHIPPED 2026-09-21.** Each new Blackboard entry snapshots
  a small structured `fingerprint` into `meta.json` at create time: `{v: 1, channel_count,
  stain_classes[], pipeline_stage}`. Set-once — no PATCH endpoint (an entry's context is what it
  was created on; a later image edit doesn't retroactively change the entry). Preserved across
  every mutation (status flip, outcome tag, revise, restore, prune) by the same read-and-pass-back
  pattern outcome uses. MCP `create_blackboard_entry` gains an optional `image_uid` arg; the
  server-side `_infer_fingerprint` reads `sizeC` + `activeValueName` + classified `channelNames`
  (see [`docs/inventory/stain_classes.md`](../inventory/stain_classes.md)). Absent when no image
  context is passed or the image is unresolvable — a fingerprint is best-effort, not a gate.
    - **P5.0 audit — DONE 2026-09-21 against zolIMa (MERTK).** Sweep against the archived D3
      candidate list:
      - AVAILABLE today: `channel_count` (from `sizeC`), `pipeline_stage` (from `activeValueName`).
      - NEEDS EXTRACTION (small classifier lift, no reader change): `stain_classes` (channel-name
        regex — Ailsa's `mem-`/`nuc-`/`CD169-…` convention). Landed as v1.
      - DEFERRED to v2: `modality`, `tissue_context` — need a profile-prose parse. Additive; a
        v1-schema reader ignores an unknown field so a v2 writer can land without a migration.
      - **DROPPED** from D3: `objective_na_band`. OME `Objective.LensNA` isn't preserved on
        import (`extraMeta` comes back empty on `Dml3RG`); costs a reader change; unlikely to
        discriminate the failure modes actually hit. Revisit only if the case for it appears.
    - **P5.0 outcome logged** in `docs/inventory/stain_classes.md` (the classifier is the human
      contract) and in this section. The audit doesn't need re-running unless the class list
      changes shape.
- **P5.2 — guardrail extraction from `bad`-tagged entries (retrieval side).** Mine recurring
  failure modes across entries tagged `bad` and grouped by fingerprint proximity to derive
  imaging-context-scoped guardrails ("when fingerprint ≈ X, avoid Y because Z"). Gated on a
  `bad`-tagged corpus that actually crosses the D5 recurrence threshold (N ≥ 3) — 1 entry today
  on zolIMa; needs weeks of P4 usage. Retrieval schema will dispatch on `fingerprint.v`, so a
  v1-only corpus is fine to mine when the time comes. Full sketch:
  [`docs/archive/blackboard-outcome-tagging-prompt.md`](../archive/blackboard-outcome-tagging-prompt.md)
  (§P5).
- **Config-artifact recall by intent** (Decision 6). "Reapply the diameter/gate/LUT from that
  case." Different problem shape from narrative search. Would need: tags on the writers that
  produce these artifacts (`gating/{vn}.json`, `runlog.json`, viewer presets — if Decision 7 is
  built), plus a retrieval that returns a directly-reusable reference (id + path) rather than a
  description. Revisit after Phase 4 lands and we know whether the "suggest processing steps" ask
  is actually served by Blackboard search+outcome or genuinely needs artifact retrieval.
- **Named viewer presets** (Decision 7). Independent `settings/viewer_presets.json` + MCP
  list/apply. Real prerequisite gap for config-artifact recall.
- **Semantic search** (Decision 4). Replace substring with embedding-backed search over
  Blackboard bodies. Only worth building if substring measurably fails on real usage.
- **Cross-project federation.** Explicitly out — see
  [`IMMUNEMAP_IMPORT_PLAN.md`](IMMUNEMAP_IMPORT_PLAN.md).

## Open questions

*(P1–P3 open questions locked as Decisions 9, 10, and the D5 empty-briefing paragraph on
2026-09-21; Phase 4 has no open questions at this stage.)*
