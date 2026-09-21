# Blackboard outcome tagging — implementation prompt

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of a P4/P5 proposal on top
> of `docs/todo/PROJECT_MEMORY_PLAN.md`. Two independent asks were bundled into one prompt; the plan
> doc is where the design work happens, not this file. Current status:
>
> - **P4 (outcome tagging).** Being folded into `PROJECT_MEMORY_PLAN.md` as a Phase 4 — fits the
>   shipped meta.json shape, one field + one endpoint + one UI pass alongside the status chip/filter
>   already staged for eyeball.
> - **D6 assumes status is filterable in the list.** It isn't — the field ships in P1, the filter
>   chip is unbuilt. P4.2b needs to add both together, not just outcome.
> - **P5 (guardrail extraction).** Deferred pending the P5.0 metadata audit — the fingerprint
>   schema depends on what per-acquisition metadata already exists in OME-XML / ccid.json /
>   user-supplied notes, and that inventory hasn't been done. If P5 is scheduled it becomes its own
>   plan doc, not a tail phase.
> - **D5 vs D2 tension.** "One tap" (D5) collides with "required note" (D2); real flow is tap +
>   type + confirm. Preserved as written; the plan doc will call it out.
>
> Base plan: `docs/todo/PROJECT_MEMORY_PLAN.md` (P1–P3 shipped 2026-09-20/21).

## Context

PROJECT_MEMORY_PLAN (#1097) locked Blackboard as the single narrative-memory
surface for Claude Code across sessions. Every entry already carries a
`status: open | resolved | parked` field (D3). This is a **follow-up phase
(P4)** on top of that plan, not a new store.

Gap: nothing currently records whether a past discussion or suggestion was
actually *right*. A future session can find a prior entry on a topic but has
no signal on whether to trust it. We need a lightweight way for the user to
mark an entry (or a specific claim inside it) as confirmed-good or
confirmed-wrong, cheaply enough that it actually gets used.

## Decisions

- **D1 — binary, not scored.** `outcome: good | bad`. No 1–10 rating. A
  number without a reason is useless to Claude; forcing good/bad pushes the
  reasoning into the required note instead of a number nobody explains.
- **D2 — note is required, not optional.** An outcome with no note is not
  saved. The note is the part Claude actually needs (e.g. "wrong
  segmentation params — used galvo defaults on a resonant-scanning image").
- **D3 — entry-level by default.** Attach `outcome` to the Blackboard entry
  as a whole, same as `status`. If a single entry mixes multiple distinct
  suggestions and the user wants to tag one specifically, that's a v2
  problem — don't build per-suggestion granularity now.
- **D4 — no third "neutral" state yet.** Don't add `inconclusive` or similar
  speculatively. Wait until a real case demands it.
- **D5 — must be fast to do in the app.** This is the actual ask: tagging
  can't require opening a form. Target: one tap/click to set good or bad,
  inline where the entry is already being read (Blackboard entry view,
  Kiwi row, wherever captures/discussions are already displayed), plus a
  lightweight prompt for the note (small inline text field or modal,
  not a full form page). If good/bad is set without a note, block save and
  ask for the note right there rather than silently dropping the tag.
- **D6 — `status` and `outcome` are filterable columns on the Blackboard
  entry list**, same as any other list column (not just visible per-entry).
  `status` (open/resolved/parked) is presumably already listed/filterable
  there — confirm, and if not, fold that in here rather than assuming it
  exists. `outcome` needs a filter for untagged / good / bad, so the user
  can pull up "everything tagged bad" as its own view.

## Schema

On the Blackboard entry's `meta.json` (same place `status` lives):

```json
{
  "status": "resolved",
  "outcome": {
    "verdict": "good" | "bad",
    "note": "string, required if verdict is set",
    "tagged_at": "ISO timestamp"
  }
}
```

`outcome` itself stays optional/absent for untagged entries — most entries
will never get tagged, and that's fine. Absence means "no signal," not
"neutral."

## Phases

- **P4.1 — schema + API.** Add `outcome` to the entry meta schema. Extend
  `POST /api/blackboard/entries/:id` (or whatever the existing update
  endpoint is) to accept `outcome`. Validate: `verdict` requires non-empty
  `note`. Reject unset verdict with a note (note without verdict is
  meaningless — drop it).
- **P4.2 — UI: quick tag control.** Inline good/bad control on the entry
  wherever it's rendered (Blackboard entry, and anywhere a capture/entry
  surfaces elsewhere — e.g. Kiwi). On tap: if no note yet, open a minimal
  inline input for the note before committing. Already-tagged entries show
  the current verdict and allow editing (re-tag or edit note) — don't lock
  it after first save.
- **P4.2b — UI: list filters.** Add `status` and `outcome` as filterable
  columns on the Blackboard entry list view. `outcome` filter options:
  untagged / good / bad. Verify `status` filtering already exists from D3;
  if it doesn't, add it in this same pass rather than treating it as
  separate scope.
- **P4.3 — surface it in retrieval.** `search_blackboard` and
  `get_session_briefing` should include `outcome` when present, and should
  bias toward showing tagged entries (especially `bad`) over untagged ones
  when a topic match is close. A session briefing on a topic with a prior
  `bad` verdict should lead with that, not bury it.

## Test plan

- [ ] Schema: verdict without note is rejected at the API layer.
- [ ] Schema: note without verdict is rejected or dropped (not silently
      stored as a half-state).
- [ ] UI: tagging an entry takes ≤2 interactions (tap verdict, type note,
      confirm) — no full-page form.
- [ ] UI: re-tagging an already-tagged entry works (not write-once).
- [ ] UI: entry list can be filtered by `status` and by `outcome`
      (untagged/good/bad), independently and combined.
- [ ] Retrieval: `search_blackboard` returns `outcome` in results when set.
- [ ] Retrieval: `get_session_briefing` surfaces `bad`-tagged entries on
      topic match ahead of untagged entries.
- [ ] Regression: entries with no `outcome` field behave exactly as before
      (no schema break for existing Blackboard data).

## Explicit non-goals for this phase

- No 1–N rating scale.
- No per-suggestion (sub-entry) granularity.
- No third "inconclusive" verdict.
- No auto-scoring or Claude self-rating its own suggestions.

---

# P5 — Guardrail extraction from bad-tagged entries (follow-up, not this pass)

## Context

Fixed guardrails work on fixed hardware/software because the environment
doesn't change. Ours can't be fixed rules — they need to be conditioned on
imaging context (modality, objective, stain panel, tissue type, pipeline
stage), since a rule that holds for resonant-scanning intravital data may
be wrong for spinning-disk fixed-tissue data. So extraction is really:
group `bad`-tagged entries by shared context, and only propose a guardrail
when the same failure mode recurs within a matching group.

## D-decisions

- **D1 — fingerprint, not free text.** Each Blackboard entry gets a small
  structured key (flat JSON object) capturing the imaging/analysis context
  it was created in. This is what failure modes get clustered on.
- **D2 — candidate fields need a real audit first, not a guess.** Before
  picking fields, **Opus should audit what metadata Cecelia already
  captures per acquisition/entry** (modality, objective, channel/stain
  info, pipeline stage, tissue context, project) and report back: what's
  already structured and attachable for free vs. what would need new
  extraction work. Field selection happens after that audit, with the
  user, not before.
- **D3 — starting candidates to validate against the audit** (not final):
  `modality` (resonant/galvo/spinning-disk/light-sheet), `objective_na_band`
  (bucketed, not exact), `channel_count` + `stain_classes` (nuclear/
  membrane/SHG/functional — not exact fluorophore names), `tissue_context`
  (intravital/fixed/organoid), `pipeline_stage` (segmentation/tracking/
  gating/registration). Bias toward fields that are (a) cheap to pull
  automatically from existing metadata and (b) plausibly predictive —
  cheap-and-available usually wins over theoretically-ideal. Leave out
  exact identity-level fields (project ID, exact frame rate/pixel size,
  laser power) — too granular to cluster on, and project identity isn't
  what a guardrail should scope to.
- **D4 — matching is scored, not exact.** `pipeline_stage` should match
  exactly (a segmentation guardrail is irrelevant during gating); other
  fields match on overlap, with guardrails surfaced above some overlap
  threshold rather than requiring all fields to match.
- **D5 — extraction only fires on a recurrence threshold.** One bad-tagged
  entry is a data point, not a rule. Only propose a guardrail when N (start
  at 3, tune later) entries with a matching fingerprint hit the same
  failure mode.
- **D6 — guardrails are proposed, not auto-enforced.** Extraction surfaces
  a candidate guardrail ("when fingerprint ≈ X, avoid Y because Z, based on
  N entries") for the user to accept/edit/reject. Never silently inject
  into session briefings unconfirmed — an overfit guardrail from one edge
  case is worse than no guardrail.
- **D7 — separate storage layer.** Guardrails are derived, not Blackboard
  entries themselves: `bad` entries → grouped by fingerprint → synthesized
  → confirmed by user → written to a `guardrails` store keyed by fingerprint
  pattern. Session briefing does a fingerprint match against current
  context and pulls in only relevant guardrails, not full history.

## Phases

- **P5.0 — metadata audit (Opus, do this first).** Inventory what
  acquisition/entry metadata already exists and is attachable without new
  extraction work. Report back against the D3 candidate list: confirmed
  available / needs extraction / not feasible. This determines the actual
  field list — output is a proposal for the user, not a final schema.
- **P5.1 — fingerprint attachment.** Once fields are settled, attach the
  fingerprint automatically at entry-creation time from available metadata
  (no manual fill-in required for fields that are already structured).
- **P5.2 — clustering + candidate extraction.** Batch job (not real-time):
  group `bad`-tagged entries by fingerprint overlap, detect recurring
  failure patterns (via note text + fingerprint), propose candidate
  guardrails meeting the D5 recurrence threshold.
- **P5.3 — review UI + guardrail store.** Simple accept/edit/reject flow
  for proposed guardrails; confirmed ones land in the `guardrails` store.
- **P5.4 — retrieval integration.** `get_session_briefing` fingerprint-
  matches current context against the guardrail store and surfaces
  relevant ones, scored by overlap per D4.

## Non-goals for P5

- No automatic enforcement of unconfirmed guardrails.
- No fingerprinting on exact/identity-level fields (project ID, exact
  acquisition params).
- No real-time extraction — clustering runs as a batch/background job.

