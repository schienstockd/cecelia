# Advanced Viewer Settings — Audit First, Then Popup

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

## Context

Follows `feat/brick-followup` (#705) and `KILN_BRICK_PLAN.md` /
`BRICK_INTEGRATION_PLAN.md`. Everything below the audit section is a
claim reconstructed from PR descriptions and planning docs on
`feat/brick-followup` — not a read of the current code. Docs and commit
messages drift from implementation, numbers get tuned across commits
without the doc being updated, and "not yet locked" decisions
(brick size is explicitly flagged as such — `KILN_BRICK_PLAN.md` Open
Question #2) can quietly become de facto fixed without anyone revisiting
them. Do not build UI against the summary below — verify it first.

## Part 1 — Audit (do this before writing any UI code)

Read the actual source (`brickScheduler.ts`, `brickAtlas.ts`,
`volumeViewer.ts`, `ViewerWindow.vue`, and wherever `SchedulerKnobs` /
`DEFAULT_KNOBS` live) and answer:

1. **What dev knobs currently exist**, and where are they read from
   (URL param, constant, computed)? For each: current name, current
   default, current valid range/type, and whether it's actually wired
   into the renderer or just plumbed-but-unused.
   - Known from docs, needs confirmation: `?bricks=`, `?brickThr=`,
     `?brickBias=`, `?brickHold=`. Are there others that landed since
     #705 or that the docs never mentioned?
2. **Brick dimensions.** `KILN_BRICK_PLAN.md` Decision 2 says
   `128×128×min(brick_z, nZ)`, called out as **not locked** (Open
   Question #2: "Measure in P4 on SispLk. Not a locked decision yet").
   Is it still 128 in code, hardcoded or configurable, and did the P4
   measurement referenced in the doc ever actually happen and get
   written down anywhere? If brick size affects how chunky a coarse
   render looks, that's directly relevant to what a quality-tier UI is
   promising the user — find out before assuming it's fixed.
3. **What `MAX_INTERSECT_BRICKS`/`brickThr` actually bounds today** —
   confirm the "core bricks only, ring===0" definition from the #705
   commit messages still matches the code, and confirm the current
   default (docs say 256, may have moved).
4. **What's already user-facing vs. dev-only.** The renderer choice
   (bricks/flat) — is there already ANY user-reachable toggle, or is
   `?bricks=` purely a URL param today? Same question for level floor
   (`viewerVolumeLevel` dropdown — this one sounds shipped, confirm).
5. **Anything else in the scheduler that reads as a knob** —
   `brickHold` (hold-going-finer-until-stable gate), halo ring width,
   LRU touch bias, T-axis prefetch policy — that might be worth
   surfacing, even if not requested yet. Flag, don't build.

Produce a short table: knob name → current state (constant / URL-only /
already UI-exposed) → plausible user-facing value (yes / no / unclear)
→ why.

## Part 2 — Decide what's worth exposing

From the audit, not from this doc's assumptions, decide which knobs
earn a place in a user-facing "Advanced" popup. Bar for inclusion:
would changing it plausibly matter to someone deciding "I just want a
fast look" vs. "I want it accurate," without needing to understand
scheduler internals. A knob that's still unmeasured/unlocked in the
docs (brick size) is a candidate to flag as **not ready**, not to
silently include or silently exclude.

Two knobs were already discussed and are likely candidates pending the
audit confirming they're real and independent of each other:

- **Renderer**: Auto / Bricks / Flat — force vs. predicate-driven
  (`shouldUseBricks`, `nX*nY < HUGE_XY_THRESHOLD_PX` per the docs,
  confirm in code).
- **Quality tier**: single knob on the core-brick intersect ceiling
  only (not bias — bias shifts *which* level SSE targets, threshold
  caps *how many* bricks it costs to get there; don't conflate the two
  axes in one control). Candidate labels/values, **pending confirmation
  the current default and valid range still match**:
  - Quick Look: low ceiling
  - Balanced: current shipped default
  - Detailed: high ceiling — do not call this "Full" or otherwise
    imply it's a validated safe maximum; per #705 the threshold values
    were tuned against two reference images, not a cost model, and the
    upper end is where the f8gzA2 shader-scaling regression (200ms
    drawP95) lives for any large single-level volume that still routes
    to bricks.

These two are independent of the existing level-floor dropdown
(`viewerVolumeLevel`, sets the floor SSE is clamped to). Quality tier
should NOT touch the floor in v1 — keep it a pure threshold control, one
axis of new complexity, note the gap in UI copy if there's an easy spot
("limits detail while zoomed in") rather than silently coupling two
previously-independent knobs.

## Part 3 — UI, once Part 1/2 are settled

- Trigger: "Advanced" (gear/sliders icon) in the existing viewer
  toolbar, near the level dropdown.
- Content: Vue `<teleport to="body">` popup, dismissible (click
  outside, Esc). Reuse whatever popup primitive already backs the
  Debug panel if one exists — don't hand-roll a new one.
- Persist choices the same way `cc.vw.brickmap` is persisted
  (confirm that's still the pattern in use).
- Tier/renderer changes apply live via the existing `setSchedulerKnobs`
  path, no reopen required — confirm this still holds for whatever the
  audit finds.
- Quality tier disabled/hidden when effective renderer is Flat.
- `?bricks=`, `?brickThr=` (and any others the audit confirms) keep
  working as dev overrides and take precedence over persisted UI
  choice.

## Explicitly not in scope

- Retuning the actual threshold numbers — that's the B1 rebaseline's
  job, separate from this task.
- Measuring or locking brick size — flag it, don't resolve it here.
- Any change to the auto-select predicate itself.
- Mobile/touch layout, unless the audit shows the reused popup
  primitive already handles it.

## Test plan

- [ ] Audit table produced and reviewed before any UI code is written
- [ ] Advanced popup opens/closes via teleport, no layout/z-index
      regressions against bench chip / minimap
- [ ] Renderer select matches confirmed `?bricks=` semantics
- [ ] Quality tier: switching tiers on an open brick-rendered volume
      visibly changes resident level within a few frames, no
      black-frame regression (the #705 swap-path bugs — hold-gate,
      prev-page-table rebuild, LRU bias — apply here too, since a tier
      change re-triggers scheduling at a new threshold)
- [ ] Tier hidden/disabled when effective renderer is Flat
- [ ] Choices persist across reload
- [ ] Confirmed dev URL params still override persisted UI choice
