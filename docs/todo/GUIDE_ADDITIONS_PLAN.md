# Guide additions plan — close the coverage gap the audit found

Status: **planning** (drafted 2026-09-25). Punch list from [`GUIDE_SYSTEM_AUDIT.md`](GUIDE_SYSTEM_AUDIT.md).
Prior art: [`GUIDE_SYSTEM_PLAN.md`](GUIDE_SYSTEM_PLAN.md) (D1–D11 still binding).

Goal: nine new guides + one tour touch-up, distributed across five phases so each phase is
independently reviewable and ship-able. Copy is authored by Dominik; this doc plans the plumbing,
the anchors, and where each guide sits in the catalogue.

## Locked decisions

**D12 — Every new guide follows the existing catalogue split.** `moduleTaskGuide` for anything with
a `TaskRunner`; bespoke `GuideDef` in `singleTopicGuides.ts` for pointer-only or unique-shape
surfaces; its own per-page-shape file only when it has enough prose that a shared file would
dominate. No new file types.

**D13 — Kiwi replaces lab-log-and-claude, not extends it.** The current `labLogGuide` is a
two-step pointer that predates Kiwi being the assistant cockpit. Rewrite it in place as
`kiwiGuide` (id `assist-with-kiwi`, group `Analysis`), keeping the lab-log step at the front but
lengthening the Kiwi half from one step to four (pairing / chat / share / handoff-to-Claude).
Change the id — pinned tests find `lab-log-and-claude` today.

**D14 — Profile picker is a `Start`-group guide, not a tour step.** The tour teaches app chrome
inside a session; the profile picker is the boot surface *before* a session. Making it a
prerequisiteless guide under `Start` (right after `find-your-way-around`) keeps the tour honest
about what it teaches. `USER_PROFILE_PLAN` says the picker is skipped when only one profile
exists — the guide's prereq (`profilesMulti`, new in `prereqs.ts`) reflects that so it does not
show up amber for single-user installs.

## Phases

### P1 — Start-group additions (profile picker + view profiles)

Two guides, both in the `Start` group, both prereqless-or-nearly-so:

- **`use-a-user-profile`** — bespoke `GuideDef` in `singleTopicGuides.ts`. Points at
  `AppProfilePicker.vue` (picker page) and the profile chip in the header. Explains what a
  profile carries (per-profile settings, project list scope, Kiwi identity). 3–4 steps.
- **`use-a-view-profile`** — bespoke `GuideDef` in `singleTopicGuides.ts`. Points at the view
  profile chip in `AppProfilePicker.vue` and the editor. Explains "curated menu, not access
  control" (VIEW_PROFILES_PLAN D1). 3 steps.

**Anchors to add** (~5, all in `AppProfilePicker.vue` + `HeaderBar.vue`):
`profilePicker.list`, `profilePicker.new`, `profilePicker.viewProfile`, `header.profileChip`,
`viewProfile.editor`.

**New prereq**: `profilesMulti` — true when `stores/userProfile.list.length > 1`. Guide reads
amber-not-hidden on single-user installs (so it's still findable via the compass).

### P2 — Kiwi guide, replacing lab-log-and-claude

One rewrite, one file. Rename `labLogGuide` → `kiwiGuide` inside `singleTopicGuides.ts` and
change its id from `lab-log-and-claude` → `assist-with-kiwi`. Uses the three existing unused
anchors in `KiwiCockpit.vue:284/301/317` (`kiwi.pairing`, `kiwi.chat`, `kiwi.share`) plus
`kiwi.assistantHelp` (already anchored). Group stays `Analysis`, prereq stays `projectOpen`.

Steps (approx): sidebar lab-log CTA → sidebar kiwi CTA → pairing row → chat row → share row →
`?` for the setup-state explainer. 5–6 steps, still short.

**Anchors to add**: 0 — all four exist. `sidebar.labLogCta` is preserved from the old guide
(still relevant).

**Test update**: `guides.test.ts` currently pins guide ids as a set. Update the id.

### P3 — Blackboard guide

One bespoke `GuideDef` in `singleTopicGuides.ts`. `/blackboard` has its own shape (a shared
markdown surface for AI turns + notes, per `feat/kiwi-blackboard-persistence`), so a hand-written
step list rather than `moduleTaskGuide`. Group `Analysis`, prereq `projectOpen`.

Steps (approx): nav `/blackboard` → new entry → attach a capture → revise flow → set outcome.
5 steps.

**Anchors to add** (~5, all in `BlackboardModule.vue`):
`blackboard.new`, `blackboard.entry`, `blackboard.attachCapture`, `blackboard.revise`,
`blackboard.outcome`.

### P4 — Module-page trivia (four `moduleTaskGuide` calls)

All four are one call each in `taskGuides.ts`, no bespoke shape needed:

- **`preprocess-images`** — `/preprocess`, `PreprocessingModule.vue`. Crop today, MIP/bin/
  resample soon. `moduleTaskGuide`, group `Data`, prereq `imageImported`.
- **`phenotype-cells`** — `/phenotype`, `PhenotypeModule.vue`. Uses gated pops. Group
  `Populations` or `Explore` (Dominik picks). Uses existing `layout.plotsSection` anchor.
- **`cluster-regions`** — `/regions`, `RegionClusteringModule.vue`. Same shape as
  `cluster-cells`, so the same `after: [...clusterToPops(...)]` tail works — cluster IDs
  become named regions via popmanager (SPATIAL_REGIONS_PLAN).
- **`spatial-analysis`** — `/spatial`, `SpatialAnalysisModule.vue`. Interaction matrix +
  aggregates + contacts; a `moduleTaskGuide` with a small bespoke `after` block naming what
  each panel shows.

**Anchors to add** (~4 total): mostly module pages already have `layout.plotsSection` from the
shared `ModuleLayout`. Confirm each has `nav:/<route>`, `TaskRunner` anchors, and a `plotsSection`
before writing the guide; add any missing on the same PR.

### P5 — Correction cockpit + tour cleanup

One short bespoke `GuideDef` in `singleTopicGuides.ts` (id `correct-a-mask`, group
`Populations`). Then drop the WIP caveat from `tour.ts:98-103` — the tour keeps its step, but
the caveat text goes.

Steps: sidebar CTA (already anchored, `sidebar.correctionCta`) → open panel → pick mode → tool
palette → apply. 4 steps.

**Anchors to add** (~3, all in the correction cockpit component):
`correction.modePicker`, `correction.toolPalette`, `correction.apply`.

## Explicitly out of scope

- **Custom modules** (`/custom/:category`) — advanced surface, per the audit; defer.
- **Task-manager pop-out** — could be one bullet in `find-your-way-around`; not worth a whole
  guide. Add as a tour touch-up in P5 if convenient.
- **A guide for the notebook Kiwi handoff** — Kiwi guide (P2) covers the pattern once.

## Sequencing rationale

P1 first because everything else assumes a user knows what profile they landed in. P2 second
because the Kiwi rewrite unblocks the `assist-with-kiwi` prereq that P3's Blackboard guide could
optionally point to. P3 and P4 are independent — pick whichever's copy is easier to write next.
P5 last because the correction cockpit copy is fiddly and the WIP-caveat drop is a one-line
sweetener that ships with it.

## Reservations

- **New anchors are per-phase discipline.** Each phase adds `data-guide` attrs; `guides.test.ts`
  fails the build if any are missing. That is the intended safety net (D5).
- **Copy is Dominik's.** This plan does not draft any guide text — every phase's PR carries the
  prose. Same rule as elsewhere in `frontend/CLAUDE.md`.
- **`assist-with-kiwi` id change (D13) will break any external doc that cites
  `lab-log-and-claude`.** Grep before merging P2 — likely zero hits, but confirm.
- **`profilesMulti` prereq (P1) reads a store that may not exist yet on branches without
  `USER_PROFILE_PLAN` merged.** Order matters: P1 lands after that plan does.
