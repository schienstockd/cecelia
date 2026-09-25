# Guide system audit — 2026-09-25

Status: **audit complete** (2026-09-25) · consolidation applied in same PR · adds tracked in [`GUIDE_ADDITIONS_PLAN.md`](GUIDE_ADDITIONS_PLAN.md).

Scope: the shipped in-app guide system (`frontend/src/lib/guides/**`, `stores/guide.ts`, `GuidesDialog.vue`, `GuideBubble.vue`, `guideAnchor.ts`, `guideAwait.ts`, `guides.test.ts`) vs. `docs/todo/GUIDE_SYSTEM_PLAN.md` and current main (5f9526e0). No code changes here — punch list only.

Method: read the plan and every guide module; grepped `data-guide=` in `frontend/src/**/*.vue` (78 attrs) against every `anchor:` literal in guide code (74 refs); walked new stores (`user-profile`, `blackboard`, `kiwi`), new routes in `main.ts`, and recent worktrees for shipped UI the guides pre-date.

## Update (broken or stale)

- `tour.ts:98-103` — the "correction cockpit" step still tags itself *"a walkthrough for this is still being written"*; the cockpit shipped as a floating panel (`docs/todo/CORRECTION_PLAN.md`) so either write the guide or drop the WIP caveat (still no `/correction` route — anchor is correct).
- `labLog.ts:37-53` — mentions Kiwi as the assistant cockpit and points at `kiwi.assistantHelp` only; three sibling anchors shipped (`kiwi.pairing`, `kiwi.chat`, `kiwi.share` in `KiwiCockpit.vue:284-328`) are untouched, so the guide undersells Kiwi (see *Add* below).
- `GUIDE_SYSTEM_PLAN.md:3` — status header says *"built (P1–P4) · 11 guides"*; catalogue now has **18** (tour + 17 across Data/Populations/Explore/Analysis/Pipeline). Update the count or drop the header — it reads as an early-2026 snapshot now.
- `GUIDE_SYSTEM_PLAN.md:319-321` — the rejected-alternatives note says *"there is no welcome page (`/` redirects to `/manage-images`)"*; `/` is now `WelcomeModule.vue` (deliberately empty of copy) and the redirect was removed with view profiles. The rejection reasoning stands, but the parenthetical is factually wrong.
- `GUIDE_SYSTEM_PLAN.md:314-316` — the D11 first-launch signal reads `settings.tipsLastShown`; `USER_PROFILE_PLAN.md` is about to move settings per-profile, so this heuristic will need a re-check (a new profile inherits an empty stamp → the tour would fire for every second person on the same box).
- `GUIDE_SYSTEM_PLAN.md:439-441` — *"Create a notebook"* step description mentions "launch server → new notebook"; `notebooks.ts:36` still uses `notebooks.startServer`/`notebooks.openHome` anchors — verified present, still valid.
- `plots.ts:32` step *"pool across your selection"* — plot pooling now has per-attribute grouping shipped (`populations-panel-accordion`, `PhenotypeModule`); guide is not wrong but understates what the board does. Low-priority freshening.
- `extraGuides.ts:129-135` (`behaviourStates.after`) — anchor `layout.plotsSection` points at the plots slot; verify still rendered after the *panel-primitive* refactor (feat/panel-primitive merged 2026-09-25). Grep confirms attr exists — no fix needed but flag for the next visual pass.
- `moduleTask.ts:44` — `withPreview` comment cites `task_previewable`; still accurate but worth confirming after any `TaskRunner` primitive change.

## Add (missing coverage)

- **User profile picker** — `/profile-picker` (`modules/AppProfilePicker.vue`, boot-guarded when > 1 profile). Zero `data-guide` anchors, no guide. The primary reason many things (Kiwi identity, per-profile settings, project list filter) behave the way they do — needs a `Start` guide before `find-your-way-around`, or a two-step add to the tour. See `USER_PROFILE_PLAN.md` Phase 2.
- **Kiwi cockpit** — `KiwiCockpit.vue` has three unused anchors (`kiwi.pairing`, `kiwi.chat`, `kiwi.share`) plus the one `labLog.ts` uses. Warrants either its own guide (`assist-with-kiwi`, group `Analysis`) or extending `labLog.ts` from 2 to ~5 steps to cover pairing / captures / share / chat handoff. Kiwi is now the canonical assist surface (`kiwiNamingRatchet.test.ts` pins it), so the current one-liner in labLog undersells it badly.
- **Blackboard** — `/blackboard` (`BlackboardModule.vue`, `stores/blackboard.ts`, `feat/kiwi-blackboard-persistence` merged 2026-09-23). Zero anchors, no guide. The shared markdown surface for AI turns + notes; would naturally be group `Analysis`, prereq `projectOpen`, ~5 steps (new entry, attach capture, revise, set outcome).
- **View profiles** — the picker + editor (`AppProfilePicker.vue`, `ViewProfileEditor.vue`) has no guide even though `stores/guide.ts:315` already derives a `profileVisible` prereq for hidden pages. A user who lands on a curated menu has no in-app explanation of what a profile does. Group `Start`, right after the tour.
- **`/preprocess`** — `PreprocessingModule.vue` (crop today, MIP/bin/resample landing). Trivial `moduleTaskGuide` — one call in `taskGuides.ts`.
- **`/phenotype`** — sibling of Behaviour (uses gated pops rather than tracks). No guide; a bespoke shape (canvas-only, no TaskRunner). Group `Populations` or `Explore`.
- **`/regions` + `/spatial`** — Region clustering + Spatial analysis pages (`SPATIAL_REGIONS_PLAN.md`). Both shipped, both empty of anchors and guides. Region clustering is a `moduleTaskGuide` (`clustRegions.cluster`); spatial is a small bespoke (interaction matrix + aggregates + contacts). One recipe should tie them together.
- **Correction cockpit** — sidebar CTA is already anchored (`sidebar.correctionCta` in tour.ts:98) and pointed at as WIP. A short bespoke guide (open panel → pick mode → tool palette → apply) would let the tour drop the caveat.
- **Custom modules** — `/custom/:category` and the drop-in machinery under `<config_dir>/modules/`. Not in the picker at all; probably fine to defer (advanced surface) but the tour could point at where custom pages appear.
- **Task manager pop-out** — shipped (#632). Not in the tour; one bullet in `find-your-way-around` would cover it.

## Drop or consolidate

Files today (15 in `lib/guides/`, ~2848 lines excluding tests + recipes):

| File | Lines | Role |
|---|---|---|
| `index.ts` | 92 | registry |
| `types.ts` | 100 | types |
| `prereqs.ts` | 80 | PREREQ |
| `moduleTask.ts` | 221 | D8 builder |
| `taskGuides.ts` | 396 | 6× moduleTaskGuide (drift/segment/track/flow/denoise/coastal) |
| `extraGuides.ts` | 347 | 2× bespoke (fixMetadata, runChain) + 3× moduleTaskGuide (behaviour, clusterCells, clusterTracks) + `clusterToPops` helper |
| `importImages.ts` | 148 | bespoke |
| `tour.ts` | 155 | bespoke |
| `gatePopulations.ts` | 114 | bespoke |
| `plots.ts` | 86 | bespoke |
| `notebooks.ts` | 81 | bespoke |
| `movies.ts` | 126 | bespoke |
| `animation.ts` | 128 | bespoke |
| `labLog.ts` | 55 | bespoke, 2 steps |
| `recipes.ts` | 101 | different axis (keep) |

- **Move 3 `moduleTaskGuide` calls out of `extraGuides.ts` into `taskGuides.ts`** — `behaviourStatesGuide`, `clusterCellsGuide`, `clusterTracksGuide` + the `clusterToPops` helper. Delta: `taskGuides.ts` grows 396 → ~640; `extraGuides.ts` shrinks 347 → ~120 (fixMetadata + runChain only). Rationale: today's split is historical (original 7 vs. later 4), no longer a useful axis. After the move, `taskGuides.ts` = "every builder call", `extraGuides.ts` = "the two odd bespoke ones".
- **Inline `labLog.ts` (55 lines) into `extraGuides.ts`** alongside fixMetadata + runChain (or into the Kiwi-expanded guide if you take that option). It's 2 steps and unlikely to grow. Delta: −1 file, ≈−15 header/import lines. Or better: **rewrite it as the Kiwi guide** and give it a proper file — currently it points at a `?` dialog and stops, which is the shape of a hint, not a guide.
- **Rename `extraGuides.ts` → `bespokeGuides.ts`** (after the above two moves) and fold `plots.ts`, `notebooks.ts`, `movies.ts`, `animation.ts`, `gatePopulations.ts` into it. Delta: 6 files → 1, ~590 lines in one file. Only worth it if you want the "one place per page-shape" mental model gone. **Recommendation: don't** — each of those has real per-page prose and merging trades navigability for a smaller file tree.
- **Realistic minimal collapse: 15 → 11 files.** Do the first two above (move module-task calls, inline labLog) and leave the rest. Result: `taskGuides.ts` (all builder calls), `extraGuides.ts` renamed `singleTopicGuides.ts` (fixMetadata + runChain + labLog), five per-page bespoke files (import/tour/gate/plots/notebooks/movies/animation), plus runtime scaffolding. Better mental model, minor line-count reduction.
- **Full collapse to ~5 topical bundles is NOT worth it.** The split by page-shape earns its keep every time a guide breaks — a `gatePopulations` bug lives in `gatePopulations.ts`, not somewhere in a 1000-line grab bag. The plan's own D8 warning (*"generalise by scenario, not per widget"*) cuts the other way here.
- **`recipes.ts` (101) stays separate** — it's a different axis (what pipeline is mine, not what step am I on), and `guides.test.ts` treats it that way already. Its own file cost is fine.
- **Nothing to drop for a dropped feature** — every guide points at a real, currently-shipped page. The one *stale-tone* candidate is `tour.ts`'s WIP correction step; fix it by writing the correction guide or trimming the caveat, not by dropping the step.

## Notes on the plan doc itself

- **Status header (line 3)** — stale count and phase framing; the plan is now largely retrospective.
- **Lines 39-158 (build-log bullets)** — durable design record; leave as-is (they justify decisions that would otherwise re-emerge).
- **D1–D10 (locked decisions)** — still accurate; nothing has fundamentally shifted.
- **D11 (orientation tour, line 298-326)** — accurate except the welcome-page rejection parenthetical; the `settings.tipsLastShown` first-launch signal needs a re-check under per-profile settings.
- **Anchors-to-add table (lines 410-425)** — reads as a completed to-do list; either mark ✅ or drop, since anchors are now ratcheted by `guides.test.ts`.
- **"The guides" (lines 428-491)** — enumerates 15 planned + one "still-obvious NOT built" (spatial neighbour analysis, export/share). Spatial IS now built (`/spatial` route exists) but has no guide — see *Add*. Export/share is still open.
- **Reservations R0–R7 (line 513 onward)** — all still applicable; R2 (no demo data) got no worse, R5 (~64 step defs coupling to UI) now stands at ~78 anchors × ~180 step refs.
- **Add a short trailer** noting the audit (this file) and next-actions: profile picker + blackboard + expand Kiwi = the three highest-value adds; then the taskGuides/extraGuides split cleanup.
