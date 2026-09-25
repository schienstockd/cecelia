# Guide additions plan — close the coverage gap the audit found

Status: **planning** (drafted 2026-09-25; rewritten same day after a ground-truth sweep against
main). Punch list from [`GUIDE_SYSTEM_AUDIT.md`](GUIDE_SYSTEM_AUDIT.md). Prior art:
[`GUIDE_SYSTEM_PLAN.md`](GUIDE_SYSTEM_PLAN.md) (D1–D11 still binding).

Goal: seven new guides + one park + one tour touch-up, distributed across five phases so each phase
is independently reviewable and ship-able. Draft copy inline per phase — Dominik redlines before
the PR. Tone: matches existing guides (`taskGuides.ts`, `singleTopicGuides.ts`) — one short
sentence per step, 1–3 punchy bullets, em-dash punch, no fluff.

## Locked decisions

**D12 — Every new guide follows the existing catalogue split.** `moduleTaskGuide` for anything with
a `TaskRunner`; bespoke `GuideDef` in `singleTopicGuides.ts` for pointer-only or unique-shape
surfaces; its own per-page-shape file only when it has enough prose that a shared file would
dominate. No new file types.

**D13 — Kiwi replaces lab-log-and-claude, not extends it.** The current `labLogGuide` is a
two-step pointer that predates Kiwi being the assistant cockpit. Rewrite it in place as
`kiwiGuide` (id `assist-with-kiwi`, group `Analysis`), keeping the lab-log step at the front but
lengthening the Kiwi half from one step to five (pairing / chat-starter / share / help). Change
the id — pinned tests find `lab-log-and-claude` today; verified no external cites.

**D14 — Profile guide targets the header chip, NOT the launch picker.** `AppProfilePicker.vue` is
a bare-route boot interstitial: the router guard bounces users there when
`appCtl.needsProfilePick === true` and away when `false`, so a "click here on the picker" step
cannot land mid-session. The reachable-anytime surfaces are the header profile chip
(`AppHeader.vue` line 147 — already implemented, no anchor yet) → `PreferencesModal.vue`
(Profiles pane, USER_PROFILE_PLAN Phase 4). Switching a profile triggers a full reload — the
guide's copy owns that caveat.

## Phases

### P1 — Start-group additions (user profile + view profile)

Two guides, both `Start` group, both open to a single-profile user (view profile is single-user
useful; user-profile guide reads amber-not-hidden when `profileCount === 1`).

- **`use-a-user-profile`** — bespoke `GuideDef` in `singleTopicGuides.ts`. Targets the header
  chip and the Profiles pane in Preferences. 4 steps.
- **`use-a-view-profile`** — bespoke `GuideDef` in `singleTopicGuides.ts`. Targets Settings →
  View profile → Edit → `ViewProfileEditor` modal. 3 steps.

**Anchors to add** (~6):

| Anchor | Component / line | Note |
|---|---|---|
| `header.profileChip` | `AppHeader.vue:147` (`.profile-chip` button) | already opens Preferences on click |
| `prefs.profilesPane` | `PreferencesModal.vue:203` (`<template v-if="active === 'profiles'">`) | wraps the whole Profiles pane |
| `prefs.newProfile` | `PreferencesModal.vue:296` (`New profile` button) | already labelled |
| `prefs.copyLogin` | `PreferencesModal.vue:300` (`Copy login command` button) | credential setup surface |
| `settings.viewProfile` | `SettingsModule.vue:748` (`View profile` field) | wraps the chip + Edit row |
| `viewProfile.editor` | `ViewProfileEditor.vue:115` (BaseModal root or `.vp-body`) | inside the modal |

**New prereq**: `profilesMulti` — true when `appControl.profileCount > 1`. Needs a new field
`profileCount: number` on `GuideCtx` (`lib/guides/types.ts`) sourced from
`useAppControlStore().profileCount`. The switching guide is *useful but not blocking* on a
single-profile install (the user can still create a new profile there) → prereq is *soft*: the
step-with-anchor `header.profileChip` sits behind `when: c => c.profileCount > 1` so single-user
installs skip it silently rather than being blocked by an amber warning.

**Draft copy** — `use-a-user-profile`:

1. `header.profileChip` · *bottom* — **Who is driving** — The chip in the header names the active profile — click to open Preferences → Profiles. · *One profile per person, not per experiment.*
2. `prefs.profilesPane` · *right* — **Profiles pane** — Every profile has its own Kiwi credentials, preferences and project list — switching one over reloads the app. · *No cross-profile sharing by default.*
3. `prefs.newProfile` · *bottom* — **Add a profile** — New profile = fresh credentials and an empty project list. · *Retire from here too; the record stays resolvable for old turn logs.*
4. `prefs.copyLogin` · *bottom* — **Log the profile in** — Copies a terminal one-liner scoped to this profile's `CLAUDE_CONFIG_DIR` — paste and run `/login`. · *Each profile logs Claude in separately.*

**Draft copy** — `use-a-view-profile`:

1. `nav:/settings` · *right* — **Settings** — View profiles live in Settings — they curate the sidebar down to the pages this user actually works on. · *This is decluttering, not access control — every page still opens by URL.*
2. `settings.viewProfile` · *left* — **Pick or edit one** — The chip row picks which profile is active; Edit opens the builder. · *"All pages" is the default; you never lose access to a page by picking a profile.*
3. `viewProfile.editor` · *bottom* — **Tick pages into a profile** — One chip row per sidebar group; drag chips to reorder. · *Great for handing a colleague a "just the segmentation bits" cecelia.*

### P2 — Kiwi guide, replacing lab-log-and-claude

One rewrite, one file. Rename `labLogGuide` → `kiwiGuide` inside `singleTopicGuides.ts` and
change its id `lab-log-and-claude` → `assist-with-kiwi`. Uses the four existing anchors in
`KiwiCockpit.vue` (`kiwi.assistantHelp` line 265, `kiwi.pairing` 284, `kiwi.chat` 301,
`kiwi.share` 317) plus `sidebar.labLogCta` and `sidebar.kiwiCta` (both in `AppSidebar.vue`).
Group `Analysis`, prereq `projectOpen`.

**Anchors to add**: 0 — all six exist.

**External cite check**: grepped `lab-log-and-claude` across the repo; two hits, both inside
`singleTopicGuides.ts` itself. Safe to rename.

**Test update**: `guides.test.ts` currently pins guide ids as a set. Update the id.

**Draft copy** — `assist-with-kiwi`:

1. `sidebar.labLogCta` · *right* — **Lab log** — A per-project, append-only record — cecelia adds a digest when a run finishes, you and Kiwi append notes. · *Floating panel, reachable from any page.*
2. `sidebar.kiwiCta` · *right* — **Kiwi is the assist cockpit** — Everything Claude-adjacent lives in this panel — pairing status, chat starter, and the buttons that share what is on screen. · *Kiwi is on-demand; nothing happens until you ask.*
3. `kiwi.pairing` · *left* — **Pairing status** — Shows whether a claude-code session on this machine is paired — the refresh button probes the socket. · *Unpaired Kiwi still holds state; it just cannot push captures.*
4. `kiwi.chat` · *left* — **Copy a chat starter** — Copies a one-line opener that names this project — paste it into the paired session. · *This is how you start a Kiwi conversation about the current project.*
5. `kiwi.share` · *left* — **Ship what you are looking at** — Send the viewer frame or a canvas plot into the capture envelope — no more "the second cluster on the UMAP". · *Falls back to the clipboard if nothing is paired.*
6. `kiwi.assistantHelp` · *bottom-start* — **What Kiwi can see and do** — This `?` explains Ask vs Chat, captures, chains, blackboard, limits — read once. · *Also reports whether the assist is set up.*

### P3 — Blackboard guide

One bespoke `GuideDef` in `singleTopicGuides.ts` (id `use-the-blackboard`). Group `Analysis`,
prereq `projectOpen`. Route `/blackboard` confirmed.

**Anchors to add** (~5, all in `BlackboardModule.vue`):

| Anchor | Where | Note |
|---|---|---|
| `blackboard.new` | toolbar `New entry` button (~line 483) | already labelled |
| `blackboard.list` | `SelectionTable` in `.bb-list` (~line 522) | list of entries |
| `blackboard.edit` | pane-head `Edit` button (~line 642) | revising = editing an existing entry |
| `blackboard.outcome` | `.bb-outcome` row (~line 655) | Tag outcome / Good / Bad |
| `blackboard.addToKiwi` | `AddToKiwiButton` in pane head (~line 613) | how the entry becomes Kiwi context |

**Corrected model**: attachments are *inbound* — captures from Kiwi land as attachment thumbnails
on the entry (see `bb-attach-strip`). There is no "attach a capture" button; the plan's original
`blackboard.attachCapture` was wrong. `addToKiwi` is the outbound counterpart and is a real
button on the pane head.

**Draft copy** — `use-the-blackboard`:

1. `nav:/blackboard` · *right* — **Blackboard** — Shared markdown between you and the assist — where a conclusion becomes durable, not chat scrollback. · *Every Kiwi turn about this project lands here too.*
2. `blackboard.new` · *bottom* — **Start an entry** — One entry per thought you are chasing — captures land under it as they arrive from the viewer or Kiwi. · *Entries are per project; open a project first.*
3. `blackboard.list` · *right* — **The entry list** — Filter by status (open / in-progress / done) and outcome (good / bad) — pinned project profile stays at the top. · *Sort by any column; the state is remembered per project.*
4. `blackboard.edit` · *bottom* — **Edit and version** — Edit takes a snapshot before saving — every revision is kept and previewable from the version dropdown. · *Restore v3 with a click if v4 went sideways.*
5. `blackboard.outcome` · *bottom* — **Tag outcome** — Good or Bad + a note when the thread is settled — what carries into the next project. · *The note is required; it is what a future session actually needs to know.*
6. `blackboard.addToKiwi` · *left* — **Send to Kiwi** — Feed this entry (or a specific version) into a Kiwi turn — it appears as context alongside your prompt. · *Ships the version you are viewing.*

### P4 — Module-page trivia

Three `moduleTaskGuide` calls (was four — Phenotype is bespoke-only, see below):

- **`preprocess-images`** — `/preprocess`, `PreprocessingModule.vue`, module key `preprocess`.
  Ships crop today; MIP/bin/resample land in `editImages/`. **`funName = editImages.cropImage`**
  (task JSON: `app/src/tasks/editImages/cropImage.json`; NOT `preprocess.crop`). Group `Data`,
  prereq `imageImported`.
- **`cluster-regions`** — `/regions`, `RegionClusteringModule.vue`. `funName = clustRegions.cluster`
  ✓ (task JSON: `app/src/tasks/clustRegions/cluster.json`). Group `Populations`. Same
  moduleTaskGuide + `clusterToPops('/regions', 'region')` tail as `cluster-cells`.
- **`spatial-analysis`** — `/spatial`, `SpatialAnalysisModule.vue`. **`funName =
  spatialAnalysis.neighbourStats`** (task JSON: `app/src/tasks/spatialAnalysis/neighbourStats.json`;
  NOT `spatial.interactionMatrix`). Group `Explore`. Multiple tasks in this module —
  neighbourStats is the readout users open the page for; the graph (`cellNeighbours`) runs first
  and is called out in the `intro`.

**PARKED** — **`phenotype-cells`**: `PhenotypeModule.vue` mounts `SummaryCanvas` only, no
`TaskRunner`. There is no `phenotype/` task directory. Not a `moduleTaskGuide` fit; a bespoke
"pick images, read the plot" guide would parrot the existing `plots.ts` guide. Park until either
a Phenotype task lands, or the page grows a distinctive shape worth its own guide.

**Anchors to add** (~3, or 0 if `nav:/route` and `TaskRunner`'s existing anchors are enough):
The three P4-live modules all use `ModuleLayout`, which already ships `layout.plotsSection`
(`ModuleLayout.vue:393`). `TaskRunner`'s function-select / params / Run anchors are shared for
all `moduleTaskGuide` uses (D8). `nav:/preprocess`, `nav:/regions`, `nav:/spatial` resolve via
the sidebar's href scheme (`AppSidebar.vue:222` comment: `nav:` is the scheme). Nothing
per-module to add; confirm during each phase's PR.

**Draft copy** — the three `moduleTaskGuide` prose blocks (builder fills the standard 5 steps —
these are the guide-specific fields):

- **`preprocess-images`** — group `Data`, `funName = editImages.cropImage`.
  *intro*: Preprocess sits between import and cleanup — crop today, MIP / bin / resample as they land.
  *funHint*: Crop today; MIP / bin / resample are separate tasks in this module.
  *params*: Bounds — pull in only the region you care about. · Value name — writes a new version, keeps the original.
  *after* (plotsSection): **Check the crop** — Compare the new version against the original in the viewer — it is a new image version, not an overwrite. · *Everything downstream reads the ACTIVE version.*

- **`cluster-regions`** — group `Populations`, `funName = clustRegions.cluster`. Uses the shared
  `clusterToPops` helper for the tail.
  *intro*: Region clustering groups spatial NEIGHBOURHOODS ("what surrounds each cell"), not the cells themselves — SET-scope so region IDs are comparable across images.
  *selectHint*: Select every image you want clustered TOGETHER — the composition vectors pool across them.
  *params*: Neighbour graph — the substrate produced by Spatial's cellNeighbours; pick one to cluster over. · Resolution — Leiden resolution; higher = smaller, more numerous regions.
  *after* (plotsSection then `...clusterToPops('/regions', 'region')`): **Read the regions** — UMAP shows the separation; the heatmap says what each region is composed of. · *Region populations are usable everywhere a gated one is.*

- **`spatial-analysis`** — group `Explore`, `funName = spatialAnalysis.neighbourStats`.
  *intro*: Spatial analysis reads relationships between populations on the image — who is near whom, and how often.
  *funHint*: Run Neighbour graph first — this readout LOADS it, does not build it. · Interaction matrix + Aggregates + Cell contacts all live here; this guide teaches the matrix.
  *params*: Populations — the pairs to score interactions between. · Permutations — the null model; more = tighter p-values, slower.
  *after* (plotsSection): **Read the matrix** — Log-odds heatmap: bright cells co-locate more than chance, dark ones avoid. · *The permutation test tells you if a pattern is a real signal or the same cell types rearranged.*

### P5 — Correction cockpit + tour cleanup

One short bespoke `GuideDef` in `singleTopicGuides.ts` (id `correct-a-mask`). Group `Populations`.
The correction cockpit is `frontend/src/components/correction/CorrectionCockpit.vue` — a
`FloatingPanel` with three modes (tracks / labels / review), a per-mode toolbar, a chip strip of
picks, and a queue+apply row.

**Anchors to add** (~4, all in `CorrectionCockpit.vue`): NONE exist today.

| Anchor | Where | Note |
|---|---|---|
| `correction.modePicker` | `.cockpit-mode` ChipSelect (~line 604) | Tracks / Labels / Review chips |
| `correction.tools` | `.cockpit-tools` wrapper (~line 621) | per-mode verb toolbar |
| `correction.selection` | `.cockpit-chip-strip` (~line 730) | picked tracks / labels chip strip |
| `correction.apply` | Apply button in `.cockpit-queue` (~line 766) | the "commit" for the queued edits |

**Steps**: sidebar CTA (already anchored `sidebar.correctionCta`, `AppSidebar.vue:181`) → mode
picker → tools → selection → apply. 5 steps.

**Draft copy** — `correct-a-mask`:

1. `sidebar.correctionCta` · *right* — **Correction cockpit** — When segmentation gets a cell wrong, fix it here — merge, split, redraw, delete. · *Floating panel — it sits over the viewer so you keep looking at the image.*
2. `correction.modePicker` · *bottom* — **Pick a mode** — Tracks / Labels / Review — each mode drives its own queue and its own toolbar. · *Review pages over the picked labels one at a time.*
3. `correction.tools` · *left* — **Toolbar for this mode** — Each verb queues an edit — nothing is applied until you click Apply below. · *Undo removes the last queued edit; Clear drops all of them.*
4. `correction.selection` · *left* — **What is picked** — Chip strip of the tracks or labels the toolbar acts on — click a chip to drop it. · *Empty chip strip = no verbs are enabled yet.*
5. `correction.apply` · *left* — **Apply, and it becomes a version** — Corrections write a new label-set version — the original run is preserved. · *Downstream tasks read the ACTIVE version, like everywhere else.*

**Tour touch-up**: drop `tour.ts:98-103`'s bullet `'A walkthrough for this is still being written.'` — the walkthrough exists now.

## Explicitly out of scope

- **Custom modules** (`/custom/:category`) — advanced surface, per the audit; defer.
- **Task-manager pop-out** — could be one bullet in `find-your-way-around`; not worth a whole
  guide. Add as a tour touch-up in P5 if convenient.
- **A guide for the notebook Kiwi handoff** — Kiwi guide (P2) covers the pattern once.
- **Phenotype** — parked in P4 above; revisit when the module grows a task or a distinct shape.

## Sequencing rationale

P1 first because everything else assumes a user knows what profile they landed in. P2 second
because the Kiwi rewrite tightens the assist story P3's Blackboard guide can lean on. P3 and P4
are independent — pick whichever's copy is easier to write next. P5 last because the correction
cockpit copy is fiddly, needs all-new anchors, and the WIP-caveat drop is a one-line sweetener
that ships with it.

## Reservations

- **Plan was rewritten after a ground-truth sweep on 2026-09-25.** First draft named
  `AppProfilePicker` as a walkthrough target (it is a launch interstitial), invented fun_names
  (`preprocess.crop`, `spatial.interactionMatrix`) that do not exist, and assumed Phenotype has a
  `TaskRunner` (it does not). All corrected here; see D14 and P4-PARKED.
- **New anchors are per-phase discipline.** Each phase adds `data-guide` attrs;
  `guides.test.ts` fails the build if any are missing. That is the intended safety net (D5).
- **`profilesMulti` prereq (P1) needs `GuideCtx.profileCount`.** New field on `types.ts`,
  populated in the guide runtime from `useAppControlStore().profileCount`. Prereq is soft — the
  P1 step behind `when: c => c.profileCount > 1` skips silently on single-user installs.
- **`assist-with-kiwi` id change (D13) is safe.** Grepped: only self-references in
  `singleTopicGuides.ts`. Update those + `guides.test.ts` when P2 lands.
- **P4 fun_names verified against `app/src/tasks/**.json`.** `editImages.cropImage`,
  `clustRegions.cluster`, `spatialAnalysis.neighbourStats` — confirmed present. The two the first
  draft named do not exist.
- **Correction cockpit is anchor-less today.** P5 is the first guide to hit that component; the
  four anchors above are all new. Confirm the DOM shape hasn't drifted between plan and PR.
