# Guide additions plan — close the coverage gap the audit found

Status: **planning** (drafted 2026-09-25). Punch list from [`GUIDE_SYSTEM_AUDIT.md`](GUIDE_SYSTEM_AUDIT.md).
Prior art: [`GUIDE_SYSTEM_PLAN.md`](GUIDE_SYSTEM_PLAN.md) (D1–D11 still binding).

Goal: nine new guides + one tour touch-up, distributed across five phases so each phase is
independently reviewable and ship-able. Draft copy inline per phase — Dominik redlines before the
PR. Tone: matches existing guides (`taskGuides.ts`, `singleTopicGuides.ts`) — one short sentence
per step, 1–3 punchy bullets, em-dash punch, no fluff.

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

**Draft copy** — `use-a-user-profile`:

1. `profilePicker.list` · *right* — **Who is using this box** — Pick a profile at launch — each one has its own settings, projects, Kiwi identity. · *One profile per person, not per experiment. · Same box, different people = different profiles.*
2. `profilePicker.new` · *left* — **Add a profile** — New profile = fresh settings and an empty project list. · *Nothing is shared across profiles by default. · Delete a profile from Settings, not here.*
3. `header.profileChip` · *bottom* — **Which one you are now** — The chip in the header shows the active profile — click to switch. · *Switching mid-session reloads the app.*

**Draft copy** — `use-a-view-profile`:

1. `profilePicker.viewProfile` · *left* — **A view profile hides pages** — Pick a curated menu — the sidebar shows only what that view enables. · *This is decluttering, not access control — every page is still reachable.*
2. `viewProfile.editor` · *right* — **Edit or make your own** — Tick which pages a view profile exposes and in what order. · *Great for handing a colleague a "just the segmentation bits" cecelia.*

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

**Draft copy** — `assist-with-kiwi`:

1. `sidebar.labLogCta` · *right* — **Lab log** — A per-project, append-only record of what was done — cecelia adds a digest when a run finishes. · *Floating panel, reachable from any page.*
2. `sidebar.kiwiCta` · *right* — **Meet Kiwi** — Kiwi is the assistant cockpit — pair a session, chat, and share context in. · *On-demand only; nothing happens until you ask.*
3. `kiwi.pairing` · *left* — **Pair a session** — Pair Kiwi with a running claude-code session on your machine — that is who does the work. · *Unpaired Kiwi still remembers state; it just cannot act.*
4. `kiwi.chat` · *left* — **Talk to Kiwi** — Ask a question here — Kiwi routes to the paired session and streams the reply back. · *A Kiwi turn is a captured record — it lands in the blackboard automatically.*
5. `kiwi.share` · *left* — **Share what you are looking at** — Ship your current selection or plot into the chat so Kiwi has the same view you do. · *This is how you avoid re-typing "the second cluster on the UMAP".*
6. `kiwi.assistantHelp` · *bottom-start* — **What Claude can see and do here** — This `?` explains the full assist surface — read it once. · *Also tells you whether the assist is actually set up.*

### P3 — Blackboard guide

One bespoke `GuideDef` in `singleTopicGuides.ts`. `/blackboard` has its own shape (a shared
markdown surface for AI turns + notes, per `feat/kiwi-blackboard-persistence`), so a hand-written
step list rather than `moduleTaskGuide`. Group `Analysis`, prereq `projectOpen`.

Steps (approx): nav `/blackboard` → new entry → attach a capture → revise flow → set outcome.
5 steps.

**Anchors to add** (~5, all in `BlackboardModule.vue`):
`blackboard.new`, `blackboard.entry`, `blackboard.attachCapture`, `blackboard.revise`,
`blackboard.outcome`.

**Draft copy** — `use-the-blackboard`:

1. `nav:/blackboard` · *right* — **Blackboard** — Shared markdown between you and the assist — every Kiwi turn lands here. · *This is where a conclusion becomes durable — not chat scrollback.*
2. `blackboard.new` · *left* — **New entry** — Start an entry when you begin a thought — attach captures and plots to it as they land. · *Entries are per project; open a project first.*
3. `blackboard.attachCapture` · *left* — **Attach a capture** — Anything you captured (a plot, a viewer snapshot, a table) can drop into an entry. · *Captures are indexed — Kiwi can cite them back at you later.*
4. `blackboard.revise` · *left* — **Revise, don't overwrite** — Every revision is kept, so you can see what changed and when. · *Kiwi revisions are marked as such.*
5. `blackboard.outcome` · *left* — **Mark it done** — Set an outcome when the question is answered — that closes the entry and pins the conclusion. · *An outcome is the "what did we learn" — it is what carries into the next project.*

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

**Draft copy** — the four `moduleTaskGuide` prose blocks (builder fills the standard 5 steps —
these are the guide-specific fields):

- **`preprocess-images`** — group `Data`, funName `preprocess.crop` (or the composite when
  MIP/bin/resample land).
  *intro*: Preprocess sits between import and cleanup — crop, project, resample, whatever the raw file needs before you segment.
  *params*: Crop bounds — pull in only the region you care about, everything else follows. · Reference image — copies bounds from a sibling to do a set in one pass.
  *after* (plotsSection): **Check what came out** — Compare the cropped version against the original in the viewer — this is destructive, no going back per version. · *Preprocessing writes a new version like the other cleanup steps.*

- **`phenotype-cells`** — group `Populations` (recommend), funName `phenotype.classify`.
  *intro*: Phenotyping is the gated version of clustering — you know the populations already, so pin cells to them from measures.
  *params*: Populations — the gated pops that define the phenotype vocabulary. · Confidence threshold — cells below it stay unlabelled rather than get a wrong pin.
  *after* (plotsSection): **Read the assignments** — The panel shows how many cells landed in each phenotype, and how many were left unlabelled. · *A big unlabelled tail means the gates do not cover what is on the slide.*

- **`cluster-regions`** — group `Populations`, funName `clustRegions.cluster`. Uses the shared
  `clusterToPops` helper for the tail.
  *intro*: Region clustering groups spatial neighbourhoods, not cells — the output is regions you name and plot on the image.
  *params*: Cluster on — spatial features (neighbour counts, distances). · Resolution — higher = smaller, more numerous regions.
  *after* (plotsSection then `...clusterToPops('/regions', 'region')`): **UMAP + heatmap** — Same read as cell clustering — UMAP shows the separation, heatmap says what each region is composed of.

- **`spatial-analysis`** — group `Explore`, funName `spatial.interactionMatrix` (or the composite
  when others land).
  *intro*: Spatial analysis reads relationships between populations on the image — who is near whom, and how often.
  *params*: Populations — the pairs to score interactions between. · Contact radius (µm) — what counts as "near"; scale by cell size.
  *after* (plotsSection): **The matrix and contacts** — Read the interaction matrix like a heatmap — bright cells are population pairs that co-locate more than chance. · *Contact plots show the same signal per image, not pooled.*

### P5 — Correction cockpit + tour cleanup

One short bespoke `GuideDef` in `singleTopicGuides.ts` (id `correct-a-mask`, group
`Populations`). Then drop the WIP caveat from `tour.ts:98-103` — the tour keeps its step, but
the caveat text goes.

Steps: sidebar CTA (already anchored, `sidebar.correctionCta`) → open panel → pick mode → tool
palette → apply. 4 steps.

**Anchors to add** (~3, all in the correction cockpit component):
`correction.modePicker`, `correction.toolPalette`, `correction.apply`.

**Draft copy** — `correct-a-mask`:

1. `sidebar.correctionCta` · *right* — **Correction cockpit** — When segmentation gets a cell wrong, fix it here — merge, split, redraw, delete. · *Floating panel — it sits over the viewer so you keep looking at the image.*
2. `correction.modePicker` · *left* — **Pick a mode** — Each mode changes what your click does — merge two, split one, redraw an outline. · *The mode name matches the tool below it.*
3. `correction.toolPalette` · *left* — **Tool palette** — Draw, click, or lasso — depends on the mode. · *Undo works per correction, not per stroke.*
4. `correction.apply` · *left* — **Apply and move on** — Corrections write to the label set as a new version — the original run is preserved. · *Downstream tasks read the ACTIVE version, like everywhere else.*

**Tour touch-up**: drop `tour.ts:98-103`'s "*a walkthrough for this is still being written*" —
the walkthrough exists now.

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
