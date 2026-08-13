# What's New — release notes modal + tips

Status: planning · no branch yet · supersedes `docs/archive/update-modal-prompt.md`

## Goal

Turn the existing "update available" plumbing into an in-app **What's New** modal that shows the
current release's notes, and add a small **tips** surface for feature onboarding. Reuse the four
notification surfaces (`docs/UI.md` → *Toast notifications*); no new surface.

## What already exists (do not rebuild)

The Sonnet prompt asked for a version-check endpoint, a check timer, a dismissal store and a
sidebar badge primitive. All four are already built:

- `api/src/update_api.jl:54` — `api_update_check` calls the GitHub releases API on demand and
  returns `{ updateAvailable, current, latest, url, scope }`.
- `api/src/update_api.jl:86` — `api_update_apply` stages the download and writes `.pending-update`.
- `frontend/src/stores/appControl.ts:43-83` — the ONE store; `checkUpdate`, `applyUpdate`,
  `dismissUpdate`, plus the reactive `updateAvailable` / `updateDismissed` / `updateLatest` state.
- `frontend/src/components/AppHeader.vue:42` — the header badge (dismissable pill), routes to
  Settings on click.
- `frontend/src/App.vue:46` — the boot-time fire-and-forget check.
- `INVENTORY.md:19` — flags "App update" as a **one canonical path — do not add a second**.

The gap is not the plumbing; it is the *content*. `api_update_check` reads the release `tag_name`
and `html_url` and **discards the `body` markdown** (`update_api.jl:79`). And there is no in-app
place to show release notes or tips.

## What's new here (scope)

1. One-field extension: `releaseNotes: string` on the update-check response (source = the release
   body).
2. `WhatsNewDialog.vue` (`BaseModal`) — patterned on `ClaudeOverviewDialog.vue`, opened from
   Settings and from the header badge.
3. `WhatNewCard.vue` — one card type for both updates and tips.
4. Static content in `frontend/src/lib/whatsNew.ts` (update card is generated from the store) and
   `frontend/src/lib/tips.ts` (a small seed catalogue), both ratcheted by the existing
   `frontend/src/utils/uiCopy.ts` copy budget.
5. A "Report a problem ↗" link on each card → the GitHub issues page (new tab). No thumbs, no
   in-app feedback endpoint (see Decision 4 for why).
6. Card schema leaves slots for the other two plans (`sketchAnimation?`, `statsAnnotation?`); each
   renders a grey placeholder box until the owning plan lands.

## Decisions (2026-07-27, draft)

1. **Reuse `/api/update/check`; extend response** with `releaseNotes: string` (source = the
   currently-discarded GitHub `body`). No new endpoint, no new poll timer.
2. **Modal on `BaseModal.vue`, patterned after `ClaudeOverviewDialog.vue`.** No new store — the
   dialog is `v-if`'d locally in `SettingsModule.vue` and in `AppHeader.vue`.
3. **No new badge primitive.** Reuse the existing header update badge for "update available".
   Tips do **not** get a persistent badge — they are opt-in from the modal and would otherwise be a
   fifth surface. Existing per-item badges (`.soon-badge`, `.lock-badge`, `.lablog-badge`) stay
   ad-hoc; a generic `Badge.vue` is deferred until three surfaces need it.
4. **No thumbs / in-app feedback capture.** Sonnet suggested thumbs → `POST /api/lablog/append`, but
   the lab log is a scientific record of work done on the *project* (imports, segmentations,
   analyses), not a bucket for app-level UX opinions — mixing them muddies the record. Alternatives
   considered:
   - Bespoke `/api/feedback` → local `<config_dir>/feedback.log`: works but Cecelia is
     single-user/single-install; there's no aggregator to send it to, so the file is
     write-only-forever. Not worth the surface.
   - Browser `localStorage`-only counter: useful to nobody but the local user.
   - GitHub issues: already the canonical channel for real problems; adding a link is free.

   **Chosen**: no thumbs. Each card has a "Report a problem ↗" link to
   `https://github.com/schienstockd/cecelia/issues/new` (opens in a new tab). The card format keeps
   `feedbackEnabled?: boolean` typed-but-unused so a future opt-in aggregator (if we ever ship one)
   can flip it on without a schema change.
5. **Card format is JSON-serialisable data**:
   ```ts
   interface WhatNewCard {
     id: string
     kind: 'update' | 'tip' | 'fix'
     title: string           // ≤ 8 words
     description: string     // ≤ 2 short sentences (uiCopy budget)
     steps?: string[]        // "Try it:" list
     feedbackEnabled?: boolean
     issueUrl?: string
     sketchAnimation?: object  // Plan 3 (SKETCH_ENGINE_PLAN); placeholder box until then
     statsAnnotation?: object  // Plan 2 (STATS_ANNOTATIONS_PLAN); unused until then
     releaseVersion?: string
     releaseUrl?: string
   }
   ```
   The two `?` slots are the cross-plan contract; do not change without touching Plans 2 & 3.
6. **Copy budget ratcheted.** All strings in `lib/whatsNew.ts` / `lib/tips.ts` go through the
   `uiCopy.ts` test surface. Same rule as tooltips: one line, imperative for steps.
7. **Dismissal**: the update card reuses `appControl.dismissUpdate()`. Per-tip dismissal extends the
   existing `unseen*` model in `stores/settings.ts` (same shape as `labLogUnseen*`); do not open a
   new persistence pattern.
8. **Tip of the day pops on app launch, opt-out.** Same modal shape as What's New; opened once
   per day (last-shown date in `stores/settings.ts`). A "Don't show tips on launch" checkbox on
   the card disables it. What's New (release-notes) uses the same modal but opens on-demand from
   Settings / the header badge. One modal component, two triggers.
9. **Users don't author sketches.** The animated sketches on tip cards come from the feijoa
   authoring repo (`SKETCH_ENGINE_PLAN.md`); a card just references a `SketchDefinition` by id.
   No editing UI in cecelia.
10. **Tips catalogue seeds** (three, minimum copy):
    - HMM behaviour states — what they are and how to assign them.
    - Cluster labels → populations — the two-step and where it lives.
    - Gate a population and cross-reference in napari — the two-clicks path.

## Phases (independently shippable)

- **W1** — Extend `api_update_check` to include `releaseNotes: body`. Extend `appControl.ts` and
  the TS response type. Header badge tooltip shows the notes as a hint on hover (optional).
- **W2** — `WhatNewCard.vue` + `WhatsNewDialog.vue`; `lib/whatsNew.ts` renders the update card
  from the store. Open from Settings "Software updates" (new "What's new →" link).
- **W3** — Wire the header badge click to open the modal alongside the existing Settings route
  (a small dropdown or a click-shift alternative — pick one after seeing it).
- **W4** — Tip-of-the-day on launch + `lib/tips.ts` with the three seeds. Per-tip dismissal +
  "don't show on launch" toggle in `stores/settings.ts`.
- **W5** — "Report a problem ↗" link per card (opens GitHub issues new tab). Card slots for
  Plans 2/3 render a labelled grey placeholder.

## Verify

- App with a stale local version → header update badge → click → What's New modal → update card
  first, with release notes rendered from `body`.
- Dismiss → badge clears; doesn't reappear (reuses `updateDismissed`).
- Tips tab rotates on Next; per-tip dismissal survives reload.
- "Report a problem ↗" opens the GitHub issues new-issue page in a new tab.
- `uiCopy.ts` ratchet passes.
- The four surfaces (toast / badge / lab-log entry / traffic light) remain exactly four
  (`docs/UI.md:269-273`); no fifth introduced.

## Out of scope

- User-submittable feature requests inside the modal — link to GitHub issues, done.
- In-app changelog page — link to the GitHub release, done.
- Auto-install without consent — already user-gated in `applyUpdate`.
- Generic `Badge.vue` primitive — deferred until three surfaces need it.

## Cross-plan slots

- `WhatNewCard.statsAnnotation` → `STATS_ANNOTATIONS_PLAN.md` (`StatsResult`). Typed but unused
  until that plan lands.
- `WhatNewCard.sketchAnimation` → optional embed hook for the sketchbook play repo
  (`SKETCH_ENGINE_PLAN.md`). Renders a grey placeholder ("Animation coming soon") or is dropped
  from the schema entirely if the sketchbook never graduates in-repo. Costs nothing to keep typed.

## References

- `INVENTORY.md:19` — App update canonical flow.
- `api/src/update_api.jl:54-86` — GitHub release fetch + apply.
- `frontend/src/stores/appControl.ts:43-83` — the ONE app-lifecycle store.
- `frontend/src/components/AppHeader.vue:42` — the update badge.
- `frontend/src/components/ClaudeOverviewDialog.vue` — modal pattern to mirror.
- `frontend/src/components/BaseModal.vue` — modal primitive.
- `docs/UI.md:260-273` — Toast + the four surfaces.
- `frontend/src/utils/uiCopy.ts` — copy budget ratchet.
- `frontend/src/stores/settings.ts:85-87` — `labLogUnseen*` pattern to mirror for tips.
