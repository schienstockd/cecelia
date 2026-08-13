> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

# Prompt 1: Update Notification + What's New System

Sonnet execution prompt. Read `INVENTORY.md`, `docs/UI.md` (UX primitive catalog), and `CLAUDE.md` before writing any code. This is narrow and well-defined — use existing components, do not build new ones except where explicitly specified.

---

## What this builds

A proactive update notification system and a "What's New / Tips" card system. Two things that share the same card format — updates are just cards marked `badge: "NEW"`. The card format is designed from the start to accept a `SketchAnimation` slot (the animation engine from Prompt 3 will slot in here — leave a clearly marked placeholder).

---

## Step 1 — Version check (Julia API)

Add `GET /api/app/version/check` endpoint:
- On startup and every 4 hours, Julia fetches `https://api.github.com/repos/schienstockd/cecelia/releases/latest`
- Extracts: `tag_name`, `body` (release notes markdown), `published_at`, `html_url`
- Compares to current running version (already in config)
- Stores result in memory: `{ hasUpdate, latestVersion, releaseNotes, releaseUrl, checkedAt }`
- Returns this state on `GET /api/app/version/check`
- Do NOT call GitHub on every request — return cached state, refresh only when cache is stale (>4h)

---

## Step 2 — Sidebar badge

When `hasUpdate` is true, the Settings sidebar entry shows a persistent accent badge (small dot, `--cc-accent` colour). This is NOT a toast — it persists until the user opens the What's New modal or dismisses. Use the existing sidebar badge mechanism if one exists; check `INVENTORY.md` first.

The badge clears when the user:
- Opens the What's New modal and sees the update card
- Clicks "Update now"
- Clicks "Dismiss"

Dismissed state persisted in `localStorage` keyed by `cc.dismissed-update:{version}` so it doesn't re-appear for the same version after dismissal.

---

## Step 3 — What's New modal

Built on `BaseModal` (mandatory — do not use PrimeVue Dialog or a hand-rolled overlay).

Opened from:
- Clicking the Settings sidebar badge
- A "What's New" button in Settings

Content: a scrollable list of `WhatNewCard` components (see Step 4). First card is the update card if `hasUpdate`, followed by tip cards.

Footer:
- "File a bug" → opens `https://github.com/schienstockd/cecelia/issues/new` in a new tab
- "Close" button
- If update available: "Update now" button → calls existing update mechanism

---

## Step 4 — Card format (`WhatNewCard.vue`)

Each card covers one feature, tip, or concept. Same component for update announcements and tips.

```typescript
interface WhatNewCard {
  id: string                    // stable id for dismissal/feedback tracking
  badge?: "NEW" | "TIP" | "FIX" // shown as a small chip on the card
  title: string                 // short, < 8 words
  description: string           // one paragraph, plain language
  steps?: string[]              // "Try it:" numbered steps
  feedbackEnabled?: boolean     // show thumbs up/down
  issueUrl?: string             // "Report a problem" → opens GitHub issue
  sketchAnimation?: object      // PLACEHOLDER — sketch engine (Prompt 3) slots here
                                // for now: if present, render a grey placeholder box
                                // with text "Animation coming soon"
  releaseVersion?: string       // which release this was introduced in
  releaseUrl?: string           // link to GitHub release
}
```

**Card layout:**
```
┌─────────────────────────────────────┐
│ [NEW]  Title                        │
│                                     │
│ [sketch placeholder or animation]   │
│ 170px tall, rounded, --cc-surface-2 │
│                                     │
│ Description paragraph               │
│                                     │
│ Try it:                             │
│ 1. Step one                         │
│ 2. Step two                         │
│                                     │
│ 👍  👎    [Report a problem ↗]      │
└─────────────────────────────────────┘
```

Thumbs feedback: `POST /api/feedback` `{ cardId, thumbs: "up"|"down", version }` — Julia logs to the lab log as a `[Cecelia]` entry. No external service. The feedback endpoint is a stub for now (logs and returns ok).

---

## Step 5 — Tip of the day

A separate small non-modal callout in the app (not the What's New modal). Shows one tip card per day. Rotates through a static array of `WhatNewCard` objects defined in `lib/tips.ts`. New tips get `badge: "NEW"`. Shown in a `FloatingPanel` or a pinned strip — check existing surfaces in `INVENTORY.md` before choosing placement.

The tip array starts with 3-5 cards covering:
- How HMM behaviour states work
- How to assign cluster labels to populations
- How to gate on cells and cross-reference in Napari
- How to use the chain whiteboard

Each tip card has `steps` and a `sketchAnimation` placeholder (grey box for now).

Dismissed per-tip in `localStorage` (`cc.tip-seen:{id}`). "Next tip" button cycles manually.

---

## Step 6 — Weave with stats (Prompt 2)

Stats results (t-test p-values, ANOVA) will eventually appear as data in tip/sketch cards. For now: leave a `statsAnnotation?: object` field in `WhatNewCard` (unused, just typed). This is the hook Prompt 3's sketch engine will read to overlay stats on a sketch.

---

## Verify

- Start app with a stale version → sidebar Settings badge appears
- Click badge → What's New modal opens, update card is first
- Dismiss → badge clears, doesn't reappear for this version on restart
- "File a bug" → GitHub issues opens in new tab
- Tip of the day rotates, dismisses per-tip, "NEW" badge on recent tips
- Thumbs feedback → lab log entry appears
- No new notification mechanism created (check: still exactly four surfaces — toast, badge, lab log, traffic light)
