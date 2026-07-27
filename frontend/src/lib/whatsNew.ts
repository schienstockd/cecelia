// What's New card model + release-notes source + shared open/close state.
//
// Two content streams share ONE card shape (updates + tips). The update card is built from the
// appControl store (release notes come from `/api/update/check` — see WHATS_NEW_PLAN.md). Tips
// come from `lib/tips.ts` (W4).
//
// The dialog is mounted ONCE in App.vue and driven by the shared `isWhatsNewOpen` ref below —
// any caller (header badge, Settings button, launch tip in W4) calls `openWhatsNew()`. One
// modal, one state.
//
// The `sketchAnimation` / `statsAnnotation` slots are typed but rendered as placeholders until
// SKETCH_ENGINE_PLAN.md / STATS_ANNOTATIONS_PLAN.md land content.
import { computed, ref, type ComputedRef } from 'vue'
import { marked } from 'marked'
import { useAppControlStore } from '../stores/appControl'

export type WhatNewKind = 'update' | 'tip' | 'fix'

export interface WhatNewCard {
  id: string
  kind: WhatNewKind
  title: string
  description?: string      // short intro line above the body
  bodyMd?: string           // raw markdown (release notes / long tip text). Rendered via `marked`.
  steps?: string[]          // "Try it:" numbered list
  issueUrl?: string         // "Report a problem" link target (defaults to the cecelia issues page)
  releaseVersion?: string   // shown as a chip on update cards
  releaseUrl?: string       // "View on GitHub" link on update cards
  publishedAt?: string      // ISO date on update cards
  sketchAnimation?: unknown // slot for SKETCH_ENGINE_PLAN
  statsAnnotation?: unknown // slot for STATS_ANNOTATIONS_PLAN
}

export const CECELIA_ISSUES_URL = 'https://github.com/schienstockd/cecelia/issues/new'

// Shared modal state — one dialog mounted in App.vue reads this. GitHub Flavored Markdown is on
// by default in marked ≥5, so task lists, tables, strikethrough all work. Source is trusted (our
// own release bodies), so no separate sanitiser.
export const isWhatsNewOpen = ref(false)
// When `openWithTip` is true, the dialog prepends today's tip card to the list. Cleared on close.
// Used by the once-per-day launch trigger in App.vue; the header/Settings entry points open
// without a tip (release notes only).
export const openWithTip = ref(false)

export function openWhatsNew(opts?: { withTip?: boolean }) {
  openWithTip.value = !!opts?.withTip
  isWhatsNewOpen.value = true
}
export function closeWhatsNew() {
  isWhatsNewOpen.value = false
  openWithTip.value = false
}

// Dev knob (Settings → Developer): force the Install button visible in the What's New footer
// regardless of `updateAvailable` / `canApplyUpdate` — for previewing the install-flow UI on a
// dev checkout, where those flags are always false. Not persisted (session-only).
export const debugForceInstallable = ref(false)

marked.setOptions({ gfm: true, breaks: false })

/** Render markdown → HTML for `v-html` in `WhatNewCard`. Sync helper (marked has a sync API). */
export function renderMarkdown(md: string | undefined | null): string {
  if (!md) return ''
  try { return marked.parse(md, { async: false }) as string }
  catch { return md }
}

/** Reactive card for the latest release — shown whenever we know a latest version, whether or not
 *  an upgrade is available (dev checkouts and up-to-date installs still deserve the release notes).
 *  The install button in the dialog footer is what gates on `updateAvailable + canApplyUpdate`. */
export function useUpdateCard(): ComputedRef<WhatNewCard | null> {
  const app = useAppControlStore()
  return computed(() => {
    if (!app.updateLatest) return null
    const description = app.updateAvailable
      ? (app.updateCurrent ? `You're running ${app.updateCurrent}.` : undefined)
      : (app.updateCurrent ? `You're up to date (${app.updateCurrent}).` : undefined)
    return {
      id: `update-${app.updateLatest}`,
      kind: 'update',
      title: `Cecelia ${app.updateLatest}`,
      description,
      bodyMd: app.updateNotes || undefined,
      releaseVersion: app.updateLatest,
      releaseUrl: app.updateUrl || undefined,
      publishedAt: app.updatePublished || undefined,
    }
  })
}

/** Format an ISO timestamp as "5 Aug 2026" for compact card display. Empty string on invalid input. */
export function formatCardDate(iso: string | undefined): string {
  if (!iso) return ''
  const d = new Date(iso)
  if (Number.isNaN(d.getTime())) return ''
  return d.toLocaleDateString(undefined, { day: 'numeric', month: 'short', year: 'numeric' })
}
