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
// `sketchAnimation` points at a feijoa sketch by id (rendered inline by WhatNewCard). Unknown ids
// fall through to the "coming soon" placeholder. `statsAnnotation` is a typed slot but rendered as
// a placeholder until STATS_ANNOTATIONS_PLAN.md lands content.
import { computed, ref, type ComputedRef } from 'vue'
import { marked } from 'marked'
import { useAppControlStore } from '../stores/appControl'

export type WhatNewKind = 'update' | 'tip' | 'fix' | 'about'

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
  sketchAnimation?: { id: string }   // feijoa sketch id; WhatNewCard resolves via `sketches[id]`
  statsAnnotation?: unknown           // slot for STATS_ANNOTATIONS_PLAN
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
// User-visible tip cycler — when null (the default) the dialog shows today's tip; setting an
// index via the dots pagination lets the user browse the rest of the catalogue in-session. Cleared
// on close so re-opening always lands back on today's tip.
export const viewedTipIndex = ref<number | null>(null)

export function openWhatsNew(opts?: { withTip?: boolean }) {
  openWithTip.value = !!opts?.withTip
  isWhatsNewOpen.value = true
}
export function closeWhatsNew() {
  isWhatsNewOpen.value = false
  openWithTip.value = false
  viewedTipIndex.value = null
}

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
