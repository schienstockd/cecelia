// The Share-with-Claude entrypoints, unified.
//
// Two surfaces initiate a share: the pop-out viewer's WebGPU frame, and (planned) the current
// module page's canvas of plots. Both land in the same `POST /api/viewer/capture` envelope; they
// differ only in what the user is pointing at. Kiwi is the front door for both, so both live
// behind this store rather than each button holding its own logic.
//
// - VIEWER share is a same-origin popup handshake: the popup exposes `__cceceliaViewerBeginDraw`
//   on `window`, we reach in via `getOpenPopoutWindow('/viewer-window')`. Availability = a live
//   handle exists. Was inlined in `ViewerPanel.openShare()`; that copy now delegates here so the
//   Kiwi button and the (transitional) ViewerPanel button run the same code.
//
// - CANVAS share targets whatever module page is currently mounted. The page registers itself
//   here on mount (`registerCanvasHost({ beginShare, label })`) and unregisters on unmount. Kiwi
//   reads `canvasHost.value` for the button's enabled state + tooltip. Store-based rather than
//   window-property-based because both surfaces live in the same window — no cross-window shim
//   needed. Behaviour page's SummaryCanvas is the first registrar; the actual selection UX comes
//   in a later slice, so `beginShare` today can be a no-op for pages that haven't wired one.
//
// The store owns AVAILABILITY POLLING for the viewer. A pop-out closing does not fire a `close`
// event this window sees, so a mounted Kiwi has to re-check periodically to keep the button's
// disabled state honest. Cheap: `getOpenPopoutWindow` is a Map lookup + `.closed` read.

import { defineStore, acceptHMRUpdate } from 'pinia'
import { computed, ref, onScopeDispose } from 'vue'
import { getOpenPopoutWindow } from '../lib/popout'

/** Contract a canvas host registers when it can be shared. */
export interface CanvasShareHost {
  /** Called by Kiwi's canvas Share button — the host enters its selection mode. */
  beginShare: () => void
  /** Short human label for the tooltip — e.g. `"behaviour · plot canvas"`. */
  label: string
}

/** Failure shape returned by the two `begin*` methods when the target isn't ready. `null` means
 *  success (the target entered share mode). Kept small: Kiwi renders it as a transient inline
 *  note the same way ViewerPanel does today. */
export type BeginShareResult =
  | null
  | { severity: 'warn' | 'fail'; short: string; detail: string }

export const useShareTargetStore = defineStore('shareTarget', () => {
  // ── Viewer availability ───────────────────────────────────────────────────
  // Polled: no `close`-event contract exists across the two windows. 750 ms is well under a
  // human's "why is this still disabled?" threshold and cheap (a Map lookup). Timer is declared
  // in the DECLARED_TIMERS inventory (`utils/continuousControls.test.ts`) same as Kiwi's own
  // relative-time nudge.
  const viewerHandle = ref<Window | null>(getOpenPopoutWindow('/viewer-window'))
  const viewerAvailable = computed(() => viewerHandle.value !== null)
  let pollTimer: ReturnType<typeof setInterval> | null = null
  function startViewerPoll() {
    if (pollTimer) return
    pollTimer = setInterval(() => {
      const w = getOpenPopoutWindow('/viewer-window')
      if (w !== viewerHandle.value) viewerHandle.value = w
    }, 750)
  }
  function stopViewerPoll() {
    if (pollTimer) { clearInterval(pollTimer); pollTimer = null }
  }

  // ── Canvas host registration ──────────────────────────────────────────────
  // Only one canvas is share-eligible at a time (a module page's `#plots` slot). A remount
  // replaces the previous entry; the unregister path is what the mounted component calls on
  // teardown. `useCanvasShareHost` below wraps both in one lifecycle-safe helper.
  const canvasHost = ref<CanvasShareHost | null>(null)
  function registerCanvasHost(host: CanvasShareHost) { canvasHost.value = host }
  function unregisterCanvasHost(host: CanvasShareHost) {
    if (canvasHost.value === host) canvasHost.value = null
  }

  // ── Begin share ───────────────────────────────────────────────────────────
  function beginViewerShare(): BeginShareResult {
    const vw = getOpenPopoutWindow('/viewer-window')
    if (!vw) return { severity: 'warn', short: 'Viewer not open',
      detail: 'Open the pop-out viewer first (click the ↗ on an image).' }
    const begin = (vw as unknown as { __cceceliaViewerBeginDraw?: () => void })
      ?.__cceceliaViewerBeginDraw
    if (typeof begin !== 'function') return { severity: 'fail', short: 'Viewer not ready',
      detail: 'The viewer popup did not expose its draw hook.' }
    begin()
    // Focus so the user sees the drawing toolbar without alt-tab.
    try { vw.focus() } catch { /* nicety */ }
    return null
  }

  function beginCanvasShare(): BeginShareResult {
    const host = canvasHost.value
    if (!host) return { severity: 'warn', short: 'No shareable canvas',
      detail: 'The current page does not have a shareable plot canvas yet.' }
    host.beginShare()
    return null
  }

  return {
    viewerAvailable, canvasHost,
    startViewerPoll, stopViewerPoll,
    registerCanvasHost, unregisterCanvasHost,
    beginViewerShare, beginCanvasShare,
  }
})

/** Register a canvas as the share target for as long as the calling component is mounted.
 *  Matches the `onScopeDispose` idiom used elsewhere (see `useCanvasZoom`) — the caller doesn't
 *  have to remember to unregister. */
export function useCanvasShareHost(host: CanvasShareHost): void {
  const store = useShareTargetStore()
  store.registerCanvasHost(host)
  onScopeDispose(() => store.unregisterCanvasHost(host))
}

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useShareTargetStore, import.meta.hot))
