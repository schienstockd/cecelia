// The toast queue's pure half: what a message becomes, what the stack holds, how much life a paused
// toast has left. `composables/useToast.ts` owns the timers and the one reactive list; `ToastHost.vue`
// renders it. Replaces PrimeVue's ToastService (docs/todo/PRIMEVUE_RETIRE_PLAN.md, P1) behind the
// same `add({ severity, summary, detail, life })` call, so no caller changed.

import { SEVERITY } from '../lib/severity'

/** The traffic-light scale (docs/UI.md → *Toast notifications*). */
export type ToastSeverity = 'info' | 'success' | 'warn' | 'error'

/** Icon + colour per toast severity: the canonical traffic light (lib/severity.ts), plus `info`
 *  (in progress), which the QC scale has no level for. */
export const TOAST_STYLE: Record<ToastSeverity, { icon: string; color: string }> = {
  info:    { icon: 'pi-info-circle', color: 'var(--cc-active)' },
  success: SEVERITY.ok,
  warn:    SEVERITY.warn,
  error:   SEVERITY.fail,
}

/** What a caller passes to `toast.add()` — PrimeVue's ToastMessageOptions, minus what we never used, plus a click action. */
export interface ToastMessage {
  severity?: ToastSeverity
  summary?: string
  detail?: string
  /** ms before it closes itself. Omitted → stays until closed (PrimeVue's semantics). */
  life?: number
  /** Clicking the toast runs this and closes it — e.g. open what was just saved. */
  onClick?: () => void
}

export interface ToastEntry {
  id: number
  severity: ToastSeverity
  summary: string
  detail: string
  life: number | null
  onClick: (() => void) | null
}

/** Older toasts drop off past this, so a burst can't stack up the screen. */
export const TOAST_MAX = 5

export function toEntry(msg: ToastMessage, id: number): ToastEntry {
  return {
    id,
    severity: msg.severity ?? 'info',
    summary: msg.summary ?? '',
    detail: msg.detail ?? '',
    life: msg.life && msg.life > 0 ? msg.life : null,
    onClick: msg.onClick ?? null,
  }
}

/** The stack after `entry` arrives: newest last, oldest dropped beyond `max`. */
export function pushToast(list: readonly ToastEntry[], entry: ToastEntry, max = TOAST_MAX): ToastEntry[] {
  const next = [...list, entry]
  return next.length > max ? next.slice(next.length - max) : next
}

/** The ids `pushToast` dropped — their timers need clearing. */
export function droppedIds(before: readonly ToastEntry[], after: readonly ToastEntry[]): number[] {
  const kept = new Set(after.map(t => t.id))
  return before.filter(t => !kept.has(t.id)).map(t => t.id)
}

/** Life left when a toast is paused (hover) at `now`, for one that would close at `deadline`. */
export function lifeLeft(deadline: number, now: number): number {
  return Math.max(0, deadline - now)
}
