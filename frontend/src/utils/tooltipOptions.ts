// The `v-tooltip` binding's pure half: what a binding value + modifiers mean. `directives/tooltip.ts`
// owns the DOM, `utils/anchorPosition.ts` the placement. Replaces PrimeVue's tooltip
// (docs/todo/PRIMEVUE_RETIRE_PLAN.md, P2) behind the same call-site API, so none of the ~1,200
// `v-tooltip` uses changed:
//
//   v-tooltip.bottom="'Reload'"                                  string form, the common case
//   v-tooltip.left="{ value: html, escape: false, class: 'qc-tip' }"   object form
//
// A falsy or blank value means no tooltip, as in PrimeVue — `v-tooltip="cond ? 'x' : ''"` is the
// idiom for a conditional tip.

import type { Side } from './anchorPosition'

/** The object form. `escape: false` renders `value` as HTML — the caller escapes every interpolation. */
export interface TooltipObject {
  value?: string | null
  escape?: boolean
  class?: string
  disabled?: boolean
}

export type TooltipBinding = string | TooltipObject | null | undefined | false

export interface TooltipSpec {
  text: string
  escape: boolean
  cls: string
  side: Side
  /** Where to go when `side` doesn't fit, in order. */
  fallbacks: Side[]
}

// PrimeVue's own fallback order (its `align()`), kept so a tip lands where it always did when the
// preferred side fits, and flips the same way when it doesn't. No modifier means `right`.
const FALLBACKS: Record<Side, Side[]> = {
  top:    ['bottom'],
  bottom: ['top'],
  left:   ['right', 'top', 'bottom'],
  right:  ['left', 'top', 'bottom'],
}

export function sideOf(modifiers: Partial<Record<string, boolean>>): Side {
  if (modifiers.top) return 'top'
  if (modifiers.left) return 'left'
  if (modifiers.bottom) return 'bottom'
  return 'right'
}

/** The tooltip a binding asks for, or `null` for none. */
export function tooltipSpec(value: TooltipBinding, modifiers: Partial<Record<string, boolean>> = {}): TooltipSpec | null {
  if (!value) return null
  const obj: TooltipObject = typeof value === 'string' ? { value } : value
  const text = typeof obj.value === 'string' ? obj.value : ''
  if (!text.trim() || obj.disabled) return null
  const side = sideOf(modifiers)
  return { text, escape: obj.escape !== false, cls: obj.class ?? '', side, fallbacks: FALLBACKS[side] }
}
