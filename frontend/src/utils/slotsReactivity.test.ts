import { describe, it, expect } from 'vitest'

// `useSlots()` returns a PLAIN object, not a reactive one. Reading it inside `computed()` therefore
// caches whatever the first render saw — and a slot's presence very often changes after that first
// render, because a child has to mount before the parent knows whether to provide one.
//
// That is not hypothetical. `CanvasPanel` had:
//
//   const hasControls = computed(() => !!slots.actions || !!slots.footer)
//
// and `InteractivePanel` provides its `#footer` only when the hosted view exposes `exportFormats` —
// which is read off a template ref, so it is empty on the first render. Every interactive panel whose
// ONLY chrome is that footer therefore decided "no controls" once, forever, and never showed the pin
// that stops the footer auto-hiding over the body. Dominik reported it twice: first as "these floating
// plots have no pin", then again on the correction panel after a partial fix.
//
// The fix is a plain function called from the template: it re-evaluates on every render, and a render
// is exactly when slots can change. This detector keeps the pattern from coming back.
const RAW = import.meta.glob('/src/**/*.vue', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
const sources = Object.entries(RAW).map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

/** `computed(...)` bodies that read a `slots.` / `$slots.` property. */
export function slotsInComputed(text: string): string[] {
  const out: string[] = []
  // computed( … ) up to the first line that closes it — enough to catch the one-liner form these are
  // always written in, without pretending to parse TypeScript
  for (const m of text.matchAll(/computed\s*(<[^>]*>)?\s*\(([^\n]*)/g)) {
    const body = m[2]
    // `(?<![.\w])` so a property named `slots` on someone else's object (PlateBuilder's
    // `buildPlate(...).slots`) is not mistaken for Vue's
    if (/(?<![.\w])\$?slots\s*[.[]/.test(body)) out.push(m[0].slice(0, 80))
  }
  return out
}

describe('slotsInComputed', () => {
  it('flags the caching shape', () => {
    expect(slotsInComputed('const has = computed(() => !!slots.footer)')).toHaveLength(1)
    expect(slotsInComputed('const has = computed(() => !!$slots.actions || x)')).toHaveLength(1)
  })
  it('leaves a plain function alone — that is the fix', () => {
    expect(slotsInComputed('const has = () => !!slots.footer')).toEqual([])
  })
  it('ignores a computed that has nothing to do with slots', () => {
    expect(slotsInComputed('const n = computed(() => rows.value.length)')).toEqual([])
  })
  it("ignores someone else's property called slots", () => {
    // PlateBuilder computes over `buildPlate(...).slots` — a plate's wells, not Vue's slots
    expect(slotsInComputed('const n = computed(() => buildPlate(a, b).slots.length)')).toEqual([])
  })
})

describe('no component caches a slot check in a computed', () => {
  it('every slots read happens at render time', () => {
    const offenders = sources
      .flatMap(s => slotsInComputed(s.text).map(hit => `${s.path}: ${hit}`))
    expect(offenders).toEqual([])
  })
})
