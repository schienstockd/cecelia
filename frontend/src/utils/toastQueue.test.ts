import { describe, it, expect } from 'vitest'
import { toEntry, pushToast, droppedIds, lifeLeft, TOAST_MAX, TOAST_STYLE } from './toastQueue'
import { SEVERITY } from '../lib/severity'

describe('toEntry', () => {
  it('fills the defaults a caller may leave out', () => {
    expect(toEntry({}, 1)).toEqual({ id: 1, severity: 'info', summary: '', detail: '', life: null, onClick: null })
  })
  it('keeps what the caller gave', () => {
    expect(toEntry({ severity: 'error', summary: 'Move failed', detail: 'x', life: 4000 }, 2))
      .toEqual({ id: 2, severity: 'error', summary: 'Move failed', detail: 'x', life: 4000, onClick: null })
  })
  it('carries the click action', () => {
    const go = () => {}
    expect(toEntry({ onClick: go }, 3).onClick).toBe(go)
  })
  it('no life (or a non-positive one) means sticky, as in PrimeVue', () => {
    expect(toEntry({ life: 0 }, 1).life).toBeNull()
    expect(toEntry({ life: -5 }, 1).life).toBeNull()
  })
})

describe('pushToast', () => {
  const e = (id: number) => toEntry({ summary: String(id) }, id)

  it('appends newest last', () => {
    expect(pushToast([e(1)], e(2)).map(t => t.id)).toEqual([1, 2])
  })
  it('drops the oldest past the cap, and reports which', () => {
    const full = Array.from({ length: TOAST_MAX }, (_, i) => e(i + 1))
    const next = pushToast(full, e(99))
    expect(next).toHaveLength(TOAST_MAX)
    expect(next.at(-1)!.id).toBe(99)
    expect(droppedIds(full, next)).toEqual([1])
  })
  it('does not mutate the list it was given', () => {
    const list = [e(1)]
    pushToast(list, e(2))
    expect(list).toHaveLength(1)
  })
})

describe('lifeLeft', () => {
  it('is the time to the deadline, never negative', () => {
    expect(lifeLeft(5000, 3000)).toBe(2000)
    expect(lifeLeft(5000, 9000)).toBe(0)
  })
})

describe('TOAST_STYLE', () => {
  it('reuses the canonical traffic light for the three QC levels', () => {
    expect(TOAST_STYLE.success.icon).toBe(SEVERITY.ok.icon)
    expect(TOAST_STYLE.warn.color).toBe(SEVERITY.warn.color)
    expect(TOAST_STYLE.error.icon).toBe(SEVERITY.fail.icon)
  })
})
