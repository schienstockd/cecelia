import { describe, it, expect } from 'vitest'
import { tooltipSpec, sideOf } from './tooltipOptions'

describe('tooltipSpec', () => {
  it('a string is the common case: escaped, no class', () => {
    expect(tooltipSpec('Reload', { bottom: true }))
      .toEqual({ text: 'Reload', escape: true, cls: '', side: 'bottom', fallbacks: ['top'] })
  })
  it('falsy or blank means no tooltip', () => {
    for (const v of ['', '   ', null, undefined, false] as const) expect(tooltipSpec(v)).toBeNull()
    expect(tooltipSpec({ value: '' })).toBeNull()
    expect(tooltipSpec({ value: null })).toBeNull()
    expect(tooltipSpec({})).toBeNull()
  })
  it('the object form carries escape and class', () => {
    const s = tooltipSpec({ value: '<b>x</b>', escape: false, class: 'qc-tip' }, { left: true })!
    expect(s.escape).toBe(false)
    expect(s.cls).toBe('qc-tip')
    expect(s.side).toBe('left')
  })
  it('the object form escapes unless told not to', () => {
    expect(tooltipSpec({ value: 'x' })!.escape).toBe(true)
  })
  it('disabled means no tooltip', () => {
    expect(tooltipSpec({ value: 'x', disabled: true })).toBeNull()
  })
})

describe('sideOf', () => {
  it('no modifier is right, as in PrimeVue', () => {
    expect(sideOf({})).toBe('right')
  })
  it('reads each modifier', () => {
    expect(sideOf({ top: true })).toBe('top')
    expect(sideOf({ left: true })).toBe('left')
    expect(sideOf({ bottom: true })).toBe('bottom')
    expect(sideOf({ right: true })).toBe('right')
  })
})

describe('fallbacks', () => {
  it('follow PrimeVue\'s flip order', () => {
    expect(tooltipSpec('x', { top: true })!.fallbacks).toEqual(['bottom'])
    expect(tooltipSpec('x', { left: true })!.fallbacks).toEqual(['right', 'top', 'bottom'])
    expect(tooltipSpec('x')!.fallbacks).toEqual(['left', 'top', 'bottom'])
  })
})
