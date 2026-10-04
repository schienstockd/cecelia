import { describe, it, expect } from 'vitest'
import { raisePanel, dropPanel, panelZ, PANEL_Z_BASE, PANEL_Z_LIFTED } from './panelStack'

describe('panelStack', () => {
  it('puts a newly raised panel on top', () => {
    const s = raisePanel(raisePanel([], 'viewer'), 'lablog')
    expect(panelZ(s, 'lablog')).toBeGreaterThan(panelZ(s, 'viewer'))
  })

  // The actual reported bug: Lab log is declared after Viewer in App.vue, so with a flat z-index it
  // always won. Raising Viewer must flip that.
  it('re-raising the lower panel flips the order', () => {
    let s = raisePanel(raisePanel([], 'viewer'), 'lablog')
    s = raisePanel(s, 'viewer')
    expect(panelZ(s, 'viewer')).toBeGreaterThan(panelZ(s, 'lablog'))
  })

  it('does not duplicate a key when raised repeatedly', () => {
    let s = raisePanel(raisePanel([], 'viewer'), 'lablog')
    s = raisePanel(raisePanel(s, 'viewer'), 'viewer')
    expect(s).toEqual(['lablog', 'viewer'])
  })

  it('starts at the base z-index and steps up by one per panel', () => {
    const s = raisePanel(raisePanel([], 'a'), 'b')
    expect(panelZ(s, 'a')).toBe(PANEL_Z_BASE)
    expect(panelZ(s, 'b')).toBe(PANEL_Z_BASE + 1)
  })

  it('gives an unknown key the base z-index', () => {
    expect(panelZ(['a'], 'never-opened')).toBe(PANEL_Z_BASE)
  })

  // Maximised plot canvas is z 200 (ModuleLayout): lifted panels sit above it, keep their order,
  // and stay below the modal layer.
  it('lifts the whole stack above the maximised canvas, order kept', () => {
    const s = raisePanel(raisePanel([], 'viewer'), 'lablog')
    expect(panelZ(s, 'viewer', true)).toBe(PANEL_Z_LIFTED)
    expect(panelZ(s, 'viewer', true)).toBeGreaterThan(200)
    expect(panelZ(s, 'lablog', true)).toBe(PANEL_Z_LIFTED + 1)
    expect(panelZ(s, 'lablog', true)).toBeLessThan(500)
  })

  // Panels must never climb into the modal (500) / popover (1000) layers.
  it('keeps even an implausible number of panels below the modal layer', () => {
    let s: string[] = []
    for (let i = 0; i < 50; i++) s = raisePanel(s, `p${i}`)
    expect(panelZ(s, 'p49')).toBeLessThan(500)
  })

  it('drops a closed panel and closes the gap it left', () => {
    let s = raisePanel(raisePanel(raisePanel([], 'a'), 'b'), 'c')
    s = dropPanel(s, 'b')
    expect(s).toEqual(['a', 'c'])
    expect(panelZ(s, 'c')).toBe(PANEL_Z_BASE + 1)
  })

  it('ignores dropping a key that was never raised', () => {
    expect(dropPanel(['a'], 'nope')).toEqual(['a'])
  })
})
