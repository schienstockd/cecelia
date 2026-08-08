import { describe, it, expect } from 'vitest'
import { defaultVis, DEFAULT_VIS } from './plot'

// A panel with no vis of its own falls back to DEFAULT_VIS. The value is the easy half; the IDENTITY is
// the half that broke — `?? defaultVis()` in a template minted a new bag per render, so every parent
// render handed each panel a "new" vis, rebuilt its chart, and fed the resulting readout back to the
// board, which rendered again. Slots lack a vis exactly when something other than the GUI wrote them
// (add_analysis_board omits the bag on purpose), so it was Claude-authored boards that looped.
describe('DEFAULT_VIS', () => {
  it('is the same object every time — the fallback must not churn prop identity', () => {
    expect(DEFAULT_VIS).toBe(DEFAULT_VIS)
    expect(defaultVis()).not.toBe(defaultVis())     // the factory still mints, for write sites
  })

  it('carries the factory values', () => {
    expect(DEFAULT_VIS).toEqual(defaultVis())
  })

  it('is frozen — it is shared, so nothing may write through it', () => {
    expect(Object.isFrozen(DEFAULT_VIS)).toBe(true)
  })
})
