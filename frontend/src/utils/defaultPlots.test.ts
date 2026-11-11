import { describe, it, expect } from 'vitest'
import { defaultSpecIds, shouldSeedDefaults } from './defaultPlots'

describe('default plots', () => {
  const specs = [
    { id: 'segmentation_qc', defaultOn: ['segment'] },
    { id: 'population_summary', defaultOn: ['phenotype'] },
    { id: 'track_measures' },
  ]
  it('picks the specs that name the page, in registry order', () => {
    expect(defaultSpecIds(specs, 'segment')).toEqual(['segmentation_qc'])
    expect(defaultSpecIds(specs, 'behaviourAnalysis')).toEqual([])
    expect(defaultSpecIds(specs, undefined)).toEqual([])   // the board: no page, no defaults
  })
  it('seeds an empty, never-seeded canvas only', () => {
    expect(shouldSeedDefaults({ panelCount: 0, seeded: false, defaults: 1 })).toBe(true)
    expect(shouldSeedDefaults({ panelCount: 0, seeded: true, defaults: 1 })).toBe(false)   // user removed them
    expect(shouldSeedDefaults({ panelCount: 2, seeded: false, defaults: 1 })).toBe(false)  // a canvas already in use
    expect(shouldSeedDefaults({ panelCount: 0, seeded: false, defaults: 0 })).toBe(false)
  })
})
