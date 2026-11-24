// RESULT-REVIEW: result-f8b662c9 — an unknown length unit must not be read as µm.
// Also covers result-6ad40ed5 — a per-axis Z unit overrides the shared unit.
import { describe, it, expect } from 'vitest'
import { tissueExtent, extentShort } from './tissueExtent'

describe('tissueExtent units', () => {
  it('a unit outside the backend table (cm, angstrom) counts as uncalibrated', () => {
    for (const u of ['centimeter', 'angstrom']) {
      const e = tissueExtent({ sizeX: 1000, sizeY: 1000 }, { physicalSizeX: 1, physicalSizeY: 1, physicalSizeUnit: u })
      expect(e.um.x).toBeNull()
      expect(e.areaUm2).toBeNull()
      expect(extentShort(e)).toBe('1000 × 1000 px')
    }
  })
  it('PhysicalSizeZUnit overrides the shared unit for Z', () => {
    // XY 500 nm/px, Z 2 µm/step × 10 planes → Z = 20 µm, not 0.02 µm
    const e = tissueExtent({ sizeX: 100, sizeY: 100, sizeZ: 10 },
      { physicalSizeX: 500, physicalSizeY: 500, physicalSizeZ: 2, physicalSizeUnit: 'nm',
        extraMeta: { PhysicalSizeZUnit: 'micrometer' } })
    expect(e.um.x).toBeCloseTo(50)
    expect(e.um.z).toBeCloseTo(20)
  })
})
