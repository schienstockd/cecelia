// RESULT-REVIEW: result-cb36b24a
// A manual gate step drawing TRACK gates (`popType: 'track'`) makes no cell population, so tracking's
// `popsToTrack` (cell gates) must not offer its `requiredPops` as "after gating".
import { describe, it, expect } from 'vitest'
import { prospectivePops, singlePopOptions } from './chainProspective'

describe('prospectivePops and track gate steps', () => {
  const nodes = [{ fn: 'gate.manual_gate', params: { valueName: 'Tcells', popType: 'track', requiredPops: '/fast' } }]
  it('offers no cell population for a track gate step', () => {
    const extra = prospectivePops(nodes, () => undefined)
    expect(extra).toEqual([])
    expect(singlePopOptions([{ label: 'NONE', value: 'NONE' }], extra, 'Tcells', null).map(o => o.value))
      .toEqual(['NONE'])
  })
})
