// RESULT-REVIEW: result-d5d6b73c
// Every slot recorded Empty (the reader keeps Empty slots) is a recorded fact: nothing in the path.
// No slot found (`[]`) or a reader that does not read dichroics (no key, e.g. .czi) is "not recorded".
import { describe, it, expect } from 'vitest'
import { lightPathStages, type LightPathImage } from './acquisitionLightPath'

const filters = (img: LightPathImage) => lightPathStages(img).find(s => s.key === 'filters')!.items
const ch = { order: 1, name: 'CH1', dyePreset: null, laser: null, detector: null, hv: null, gain: null, offset: null, emissionFilter: 'BA410-455' }

describe('dichroics: recorded empty vs not read', () => {
  it('an .oir with every slot Empty records no dichroic, not a missing one', () => {
    const empty = [{ slot: 'dmSlider', name: 'Empty' }, { slot: 'firstMirrorCube', name: 'Empty' }]
    expect(filters({ acquisition: { source: 'oir', dichroics: empty, channels: [ch] } })[0])
      .toEqual({ text: 'No dichroic in the path', source: 'file' })
  })
  it('no slot found in the file is not recorded, not "no dichroic"', () => {
    expect(filters({ acquisition: { source: 'oir', dichroics: [], channels: [ch] } })[0])
      .toEqual({ text: 'Dichroics not recorded', source: 'missing' })
  })
  it('a reader that does not read dichroics says not recorded', () => {
    expect(filters({ acquisition: { source: 'czi', channels: [{ ...ch, emissionFilter: null }] } })[0])
      .toEqual({ text: 'Dichroics not recorded', source: 'missing' })
  })
})
