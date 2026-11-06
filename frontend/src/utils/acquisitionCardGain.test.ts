// RESULT-REVIEW: result-13f7982f
// Under Auto HV the card merged every detector into one HV cell and printed gain/offset only when all
// channels agreed — differing offsets vanished from the printed card with nothing saying so.
import { describe, it, expect } from 'vitest'
import { acquisitionCard, type CardImage } from './acquisitionCard'
import REAL from './acquisitionLightPath.fixture.json'

const base = REAL as unknown as CardImage
const offsets = [0, 3, 5, 8]
const img: CardImage = { ...base, uid: 'x', name: 'x', acquisition: { ...base.acquisition,
  channels: base.acquisition!.channels!.map((c, i) => ({ ...c, offset: offsets[i] })) } }

describe('acquisitionCard — per-channel gain and offset under Auto HV', () => {
  it('prints every channel’s gain and offset, not only when they agree', () => {
    const card = acquisitionCard(img, { projectUid: 'p', printedOn: '2026-11-06' })
    expect(card.detectors.map(d => d.gainOffset.text)).toEqual(offsets.map(o => `gain 1 · offset ${o}`))
  })
})
