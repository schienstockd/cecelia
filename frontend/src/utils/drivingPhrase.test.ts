import { describe, it, expect } from 'vitest'
import { pickDrivingPhrase, PLAIN_PHRASE, VARIANT_PHRASES } from './drivingPhrase'

describe('pickDrivingPhrase', () => {
  it('shows the plain phrase for the lower half', () => {
    expect(pickDrivingPhrase(0)).toBe(PLAIN_PHRASE)
    expect(pickDrivingPhrase(0.4999)).toBe(PLAIN_PHRASE)
  })

  it('reaches every variant across the upper half, never out of range', () => {
    const seen = new Set<string>()
    for (let r = 0.5; r < 1; r += 0.001) seen.add(pickDrivingPhrase(r).text)
    expect(seen.size).toBe(VARIANT_PHRASES.length)
    expect(pickDrivingPhrase(0.999999)).toBe(VARIANT_PHRASES[VARIANT_PHRASES.length - 1])
  })
})
