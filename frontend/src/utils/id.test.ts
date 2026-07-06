import { describe, it, expect } from 'vitest'
import { shortId, ID_CHARS, ID_LENGTH } from './id'

// shortId is the shared client-side run-id generator (mirrors the backend gen_uid shape). It only
// needs to hold two properties: the right shape, and no collisions at the volume a session produces.
describe('shortId', () => {
  it('is ID_LENGTH chars from the base62 alphabet by default', () => {
    for (let i = 0; i < 100; i++) {
      const id = shortId()
      expect(id).toHaveLength(ID_LENGTH)
      expect(id).toMatch(/^[a-zA-Z0-9]+$/)
      for (const ch of id) expect(ID_CHARS).toContain(ch)
    }
  })

  it('honours a custom length', () => {
    expect(shortId(3)).toHaveLength(3)
    expect(shortId(12)).toHaveLength(12)
  })

  it('is collision-free across a session-scale batch', () => {
    // 2000 draws from 62^6 (~57e9): birthday-bound collision probability ~3e-5, negligible for a
    // per-session correlation handle (task runs never approach this volume in one session).
    const n = 2000
    const seen = new Set<string>()
    for (let i = 0; i < n; i++) seen.add(shortId())
    expect(seen.size).toBe(n)
  })
})
