import { describe, it, expect } from 'vitest'
import { nextUpdateStamp } from './updateStamp'

describe('nextUpdateStamp', () => {
  it('is strictly increasing within a load, even when the clock stands still', () => {
    const a = nextUpdateStamp(0, 1000)
    const b = nextUpdateStamp(a, 1000)
    const c = nextUpdateStamp(b, 999)          // clock stepped back
    expect([a, b, c]).toEqual([1000, 1001, 1002])
  })

  it('does not repeat the previous load\'s stamps after a reload', () => {
    // the last session stamped up to `t0 + 30`; a reload restarts from 0 but the clock has moved on
    const t0 = 1_800_000_000_000
    let last = 0
    const before: number[] = []
    for (let i = 0; i < 30; i++) before.push(last = nextUpdateStamp(last, t0))
    const after = nextUpdateStamp(0, t0 + 5_000)
    expect(before).not.toContain(after)
    expect(after).toBeGreaterThan(Math.max(...before))
  })
})
