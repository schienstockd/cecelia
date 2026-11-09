import { describe, it, expect } from 'vitest'
import { createApplyHold } from './applyHold'

describe('createApplyHold', () => {
  it('runs nothing until flush, then each key once', () => {
    const ran: string[] = []
    const h = createApplyHold()
    h.hold('viewer', () => ran.push('viewer-1'))
    h.hold('plots', () => ran.push('plots'))
    h.hold('viewer', () => ran.push('viewer-2'))
    expect(ran).toEqual([])
    expect(h.count).toBe(3)          // repeats counted for the chip
    h.flush()
    expect(ran).toEqual(['viewer-2', 'plots'])   // latest run per key, one each
    expect(h.count).toBe(0)
    h.flush()
    expect(ran).toHaveLength(2)      // a second flush has nothing to run
  })
  it('drop forgets a key without running it', () => {
    const ran: string[] = []
    const h = createApplyHold()
    h.hold('viewer', () => ran.push('viewer'))
    h.hold('plots', () => ran.push('plots'))
    h.drop('viewer')
    expect(h.count).toBe(1)
    h.flush()
    expect(ran).toEqual(['plots'])
  })
  it('reports the count on every change', () => {
    const seen: number[] = []
    const h = createApplyHold(n => seen.push(n))
    h.hold('a', () => {}); h.hold('a', () => {}); h.drop('missing'); h.flush()
    expect(seen).toEqual([1, 2, 0])
  })
  it('a run that holds again queues for the NEXT apply', () => {
    const h = createApplyHold()
    let n = 0
    h.hold('a', () => { n++; h.hold('a', () => { n++ }) })
    h.flush()
    expect(n).toBe(1)
    expect(h.count).toBe(1)
  })
})
