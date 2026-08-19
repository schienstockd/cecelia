import { describe, it, expect } from 'vitest'
import { trackOpsKey } from './trackOpsQueue'

// The whole point of the store is its KEY: the queue is an un-run task draft, so it must be scoped to
// what the ops edit — one (image, segmentation) — and not to the canvas that authored it. It was in the
// timeline panel's state, and the Track canvas keys itself on the page-level segmentation select, so
// changing that select took the pending edits out of view.
describe('trackOpsKey', () => {
  it('scopes a queue to the (image, segmentation) its ops edit', () => {
    expect(trackOpsKey('P', 'img', 'memTom')).toBe('P|img|memTom')
  })

  it('separates the two things that used to collide', () => {
    // two segmentations of one image are two different track sets — a track id means something
    // different in each, so their queues must never merge
    expect(trackOpsKey('P', 'img', 'memTom')).not.toBe(trackOpsKey('P', 'img', 'importTest2'))
    // ...and the same segmentation reached from two panels is ONE queue: one run, one journal entry
    expect(trackOpsKey('P', 'img', 'memTom')).toBe(trackOpsKey('P', 'img', 'memTom'))
  })

  it('is empty until the scope is known — never a partial key', () => {
    expect(trackOpsKey('', 'img', 'memTom')).toBe('')
    expect(trackOpsKey('P', '', 'memTom')).toBe('')
    expect(trackOpsKey('P', 'img', '')).toBe('')
  })
})
