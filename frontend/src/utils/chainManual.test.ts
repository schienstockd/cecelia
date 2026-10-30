import { describe, it, expect } from 'vitest'
import { waitingByNode, continueFrame, refusalLines } from './chainManual'

describe('waitingByNode', () => {
  it('groups parked images by node and ignores every other status', () => {
    const m = waitingByNode([
      { chainNodeId: 'gate', imageUid: 'a', status: 'waiting' },
      { chainNodeId: 'gate', imageUid: 'b', status: 'done' },
      { chainNodeId: 'gate', imageUid: 'c', status: 'waiting' },
      { chainNodeId: 'seg',  imageUid: 'a', status: 'running' },
      { chainNodeId: 'gate', imageUid: 'a', status: 'waiting' },   // a duplicate row (live over persisted)
    ])
    expect([...m.keys()]).toEqual(['gate'])
    expect(m.get('gate')).toEqual(['a', 'c'])
  })

  it('is empty when nothing waits', () => {
    expect(waitingByNode([{ chainNodeId: 'n', imageUid: 'a', status: 'done' }]).size).toBe(0)
  })
})

describe('continueFrame', () => {
  it('names one image for a per-image continue', () => {
    expect(continueFrame('P', 'R', 'gate', 'a'))
      .toEqual({ type: 'chain:continue', projectUid: 'P', runId: 'R', nodeId: 'gate', imageUids: ['a'] })
  })
  it('omits imageUids to continue every parked image', () => {
    expect(continueFrame('P', 'R', 'gate')).not.toHaveProperty('imageUids')
  })
})

describe('refusalLines', () => {
  it('names each refused image with its reason', () => {
    expect(refusalLines({ refused: { a: 'no gates on a' } }, u => `img-${u}`))
      .toEqual(['img-a: no gates on a'])
  })
  it('reports a handler error as the only line', () => {
    expect(refusalLines({ error: 'runId required', refused: { a: 'x' } })).toEqual(['runId required'])
  })
  it('is empty when everything went on', () => {
    expect(refusalLines({ refused: {} })).toEqual([])
  })
})
