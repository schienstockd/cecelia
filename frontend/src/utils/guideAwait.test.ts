import { describe, it, expect } from 'vitest'
import { awaitCandidates, awaitedRun, highestSeq } from './guideAwait'

const task = (seq: number, status: string, funName = 'segment.cellposeMeasure') =>
  ({ id: `t${seq}`, seq, funName, module: 'segment', status })

describe('highestSeq', () => {
  it('is the mark a guide starts from, and 0 for an empty list', () => {
    expect(highestSeq([])).toBe(0)
    expect(highestSeq([task(3, 'done'), task(7, 'done'), task(5, 'done')])).toBe(7)
  })
})

describe('awaitCandidates', () => {
  const spec = { fun: 'segment.cellposeMeasure' }

  // THE bug. Run segmentation, then open the guide to learn it properly and press Next past the Run
  // step (which plan D2 always allows): the step used to adopt the earlier run and act on it — jump
  // ahead on a `done`, or show "That run failed." on a `failed`.
  it('ignores a run that had already finished before the guide started', () => {
    const before = [task(1, 'done'), task(2, 'failed'), task(3, 'cancelled')]
    expect(awaitCandidates(before, spec, highestSeq(before))).toEqual([])
    expect(awaitedRun(before, spec, highestSeq(before))).toBeNull()
  })

  it('takes a run started during the guide, even one that finished fast', () => {
    const mark = highestSeq([task(1, 'done')])
    const now = [task(1, 'done'), task(2, 'done')]      // #2 clicked from the Run step
    expect(awaitedRun(now, spec, mark)?.seq).toBe(2)
  })

  it('takes a run still in flight from before the guide — Run first, guide after', () => {
    const before = [task(1, 'running')]
    expect(awaitedRun(before, spec, highestSeq(before))?.seq).toBe(1)
  })

  it('parks on a queued run, not just a running one', () => {
    const before = [task(1, 'queued')]
    expect(awaitedRun(before, spec, highestSeq(before))?.seq).toBe(1)
  })

  it('prefers the newest qualifying run', () => {
    const now = [task(2, 'done'), task(4, 'running'), task(3, 'done')]
    expect(awaitedRun(now, spec, 1)?.seq).toBe(4)
  })

  it('matches on fun, on module, and on neither', () => {
    const rows = [task(2, 'running', 'segment.coastalMeasure'), task(3, 'running')]
    expect(awaitCandidates(rows, spec, 1).map(t => t.seq)).toEqual([3])
    expect(awaitCandidates(rows, { module: 'segment' }, 1).map(t => t.seq)).toEqual([2, 3])
    expect(awaitCandidates(rows, {}, 1).map(t => t.seq)).toEqual([2, 3])
  })

  it('is empty for a step that awaits nothing', () => {
    expect(awaitCandidates([task(9, 'running')], undefined, 0)).toEqual([])
    expect(awaitedRun([task(9, 'running')], undefined, 0)).toBeNull()
  })
})
