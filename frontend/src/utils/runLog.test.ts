import { describe, it, expect } from 'vitest'
import {
  runStatus, lastRun, lastSuccessfulRun, funsRun, funsRunAcross, wasProcessedWith,
  funCategory, funModuleLabel, type RunLogEntry,
} from './runLog'

const e = (fun: string, at: string, status?: string): RunLogEntry => ({ fun, at, status })

// oldest→newest, like the backend ships it
const log: RunLogEntry[] = [
  e('importImages.omezarr',        '2026-07-10T09:00:00'),
  e('cleanupImages.cellposeCorrect', '2026-07-11T10:00:00'),
  e('segment.cellpose',            '2026-07-12T11:00:00', 'failed'),
]

describe('runStatus', () => {
  it('defaults a missing/empty status to done (legacy entries)', () => {
    expect(runStatus(e('a', 'x'))).toBe('done')
    expect(runStatus(e('a', 'x', ''))).toBe('done')
    expect(runStatus(e('a', 'x', 'failed'))).toBe('failed')
  })
})

describe('lastRun', () => {
  it('returns the newest (last) entry', () => {
    expect(lastRun(log)?.fun).toBe('segment.cellpose')
  })
  it('handles empty/nullish', () => {
    expect(lastRun([])).toBeUndefined()
    expect(lastRun(undefined)).toBeUndefined()
    expect(lastRun(null)).toBeUndefined()
  })
})

describe('lastSuccessfulRun', () => {
  it('skips a failed newest run and returns the last non-failed one', () => {
    // newest entry (segment.cellpose) failed → falls back to cleanupImages.cellposeCorrect
    expect(lastSuccessfulRun(log)?.fun).toBe('cleanupImages.cellposeCorrect')
  })
  it('returns the newest when it succeeded', () => {
    const l2: RunLogEntry[] = [e('importImages.omezarr', '2026-07-10T09:00:00'),
                               e('cleanupImages.cellposeCorrect', '2026-07-11T10:00:00')]
    expect(lastSuccessfulRun(l2)?.fun).toBe('cleanupImages.cellposeCorrect')
  })
  it('undefined when nothing succeeded / empty', () => {
    expect(lastSuccessfulRun([e('segment.cellpose', 'x', 'failed')])).toBeUndefined()
    expect(lastSuccessfulRun([])).toBeUndefined()
    expect(lastSuccessfulRun(undefined)).toBeUndefined()
  })
})

describe('funsRun', () => {
  it('collects funs, dropping failed runs by default', () => {
    expect([...funsRun(log)].sort()).toEqual(['cleanupImages.cellposeCorrect', 'importImages.omezarr'])
  })
  it('includes failed runs when succeededOnly=false', () => {
    expect(funsRun(log, false).has('segment.cellpose')).toBe(true)
  })
  it('is empty for no log', () => {
    expect(funsRun(undefined).size).toBe(0)
  })
})

describe('funsRunAcross', () => {
  it('unions across images and sorts', () => {
    const other: RunLogEntry[] = [e('tracking.bayesian_tracking', '2026-07-13T09:00:00')]
    expect(funsRunAcross([log, other, undefined])).toEqual([
      'cleanupImages.cellposeCorrect', 'importImages.omezarr', 'tracking.bayesian_tracking',
    ])
  })
})

describe('wasProcessedWith', () => {
  it('ever: matches any non-failed run', () => {
    expect(wasProcessedWith(log, 'cleanupImages.cellposeCorrect', 'ever')).toBe(true)
    expect(wasProcessedWith(log, 'importImages.omezarr', 'ever')).toBe(true)
  })
  it('ever: a failed-only run does not count as processed (by default)', () => {
    expect(wasProcessedWith(log, 'segment.cellpose', 'ever')).toBe(false)
    expect(wasProcessedWith(log, 'segment.cellpose', 'ever', false)).toBe(true)
  })
  it('last: only matches when the newest run is that fun', () => {
    // newest run here is segment.cellpose, but it failed → not "processed" by default
    expect(wasProcessedWith(log, 'segment.cellpose', 'last')).toBe(false)
    expect(wasProcessedWith(log, 'segment.cellpose', 'last', false)).toBe(true)
    expect(wasProcessedWith(log, 'cleanupImages.cellposeCorrect', 'last')).toBe(false)
  })
  it('last: matches a successful newest run', () => {
    const l2: RunLogEntry[] = [e('importImages.omezarr', '2026-07-10T09:00:00'),
                               e('cleanupImages.cellposeCorrect', '2026-07-11T10:00:00')]
    expect(wasProcessedWith(l2, 'cleanupImages.cellposeCorrect', 'last')).toBe(true)
    expect(wasProcessedWith(l2, 'importImages.omezarr', 'last')).toBe(false)
  })
  it('empty fun or empty log never matches', () => {
    expect(wasProcessedWith(log, '', 'ever')).toBe(false)
    expect(wasProcessedWith(undefined, 'segment.cellpose', 'ever')).toBe(false)
  })
})

describe('funCategory / funModuleLabel', () => {
  it('splits the category off the fun_name', () => {
    expect(funCategory('cleanupImages.cellposeCorrect')).toBe('cleanupImages')
    expect(funCategory('segment.cellpose')).toBe('segment')
    expect(funCategory('')).toBe('')
  })
  it('prettifies the module label', () => {
    expect(funModuleLabel('cleanupImages.cellposeCorrect')).toBe('Cleanup')
    expect(funModuleLabel('importImages.omezarr')).toBe('Import')
    expect(funModuleLabel('segment.cellpose')).toBe('Segment')
    expect(funModuleLabel('clustPops.cluster')).toBe('Clust Pops')
    expect(funModuleLabel('spatialAnalysis.cellNeighbours')).toBe('Spatial Analysis')
  })
})
