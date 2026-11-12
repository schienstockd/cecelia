import { describe, it, expect } from 'vitest'
import { resultLine, resultVerb } from './resultLine'

const seg = (fun: string) => fun.startsWith('segment.')
const img = (...log: { fun: string; status?: string; at: string }[]) => ({ runLog: log })

describe('result line', () => {
  it('is null until something of this page has run', () => {
    expect(resultLine([img({ fun: 'cleanup.af', at: '2026-11-11T09:00:00' })], seg)).toBeNull()
    expect(resultLine([{}], seg)).toBeNull()
  })
  it('takes the newest run and counts the images that share its function and outcome', () => {
    const r = resultLine([
      img({ fun: 'segment.cellpose', at: '2026-11-11T09:00:00' }),
      img({ fun: 'segment.cellpose', at: '2026-11-11T09:05:00' }, { fun: 'cleanup.af', at: '2026-11-11T10:00:00' }),
      img({ fun: 'segment.cellpose', status: 'failed', at: '2026-11-11T08:00:00' }),
    ], seg)
    expect(r).toEqual({ fun: 'segment.cellpose', status: 'done', n: 2, at: '2026-11-11T09:05:00' })
    expect(resultVerb(r!)).toBe('finished on 2 images')
  })
  it('reads each image at its LATEST run here, so a fixed failure counts as done', () => {
    const r = resultLine([img(
      { fun: 'segment.cellpose', status: 'failed', at: '2026-11-11T08:00:00' },
      { fun: 'segment.cellpose', at: '2026-11-11T09:00:00' },
    )], seg)
    expect(r?.status).toBe('done')
  })
  it('passes over cancelled and skipped runs, and says failed and running plainly', () => {
    expect(resultLine([img({ fun: 'segment.cellpose', status: 'cancelled', at: '2026-11-11T09:00:00' })], seg)).toBeNull()
    const f = resultLine([img({ fun: 'segment.cellpose', status: 'interrupted', at: '2026-11-11T09:00:00' })], seg)!
    expect(resultVerb(f)).toBe('failed on 1 image')
    const run = resultLine([img({ fun: 'segment.cellpose', status: 'running', at: '2026-11-11T09:00:00' })], seg)!
    expect(resultVerb(run)).toBe('running on 1 image')
  })
})
