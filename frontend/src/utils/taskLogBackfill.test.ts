import { describe, it, expect, vi, afterEach } from 'vitest'
import { logLines, fetchLogBackfill } from './taskLogBackfill'

afterEach(() => vi.unstubAllGlobals())

describe('logLines', () => {
  it('splits into lines and drops the file\'s trailing newline', () => {
    expect(logLines('a\nb\n')).toEqual(['a', 'b'])
    expect(logLines('a\nb')).toEqual(['a', 'b'])
  })

  it('keeps blank lines inside the output', () => {
    // a task's own spacing is part of what it printed
    expect(logLines('a\n\nb\n')).toEqual(['a', '', 'b'])
  })

  it('is empty for empty content', () => {
    expect(logLines('')).toEqual([])
  })
})

describe('fetchLogBackfill', () => {
  const target = {
    projectUid: 'p1', imageUid: 'EaMaVq', funName: 'segment.cellpose',
    startedAt: new Date('2026-08-04T05:00:04.000Z'),
  }
  const ok = (body: unknown) =>
    vi.fn().mockResolvedValue({ ok: true, json: () => Promise.resolve(body) })

  it('asks for that run\'s slice only', async () => {
    const f = ok({ exists: true, content: 'line one\nline two\n' })
    vi.stubGlobal('fetch', f)
    expect(await fetchLogBackfill(target)).toEqual(['line one', 'line two'])
    const url = String(f.mock.calls[0][0])
    expect(url).toContain('/api/images/tasklog?')
    expect(url).toContain('fun=segment.cellpose')
    expect(url).toContain('imageUid=EaMaVq')
    // without `since` the cumulative file would come back with every previous run in it
    expect(url).toContain('since=2026-08-04T05%3A00%3A04.000Z')
  })

  it('does not ask at all when the start is unknown', async () => {
    // a queued task, or a backend too old to report one: fetching would show a PREVIOUS run's output as
    // this row's, which is worse than showing none
    const f = ok({ exists: true, content: 'stale\n' })
    vi.stubGlobal('fetch', f)
    expect(await fetchLogBackfill({ ...target, startedAt: undefined })).toEqual([])
    expect(f).not.toHaveBeenCalled()
  })

  it('is empty when the log file does not exist', async () => {
    vi.stubGlobal('fetch', ok({ exists: false, content: '' }))
    expect(await fetchLogBackfill(target)).toEqual([])
  })

  // Fails CLOSED: this fills a display gap from a click handler, so nothing here may throw.
  it('is empty on a non-ok response, a throw, or a junk payload', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({ ok: false, json: () => Promise.resolve({}) }))
    expect(await fetchLogBackfill(target)).toEqual([])
    vi.stubGlobal('fetch', vi.fn().mockRejectedValue(new Error('offline')))
    expect(await fetchLogBackfill(target)).toEqual([])
    vi.stubGlobal('fetch', ok({ exists: true }))
    expect(await fetchLogBackfill(target)).toEqual([])
  })

  it('does not ask without a project, image or fun', async () => {
    const f = ok({ exists: true, content: 'x' })
    vi.stubGlobal('fetch', f)
    expect(await fetchLogBackfill({ ...target, projectUid: '' })).toEqual([])
    expect(await fetchLogBackfill({ ...target, imageUid: '' })).toEqual([])
    expect(await fetchLogBackfill({ ...target, funName: '' })).toEqual([])
    expect(f).not.toHaveBeenCalled()
  })
})
