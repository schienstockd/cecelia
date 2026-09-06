import { describe, it, expect } from 'vitest'
import { taskHistoryEntries, type HistoryImage } from './taskHistoryRows'

const img = (uid: string, name: string, runLog: HistoryImage['runLog']): HistoryImage =>
  ({ uid, name, runLog })

const CTX = { projectUid: 'P1' }

describe('taskHistoryEntries', () => {
  it('turns a closed run into a terminal row with a real elapsed', () => {
    const [r] = taskHistoryEntries([img('i1', 'Image one', [
      { fun: 'segment.cellpose', at: '2026-08-20T10:00:00', finishedAt: '2026-08-20T10:02:30',
        status: 'done', taskId: 'T7', params: { diameter: 30 } },
    ])], CTX)
    expect(r.status).toBe('done')
    expect(r.id).toBe('T7')                       // the run's own id — how a live row dedups it away
    expect(r.imageUid).toBe('i1')
    expect(r.imageName).toBe('Image one')
    expect(r.funName).toBe('segment.cellpose')
    expect(r.projectUid).toBe('P1')
    expect(r.params).toEqual({ diameter: 30 })
    expect(r.history).toBe(true)
    expect(r.adopted).toBe(true)                  // clicking it fetches the run's log from disk
    expect(r.seq).toBe(0)                         // the `#N` counter is this session's, not history's
    expect(r.finishedAt!.getTime() - r.startedAt!.getTime()).toBe(150_000)
  })

  it('leaves finishedAt undefined on a one-shot entry rather than inventing one', () => {
    // ~88% of the entries in a real project are these (`append_run_log!`): no finishedAt, no taskId.
    // A blank Time is the honest reading; a zero would claim the run was instant.
    const [r] = taskHistoryEntries([img('i1', 'A', [{ fun: 'track.bayesian', at: '2026-08-20T10:00:00' }])], CTX)
    expect(r.finishedAt).toBeUndefined()
    expect(r.startedAt).toBeInstanceOf(Date)
    expect(r.status).toBe('done')                 // a missing status is a legacy entry = success
    expect(r.id).toMatch(/^history::i1::0::track\.bayesian$/)
  })

  it('maps every run-log status onto the five-state rail', () => {
    const statuses = ['done', 'failed', 'cancelled', 'interrupted', 'running', 'nonsense']
    const rows = taskHistoryEntries([img('i1', 'A',
      statuses.map((s, i) => ({ fun: 'segment.cellpose', at: `2026-08-20T10:0${i}:00`, status: s })))], CTX)
    // newest first, so reverse to line back up with `statuses`
    expect(rows.map(r => r.status).reverse())
      // `interrupted` (the run's process died) reads as failed: the output is missing and it is news.
      .toEqual(['done', 'failed', 'cancelled', 'failed', 'running', 'done'])
  })

  it('drops a run that already has a live row, and keeps the ones that do not', () => {
    const rows = taskHistoryEntries([img('i1', 'A', [
      { fun: 'segment.cellpose', at: '2026-08-20T10:00:00', taskId: 'LIVE' },
      { fun: 'track.bayesian',   at: '2026-08-20T09:00:00', taskId: 'OLD' },
    ])], { ...CTX, hasId: id => id === 'LIVE' })
    expect(rows.map(r => r.id)).toEqual(['OLD'])
  })

  it('skips an entry with no fun — there would be no module, label or log to show', () => {
    expect(taskHistoryEntries([img('i1', 'A', [{ fun: '', at: '2026-08-20T10:00:00' }])], CTX)).toEqual([])
  })

  it('sorts newest first across images, with unstamped rows last', () => {
    const rows = taskHistoryEntries([
      img('i1', 'A', [{ fun: 'a.one', at: '2026-08-19T10:00:00' }, { fun: 'a.two', at: 'not a date' }]),
      img('i2', 'B', [{ fun: 'b.one', at: '2026-08-21T10:00:00' }]),
    ], CTX)
    expect(rows.map(r => r.funName)).toEqual(['b.one', 'a.one', 'a.two'])
  })

  it('labels a row through the task defs, falling back to the fun name', () => {
    const [withDefs] = taskHistoryEntries([img('i1', 'A', [{ fun: 'segment.cellpose', at: '2026-08-20T10:00:00' }])],
      { ...CTX, labelFor: f => (f === 'segment.cellpose' ? 'Cellpose' : '') })
    expect(withDefs.label).toBe('Cellpose')
    const [noDefs] = taskHistoryEntries([img('i1', 'A', [{ fun: 'segment.cellpose', at: '2026-08-20T10:00:00' }])], CTX)
    expect(noDefs.label).toBe('cellpose')
  })

  it('is empty for an image with no run log at all', () => {
    expect(taskHistoryEntries([img('i1', 'A', undefined), img('i2', 'B', [])], CTX)).toEqual([])
  })

  // Log files are cumulative on disk (one per image+fun, appended per run), so a history row for an
  // older run has to know when the NEXT run started — the server slices half-open on the pair. The
  // rule is per (image, fun); a different fun on the same image is not a bound.
  describe('logSliceUntil', () => {
    it('is undefined for the newest run of its (image, fun) — nothing came after it', () => {
      const [r] = taskHistoryEntries([img('i1', 'A', [
        { fun: 'segment.cellpose', at: '2026-08-20T10:00:00' },
      ])], CTX)
      expect(r.logSliceUntil).toBeUndefined()
    })

    it("bounds an older row at the next same-fun run's start", () => {
      const rows = taskHistoryEntries([img('i1', 'A', [
        { fun: 'segment.cellpose', at: '2026-08-20T10:00:00' },     // old
        { fun: 'segment.cellpose', at: '2026-08-21T10:00:00' },     // new
      ])], CTX)
      // newest first
      expect(rows[0].logSliceUntil).toBeUndefined()
      expect(rows[1].logSliceUntil?.toISOString()).toBe(new Date('2026-08-21T10:00:00').toISOString())
    })

    it('scopes the bound to the same fun — a different task is not a boundary', () => {
      const rows = taskHistoryEntries([img('i1', 'A', [
        { fun: 'segment.cellpose', at: '2026-08-20T10:00:00' },
        { fun: 'track.bayesian',   at: '2026-08-21T10:00:00' },     // different fun, ignored
        { fun: 'segment.cellpose', at: '2026-08-22T10:00:00' },
      ])], CTX)
      const cell = rows.find(r => r.funName === 'segment.cellpose' && r.startedAt?.toISOString() === new Date('2026-08-20T10:00:00').toISOString())!
      expect(cell.logSliceUntil?.toISOString()).toBe(new Date('2026-08-22T10:00:00').toISOString())
    })

    it("still sees a next run that is skipped as a live-row dedup — a live run's start is the bound", () => {
      const rows = taskHistoryEntries([img('i1', 'A', [
        { fun: 'segment.cellpose', at: '2026-08-20T10:00:00', taskId: 'OLD' },
        { fun: 'segment.cellpose', at: '2026-08-21T10:00:00', taskId: 'LIVE' },
      ])], { ...CTX, hasId: id => id === 'LIVE' })
      // Only OLD is emitted; its bound is still the LIVE run's start
      expect(rows.map(r => r.id)).toEqual(['OLD'])
      expect(rows[0].logSliceUntil?.toISOString()).toBe(new Date('2026-08-21T10:00:00').toISOString())
    })

    it('is scoped to the image — the same fun on another image is not a bound', () => {
      const rows = taskHistoryEntries([
        img('i1', 'A', [{ fun: 'segment.cellpose', at: '2026-08-20T10:00:00' }]),
        img('i2', 'B', [{ fun: 'segment.cellpose', at: '2026-08-21T10:00:00' }]),
      ], CTX)
      // both are "newest of their image+fun" → no bound on either
      for (const r of rows) expect(r.logSliceUntil).toBeUndefined()
    })
  })
})
