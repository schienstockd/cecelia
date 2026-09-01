import { describe, it, expect } from 'vitest'
import {
  buildAutoShowPlan, activeValueName, createClaimRegistry, type AutoShowInput,
  liveLabelPreviews, shouldRefreshPreview, PREVIEW_REFRESH_MIN_MS, type TaskListEntry,
} from './overlayAutoShow'

const base: AutoShowInput = {
  labels: {}, branchLabels: {},
  labelVisibility: {}, branchVisibility: {}, trackVisibility: {},
  popTypes: [], showGatedTracks: false, showTrackclust: false,
}

describe('buildAutoShowPlan', () => {
  it('asks for exactly the label sets whose toggle is on', () => {
    const plan = buildAutoShowPlan({
      ...base,
      labels: { A: ['A.zarr'], B: ['B.zarr'], C: ['C.zarr'] },
      labelVisibility: { A: true, B: false, C: true },
    })
    expect(plan.labels).toEqual({ A: ['A.zarr'], C: ['C.zarr'] })
  })

  it('routes branch labels separately from cell labels', () => {
    const plan = buildAutoShowPlan({
      ...base,
      labels: { A: ['A.zarr'] }, labelVisibility: { A: true },
      branchLabels: { A: ['A.zarr'] }, branchVisibility: { A: true },
    })
    expect(plan.labels).toEqual({ A: ['A.zarr'] })
    expect(plan.branchLabels).toEqual({ A: ['A.zarr'] })
  })

  // A toggle can outlive its data (label set deleted, project restored from an export). Asking for a
  // set with no files is a request the bridge can only skip, so it must not be sent.
  it('drops a toggled-on set that has no files registered', () => {
    const plan = buildAutoShowPlan({
      ...base, labels: { A: [] }, labelVisibility: { A: true },
    })
    expect(plan.labels).toEqual({})
  })

  it('collects track ribbons from the per-segmentation toggles', () => {
    const plan = buildAutoShowPlan({ ...base, trackVisibility: { A: true, B: false, C: true } })
    expect(plan.trackValueNames).toEqual(['A', 'C'])
    expect(plan.pushTracks).toBe(true)
  })

  // The master toggles still need a show-tracks request even with no per-segmentation ribbons on.
  it('pushes tracks for a master toggle alone', () => {
    expect(buildAutoShowPlan({ ...base, showGatedTracks: true }).pushTracks).toBe(true)
    expect(buildAutoShowPlan({ ...base, showTrackclust: true }).pushTracks).toBe(true)
  })

  it('skips the tracks request when nothing track-shaped is on', () => {
    const plan = buildAutoShowPlan({ ...base, trackVisibility: { A: false } })
    expect(plan.pushTracks).toBe(false)
    expect(plan.trackValueNames).toEqual([])
  })

  it('is empty for an image with nothing registered', () => {
    const plan = buildAutoShowPlan(base)
    expect(plan).toMatchObject({
      labels: {}, branchLabels: {}, trackValueNames: [], popTypes: [], pushTracks: false,
    })
  })

  it('passes the remembered cell-grained pop types through', () => {
    expect(buildAutoShowPlan({ ...base, popTypes: ['flow', 'region'] }).popTypes)
      .toEqual(['flow', 'region'])
  })

  it('tolerates missing registries/visibility maps', () => {
    const plan = buildAutoShowPlan({} as AutoShowInput)
    expect(plan.labels).toEqual({})
    expect(plan.branchLabels).toEqual({})
    expect(plan.trackValueNames).toEqual([])
    expect(plan.pushTracks).toBe(false)
  })
})

describe('activeValueName', () => {
  it('prefers the registered active version', () => {
    expect(activeValueName({ filepaths: { default: 'a', corrected: 'b' }, activeValueName: 'corrected' }))
      .toBe('corrected')
  })

  // A stale _active (version since removed) must not win — the server would open something else.
  it('ignores an active version that is no longer registered', () => {
    expect(activeValueName({ filepaths: { default: 'a' }, activeValueName: 'gone' })).toBe('default')
  })

  it('falls back to the last non-default version', () => {
    expect(activeValueName({ filepaths: { default: 'a', one: 'b', two: 'c' } })).toBe('two')
  })

  it('falls back to default, then to nothing', () => {
    expect(activeValueName({ filepaths: { default: 'a' } })).toBe('default')
    expect(activeValueName({ filepaths: {} })).toBe('')
    expect(activeValueName(null)).toBe('')
  })
})

describe('createClaimRegistry', () => {
  it('consumes a claim exactly once', () => {
    const r = createClaimRegistry()
    r.claim('img1')
    expect(r.consume('img1')).toBe(true)    // claimed → autoshow stands down
    expect(r.consume('img1')).toBe(false)   // and only for that one open
  })

  it('reports no claim for an unclaimed image', () => {
    const r = createClaimRegistry()
    r.claim('img1')
    expect(r.consume('img2')).toBe(false)
  })

  // The regression this registry exists for: two zoom-to-source clicks in quick succession both have
  // opens in flight. A single-slot suppression would drop the first claim, and that image would get the
  // remembered overlays pushed over its captured frame.
  it('keeps concurrent claims for different images independent', () => {
    const r = createClaimRegistry()
    r.claim('img1')
    r.claim('img2')
    expect(r.size()).toBe(2)
    expect(r.consume('img1')).toBe(true)
    expect(r.consume('img2')).toBe(true)
  })

  it('releases a single claim by uid', () => {
    const r = createClaimRegistry()
    r.claim('img1'); r.claim('img2')
    r.release('img1')
    expect(r.consume('img1')).toBe(false)
    expect(r.consume('img2')).toBe(true)
  })

  it('releases every claim when called with no uid', () => {
    const r = createClaimRegistry()
    r.claim('img1'); r.claim('img2')
    r.release()
    expect(r.size()).toBe(0)
  })

  it('ignores an empty uid and is idempotent per claim', () => {
    const r = createClaimRegistry()
    r.claim(''); expect(r.size()).toBe(0)
    r.claim('img1'); r.claim('img1')
    expect(r.size()).toBe(1)
  })
})

describe('liveLabelPreviews', () => {
  const run = (over: Partial<TaskListEntry> = {}): TaskListEntry => ({
    id: 't1', fun_name: 'segment.cellpose', image_uid: 'imgA', status: 'running',
    live_outputs: [{ kind: 'labels', value_name: 'X', files: ['X.zarr'] }],
    ...over,
  })

  it('offers a running task\'s label store for the open image', () => {
    expect(liveLabelPreviews([run()], 'imgA'))
      .toEqual([{ taskId: 't1', valueName: 'X', files: ['X.zarr'] }])
  })

  it('ignores tasks on other images', () => {
    expect(liveLabelPreviews([run()], 'imgB')).toEqual([])
  })

  // a queued task has created nothing yet — a preview row would resolve to a store not on disk
  it('ignores queued tasks, and every terminal status', () => {
    for (const status of ['queued', 'done', 'failed', 'cancelled']) {
      expect(liveLabelPreviews([run({ status })], 'imgA')).toEqual([])
    }
  })

  // most tasks declare nothing — the whole point of the trait being opt-in
  it('ignores tasks that declare no live outputs', () => {
    expect(liveLabelPreviews([run({ live_outputs: [] })], 'imgA')).toEqual([])
    expect(liveLabelPreviews([run({ live_outputs: undefined })], 'imgA')).toEqual([])
  })

  it('carries every file of a multi-type run (base + nuc)', () => {
    const t = run({ live_outputs: [{ kind: 'labels', value_name: 'X', files: ['X.zarr', 'X_nuc.zarr'] }] })
    expect(liveLabelPreviews([t], 'imgA')[0].files).toEqual(['X.zarr', 'X_nuc.zarr'])
  })

  // only the cell-label family has a preview path; branchLabels is written once, at the end
  it('ignores non-labels kinds and malformed entries', () => {
    const t = run({ live_outputs: [
      { kind: 'branchLabels', value_name: 'X', files: ['X.zarr'] },
      { kind: 'labels', value_name: '', files: ['X.zarr'] },
      { kind: 'labels', value_name: 'Y', files: [] },
    ] })
    expect(liveLabelPreviews([t], 'imgA')).toEqual([])
  })

  it('de-duplicates a value_name declared by two tasks, and sorts by name', () => {
    const a = run({ id: 't1', live_outputs: [{ kind: 'labels', value_name: 'Z', files: ['Z.zarr'] }] })
    const b = run({ id: 't2', live_outputs: [{ kind: 'labels', value_name: 'Z', files: ['Z.zarr'] }] })
    const c = run({ id: 't3', live_outputs: [{ kind: 'labels', value_name: 'A', files: ['A.zarr'] }] })
    const got = liveLabelPreviews([a, b, c], 'imgA')
    expect(got.map(p => p.valueName)).toEqual(['A', 'Z'])
    expect(got.find(p => p.valueName === 'Z')!.taskId).toBe('t1')
  })

  it('tolerates no task list and no open image', () => {
    expect(liveLabelPreviews(null, 'imgA')).toEqual([])
    expect(liveLabelPreviews(undefined, 'imgA')).toEqual([])
    expect(liveLabelPreviews([run()], '')).toEqual([])
  })
})

describe('shouldRefreshPreview', () => {
  it('always refreshes when nothing has been read yet', () => {
    expect(shouldRefreshPreview(undefined, 1_000)).toBe(true)
  })

  it('coalesces ticks inside the interval and allows one at the boundary', () => {
    expect(shouldRefreshPreview(1_000, 1_000 + PREVIEW_REFRESH_MIN_MS - 1)).toBe(false)
    expect(shouldRefreshPreview(1_000, 1_000 + PREVIEW_REFRESH_MIN_MS)).toBe(true)
  })
})
