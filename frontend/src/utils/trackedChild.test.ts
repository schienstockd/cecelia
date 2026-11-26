import { describe, it, expect } from 'vitest'
import { canAddTrackedChild, isTrackedRule, IS_TRACKED, TRACKED_CHILD_NAME } from './trackedChild'

const rule = { ...IS_TRACKED, default_all: false }
const frags = { path: '/qc/frags', parent: '/qc', name: 'frags' }
const OBS = ['track_id', 'track_source']

describe('isTrackedRule', () => {
  it('is exactly track_id > 0, single or as the one condition, scalar or one-element list', () => {
    expect(isTrackedRule(rule)).toBe(true)
    expect(isTrackedRule({ measure: '', fun: '', values: '', default_all: false, conditions: [IS_TRACKED] })).toBe(true)
    expect(isTrackedRule({ ...rule, values: [0] })).toBe(true)
  })
  it('anything else is not the rule', () => {
    expect(isTrackedRule(undefined)).toBe(false)
    expect(isTrackedRule({ ...rule, values: 5 })).toBe(false)          // the old hand-made proxy, a threshold
    expect(isTrackedRule({ ...rule, fun: 'gte' })).toBe(false)
    expect(isTrackedRule({ ...rule, conditions: [IS_TRACKED, { measure: 'area', fun: 'gt', values: 3 }] })).toBe(false)
  })
})

describe('canAddTrackedChild', () => {
  it('offered on a stored flow population of a tracked segmentation', () => {
    expect(canAddTrackedChild(frags, [frags], OBS, 'flow')).toBe(true)
  })
  it('not on another map, an untracked segmentation, or a transient population', () => {
    expect(canAddTrackedChild(frags, [frags], OBS, 'clust')).toBe(false)
    expect(canAddTrackedChild(frags, [frags], ['live.cell.speed'], 'flow')).toBe(false)
    expect(canAddTrackedChild({ ...frags, transient: true }, [frags], OBS, 'flow')).toBe(false)
  })
  it('only once: a child with the rule, or a child already called tracked, blocks it', () => {
    const viaRule = { path: '/qc/frags/followed', parent: '/qc/frags', name: 'followed', filter: rule }
    const byName = { path: '/qc/frags/tracked', parent: '/qc/frags', name: TRACKED_CHILD_NAME }
    expect(canAddTrackedChild(frags, [frags, viaRule], OBS, 'flow')).toBe(false)
    expect(canAddTrackedChild(frags, [frags, byName], OBS, 'flow')).toBe(false)
    // a grandchild with the rule does not count: it is another population's tracked cells
    const deeper = { ...viaRule, path: '/qc/frags/x/followed', parent: '/qc/frags/x' }
    expect(canAddTrackedChild(frags, [frags, deeper], OBS, 'flow')).toBe(true)
  })
})
