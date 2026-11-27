import { describe, it, expect } from 'vitest'
import { canAddTrackedChild, canAddMostlyPops, classChildren, isTrackedRule, IS_TRACKED, TRACKED_CHILD_NAME } from './trackedChild'
import type { FlatPop } from '../stores/gating'

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

describe('canAddMostlyPops — "mostly <child>" per sub-population', () => {
  const pop = (path: string, extra: Partial<FlatPop> = {}) =>
    ({ path, parent: path.slice(0, path.lastIndexOf('/')) || 'root', name: path.split('/').pop()!, ...extra }) as FlatPop
  const P = pop('/P')
  const A = pop('/P/A'), B = pop('/P/B')
  const tracked = pop('/P/tracked', { filter: { measure: 'track_id', fun: 'gt', values: 0 } as FlatPop['filter'] })
  it('offered on a tracked flow pop with sub-populations', () => {
    expect(canAddMostlyPops(P, [P, A, B], ['track_id'], 'flow')).toBe(true)
    expect(classChildren(P, [P, A, B, tracked]).map(c => c.path)).toEqual(['/P/A', '/P/B'])
  })
  it('not without sub-populations, tracks, or on another pop type', () => {
    expect(canAddMostlyPops(P, [P, tracked], ['track_id'], 'flow')).toBe(false)
    expect(canAddMostlyPops(P, [P, A], [], 'flow')).toBe(false)
    expect(canAddMostlyPops(P, [P, A], ['track_id'], 'clust')).toBe(false)
  })
  it('not once every sub-population has one, and the mostly pops are not sub-populations', () => {
    const mA = pop('/P/mostly A', { trackClassOf: '/P/A' }), mB = pop('/P/mostly B', { trackClassOf: '/P/B' })
    expect(canAddMostlyPops(P, [P, A, B, mA], ['track_id'], 'flow')).toBe(true)
    expect(canAddMostlyPops(P, [P, A, B, mA, mB], ['track_id'], 'flow')).toBe(false)
    expect(classChildren(P, [P, A, B, mA, mB]).map(c => c.path)).toEqual(['/P/A', '/P/B'])
  })
})
