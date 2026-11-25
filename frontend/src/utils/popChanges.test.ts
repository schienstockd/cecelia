import { describe, it, expect } from 'vitest'
import { gateSignatures, changedPopPaths, seriesReloadToken, clusterAwareFetchToken, statsToFetch, type SigNode } from './popChanges'

const rect = (x: number) => ({ kind: 'rectangle', x_min: x })
const tree = (tGate = 1, bGate = 2): SigNode[] => [
  { name: 'T', gate: rect(tGate), children: [{ name: 'CD4', gate: rect(3) }] },
  { name: 'B', gate: rect(bGate) },
]

describe('changedPopPaths', () => {
  it('no earlier tree → every path changed', () => {
    expect([...changedPopPaths(undefined, gateSignatures(tree()))].sort()).toEqual(['/B', '/T', '/T/CD4'])
  })
  it('an unchanged tree changes nothing', () => {
    expect(changedPopPaths(gateSignatures(tree()), gateSignatures(tree())).size).toBe(0)
  })
  it('a redrawn gate changes the pop and its descendants, not its siblings', () => {
    expect([...changedPopPaths(gateSignatures(tree()), gateSignatures(tree(9)))].sort()).toEqual(['/T', '/T/CD4'])
  })
  it('a NEW gate changes only itself', () => {
    const next = tree(); next[1].children = [{ name: 'naive', gate: rect(5) }]
    expect([...changedPopPaths(gateSignatures(tree()), gateSignatures(next))]).toEqual(['/B/naive'])
  })
  it('a deleted pop counts as changed', () => {
    expect([...changedPopPaths(gateSignatures(tree()), gateSignatures([tree()[0]]))]).toEqual(['/B'])
  })
  it('an explicit-label pop changes with its membership_sig', () => {
    const a: SigNode[] = [{ name: 'sel', membership_sig: 'x' }]
    const b: SigNode[] = [{ name: 'sel', membership_sig: 'y' }]
    expect([...changedPopPaths(gateSignatures(a), gateSignatures(b))]).toEqual(['/sel'])
  })
})

describe('seriesReloadToken', () => {
  const s = (pop: string, valueName = 'default') => ({ valueName, pop })
  it('moves only for panels plotting a changed pop', () => {
    const changes = { 'default/T': 1 }
    expect(seriesReloadToken(0, changes, [s('/B')], 0)).toBe(0)
    expect(seriesReloadToken(0, changes, [s('/T')], 0)).toBe(1)
  })
  it('follows an ancestor — a derived pop missing from the gating tree', () => {
    expect(seriesReloadToken(0, { 'default/T': 2 }, [s('/T/_tracked')], 0)).toBe(2)
    expect(seriesReloadToken(0, { 'default/T': 2 }, [s('/Tc')], 0)).toBe(0)   // prefix ≠ ancestor
  })
  it('keys by segmentation', () => {
    expect(seriesReloadToken(0, { 'default/T': 1 }, [s('/T', 'nuc')], 0)).toBe(0)
  })
  it('a panel plotting nothing follows every change', () => {
    expect(seriesReloadToken(5, {}, [], 3)).toBe(8)
  })
  it('the base (manual reload, task completion) always counts', () => {
    expect(seriesReloadToken(4, {}, [s('/B')], 9)).toBe(4)
  })
})

describe('clusterAwareFetchToken', () => {
  // Track cluster frequency of /P14/_tracked: the measure is the track-cluster column, whose
  // categories are the cluster pops' names — editing one must refetch though the series didn't change
  it('follows every pop change while a cluster column is read', () => {
    expect(clusterAwareFetchToken(2, 5, ['clusters.default', '', ''])).toBe(7)
    expect(clusterAwareFetchToken(2, 5, ['speed', 'regions.niches', ''])).toBe(7)
    expect(clusterAwareFetchToken(2, 5, ['speed', '', 'clusters.movement'])).toBe(7)
  })
  it('stays targeted for any other measure', () => {
    expect(clusterAwareFetchToken(2, 5, ['speed', '', ''])).toBe(2)
    expect(clusterAwareFetchToken(2, 5, [undefined, null, ''])).toBe(2)
    // a column merely containing the word is not a cluster run
    expect(clusterAwareFetchToken(2, 5, ['n_clusters.x'])).toBe(2)
  })
})

describe('statsToFetch', () => {
  const cur = { '/T': 1, '/T/CD4': 2, '/B': 3, '/gone': 4 }
  it('no change set → every pop, nothing kept', () => {
    expect(statsToFetch(['/T', '/B'], cur)).toEqual({ fetch: ['/T', '/B'], kept: {} })
  })
  it('a colour change (nothing changed) fetches nothing and keeps every count', () => {
    const paths = ['/T', '/T/CD4', '/B']
    const changed = changedPopPaths(gateSignatures(tree()), gateSignatures(tree()))
    expect(statsToFetch(paths, cur, changed)).toEqual({ fetch: [], kept: { '/T': 1, '/T/CD4': 2, '/B': 3 } })
  })
  it('a moved gate fetches it and its subtree; a new pop is fetched; a deleted one dropped', () => {
    const changed = changedPopPaths(gateSignatures(tree()), gateSignatures(tree(9)))
    const r = statsToFetch(['/T', '/T/CD4', '/B', '/new'], cur, changed)
    expect(r.fetch.sort()).toEqual(['/T', '/T/CD4', '/new'])
    expect(r.kept).toEqual({ '/B': 3 })
  })
})
