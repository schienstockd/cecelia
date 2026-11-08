import { describe, it, expect } from 'vitest'
import { gateSignatures, changedPopPaths, seriesReloadToken, type SigNode } from './popChanges'

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
