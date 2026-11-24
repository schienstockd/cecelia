import { describe, it, expect } from 'vitest'
import { actionEnabled, actionTargets, deleteCount, moveTargetsFor, selectionRoots, sharedParent,
         MULTI_ACTIONS } from './popSelection'

// qc ─ C          A ─ A1          B          (viewer pick, transient)
const pops = [
  { path: '/qc', parent: 'root' }, { path: '/qc/C', parent: '/qc' },
  { path: '/A', parent: 'root' }, { path: '/A/A1', parent: '/A' },
  { path: '/B', parent: 'root' }, { path: '/_pick', parent: 'root', transient: true },
]

describe('selectionRoots', () => {
  it('drops a selected child of a selected parent (it travels with the parent) and duplicates', () => {
    expect(selectionRoots(['/A/A1', '/A', '/B', '/B'])).toEqual(['/A', '/B'])
  })
  it('keeps a sibling whose name merely starts with another selected name', () => {
    expect(selectionRoots(['/A', '/AB'])).toEqual(['/A', '/AB'])
  })
})

describe('actionTargets', () => {
  const listed = pops.map(p => p.path)
  it('is the whole selection when the menu row is in it', () => {
    expect(actionTargets(['/A', '/B'], '/B', listed)).toEqual(['/A', '/B'])
  })
  it('is just the menu row when it is outside the selection, or the selection is one row', () => {
    expect(actionTargets(['/A', '/B'], '/qc', listed)).toEqual(['/qc'])
    expect(actionTargets(['/A'], '/A', listed)).toEqual(['/A'])
  })
  it('never reaches a selected row that is no longer listed', () => {
    expect(actionTargets(['/A', '/gone'], '/A', listed)).toEqual(['/A'])
  })
})

describe('moveTargetsFor', () => {
  it('offers nothing inside a selected subtree, nor the transient pick, nor the parent all share', () => {
    expect(moveTargetsFor(pops, ['/A', '/B'])).toEqual(['/qc', '/qc/C'])
  })
  it('keeps all cells when the selection sits under different parents', () => {
    expect(moveTargetsFor(pops, ['/qc/C', '/B'])).toEqual(['root', '/qc', '/A', '/A/A1'])
  })
  it('a single population: everywhere but its own subtree and its current parent', () => {
    expect(moveTargetsFor(pops, ['/A'])).toEqual(['/qc', '/qc/C', '/B'])
  })
})

describe('sharedParent', () => {
  it('is the common parent, else root', () => {
    expect(sharedParent(pops, ['/qc/C'])).toBe('/qc')
    expect(sharedParent(pops, ['/A/A1', '/qc/C'])).toBe('root')
  })
})

describe('deleteCount', () => {
  it('counts every selected subtree once', () => {
    expect(deleteCount(pops, ['/A', '/A/A1', '/B'])).toBe(3)
    expect(deleteCount(pops, ['/qc'])).toBe(2)
  })
})

describe('actionEnabled', () => {
  it('lets only the multi actions through a multi-selection', () => {
    expect([...MULTI_ACTIONS].sort()).toEqual(['combine', 'delete', 'kiwi', 'move'])
    expect(actionEnabled('rename', 2)).toBe(false)
    expect(actionEnabled('deleteBelow', 3)).toBe(false)
    expect(actionEnabled('delete', 3)).toBe(true)
    expect(actionEnabled('rename', 1)).toBe(true)
  })
})
