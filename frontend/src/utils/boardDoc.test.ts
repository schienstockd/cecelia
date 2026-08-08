import { describe, it, expect } from 'vitest'
import { boardsPayload, tabGroupOf, shouldReloadBoards } from './boardDoc'

describe('boardsPayload', () => {
  it('flattens the tab group into the document', () => {
    const p = boardsPayload({ tabs: [{ id: 1, name: 'A' }], activeId: 1, nextId: 2 }, { 'tab:1': { cols: 2 } })
    expect(p).toEqual({ tabs: [{ id: 1, name: 'A' }], activeId: 1, nextId: 2, layouts: { 'tab:1': { cols: 2 } } })
  })

  it('survives a project with no tab group yet', () => {
    expect(boardsPayload(null, {})).toEqual({ tabs: [], activeId: 0, nextId: 0, layouts: {} })
  })
})

describe('tabGroupOf', () => {
  it('round-trips with boardsPayload', () => {
    const g = { tabs: [{ id: 3, name: 'Motility' }], activeId: 3, nextId: 4 }
    expect(tabGroupOf(boardsPayload(g, {}))).toEqual(g)
  })

  it('defaults a missing or empty document', () => {
    expect(tabGroupOf(null)).toEqual({ tabs: [], activeId: 0, nextId: 0 })
  })
})

describe('shouldReloadBoards', () => {
  it('reloads when another client advanced the document', () => {
    expect(shouldReloadBoards({ projectUid: 'p1', version: 5 }, 'p1', 4)).toBe(true)
  })

  // The writer broadcasts too, and it already holds that state — reloading it would bounce the board
  // through a restore for nothing.
  it('ignores the echo of our own write', () => {
    expect(shouldReloadBoards({ projectUid: 'p1', version: 5 }, 'p1', 5)).toBe(false)
  })

  it('ignores an out-of-order broadcast that is older than what we hold', () => {
    expect(shouldReloadBoards({ projectUid: 'p1', version: 3 }, 'p1', 4)).toBe(false)
  })

  it('ignores another project, and anything malformed', () => {
    expect(shouldReloadBoards({ projectUid: 'other', version: 9 }, 'p1', 0)).toBe(false)
    expect(shouldReloadBoards({ projectUid: 'p1', version: 'x' }, 'p1', 0)).toBe(false)
    expect(shouldReloadBoards(null, 'p1', 0)).toBe(false)
    expect(shouldReloadBoards({ projectUid: 'p1', version: 1 }, null, 0)).toBe(false)
  })
})
