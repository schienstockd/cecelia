import { describe, it, expect } from 'vitest'
import { createUndoStack } from './undoStack'

describe('createUndoStack', () => {
  it('undoes and redoes snapshots in order', () => {
    const u = createUndoStack<number>()
    u.push(0); u.push(1)                       // state went 0 → 1 → 2
    expect(u.undo(2)).toBe(1)
    expect(u.undo(1)).toBe(0)
    expect(u.undo(0)).toBeNull()
    expect(u.redo(0)).toBe(1)
    expect(u.redo(1)).toBe(2)
    expect(u.canRedo).toBe(false)
  })
  it('folds a run of edits with one coalesce key into one step', () => {
    const u = createUndoStack<string>()
    u.push('a', 'n1:x'); u.push('ab', 'n1:x'); u.push('abc', 'n1:x')
    expect(u.undo('abcd')).toBe('a')
    expect(u.canUndo).toBe(false)
  })
  it('a new edit drops the redo branch', () => {
    const u = createUndoStack<number>()
    u.push(0); expect(u.undo(1)).toBe(0)
    u.push(0)
    expect(u.canRedo).toBe(false)
  })
  it('keeps at most `limit` steps', () => {
    const u = createUndoStack<number>(2)
    u.push(0); u.push(1); u.push(2)
    expect(u.undo(3)).toBe(2); expect(u.undo(2)).toBe(1); expect(u.undo(1)).toBeNull()
  })
})
