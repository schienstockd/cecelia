// A snapshot undo/redo stack for an editor whose whole state is one serialisable value (the chain
// whiteboard's nodes + edges). Pure: the caller takes the snapshot and applies what comes back.
// `push` records the state BEFORE a change; a `coalesce` key folds a run of edits to the same thing
// (typing in one param) into one step, so one Ctrl+Z undoes the edit, not each keystroke.

export interface UndoStack<T> {
  push(before: T, coalesce?: string): void
  undo(current: T): T | null
  redo(current: T): T | null
  clear(): void
  readonly canUndo: boolean
  readonly canRedo: boolean
}

export function createUndoStack<T>(limit = 100): UndoStack<T> {
  let past: T[] = []
  let future: T[] = []
  let lastKey: string | null = null
  return {
    push(before, coalesce) {
      future = []
      if (coalesce && coalesce === lastKey && past.length) return
      lastKey = coalesce ?? null
      past.push(before)
      if (past.length > limit) past = past.slice(past.length - limit)
    },
    undo(current) {
      const prev = past.pop()
      if (prev === undefined) return null
      future.push(current); lastKey = null
      return prev
    },
    redo(current) {
      const next = future.pop()
      if (next === undefined) return null
      past.push(current); lastKey = null
      return next
    },
    clear() { past = []; future = []; lastKey = null },
    get canUndo() { return past.length > 0 },
    get canRedo() { return future.length > 0 },
  }
}
