import { describe, it, expect } from 'vitest'
import { taskDraftScope, taskDraftKey } from './taskDrafts'

// The draft key must mirror how funParams are scoped (image → set) so an in-progress draft lives at
// the same granularity the form loads/saves at. These pure helpers are the whole of that logic.
describe('task draft scoping', () => {
  it('scope is the driving image when one is selected, else the set', () => {
    expect(taskDraftScope('img1', 'setA')).toBe('img1')   // single image selected
    expect(taskDraftScope('', 'setA')).toBe('setA')       // multi/none → set-level
    expect(taskDraftScope('', '')).toBe('')               // nothing known yet
  })

  it('key composes project|fun|scope, and is empty until all parts are known', () => {
    expect(taskDraftKey('proj', 'cleanupImages.driftCorrect', 'img1'))
      .toBe('proj|cleanupImages.driftCorrect|img1')
    expect(taskDraftKey('', 'fun', 'scope')).toBe('')     // no project → no draft
    expect(taskDraftKey('proj', '', 'scope')).toBe('')    // no fun → no draft
    expect(taskDraftKey('proj', 'fun', '')).toBe('')      // no scope → no draft
  })

  it('per-image and per-set edits get distinct keys (the bug: A vs B vs set)', () => {
    const fun = 'segment.cellpose'
    const a   = taskDraftKey('p', fun, taskDraftScope('imgA', 'setA'))
    const b   = taskDraftKey('p', fun, taskDraftScope('imgB', 'setA'))
    const set = taskDraftKey('p', fun, taskDraftScope('', 'setA'))
    expect(new Set([a, b, set]).size).toBe(3)
  })
})
