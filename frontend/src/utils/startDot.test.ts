import { describe, it, expect } from 'vitest'
import { START_ID, startTargetsOf, touchesStart, buildStartGraph } from './startDot'

// Round-trip the whiteboard's UML start dot through save (currentTemplate) → load (applyTemplate),
// the reservation that was previously only static-verified. Exercises the exact pure helpers both
// sides of ChainModule use.
describe('start dot round-trip', () => {
  it('extracts startTargets from the start-dot edges only', () => {
    const edges = [{ source: START_ID, target: 'seg' }, { source: 'seg', target: 'meas' }]
    expect(startTargetsOf(edges)).toEqual(['seg'])
  })

  it('save → load preserves the linked target (and excludes the start dot from task edges)', () => {
    const edges = [{ source: START_ID, target: 'seg' }, { source: 'seg', target: 'meas' }]
    // save side
    const startTargets = startTargetsOf(edges)
    const taskEdges = edges.filter(e => !touchesStart(e))
    expect(taskEdges).toEqual([{ source: 'seg', target: 'meas' }])
    // load side
    const start = buildStartGraph(startTargets, new Set(['seg', 'meas']), { x: 20, y: 40 }, true)
    expect(start).not.toBeNull()
    expect(start!.node.id).toBe(START_ID)
    expect(start!.node.position).toEqual({ x: 20, y: 40 })
    expect(start!.edges.map(e => e.target)).toEqual(['seg'])
    expect(start!.edges[0].source).toBe(START_ID)
  })

  it('keeps an unlinked dot when a position was persisted (default on a new chain)', () => {
    const start = buildStartGraph([], new Set(['seg']), { x: 20, y: 40 }, true)
    expect(start).not.toBeNull()
    expect(start!.edges).toHaveLength(0)
  })

  it('drops the dot entirely when there are no targets and no persisted position', () => {
    expect(buildStartGraph([], new Set(['seg']), { x: 0, y: 0 }, false)).toBeNull()
    expect(buildStartGraph(undefined, new Set(['seg']), { x: 0, y: 0 }, false)).toBeNull()
  })

  it('drops links to since-deleted nodes but keeps the valid ones', () => {
    const start = buildStartGraph(['seg', 'ghost'], new Set(['seg']), { x: 0, y: 0 }, false)
    expect(start!.edges.map(e => e.target)).toEqual(['seg'])
  })
})
