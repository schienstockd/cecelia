import { describe, it, expect } from 'vitest'
import { START_ID, startTargetsOf, touchesStart, buildStartGraph, startDotPosition, DEFAULT_START_POS, clearCanvas } from './startDot'

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

describe('startDotPosition', () => {
  // The regression: an authored template carries no `positions`, and `layoutDag` never sees the dot
  // (it is not a template node), so the two used to be placed by unrelated mechanisms — the dot at a
  // constant, the tasks on the grid. It rendered as an unconnected start node.
  const layout = { train: { x: 80, y: 120 }, seg: { x: 300, y: 120 } }

  it('places the dot one depth step left of its target, on the same row', () => {
    expect(startDotPosition(['train'], layout, 220)).toEqual({ x: -140, y: 120 })
  })

  it('uses the FIRST target that the layout actually placed', () => {
    expect(startDotPosition(['ghost', 'seg'], layout, 220)).toEqual({ x: 80, y: 120 })
  })

  it('falls back to the default when no target is laid out', () => {
    expect(startDotPosition(['ghost'], layout, 220)).toEqual(DEFAULT_START_POS)
    expect(startDotPosition([], layout, 220)).toEqual(DEFAULT_START_POS)
    expect(startDotPosition(undefined, layout, 220)).toEqual(DEFAULT_START_POS)
  })

  it('lands left of the first task, which is what "connected" reads as', () => {
    const pos = startDotPosition(['train'], layout, 220)
    expect(pos.x).toBeLessThan(layout.train.x)
    expect(pos.y).toBe(layout.train.y)
  })
})

describe('clearCanvas — a full clear lets Vue unmount before anything is added', () => {
  it('removes every node and edge, and resolves only after the tick has run', async () => {
    const calls: string[] = []
    let releaseTick!: () => void
    const tick = () => new Promise<void>(r => { calls.push('tick'); releaseTick = () => { calls.push('flushed'); r() } })
    const done = clearCanvas({
      nodeIds: [START_ID, 'a'], edgeIds: ['e1'],
      removeNodes: ids => { calls.push(`nodes:${ids.join(',')}`) },
      removeEdges: ids => { calls.push(`edges:${ids.join(',')}`) },
    }, tick).then(() => { calls.push('resolved') })
    await Promise.resolve()
    expect(calls).toEqual([`nodes:${START_ID},a`, 'edges:e1', 'tick'])
    releaseTick()
    await done
    expect(calls).toEqual([`nodes:${START_ID},a`, 'edges:e1', 'tick', 'flushed', 'resolved'])
  })

  // A same-tick remove-all + add of `__start__` reuses the old element and its stale handle bounds, so
  // the dot's edge draws from the corner (New chain did exactly that). Every full clear in the
  // whiteboard must go through clearWhiteboard → clearCanvas, which waits for the unmount.
  it('ChainModule never clears the canvas by hand', () => {
    const files = import.meta.glob('/src/modules/ChainModule.vue', { query: '?raw', import: 'default', eager: true })
    const src = Object.values(files)[0] as string
    expect(src).toBeTruthy()
    expect(src.match(/removeNodes\(\s*nodes\.value\.map/g) ?? []).toHaveLength(0)
    expect(src).toMatch(/function clearWhiteboard\(\)[\s\S]{0,200}clearCanvas\(/)
  })
})
