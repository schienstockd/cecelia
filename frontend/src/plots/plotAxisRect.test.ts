import { describe, expect, it } from 'vitest'
import { svgAxisRect, clientAxisRectOf } from './plotAxisRect'

describe('svgAxisRect', () => {
  it('builds the axis rect from x/y range pairs', () => {
    // Observable Plot convention: x range = [marginLeft, width - marginRight],
    // y range = [height - marginBottom, marginTop] (inverted)
    const x = { range: [40, 260] }
    const y = { range: [180, 20] }
    expect(svgAxisRect(x, y)).toEqual({ left: 40, top: 20, width: 220, height: 160 })
  })

  it('is robust to a non-inverted y range', () => {
    expect(svgAxisRect({ range: [40, 260] }, { range: [20, 180] })).toEqual(
      { left: 40, top: 20, width: 220, height: 160 })
  })

  it('returns null when either scale is missing', () => {
    expect(svgAxisRect(null, { range: [0, 100] })).toBeNull()
    expect(svgAxisRect({ range: [0, 100] }, undefined)).toBeNull()
    expect(svgAxisRect(undefined, undefined)).toBeNull()
  })

  it('returns null on a degenerate range', () => {
    expect(svgAxisRect({ range: [50, 50] }, { range: [100, 0] })).toBeNull()
    expect(svgAxisRect({ range: [0, 100] }, { range: [50, 50] })).toBeNull()
    expect(svgAxisRect({}, { range: [0, 100] })).toBeNull()
  })
})

describe('clientAxisRectOf', () => {
  // Minimal node stub — just enough surface for the function to walk. `clientWidth`/`clientHeight`
  // match `getBoundingClientRect()` (the untransformed case), so sx/sy = 1.
  function node(
    r: { left: number; top: number; width: number; height: number },
    cw = r.width, ch = r.height,
  ): Element {
    return {
      getBoundingClientRect: () => ({ ...r, right: r.left + r.width, bottom: r.top + r.height } as DOMRect),
      clientWidth: cw, clientHeight: ch,
    } as unknown as Element
  }

  it('translates SVG axis coords to client-space via the node rect', () => {
    const el = node({ left: 100, top: 50, width: 300, height: 200 })
    const rect = clientAxisRectOf(el, { range: [40, 260] }, { range: [180, 20] })
    expect(rect).toEqual({ left: 140, top: 70, width: 220, height: 160 })
  })

  it('scales SVG coords when an ancestor CSS transform stretched the node', () => {
    // A 2× ancestor scale doubles the client rect but leaves clientWidth/Height as the native px.
    const el = node({ left: 0, top: 0, width: 600, height: 400 }, 300, 200)
    const rect = clientAxisRectOf(el, { range: [40, 260] }, { range: [180, 20] })
    expect(rect).toEqual({ left: 80, top: 40, width: 440, height: 320 })
  })

  it('returns null when the node is null or degenerate', () => {
    expect(clientAxisRectOf(null, { range: [0, 100] }, { range: [0, 100] })).toBeNull()
    const zero = node({ left: 0, top: 0, width: 0, height: 0 })
    expect(clientAxisRectOf(zero, { range: [0, 100] }, { range: [0, 100] })).toBeNull()
  })

  it('returns null when either scale is missing', () => {
    const el = node({ left: 0, top: 0, width: 300, height: 200 })
    expect(clientAxisRectOf(el, null, { range: [0, 100] })).toBeNull()
    expect(clientAxisRectOf(el, { range: [0, 100] }, null)).toBeNull()
  })
})
