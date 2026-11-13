import { describe, it, expect } from 'vitest'
import { outlineSwatches } from './overlays'

// just enough of the legend DOM: a root whose swatch <rect>s sit in a sized <svg>
const attrsEl = (tagName: string, a: Record<string, string>) => ({
  tagName, a, getAttribute: (k: string) => a[k] ?? null, setAttribute: (k: string, v: string) => { a[k] = v },
})

describe('outlineSwatches — a white swatch still reads on white', () => {
  it('insets each swatch rect by half a pixel and strokes it in the legend ink', () => {
    const svg = attrsEl('svg', { width: '15', height: '15', fill: '#ffffff' })
    const rect = { ...attrsEl('rect', { width: '100%', height: '100%' }), parentElement: svg }
    const root = { tagName: 'DIV', querySelectorAll: () => [rect] } as unknown as Element
    outlineSwatches(root)
    expect(rect.a).toEqual({ x: '0.5', y: '0.5', width: '14', height: '14',
                             stroke: 'currentColor', 'stroke-width': '1' })
  })
  it('leaves a continuous ramp (root is an <svg>) alone', () => {
    let queried = false
    const root = { tagName: 'svg', querySelectorAll: () => { queried = true; return [] } } as unknown as Element
    outlineSwatches(root)
    expect(queried).toBe(false)
  })
})
