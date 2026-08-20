import { describe, it, expect } from 'vitest'
import { selfResized, describeTarget, creationSite, formatRoLoop } from './roLoopTrace'

// The wrapper itself needs a DOM (and a real ResizeObserver) to exercise, which this suite has no
// environment for — so the DECISIONS live in pure functions and are pinned here, and the wrapper is
// the thin part that only wires them to `offsetWidth`/`offsetHeight`.

describe('selfResized', () => {
  it('any change counts — a 1px settle is still an undelivered notification', () => {
    // CanvasPanel's `>1px` guard was read as "so we don't loop the ResizeObserver". It does bound the
    // loop; it does NOT stop the browser reporting the first write, which is the whole confusion this
    // detector exists to end. 1px must therefore report.
    expect(selfResized({ w: 400, h: 300 }, { w: 400, h: 301 })).toBe(true)
    expect(selfResized({ w: 400, h: 300 }, { w: 401, h: 300 })).toBe(true)
    expect(selfResized({ w: 400, h: 300 }, { w: 400, h: 300 })).toBe(false)
  })
})

describe('describeTarget', () => {
  it('names the element the way you would recognise it on screen', () => {
    expect(describeTarget('DIV', 'gate-1', 'panel square active')).toBe('div#gate-1.panel.square.active')
    expect(describeTarget('CANVAS', '', 'plot-layers')).toBe('canvas.plot-layers')
    expect(describeTarget('DIV', '', '')).toBe('div')
  })

  it('caps the class list — a panel carries a dozen state classes and they are not the answer', () => {
    expect(describeTarget('DIV', '', 'a b c d e f')).toBe('div.a.b.c')
  })
})

describe('creationSite', () => {
  it('drops the wrapper frames, so the first line is the code that built the observer', () => {
    const stack = [
      'Error',
      '    at new TracedResizeObserver (http://localhost:5173/src/utils/roLoopTrace.ts:78:20)',
      '    at http://localhost:5173/src/components/canvas/CanvasPanel.vue:122:10',
      '    at callWithErrorHandling (http://localhost:5173/node_modules/.vite/deps/vue.js:1:1)',
    ].join('\n')
    const site = creationSite(stack)
    expect(site).not.toMatch(/roLoopTrace/)
    expect(site.split('\n')[0]).toContain('CanvasPanel.vue:122')
  })

  it('keeps a dependency frame — "it is not ours" is the finding, not a reason to hide it', () => {
    const stack = [
      'Error',
      '    at new TracedResizeObserver (http://localhost:5173/src/utils/roLoopTrace.ts:78:20)',
      '    at createRenderer (http://localhost:5173/node_modules/regl-scatterplot/dist/x.js:9:1)',
    ].join('\n')
    expect(creationSite(stack)).toContain('regl-scatterplot')
  })

  it('falls back to the raw stack rather than reporting nothing', () => {
    expect(creationSite('Error\n    at <anonymous>')).toContain('at <anonymous>')
  })
})

describe('formatRoLoop', () => {
  it('states the box change, and points at the fix rather than the symptom', () => {
    const { message, detail } = formatRoLoop('at CanvasPanel.vue:122', 'div.panel.square',
                                             { w: 400, h: 300 }, { w: 400, h: 412 })
    expect(message).toContain('div.panel.square')
    expect(message).toContain('400x300 → 400x412')
    expect(detail).toContain('rafCoalesce')
    expect(detail).toContain('CanvasPanel.vue:122')
  })
})
