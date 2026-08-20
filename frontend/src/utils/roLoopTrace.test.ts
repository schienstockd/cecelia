import { describe, it, expect } from 'vitest'
import { selfResized, describeTarget, creationSite, formatRoLoop,
         describeResize } from './roLoopTrace'

/** a box where the inner and outer measurements agree — the ordinary case */
const box = (w: number, h: number, cw = w, ch = h) => ({ w, h, cw, ch })

// The wrapper itself needs a DOM (and a real ResizeObserver) to exercise, which this suite has no
// environment for — so the DECISIONS live in pure functions and are pinned here, and the wrapper is
// the thin part that only wires them to `offsetWidth`/`offsetHeight`.

describe('selfResized', () => {
  it('any change counts — a 1px settle is still an undelivered notification', () => {
    // CanvasPanel's `>1px` guard was read as "so we don't loop the ResizeObserver". It does bound the
    // loop; it does NOT stop the browser reporting the first write, which is the whole confusion this
    // detector exists to end. 1px must therefore report.
    expect(selfResized(box(400, 300), box(400, 301))).toBe(true)
    expect(selfResized(box(400, 300), box(401, 300))).toBe(true)
    expect(selfResized(box(400, 300), box(400, 300))).toBe(false)
  })

  it('catches the scrollbar case, where the border box never moves', () => {
    // an overflow:auto element whose callback sizes a CHILD from clientWidth: the child grows past the
    // box, a scrollbar appears, the INNER box shrinks — and the outer numbers say nothing happened.
    // This is the loop the detector used to miss entirely.
    expect(selfResized(box(400, 300), box(400, 300, 385, 300))).toBe(true)
  })
})

describe('describeResize', () => {
  it('reports the border box when that is what moved', () => {
    expect(describeResize(box(400, 300), box(400, 412))).toBe('400x300 → 400x412')
  })

  it('names the scrollbar when only the inner box moved — the outer numbers would read as a non-event', () => {
    expect(describeResize(box(400, 300), box(400, 300, 385, 300)))
      .toBe('inner 400x300 → 385x300, a scrollbar appeared or went')
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
                                             box(400, 300), box(400, 412))
    expect(message).toContain('div.panel.square')
    expect(message).toContain('400x300 → 400x412')
    expect(detail).toContain('rafCoalesce')
    expect(detail).toContain('CanvasPanel.vue:122')
  })
})
