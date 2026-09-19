import { describe, it, expect } from 'vitest'
import {
  isCaptureTarget, coordMode, isKnownKind,
  resolveRect, resolveCircle, resolveArrow, resolvePoints,
  markAnchor, markMatchesViewer, paintableFor, pointsToSvgAttr,
} from './freeformRender'

describe('target detection', () => {
  it('recognises captureId targets vs live_viewer', () => {
    expect(isCaptureTarget('cap-20260919T140000-abcdef')).toBe(true)
    expect(isCaptureTarget('live_viewer')).toBe(false)
    expect(isCaptureTarget('')).toBe(false)
    // A stray target that isn't cap-* isn't rendered as normalised — safer to fall through to px
    expect(isCaptureTarget('viewer_frame')).toBe(false)
  })
  it('coordMode picks the right scaling per target', () => {
    expect(coordMode('cap-abc')).toBe('norm')
    expect(coordMode('live_viewer')).toBe('px')
  })
})

describe('resolveRect', () => {
  it('scales normalised coords by the box', () => {
    const r = resolveRect({ x: 0.1, y: 0.2, w: 0.3, h: 0.4 }, 100, 200, 'norm')
    expect(r).toEqual({ x: 10, y: 40, w: 30, h: 80 })
  })
  it('leaves px coords unscaled', () => {
    expect(resolveRect({ x: 5, y: 10, w: 20, h: 30 }, 100, 200, 'px'))
      .toEqual({ x: 5, y: 10, w: 20, h: 30 })
  })
  it('returns null on non-finite fields', () => {
    expect(resolveRect({ x: 'nope', y: 0, w: 1, h: 1 }, 100, 100, 'norm')).toBeNull()
    expect(resolveRect(null, 100, 100, 'norm')).toBeNull()
    expect(resolveRect('rect', 100, 100, 'norm')).toBeNull()
  })
})

describe('resolveCircle', () => {
  it('scales r by the SMALLER box dim so a "10% circle" reads uniformly on a non-square viewer', () => {
    const c = resolveCircle({ cx: 0.5, cy: 0.5, r: 0.1 }, 400, 200, 'norm')
    expect(c).toEqual({ cx: 200, cy: 100, r: 20 })  // min(400,200) * 0.1 = 20, not 40
  })
  it('leaves px coords unscaled', () => {
    expect(resolveCircle({ cx: 50, cy: 60, r: 15 }, 100, 200, 'px'))
      .toEqual({ cx: 50, cy: 60, r: 15 })
  })
})

describe('resolveArrow', () => {
  it('scales both endpoints', () => {
    expect(resolveArrow({ x1: 0.1, y1: 0.2, x2: 0.9, y2: 0.8 }, 100, 100, 'norm'))
      .toEqual({ x1: 10, y1: 20, x2: 90, y2: 80 })
  })
})

describe('resolvePoints', () => {
  it('scales each point, skipping malformed ones', () => {
    const p = resolvePoints({ pts: [[0.1, 0.2], [0.5, 0.5], ['bad', 0.5], [0.9, 0.9]] },
                            100, 200, 'norm')
    expect(p).toEqual([[10, 40], [50, 100], [90, 180]])
  })
  it('returns null when pts is empty or missing', () => {
    expect(resolvePoints({ pts: [] }, 100, 100, 'norm')).toBeNull()
    expect(resolvePoints({}, 100, 100, 'norm')).toBeNull()
    expect(resolvePoints(null, 100, 100, 'norm')).toBeNull()
  })
})

describe('markAnchor — where the label chip goes (top-right of bounding box)', () => {
  it('rect anchor is top-right corner', () => {
    expect(markAnchor('rect', { x: 0.1, y: 0.2, w: 0.3, h: 0.4 }, 100, 100, 'norm'))
      .toEqual({ x: 40, y: 20 })
  })
  it('circle anchor is at (cx+r, cy-r)', () => {
    expect(markAnchor('circle', { cx: 0.5, cy: 0.5, r: 0.1 }, 100, 100, 'norm'))
      .toEqual({ x: 60, y: 40 })
  })
  it('arrow anchor is the max-x + min-y endpoint corner', () => {
    expect(markAnchor('arrow', { x1: 10, y1: 90, x2: 90, y2: 10 }, 100, 100, 'px'))
      .toEqual({ x: 90, y: 10 })
  })
  it('poly / stroke anchor is the bounding-box top-right', () => {
    expect(markAnchor('poly',
      { pts: [[0.1, 0.5], [0.9, 0.3], [0.5, 0.7]] }, 100, 100, 'norm'))
      .toEqual({ x: 90, y: 30 })
  })
})

describe('markMatchesViewer — imageUid scoping', () => {
  it('unscoped mark renders anywhere', () => {
    expect(markMatchesViewer({ target: 'cap-abc' }, 'IMG1')).toBe(true)
    expect(markMatchesViewer({ target: 'cap-abc' }, '')).toBe(true)
  })
  it('scoped mark renders only on its image', () => {
    expect(markMatchesViewer({ target: 'live_viewer', imageUid: 'IMG1' }, 'IMG1')).toBe(true)
    expect(markMatchesViewer({ target: 'live_viewer', imageUid: 'IMG1' }, 'IMG2')).toBe(false)
    expect(markMatchesViewer({ target: 'live_viewer', imageUid: 'IMG1' }, '')).toBe(false)
  })
})

describe('isKnownKind — unknown kinds are dropped, not painted as garbage', () => {
  it('accepts the five schema kinds', () => {
    for (const k of ['rect', 'poly', 'stroke', 'circle', 'arrow']) expect(isKnownKind(k)).toBe(true)
  })
  it('rejects anything else', () => {
    expect(isKnownKind('nope')).toBe(false)
    expect(isKnownKind('')).toBe(false)
  })
})

describe('paintableFor — end-to-end resolution', () => {
  it('resolves valid marks and drops bad ones without failing the batch', () => {
    const overlay = [
      { kind: 'rect' as const,   geom: { x: 0.1, y: 0.1, w: 0.2, h: 0.2 }, label: 'A' },
      { kind: 'nope' as const,   geom: { } },                                              // unknown kind
      { kind: 'circle' as const, geom: { cx: 0.5, cy: 0.5, r: 0.1 } },
      { kind: 'poly' as const,   geom: { pts: [] } },                                      // empty pts
    ]
    const p = paintableFor(overlay, 100, 100, 'norm')
    expect(p.map(x => x.kind)).toEqual(['rect', 'circle'])
    expect(p[0].label).toBe('A')
    expect(p[1].label).toBeUndefined()
  })
})

describe('pointsToSvgAttr', () => {
  it('joins coordinate pairs with spaces', () => {
    expect(pointsToSvgAttr([[10, 20], [30, 40], [50, 60]])).toBe('10,20 30,40 50,60')
    expect(pointsToSvgAttr([])).toBe('')
  })
})
