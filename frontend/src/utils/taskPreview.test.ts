import { describe, it, expect } from 'vitest'
import {
  previewBlocker, hasPreviewableModel, blockerMessage, previewSummary, FALLBACK_2D_WARN,
  type PreviewContext, type PreviewStatus,
} from './taskPreview'

const ctx = (over: Partial<PreviewContext> = {}): PreviewContext => ({
  projectUid: 'p', imageUid: 'img1', valueName: 'A',
  params: { models: { m1: { matchAs: 'base' } } },
  ...over,
})
const status = (over: Partial<PreviewStatus> = {}): PreviewStatus => ({
  alive: true, starting: false, imageUid: 'img1',
  zarrPath: '/p/0/img1/x.ome.zarr', taskDir: '/p/1/img1',
  ...over,
})
const on = { enabled: true, pinned: false }

describe('previewBlocker', () => {
  it('allows a preview when everything lines up', () => {
    expect(previewBlocker(ctx(), status(), on)).toBeNull()
  })

  it('blocks when the toggle is off, before looking at anything else', () => {
    // cheapest check first: an off toggle must not cause status/context work
    expect(previewBlocker(null, null, { enabled: false, pinned: false })).toBe('off')
  })

  it('blocks when pinned, so a pinned result stops chasing the view', () => {
    expect(previewBlocker(ctx(), status(), { enabled: true, pinned: true })).toBe('pinned')
  })

  it('blocks on a missing page context', () => {
    expect(previewBlocker(null, status(), on)).toBe('no-context')
    expect(previewBlocker(ctx({ projectUid: '' }), status(), on)).toBe('no-context')
    expect(previewBlocker(ctx({ imageUid: '' }), status(), on)).toBe('no-context')
  })

  it('blocks before params resolve', () => {
    expect(previewBlocker(ctx({ params: null }), status(), on)).toBe('no-params')
  })

  it('blocks when the viewer has nothing open', () => {
    expect(previewBlocker(ctx(), null, on)).toBe('no-image-open')
    expect(previewBlocker(ctx(), status({ imageUid: null }), on)).toBe('no-image-open')
  })

  it('blocks when the viewer is showing a DIFFERENT image', () => {
    // The region comes from whatever the viewer shows, so previewing another image would compute the
    // wrong area — and silently opening the right one would move the user's viewer under them.
    expect(previewBlocker(ctx({ imageUid: 'other' }), status(), on)).toBe('image-mismatch')
  })
})

describe('hasPreviewableModel', () => {
  it('accepts a base model, explicit or defaulted', () => {
    expect(hasPreviewableModel({ models: { a: { matchAs: 'base' } } })).toBe(true)
    expect(hasPreviewableModel({ models: { a: {} } })).toBe(true)           // matchAs defaults to base
    expect(hasPreviewableModel({ models: { a: { matchAs: '' } } })).toBe(true)
  })

  it('rejects params the worker would raise on', () => {
    // the worker previews base models only; sending nucleus-only params would be an error, not a preview
    expect(hasPreviewableModel({ models: { a: { matchAs: 'nuc' } } })).toBe(false)
    expect(hasPreviewableModel({ models: {} })).toBe(false)
    expect(hasPreviewableModel({})).toBe(false)
    expect(hasPreviewableModel(null)).toBe(false)
  })

  it('accepts a mixed set — the base model is what gets previewed', () => {
    expect(hasPreviewableModel({ models: { a: { matchAs: 'nuc' }, b: { matchAs: 'base' } } })).toBe(true)
  })

  it('survives a malformed models value instead of throwing', () => {
    expect(hasPreviewableModel({ models: 'nope' as unknown })).toBe(false)
    expect(hasPreviewableModel({ models: [{ matchAs: 'base' }] as unknown })).toBe(true)  // array works
  })
})

describe('blockerMessage', () => {
  it('is imperative and short where the user can act', () => {
    expect(blockerMessage('no-image-open')).toBe('Open the image to preview it')
    expect(blockerMessage('image-mismatch')).toBe('Open this image to preview it')
    expect(blockerMessage('no-models')).toBe('Add a model to preview')
    for (const m of ['no-image-open', 'image-mismatch', 'no-models'] as const) {
      expect(blockerMessage(m).split(' ').length).toBeLessThanOrEqual(8)
      expect(blockerMessage(m)).not.toMatch(/\.$/)      // house style: no trailing full stop
    }
  })

  it('says nothing for states the user chose or can already see', () => {
    expect(blockerMessage('off')).toBe('')
    expect(blockerMessage('pinned')).toBe('')
    expect(blockerMessage('no-context')).toBe('')
    expect(blockerMessage('no-params')).toBe('')
    expect(blockerMessage(null)).toBe('')
  })
})

describe('previewSummary', () => {
  it('counts cells, singular and plural', () => {
    expect(previewSummary({ base: 1 }, false).text).toBe('1 cell')
    expect(previewSummary({ base: 42 }, false).text).toBe('42 cells')
  })

  it('reports zero as a count, not as an absence', () => {
    // 0 is ambiguous (no signal in the region vs a parameter finding nothing), so it must stay a
    // number the UI can qualify — never collapsed into "no cells found" (see task #18)
    const s = previewSummary({ base: 0 }, false)
    expect(s.cells).toBe(0)
    expect(s.text).toBe('0 cells')
  })

  it('distinguishes "not run" from "found zero"', () => {
    expect(previewSummary(null, false).cells).toBeNull()
    expect(previewSummary({}, false).cells).toBeNull()
    expect(previewSummary(null, false).text).toBe('')
  })

  it('surfaces the 2D fallback rather than silently dropping to one plane', () => {
    expect(previewSummary({ base: 5 }, true).warn).toBe(FALLBACK_2D_WARN.short)
    expect(previewSummary({ base: 5 }, false).warn).toBe('')
    expect(previewSummary({ base: 5 }, false).warnDetail).toBe('')
    // the warning must not replace the count — both are true at once
    expect(previewSummary({ base: 5 }, true).text).toBe('5 cells')
  })

  it('the 2D-fallback detail says what the result does and does not tell you', () => {
    // The QC-text convention: short = the problem, detail = the consequence, specifically. "Results may
    // vary" would be useless — the point is that per-plane tuning IS faithful and only z-stitching
    // (hence counts / z-extents) is not, which is what lets a user still judge diameter from a plane.
    const d = previewSummary({ base: 5 }, true).warnDetail
    expect(d).toBe(FALLBACK_2D_WARN.detail)
    expect(d).toMatch(/z-stitching/)
    expect(d).toMatch(/counts/)
    expect(d).not.toMatch(/may vary/i)
    expect(FALLBACK_2D_WARN.short.split(' ').length).toBeLessThanOrEqual(4)   // short really is short
    expect(d.split(' ').length).toBeLessThanOrEqual(20)                       // one line, not an essay
  })
})
