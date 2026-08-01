import { describe, it, expect } from 'vitest'
import {
  previewBlocker, hasPreviewableModel, blockerMessage, previewNotice, previewSummary,
  FALLBACK_2D_WARN, baseOnlyWarning, tilingWarning, compositeWarning,
  paramsBlocker, hasAfCombination,
  type PreviewContext, type PreviewStatus,
} from './taskPreview'

const ctx = (over: Partial<PreviewContext> = {}): PreviewContext => ({
  projectUid: 'p', imageUid: 'img1', valueName: 'A', funName: 'segment.cellpose',
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

// The readers below take FLAT params, and TaskRunner is what guarantees that (`previewParams` →
// `flattenParams`). These pin why: fed the nested shape the form holds, they don't error — they read
// undefined and fall back, which is how a preview quietly disagreed with the run it was previewing.
describe('the params these readers get must be flat', () => {
  const NESTED = {
    models: { m1: { matchAs: 'base' }, m2: { matchAs: 'nuc' } },
    labelModifications: { removeUnmatched: true },     // section-nested, as the form holds it
  }
  const FLAT = { models: NESTED.models, removeUnmatched: true }

  it('reads the stronger base-model warning only from the flat shape', () => {
    // removeUnmatched changes the ADVICE: the run deletes base labels with no nucleus, so it finds
    // FEWER cells than the preview shows. Nested, that fact is silently lost.
    expect(baseOnlyWarning(FLAT).detail).toContain('fewer')
    expect(baseOnlyWarning(NESTED).detail).not.toContain('fewer')
  })

  it('still detects a previewable model either way, because models is not section-nested', () => {
    expect(hasPreviewableModel(NESTED)).toBe(true)
    expect(hasPreviewableModel(FLAT)).toBe(true)
  })
})

describe('paramsBlocker', () => {
  // THE reported bug: the AF and AF+drift tasks showed no preview button at all. The readiness check
  // was `hasPreviewableModel` — a cellpose question ("is there a base model?") asked of every task —
  // so AF, which has `afCombinations` and no models, was reported not-runnable despite the backend
  // declaring it previewable. Silently: no button, no message.
  it('accepts an AF task with a division channel', () => {
    expect(paramsBlocker({ afCombinations: { '2': { divisionChannels: [3] } } })).toBeNull()
  })

  it('names what an AF task is missing rather than hiding', () => {
    expect(paramsBlocker({ afCombinations: {} })).toBe('no-af-channels')
    expect(paramsBlocker({ afCombinations: { '2': { divisionChannels: [] } } })).toBe('no-af-channels')
    expect(paramsBlocker({ afCombinations: { '2': {} } })).toBe('no-af-channels')
    expect(blockerMessage('no-af-channels')).toBe('Add a division channel to preview')
  })

  it('still asks the cellpose question of a cellpose task', () => {
    expect(paramsBlocker({ models: { a: { matchAs: 'base' } } })).toBeNull()
    expect(paramsBlocker({ models: {} })).toBe('no-models')
    expect(paramsBlocker({ models: { a: { matchAs: 'nuc' } } })).toBe('no-models')
  })

  it('is PERMISSIVE for a shape it does not recognise', () => {
    // whether a task can be previewed is the backend's statement; if the params are wrong the worker
    // refuses with a readable message. Hiding a control because the frontend doesn't know the shape is
    // the worse failure, because it is silent — which is exactly what happened to AF.
    expect(paramsBlocker({ someFutureBag: { x: 1 } })).toBeNull()
    expect(paramsBlocker({})).toBeNull()
  })

  it('reports no-params for absent params', () => {
    expect(paramsBlocker(null)).toBe('no-params')
  })

  it('flows through previewBlocker for both task shapes', () => {
    const afCtx = ctx({ funName: 'cleanupImages.afCorrect',
                        params: { afCombinations: { '2': { divisionChannels: [3] } } } })
    expect(previewBlocker(afCtx, status(), on)).toBeNull()
    const emptyAf = ctx({ funName: 'cleanupImages.afCorrect', params: { afCombinations: {} } })
    expect(previewBlocker(emptyAf, status(), on)).toBe('no-af-channels')
  })
})

describe('hasAfCombination', () => {
  it('needs at least one combination naming a reference channel', () => {
    expect(hasAfCombination({ afCombinations: { '2': { divisionChannels: [3] } } })).toBe(true)
    expect(hasAfCombination({ afCombinations: { '1': { divisionChannels: [] },
                                               '2': { divisionChannels: [0, 3] } } })).toBe(true)
    expect(hasAfCombination({ afCombinations: {} })).toBe(false)
    expect(hasAfCombination(null)).toBe(false)
  })

  it('survives a malformed value instead of throwing', () => {
    expect(hasAfCombination({ afCombinations: 'nope' as unknown })).toBe(false)
    expect(hasAfCombination({ afCombinations: { '2': { divisionChannels: 'x' as unknown } } })).toBe(false)
  })
})

describe('compositeWarning', () => {
  // afDriftCorrect is the case that forced this: it previews AF and skips drift correction, which
  // expands the canvas and shifts every frame — so the geometry on screen is not the run's.
  it('names the step the run does and the preview does not', () => {
    const w = compositeWarning([{ fun: 'cleanupImages.driftCorrect', label: 'Drift correction' }])
    expect(w.short).toBe('Drift correction not previewed')
    expect(w.detail).toContain('Drift correction')
    expect(w.detail).toContain('before that')
  })

  it('stays short when several steps are skipped, and names them in the detail', () => {
    const w = compositeWarning([{ label: 'Drift correction' }, { label: 'Measure labels' }])
    expect(w.short).toBe('Later steps not previewed')
    expect(w.short.split(' ').length).toBeLessThanOrEqual(4)
    expect(w.detail).toContain('Drift correction, Measure labels')
  })

  it('says nothing for a plain task', () => {
    for (const v of [null, undefined, []]) {
      expect(compositeWarning(v)).toEqual({ short: '', detail: '' })
    }
  })

  it('falls back to the fun_name when the backend sent no label', () => {
    expect(compositeWarning([{ fun: 'cleanupImages.driftCorrect' }]).short)
      .toBe('cleanupImages.driftCorrect not previewed')
  })

  it('ignores entries with neither label nor fun rather than rendering blanks', () => {
    expect(compositeWarning([{}, { label: '  ' }, { label: 'Drift correction' }]).short)
      .toBe('Drift correction not previewed')
  })
})

describe('previewNotice', () => {
  // The reason this exists: as muted 2xs text under the button, "the viewer is showing a different
  // version than this task reads" is invisible — and it is the one refusal that looks like a working
  // preview of the wrong pixels.
  it('makes a version mismatch amber, with the backend message as the detail', () => {
    const n = previewNotice(null, {
      message: "The viewer is showing 'ccidCorrected.ome.zarr'; this task reads 'default'. " +
               'Open that version to preview it.',
      code: 'version-mismatch',
    })
    expect(n.warn).toBe(true)
    expect(n.short).toBe('Wrong version open')
    expect(n.detail).toContain('default')
    expect(n.detail).toContain('ccidCorrected.ome.zarr')
  })

  it('warns on an image mismatch the frontend caught itself, before any request', () => {
    const n = previewNotice('image-mismatch', null)
    expect(n.warn).toBe(true)
    expect(n.short).toBe('Wrong image open')
    expect(n.detail).toBe('Open this image to preview it')   // short = problem, detail = the action
  })

  it('stays quiet for setup the user can see in the viewer and the form', () => {
    for (const b of ['no-image-open', 'no-models'] as const) {
      const n = previewNotice(b, null)
      expect(n.warn).toBe(false)
      expect(n.short).toBe(blockerMessage(b))
      expect(n.detail).toBe('')
    }
    expect(previewNotice('off', null)).toEqual({ short: '', detail: '', warn: false })
    expect(previewNotice('pinned', null)).toEqual({ short: '', detail: '', warn: false })
  })

  it('still warns for a failure it has no label for, rather than falling silent', () => {
    const n = previewNotice(null, { message: 'worker died' })
    expect(n.warn).toBe(true)
    expect(n.short).toBe('Preview failed')
    expect(n.detail).toBe('worker died')
  })

  // The label is the thing the user reads at a glance; the detail is free to be a sentence.
  it('keeps every short label to a glance', () => {
    for (const code of ['version-mismatch', 'image-mismatch', 'no-image-open', 'no-region',
                        'params-not-previewable', 'unknown-code']) {
      const n = previewNotice(null, { message: 'x', code })
      expect(n.short.split(' ').length).toBeLessThanOrEqual(4)
      expect(n.short).not.toMatch(/\.$/)
    }
  })

  // An error outranks a blocker: the request was made and refused, which is more specific than
  // whatever the local blocker computation would say about the same moment.
  it('prefers the backend reason over a local blocker', () => {
    const n = previewNotice('image-mismatch', { message: 'no region on screen', code: 'no-region' })
    expect(n.short).toBe('No region to preview')
  })
})

describe('baseOnlyWarning', () => {
  const base = { m1: { matchAs: 'base' } }

  it('says nothing for a single-model run — the preview IS the run', () => {
    expect(baseOnlyWarning({ models: base }).short).toBe('')
    expect(baseOnlyWarning({ models: { a: {} } }).short).toBe('')
    expect(baseOnlyWarning(null).short).toBe('')
    expect(baseOnlyWarning({}).short).toBe('')
  })

  it('warns when a nucleus model is declared', () => {
    const w = baseOnlyWarning({ models: { ...base, m2: { matchAs: 'nuc' } } })
    expect(w.short).toBe('Base model only')
    expect(w.detail).not.toBe('')
  })

  it('changes the advice when removeUnmatched would DELETE base cells', () => {
    // The concrete dishonesty: matching drops base labels with no nucleus, so the run finds FEWER
    // cells than the preview shows. That is a different warning from "a layer is missing".
    const on = baseOnlyWarning({ models: { ...base, m2: { matchAs: 'nuc' } }, removeUnmatched: true })
    const off = baseOnlyWarning({ models: { ...base, m2: { matchAs: 'nuc' } }, removeUnmatched: false })
    expect(on.detail).toMatch(/fewer/)
    expect(off.detail).not.toMatch(/fewer/)
    expect(off.detail).toMatch(/what the run produces/)
    expect(on.detail).not.toBe(off.detail)
  })

  it('keeps both strings within the house budget', () => {
    for (const p of [{ models: { ...base, m2: { matchAs: 'nuc' } }, removeUnmatched: true },
                     { models: { ...base, m2: { matchAs: 'nuc' } } }]) {
      const w = baseOnlyWarning(p)
      expect(w.short.split(' ').length).toBeLessThanOrEqual(4)
      expect(w.detail.split(' ').length).toBeLessThanOrEqual(20)
      expect(w.detail).not.toMatch(/\.$/)
      expect(w.detail).not.toMatch(/may vary/i)
    }
  })
})

describe('tilingWarning', () => {
  it('says nothing when no seam crosses the region', () => {
    // The common case: the visible region sits inside one tile, so the preview IS the run here
    expect(tilingWarning(null, 512).short).toBe('')
    expect(tilingWarning({}, 512).short).toBe('')
    expect(tilingWarning({ Y: 0, X: 0 }, 512).short).toBe('')
  })

  it('warns when the run would split the region', () => {
    const w = tilingWarning({ X: 1 }, 512)
    expect(w.short).toBe('Run would tile this')
    expect(w.detail).toMatch(/512 px/)          // the number belongs in the detail
    expect(w.detail).toMatch(/re-stitches/)     // what actually differs, not "may vary"
  })

  it('warns on a seam in either axis, and on several', () => {
    expect(tilingWarning({ Y: 1 }, 512).short).not.toBe('')
    expect(tilingWarning({ Y: 2, X: 1 }, 512).short).not.toBe('')
  })

  it('degrades without a blockSize rather than printing undefined', () => {
    expect(tilingWarning({ X: 1 }).detail).toMatch(/the tile size/)
    expect(tilingWarning({ X: 1 }, 0).detail).toMatch(/the tile size/)
    expect(tilingWarning({ X: 1 }).detail).not.toMatch(/undefined|NaN/)
  })

  it('keeps to the house budget', () => {
    const w = tilingWarning({ X: 1 }, 512)
    expect(w.short.split(' ').length).toBeLessThanOrEqual(4)
    expect(w.detail.split(' ').length).toBeLessThanOrEqual(20)
    expect(w.detail).not.toMatch(/\.$/)
    expect(w.detail).not.toMatch(/may vary/i)
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

  it('says "no image data here" rather than letting 0 cells look like a bad parameter', () => {
    // The case this exists for: a drift-padded plane returns 0 cells and is indistinguishable from a
    // diameter that is too large, so the user retunes against a region that CANNOT produce a mask.
    const s = previewSummary({ base: 0 }, false, { hasSignal: false, noSignalWhy: 'padding' })
    expect(s.warn).toBe('No image data here')
    expect(s.warnDetail).toMatch(/padding/)
    expect(s.cells).toBe(0)                                  // still reported, not hidden
  })

  it('distinguishes drift padding from a genuinely blank region', () => {
    const blank = previewSummary({ base: 0 }, false, { hasSignal: false, noSignalWhy: 'blank' })
    expect(blank.warn).toBe('Region is blank')
    expect(blank.warnDetail).toMatch(/channel/)              // different action from padding
    expect(blank.warn).not.toBe('No image data here')
  })

  it('stays quiet about signal when cells were actually found', () => {
    // hasSignal:false with a non-zero count would be contradictory; trust the count
    expect(previewSummary({ base: 12 }, false, { hasSignal: false, noSignalWhy: 'blank' }).warn).toBe('')
    expect(previewSummary({ base: 0 }, false, { hasSignal: true }).warn).toBe('')
    expect(previewSummary({ base: 0 }, false).warn).toBe('')  // no signal info → no claim
  })

  it('the empty-region warning wins over the 2D-fallback one', () => {
    // explaining z-stitching for a region with no data in it is noise
    const s = previewSummary({ base: 0 }, true, { hasSignal: false, noSignalWhy: 'padding' })
    expect(s.warn).toBe('No image data here')
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
