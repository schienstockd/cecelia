import { describe, it, expect } from 'vitest'
import { refKey, refLabel, chipState, pointTarget, draftAdd, draftRemove, parseDraft, upsertTurn,
         turnMeta, searchRefs, viewerRefFor, claimText, plainError, attachmentRows, claimRows,
         TASK_PAGES, type KiwiTurn } from './kiwiTurn'
import { KIWI_REF_KINDS, type KiwiRef } from './kiwiRef'

const img: KiwiRef = { kind: 'image', imageUid: 'KDIeEm' }
const ok = (check: 'exists' | 'live' | 'format' = 'exists') => ({ ok: true, check, label: 'L', error: '' }) as const

describe('refKey', () => {
  it('ignores key order', () => {
    expect(refKey({ kind: 'image', imageUid: 'a' })).toBe(refKey({ imageUid: 'a', kind: 'image' } as KiwiRef))
    expect(refKey({ kind: 'image', imageUid: 'a' })).not.toBe(refKey({ kind: 'image', imageUid: 'b' }))
  })
})

describe('refLabel + pointTarget cover every kind', () => {
  const samples: Record<string, KiwiRef> = {
    project: { kind: 'project' }, set: { kind: 'set', setUid: 's' }, image: img,
    population: { kind: 'population', imageUid: 'i', valueName: 'B', popPath: '/Directed' },
    cells: { kind: 'cells', imageUid: 'i', valueName: 'B', labelIds: [1, 2, 3, 4, 5] },
    tracks: { kind: 'tracks', imageUid: 'i', valueName: 'B', trackIds: [12] },
    viewer: { kind: 'viewer', imageUid: 'i', t: 3, z: 7 }, plot: { kind: 'plot', plotId: 'p' },
    tile: { kind: 'tile', imageUid: 'i', valueName: 'B', cellId: 'A1' }, capture: { kind: 'capture', captureId: 'c' },
    task: { kind: 'task', funName: 'segment.cellpose' }, ui: { kind: 'ui', anchor: 'nav:/gate' },
    blackboard: { kind: 'blackboard', entryId: 'e', version: 2 },
  }
  it('has a sample per schema kind', () => expect(Object.keys(samples).sort()).toEqual([...KIWI_REF_KINDS].sort()))
  it.each(Object.values(samples))('%o → a label and a target', (r) => {
    expect(refLabel(r)).not.toBe('')
    expect(pointTarget(r).action).toBeTruthy()
  })
  it('labels read naturally', () => {
    expect(refLabel(samples.cells)).toBe('cells 1, 2, 3 +2 · B')
    expect(refLabel(samples.tracks)).toBe('track 12 · B')
    expect(refLabel(samples.viewer)).toBe('viewer i t=3 z=7')
    expect(refLabel(samples.ui)).toBe('/gate')
  })
})

describe('pointTarget', () => {
  it('tracks and cells open the viewer with the highlight', () => {
    expect(pointTarget({ kind: 'tracks', imageUid: 'i', valueName: 'B', trackIds: [4] }))
      .toEqual({ action: 'viewer', imageUid: 'i', tracks: { valueName: 'B', ids: [4] } })
    expect(pointTarget({ kind: 'viewer', imageUid: 'i', t: 0 })).toEqual({ action: 'viewer', imageUid: 'i', t: 0 })
  })
  it('a task opens its page and remembers the function', () => {
    expect(pointTarget({ kind: 'task', funName: 'cleanupImages.driftCorrect' }))
      .toEqual({ action: 'route', path: '/cleanup', rememberFn: { module: 'cleanup', task: 'driftCorrect' } })
    expect(pointTarget({ kind: 'task', funName: 'myCat.thing' })).toEqual({ action: 'route', path: '/custom/myCat' })
  })
  it('every task page is a route the app has', () => {
    for (const p of Object.values(TASK_PAGES)) expect(p.path).toMatch(/^\/[a-z-]+$/)
  })
  it('a population outlines its cells in the viewer, not a bare gating page', () => {
    expect(pointTarget({ kind: 'population', imageUid: 'i', valueName: 'B', popPath: '/qc' }))
      .toEqual({ action: 'population', imageUid: 'i', valueName: 'B', popPath: '/qc' })
  })
  it('a blackboard entry opens by query; a tile says why not', () => {
    expect(pointTarget({ kind: 'blackboard', entryId: 'e1' })).toEqual({ action: 'route', path: '/blackboard', query: { entry: 'e1' } })
    expect(pointTarget({ kind: 'tile', imageUid: 'i', valueName: 'B', cellId: 'A1' }).action).toBe('none')
  })
})

describe('chipState (Decision 10)', () => {
  it('never claims more than was checked', () => {
    expect(chipState(ok()).tone).toBe('ok')
    expect(chipState(ok()).tip).toMatch(/not checked against the claim/)
    expect(chipState(ok('live')).tone).toBe('soft')
    expect(chipState(ok('format')).tone).toBe('soft')
    expect(chipState(undefined).tone).toBe('soft')
  })
  it('fails a ref that does not resolve, or was not seen this turn', () => {
    expect(chipState({ ok: false, check: 'exists', label: '', error: 'no image zz' })).toEqual({ tone: 'fail', tip: 'no image zz' })
    expect(chipState(ok(), false).tone).toBe('fail')
  })
})

describe('draft', () => {
  it('adds once, removes, and resets for another project', () => {
    let d = draftAdd({ projectUid: '', refs: [] }, 'P1', img)
    d = draftAdd(d, 'P1', { imageUid: 'KDIeEm', kind: 'image' } as KiwiRef)
    expect(d.refs).toHaveLength(1)
    expect(draftRemove(d, img).refs).toHaveLength(0)
    expect(draftAdd(d, 'P2', { kind: 'project' })).toEqual({ projectUid: 'P2', refs: [{ kind: 'project' }] })
  })
  it('parses defensively', () => {
    expect(parseDraft(null)).toEqual({ projectUid: '', refs: [] })
    expect(parseDraft('{"projectUid":1}')).toEqual({ projectUid: '', refs: [] })
    expect(parseDraft('{"projectUid":"P","refs":[{"kind":"project"}]}').refs).toHaveLength(1)
  })
})

const T = (over: Partial<KiwiTurn>): KiwiTurn => ({
  turnId: 't1', projectUid: 'P', prompt: 'q', refs: [], reasoning: false, model: 'sonnet', engine: 'E',
  status: 'done', startedAt: '', steps: [], ...over,
})

describe('turns', () => {
  it('upserts by id', () => {
    const a = upsertTurn([], T({}))
    expect(upsertTurn(a, T({ status: 'failed' }))).toEqual([T({ status: 'failed' })])
    expect(upsertTurn(a, T({ turnId: 't2' }))).toHaveLength(2)
  })
  it('meta line', () => {
    const reply = { ok: true, abstain: false, claims: [], errors: [], reasked: true, reasoning: '',
                    usage: { input: 0, output: 0 }, toolCalls: 1, seconds: 41.6 }
    expect(turnMeta(T({ reply, reasoning: true }))).toBe('42s · 1 look · re-asked · sonnet · thought first')
    expect(turnMeta(T({ reply: { ...reply, seconds: 120.6, usage: { input: 18, output: 10468 } } })))
      .toBe('2m 01s · 1 look · re-asked · sonnet · 10.5k tokens')
    expect(turnMeta(T({ model: '' }))).toBe('')
  })
})

describe('searchRefs', () => {
  const src = {
    sets: [{ uid: 'obWDNS', name: 'MERTK crop', images: [{ uid: 'nG1jSi', name: 'M2c-MERTK' }, { uid: 'x4E5HU', name: 'other' }] }],
    tasks: [{ fun_name: 'tracking.bayesian_tracking', label: 'Bayesian tracking' }],
  }
  it('empty query finds nothing', () => expect(searchRefs('  ', src)).toEqual([]))
  it('ranks name prefixes first, then substrings, across kinds', () => {
    const r = searchRefs('mertk', src)
    expect(r.map(c => c.ref.kind)).toEqual(['set', 'image'])
    expect(searchRefs('x4e5', src)[0].ref).toEqual({ kind: 'image', imageUid: 'x4E5HU' })
    expect(searchRefs('bayes', src)[0].ref).toEqual({ kind: 'task', funName: 'tracking.bayesian_tracking' })
  })
  it('caps the list', () => expect(searchRefs('e', src, 1)).toHaveLength(1))
})

describe('viewerRefFor', () => {
  const base = { imageUid: 'I', t: 4, z: 9, plane: true }
  it('the view, with z only in 2D', () => {
    expect(viewerRefFor(base).ref).toEqual({ kind: 'viewer', imageUid: 'I', t: 4, z: 9 })
    expect(viewerRefFor({ ...base, plane: false }).ref).toEqual({ kind: 'viewer', imageUid: 'I', t: 4 })
  })
  it('the user selection wins; the assistant’s marks and other images do not', () => {
    const tracks = { imageUid: 'I', valueName: 'B', trackIds: [3, 5] }
    expect(viewerRefFor({ ...base, tracks }).ref).toEqual({ kind: 'tracks', imageUid: 'I', valueName: 'B', trackIds: [3, 5] })
    expect(viewerRefFor({ ...base, tracks }).tip).toBe('Add the 2 selected tracks to Kiwi')
    expect(viewerRefFor({ ...base, tracks: { ...tracks, origin: 'claude' } }).ref.kind).toBe('viewer')
    expect(viewerRefFor({ ...base, tracks: { ...tracks, imageUid: 'J' } }).ref.kind).toBe('viewer')
    expect(viewerRefFor({ ...base, cells: { imageUid: 'I', valueName: 'B', labels: [7] } }).ref)
      .toEqual({ kind: 'cells', imageUid: 'I', valueName: 'B', labelIds: [7] })
  })
})

describe('claim text + failures, as the user reads them', () => {
  it('the flag says "I think" — a written-out one is not doubled', () => {
    expect(claimText({ kind: 'interpretation', text: 'I think no single measure is best', refs: [] })).toBe('No single measure is best')
    expect(claimText({ kind: 'observation', text: 'I think is fine here', refs: [] })).toBe('I think is fine here')
  })
  it('errors lose the raw ref JSON', () => {
    const errs = ['claim 1: ref {"kind":"plot","plotId":"summary:b:X:4"} — that plot isn’t open any more',
                  'claim 1 is more than one fact (longer than 160 characters) — split it, one fact per claim',
                  'claim 19: ref {"kind":"image","imageUid":"Q"} was not in any tool result or attachment this turn']
    expect(errs.map(plainError)).toEqual(['Claim 1: that plot isn’t open any more',
                                          'Claim 1 is more than one fact (longer than 160 characters)',
                                          'Claim 19: cites something Kiwi didn’t look at'])
  })
})

describe('attachmentRows', () => {
  const plot: KiwiRef = { kind: 'plot', plotId: 'summary:b:X:4' }
  it('the resolver’s label + detail once it has answered, the ref’s own label until then', () => {
    expect(attachmentRows([plot], {})[0]).toMatchObject({ kind: 'plot', label: 'plot', detail: '', tip: 'Not checked yet' })
    const res = { ok: true, check: 'live', label: 'Track measures · straightness', error: '',
                  detail: 'B/qc, T/qc · MERTK · 12 images' } as const
    expect(attachmentRows([plot], { [refKey(plot)]: res })[0])
      .toMatchObject({ label: 'Track measures · straightness', detail: 'B/qc, T/qc · MERTK · 12 images' })
    const gone = { ok: false, check: 'live', label: '', error: 'that plot isn’t open any more' } as const
    expect(attachmentRows([plot], { [refKey(plot)]: gone })[0]).toMatchObject({ label: 'plot', detail: 'that plot isn’t open any more' })
  })
})

describe('claimRows', () => {
  it('numbers claims like the errors do, trims the flag, marks a failed ref', () => {
    const good = { ref: img, result: ok(), seen: true }
    const bad = { ref: img, result: ok(), seen: false }
    const reply = { ok: false, abstain: false, errors: [], reasked: false, reasoning: '', usage: { input: 0, output: 0 },
                    toolCalls: 0, seconds: 0,
                    claims: [{ kind: 'observation' as const, text: 'One image.', refs: [good] },
                             { kind: 'interpretation' as const, text: 'I think it is fine', refs: [bad] }] }
    expect(claimRows(reply).map(r => [r.n, r.text, r.failed])).toEqual([[1, 'One image.', false], [2, 'It is fine', true]])
    // the failed claim's tip names its ref problem; a shape problem is never shown
    const errs = ['claim 2: ref {"kind":"image","imageUid":"KDIeEm"} was not in any tool result or attachment this turn',
                  'claim 2 is more than one fact (longer than 160 characters) — split it, one fact per claim',
                  'claim 12: ref {"kind":"image","imageUid":"Q"} — gone']
    const rows = claimRows({ ...reply, errors: errs, shapeErrors: [errs[1]] })
    expect(rows[1].tip).toBe('Claim 2: cites something Kiwi didn’t look at')
    expect(rows[0].tip).toBe('')
  })
})
