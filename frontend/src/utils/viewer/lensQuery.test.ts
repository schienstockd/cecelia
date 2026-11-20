import { describe, expect, it } from 'vitest'
import { applyLensLook, lensLabel, lensNamesOf, figureLensBody, lensFigureSpec, lensFiguresFor, lensLookOf, lensOpenPending, lensQueryParams,
         lensScoreColumn, nextLensName, regionRunsIn, type FigureLens, type LensLook, type LensLookTarget } from './lensQuery'

describe('lensQuery', () => {
  it('finds region runs: regions.<suffix> only, not the composition columns', () => {
    expect(regionRunsIn('cells', ['regions.default', 'spatial.comp.cells_B.default', 'clusters.a', 'regions.win50']))
      .toEqual([{ vn: 'cells', suffix: 'default' }, { vn: 'cells', suffix: 'win50' }])
  })
  it('names the column as Julia lens_score_col does', () => {
    expect(lensScoreColumn('crypt rim')).toBe('spatial.lens.crypt_rim')
    expect(lensScoreColumn('lens2')).toBe('spatial.lens.lens2')
  })
  it('the next name skips the taken ones, so a query never overwrites another', () => {
    expect(nextLensName([])).toBe('lens1')
    expect(nextLensName(['spatial.lens.lens1', 'spatial.lens.lens3'])).toBe('lens2')
    // a lens figure whose column is gone still holds its name
    expect(nextLensName(['spatial.lens.lens1'], ['spatial.lens.lens2'])).toBe('lens3')
  })
  it('params: rounded level-0 px, radius at least 1, -1 for no frame', () => {
    expect(lensQueryParams({ vn: 'cells', suffix: 'default' }, { x: 10.4, y: 20.6, r: 0.3 }, null, 'lens1'))
      .toEqual({ valueName: 'cells', regionSuffix: 'default', lensX: 10, lensY: 21, lensR: 1, lensT: -1, outputName: 'lens1' })
  })
})

describe('lens figures', () => {
  const look: LensLook = { view: { camera: { zoom: 2 } }, labelVisible: { cells: true }, popVisible: { flow: true },
                           popEyes: {}, ribbonEyes: {}, trackVisible: {}, graphVisible: {}, showGatedTracks: false }
  const lens: FigureLens = { sourceUid: 'SRC', x: 100, y: 200, r: 40, t: -1, column: 'spatial.lens.lens2', look }
  it('opening restores the view it was found in and re-pins the lens with its look — scores stay off', () => {
    expect(lensOpenPending(lens)).toEqual({ viewState: look.view, lens: { x: 100, y: 200, r: 40, look }, imageUid: 'SRC' })
    expect(lensOpenPending({ ...lens, t: 3 }).focus).toEqual({ t: 3 })
    expect(lensOpenPending({ ...lens, look: undefined })).toEqual({ lens: { x: 100, y: 200, r: 40 }, imageUid: 'SRC' })
  })
  it('crops to the lens as drawn — its diameter, the outline just inside — never tiny', () => {
    const base = { center: [0, 0] as [number, number], sizePx: 500, frames: [0] }
    expect(lensFigureSpec(base, { x: 100.4, y: 200.6, r: 40 })).toEqual(
      { center: [201, 100], sizePx: 84, frames: [0], circles: [{ x: 100, y: 201, r: 40 }] })
    expect(lensFigureSpec(base, { x: 0, y: 0, r: 2 }).sizePx).toBe(16)
  })
  it('the figure carries the lens, the column it scored and the look', () => {
    expect(figureLensBody({ vn: 'cells', suffix: 'default' }, { x: 10.6, y: 4.2, r: 0.3 }, null, 'spatial.lens.lens1'))
      .toEqual({ x: 11, y: 4, r: 1, t: -1, column: 'spatial.lens.lens1', valueName: 'cells', regionSuffix: 'default' })
    expect(figureLensBody({ vn: 'c', suffix: 's' }, { x: 1, y: 1, r: 1 }, 2, 'spatial.lens.x', look).look).toBe(look)
  })
  it('lists the lens figures whose scores are on this image, newest first, by display name else lens name', () => {
    const m = (name: string, mtime: number, l: Partial<FigureLens> | null, displayName = '') =>
      ({ name, mtime, displayName, lens: l ? { ...lens, ...l } : null })
    const rows = lensFiguresFor([m('a.png', 1, { images: ['SRC', 'B'] }), m('plain.png', 9, null),
                                 m('b.png', 5, { column: 'spatial.lens.crypt', images: ['SRC'] }, 'Crypt edge'),
                                 m('c.png', 7, { images: ['OTHER'] })], 'SRC')
    expect(rows.map(r => [r.name, r.label])).toEqual([['b.png', 'Crypt edge'], ['a.png', 'lens2']])
    expect(lensFiguresFor([m('a.png', 1, { images: undefined })], 'SRC').length).toBe(1)   // no list → its own image
  })
  it('the look is what was toggled, as plain data: one segmentation shown, the switches, the eyes', () => {
    const live = { viewState: { a: 1 }, maskValueName: 'cells', labelNames: ['cells', 'nuclei'], popVisible: { flow: true, clust: false },
                   popEyes: { cells: { '/T': true } }, ribbonEyes: {}, trackVisible: { cells: true }, graphVisible: {},
                   showGatedTracks: true }
    const got = lensLookOf(live)
    expect(got.labelVisible).toEqual({ cells: true, nuclei: false })
    expect(got.popEyes).toEqual({ cells: { '/T': true } }); expect(got.popEyes).not.toBe(live.popEyes)
    expect(got.showGatedTracks).toBe(true)
  })
  it('applying a look writes each switch back', () => {
    const calls: string[] = []
    const t: LensLookTarget = {
      setLabelVisibility: (i, v) => calls.push(`label ${i} ${JSON.stringify(v)}`),
      setPopVisible: (s, pt, v) => calls.push(`pop ${s} ${pt} ${v}`),
      setPopEyes: (_i, vn, e, layer) => calls.push(`eyes ${layer} ${vn} ${JSON.stringify(e)}`),
      setTrackVisibility: (_i, v) => calls.push(`tracks ${JSON.stringify(v)}`),
      setGraphVisibility: (_i, v) => calls.push(`graphs ${JSON.stringify(v)}`),
      setShowGatedTracks: (_s, v) => calls.push(`gated ${v}`),
    }
    applyLensLook({ ...look, popEyes: { cells: { '/T': true } }, trackVisible: { cells: true } }, 'IMG', 'SET', t)
    expect(calls).toEqual(['label IMG {"cells":true}', 'pop SET flow true', 'gated false',
                           'eyes points cells {"/T":true}', 'tracks {"cells":true}'])
  })
})

describe('lensNamesOf / lensLabel', () => {
  const lens = (column: string) => ({ sourceUid: 'a', x: 1, y: 1, r: 1, column })
  it('names a score column as its figure was renamed; the newest wins; else lensN', () => {
    const names = lensNamesOf([
      { displayName: 'old', mtime: 1, lens: lens('spatial.lens.lens1') },
      { displayName: 'test1', mtime: 2, lens: lens('spatial.lens.lens1') },
      { displayName: '', mtime: 3, lens: lens('spatial.lens.lens2') },
      { displayName: 'a movie', mtime: 4 },
    ])
    expect(names).toEqual({ 'spatial.lens.lens1': 'test1' })
    expect(lensLabel('spatial.lens.lens1', names)).toBe('test1')
    expect(lensLabel('spatial.lens.lens2', names)).toBe('lens2')
  })
})
