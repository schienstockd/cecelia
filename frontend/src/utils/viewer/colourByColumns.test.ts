import { describe, it, expect } from 'vitest'
import { allColourBy, colourByColumns, colourByGroups } from './colourByColumns'

const cols = (cells: string[], markers: string[] = [], afMarkers: string[] = []) => ({ cells, markers, afMarkers })

describe('colourByColumns', () => {
  it('cell obs columns first, then track ones, once each; markers apart', () => {
    expect(colourByColumns({ obsColumns: ['hmm', 'clusters.a'], trackColourColumns: ['clusters.a', 'clusters.t'],
                             markerColumns: ['CD4', 'nuc_CD4', 'af_CD4'],
                             channels: ['mean_intensity_0', 'nuc_mean_intensity_0', 'af_mean_intensity_0'] }))
      .toEqual({ cells: ['hmm', 'clusters.a', 'clusters.t'], markers: ['CD4', 'nuc_CD4'], afMarkers: ['af_CD4'] })
    expect(colourByColumns({})).toEqual({ cells: [], markers: [], afMarkers: [] })
  })
  it('AF-corrected is decided on the RAW column, not the name', () => {
    // a channel literally named `af_tag` is a measured marker; a raw `af_` column is a corrected one
    expect(colourByColumns({ markerColumns: ['af_tag', 'af_CD8'], channels: ['mean_intensity_3', 'af_mean_intensity_1'] }))
      .toMatchObject({ markers: ['af_tag'], afMarkers: ['af_CD8'] })
  })
  it('a marker named like a cell column is offered once, as the cell column', () => {
    expect(colourByColumns({ obsColumns: ['area'], markerColumns: ['area', 'CD8'] }).markers).toEqual(['CD8'])
  })
  it('every column, cells first', () => {
    expect(allColourBy(cols(['hmm'], ['CD4'], ['af_CD4']))).toEqual(['hmm', 'CD4', 'af_CD4'])
  })
})

describe('colourByGroups', () => {
  it('offers the default, then the lens scores by name, then every other column, then the markers', () => {
    const g = colourByGroups(cols(['area', 'spatial.lens.lens1', 'regions.default', 'spatial.lens.crypt'], ['CD4', 'CD8'], ['af_CD4']))
    expect(g.map(x => x.title)).toEqual([undefined, 'Lens scores', 'Cell measures', 'Markers', 'AF-corrected markers'])
    expect(g[0]!.options).toEqual([{ value: '', label: 'Populations' }])
    expect(g[1]!.options).toEqual([{ value: 'spatial.lens.lens1', label: 'lens1' },
                                   { value: 'spatial.lens.crypt', label: 'crypt' }])
    expect(g[2]!.options.map(o => o.value)).toEqual(['area', 'regions.default'])
    expect(g[3]!.options).toEqual([{ value: 'CD4', label: 'CD4' }, { value: 'CD8', label: 'CD8' }])
    expect(g[4]!.options).toEqual([{ value: 'af_CD4', label: 'af_CD4' }])
    expect(colourByGroups(cols(['area'])).map(x => x.title)).toEqual([undefined, 'Cell measures'])
    expect(colourByGroups(cols([]), {}, 'population')[0]!.options).toEqual([{ value: '', label: 'population' }])
  })
  it('a renamed lens is listed by the name it was given', () => {
    const g = colourByGroups(cols(['spatial.lens.lens1', 'spatial.lens.lens2']), { 'spatial.lens.lens1': 'test1' })
    expect(g[1]!.options).toEqual([{ value: 'spatial.lens.lens1', label: 'test1' }, { value: 'spatial.lens.lens2', label: 'lens2' }])
  })
})
