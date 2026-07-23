import { describe, it, expect } from 'vitest'
import { buildBatchMovieConfig, movieFilename, seedConfigFromViewState, defaultChannelSeed, MOVIE_CHANNELS_TOKEN } from './batchMovie'

describe('buildBatchMovieConfig', () => {
  it('fills defaults for an empty config', () => {
    const c = buildBatchMovieConfig({}, ['A', 'B'], {})
    expect(c.valueName).toBe('')
    expect(c.channels).toEqual({})
    expect(c.colourBy).toBe('')
    expect(c.showTracks).toBe(false)
    expect(c.trackValueNames).toEqual([])   // tracks off → no segmentations sent
    expect(c.tailWidth).toBe(4)
    expect(c.popType).toBe('flow')
    expect(c.pointsSize).toBe(6)
  })

  it('sends ALL segmentations when tracks are on', () => {
    const c = buildBatchMovieConfig({ showTracks: true }, ['segA', 'segB'], {})
    expect(c.showTracks).toBe(true)
    expect(c.trackValueNames).toEqual(['segA', 'segB'])
  })

  it('passes through channels, colour-by and overlay flags', () => {
    const c = buildBatchMovieConfig({
      channels: { Tcells: 'green', SHG: 'bop purple' },
      colourBy: 'live.cell.track.clusters.movement',
      showTrackclust: true, colourLabels: true, popType: 'clust', pointsSize: 10, tailWidth: 8,
    }, [], { '2': '#ff1493' })
    expect(c.channels).toEqual({ Tcells: 'green', SHG: 'bop purple' })
    expect(c.colourBy).toBe('live.cell.track.clusters.movement')
    expect(c.showTrackclust).toBe(true)
    expect(c.colourLabels).toBe(true)
    expect(c.popType).toBe('clust')
    expect(c.pointsSize).toBe(10)
    expect(c.tailWidth).toBe(8)
    expect(c.colourOverrides).toEqual({ '2': '#ff1493' })
  })
})

describe('movieFilename', () => {
  const attrs = { Day: '3', Treatment: 'CNO', Blank: '  ' }
  it('joins attrs in order and terminates with the uid', () => {
    expect(movieFilename(['Day', 'Treatment'], attrs, 'AbC123')).toBe('3_CNO_AbC123.mp4')
  })
  it('falls back to just the uid with no attrs', () => {
    expect(movieFilename([], attrs, 'AbC123')).toBe('AbC123.mp4')
  })
  it('drops blank / missing attr values (no dangling separator)', () => {
    expect(movieFilename(['Blank', 'Missing', 'Day'], attrs, 'AbC123')).toBe('3_AbC123.mp4')
  })
  it('sanitises unsafe characters to underscores', () => {
    expect(movieFilename(['T'], { T: 'a/b c:d' }, 'u1')).toBe('a_b_c_d_u1.mp4')
  })
  it('expands the channels token to the shown channel names joined by "-", in token position', () => {
    const chans = ['CD3', 'CD8']
    expect(movieFilename(['Day', MOVIE_CHANNELS_TOKEN], attrs, 'AbC123', chans)).toBe('3_CD3-CD8_AbC123.mp4')
    expect(movieFilename([MOVIE_CHANNELS_TOKEN, 'Treatment'], attrs, 'AbC123', chans)).toBe('CD3-CD8_CNO_AbC123.mp4')
  })
  it('drops the channels token cleanly when no channels are shown', () => {
    expect(movieFilename(['Day', MOVIE_CHANNELS_TOKEN], attrs, 'AbC123', [])).toBe('3_AbC123.mp4')
  })
})

describe('seedConfigFromViewState', () => {
  it('takes visible channels + their colormap, skips hidden ones', () => {
    const vs = { layers: {
      Tcells: { colormap: 'green', visible: true },
      SHG:    { colormap: 'bop purple', visible: true },
      DAPI:   { colormap: 'blue', visible: false },      // hidden → not seeded
    } }
    const seed = seedConfigFromViewState(vs, ['Tcells', 'SHG', 'DAPI', 'Absent'])
    expect(seed.channels).toEqual({ Tcells: 'green', SHG: 'bop purple' })
  })
  it('detects overlays from the layer-name prefixes (distinguishes track vs trackclust)', () => {
    const vs = { layers: {
      Tcells: { colormap: 'green', visible: true },
      '(trackclust) (A) Tracks /meandering': { visible: true },
      '(flow) (A) /tcells': { visible: true },
    } }
    const seed = seedConfigFromViewState(vs, ['Tcells'])
    expect(seed.showTrackclust).toBe(true)
    expect(seed.showTracks).toBeUndefined()             // '(track)' ≠ '(trackclust)'
    expect(seed.showPopulations).toBe(true)
    expect(seed.popType).toBe('flow')
  })
  it('is safe on an empty / missing view state', () => {
    expect(seedConfigFromViewState(null, ['A']).channels).toEqual({})
    expect(seedConfigFromViewState({}, ['A']).channels).toEqual({})
  })
})

describe('defaultChannelSeed', () => {
  it('assigns palette colours in order, wrapping when channels exceed the palette', () => {
    expect(defaultChannelSeed(['a', 'b', 'c'], ['red', 'green'])).toEqual({ a: 'red', b: 'green', c: 'red' })
    expect(defaultChannelSeed(['a'], [])).toEqual({})
  })
})
