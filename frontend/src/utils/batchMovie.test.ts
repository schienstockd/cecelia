import { describe, it, expect } from 'vitest'
import { clampContour, LABEL_CONTOUR_MAX, buildBatchMovieConfig, movieFilename, seedConfigFromViewState, defaultChannelSeed, MOVIE_CHANNELS_TOKEN, safeNamePart } from './batchMovie'

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

  it('defaults the title card ON with a 3s duration', () => {
    const c = buildBatchMovieConfig({}, [], {})
    expect(c.titleCard).toEqual({ enabled: true, note: '', durationSec: 3 })
  })

  it('passes the title card through and clamps duration to 1–10s', () => {
    expect(buildBatchMovieConfig({ titleCard: { enabled: false, note: 'day 3', durationSec: 5 } }, [], {}).titleCard)
      .toEqual({ enabled: false, note: 'day 3', durationSec: 5 })
    expect(buildBatchMovieConfig({ titleCard: { enabled: true, note: '', durationSec: 99 } }, [], {}).titleCard.durationSec).toBe(10)
    expect(buildBatchMovieConfig({ titleCard: { enabled: true, note: '', durationSec: 0 } }, [], {}).titleCard.durationSec).toBe(1)
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

describe('buildBatchMovieConfig — segmentation masks', () => {
  it('always sends a mask list, so an authored batch means what it says', () => {
    // ABSENT and EMPTY differ on the backend: absent leaves the canvas alone, empty is "no masks".
    // A batch config is authored, so it must be explicit — otherwise a user who cleared the picker
    // would still get whatever masks happened to be on screen for the first image.
    expect(buildBatchMovieConfig({}, [], {}).labelValueNames).toEqual([])
    expect(buildBatchMovieConfig({ labelValueNames: ['cellpose'] }, [], {}).labelValueNames)
      .toEqual(['cellpose'])
  })

  it('keeps the mask order — the chip order is the column order', () => {
    expect(buildBatchMovieConfig({ labelValueNames: ['coastal', 'cellpose'] }, [], {}).labelValueNames)
      .toEqual(['coastal', 'cellpose'])
  })
})

describe('clampContour / labelContour', () => {
  it('clamps rather than rejects — a bad outline must not fail a whole batch', () => {
    expect(clampContour(undefined)).toBe(0)
    expect(clampContour(-4)).toBe(0)
    expect(clampContour(999)).toBe(LABEL_CONTOUR_MAX)
    expect(clampContour(2.7)).toBe(3)
  })

  it('rides the batch config, defaulting to filled', () => {
    expect(buildBatchMovieConfig({}, [], {}).labelContour).toBe(0)
    expect(buildBatchMovieConfig({ labelContour: 3 }, [], {}).labelContour).toBe(3)
    expect(buildBatchMovieConfig({ labelContour: -1 }, [], {}).labelContour).toBe(0)
  })
})

// Mirrors the Julia `_safe_name_part` testset (api/test/runtests.jl) — the two sanitisers must agree,
// or the filename the batch panel PREVIEWS is not the one the recorder writes.
// napari's own 3D choice is the COARSEST pyramid level, which erases a strided label pyramid — so an
// authored batch config says "full resolution" rather than leaving it unsaid. See docs/NAPARI.md.
describe('buildBatchMovieConfig 3D detail', () => {
  const build = (cfg: Record<string, unknown>) =>
    buildBatchMovieConfig(cfg, ['segA'], {})
  it('sends full resolution by default in 3D', () => {
    expect(build({ show3D: true }).detail3d).toBe(0)
  })
  it('carries an explicitly chosen level', () => {
    expect(build({ show3D: true, detail3d: 2 }).detail3d).toBe(2)
  })
  it('sends nothing in 2D — the level only applies to a volumetric render', () => {
    expect(build({ show3D: false, detail3d: 2 }).detail3d).toBeNull()
  })
})

describe('safeNamePart', () => {
  it('drops the separator a trailing bracket leaves behind', () => {
    // the reported one: an image named "… -res (cropped)" showed up as "…-res_cropped_"
    expect(safeNamePart('M2b-MERTK_KAT-SWHL-GFP-Tom-res (cropped)'))
      .toBe('M2b-MERTK_KAT-SWHL-GFP-Tom-res_cropped')
  })
  it('keeps the characters a filename may hold, collapses the rest', () => {
    expect(safeNamePart('a/b c:d')).toBe('a_b_c_d')
    expect(safeNamePart('Day 3.v2-final')).toBe('Day_3.v2-final')
  })
  it('strips leading separators and dots too', () => {
    expect(safeNamePart('../../etc/passwd')).toBe('etc_passwd')
    expect(safeNamePart('__x__')).toBe('x')
  })
  it('a name with nothing usable in it comes back empty', () => {
    expect(safeNamePart('   ')).toBe('')
    expect(safeNamePart('()')).toBe('')
  })
})
