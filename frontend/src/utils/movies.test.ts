import { describe, it, expect } from 'vitest'
import { movieStreamUrl, movieDisplayName, sortMovies, anchoredScroll, movieRows,
         filterMovieRows, movieFilterOptions, parseMovieTags,
         resolveMovieImageUid, movieChannelCells, movieChannelCount,
         type MovieEntry, type MovieImage } from './movies'

describe('movieStreamUrl', () => {
  it('builds the range-serve URL with encoded params', () => {
    expect(movieStreamUrl('NRUBxU', 'myImage_animation.mp4'))
      .toBe('/api/movies/file?projectUid=NRUBxU&name=myImage_animation.mp4')
  })
  it('encodes names/uids that need escaping', () => {
    expect(movieStreamUrl('p 1', 'a b.mp4')).toBe('/api/movies/file?projectUid=p%201&name=a%20b.mp4')
  })
})

describe('movieDisplayName', () => {
  it('drops the .mp4 extension', () => {
    expect(movieDisplayName('gBT_animation.mp4')).toBe('gBT_animation')
    expect(movieDisplayName('CASE.MP4')).toBe('CASE')
  })
  it('leaves a name without extension alone', () => {
    expect(movieDisplayName('plain')).toBe('plain')
  })
  it('prefers the display name when the entry has one', () => {
    expect(movieDisplayName({ name: 'gBT.mp4', size: 1, mtime: 1, displayName: 'Day 3 CNO' }))
      .toBe('Day 3 CNO')
  })
  it('falls back to the file name when the display name is blank or whitespace', () => {
    // clearing an inline edit sends '', which must read as "not set" rather than as an empty label
    expect(movieDisplayName({ name: 'gBT.mp4', size: 1, mtime: 1, displayName: '' })).toBe('gBT')
    expect(movieDisplayName({ name: 'gBT.mp4', size: 1, mtime: 1, displayName: '   ' })).toBe('gBT')
  })
  it('reads a pre-registry entry exactly as before', () => {
    expect(movieDisplayName({ name: 'gBT.mp4', size: 1, mtime: 1 })).toBe('gBT')
  })
})

describe('filterMovieRows', () => {
  const rows = movieRows([
    { name: 'a.mp4', size: 1, mtime: 3, starred: true,  tags: ['figure 2'], producedBy: 'animation' },
    { name: 'b.mp4', size: 1, mtime: 2, starred: false, tags: ['draft'],    producedBy: 'batch' },
    { name: 'c.mp4', size: 1, mtime: 1 },   // pre-registry: no star, no tags, no producer
  ], String, String)

  it('passes everything through when nothing is active', () => {
    expect(filterMovieRows(rows, false, []).map(r => r.name)).toEqual(['a.mp4', 'b.mp4', 'c.mp4'])
  })
  it('starred-only keeps just the starred rows', () => {
    expect(filterMovieRows(rows, true, []).map(r => r.name)).toEqual(['a.mp4'])
  })
  it('tags are ANY-of, not ALL-of', () => {
    expect(filterMovieRows(rows, false, ['figure 2', 'draft']).map(r => r.name))
      .toEqual(['a.mp4', 'b.mp4'])
  })
  it('a producer filters from the same list as a tag', () => {
    expect(filterMovieRows(rows, false, ['batch']).map(r => r.name)).toEqual(['b.mp4'])
  })
  it('star and tags COMPOSE — a row must pass both', () => {
    expect(filterMovieRows(rows, true, ['draft']).map(r => r.name)).toEqual([])
    expect(filterMovieRows(rows, true, ['figure 2']).map(r => r.name)).toEqual(['a.mp4'])
  })
})

describe('movieFilterOptions', () => {
  it('collects the tags and producers in use, sorted and deduped', () => {
    expect(movieFilterOptions([
      { name: 'a.mp4', size: 1, mtime: 1, tags: ['zeta', 'alpha'], producedBy: 'batch' },
      { name: 'b.mp4', size: 1, mtime: 1, tags: ['alpha'],         producedBy: 'animation' },
      { name: 'c.mp4', size: 1, mtime: 1 },
    ])).toEqual({ tags: ['alpha', 'zeta'], producers: ['animation', 'batch'] })
  })
  it('is empty for a project whose movies all predate the registry', () => {
    expect(movieFilterOptions([{ name: 'a.mp4', size: 1, mtime: 1 }]))
      .toEqual({ tags: [], producers: [] })
  })
})

describe('parseMovieTags', () => {
  it('splits on commas and newlines, trims, and dedupes in order', () => {
    expect(parseMovieTags(' figure 2, draft ,figure 2\nfinal ')).toEqual(['figure 2', 'draft', 'final'])
  })
  it('collapses inner whitespace, so one tag cannot be two things', () => {
    expect(parseMovieTags('figure   2')).toEqual(['figure 2'])
  })
  it('is empty for blank input', () => {
    expect(parseMovieTags('   ,  , ')).toEqual([])
  })
})

describe('anchoredScroll', () => {
  const vp = { w: 1000, h: 500 }

  it('no scroll while the content still fits after zoom', () => {
    // fit (800×400) zoomed to just-fits (1000×500) → both axes fit → no scroll
    expect(anchoredScroll({ w: 800, h: 400 }, { w: 1000, h: 500 }, vp, { x: 500, y: 250 }, { left: 0, top: 0 }))
      .toEqual({ left: 0, top: 0 })
  })

  it('zooming about the centre keeps the centre fixed', () => {
    // centred fit 1000×500 → 2000×1000, focal = viewport centre → scroll to keep centre centred
    const s = anchoredScroll({ w: 1000, h: 500 }, { w: 2000, h: 1000 }, vp, { x: 500, y: 250 }, { left: 0, top: 0 })
    expect(s).toEqual({ left: 500, top: 250 })   // (0.5*2000 - 500), (0.5*1000 - 250)
  })

  it('zooming about the left edge keeps the left edge fixed', () => {
    const s = anchoredScroll({ w: 1000, h: 500 }, { w: 2000, h: 1000 }, vp, { x: 0, y: 0 }, { left: 0, top: 0 })
    expect(s).toEqual({ left: 0, top: 0 })       // fraction 0 under x=0 → no left scroll
  })

  it('clamps to the scrollable range', () => {
    // focal at far right → would want scroll beyond max; clamped to a-v = 1000
    const s = anchoredScroll({ w: 1000, h: 500 }, { w: 2000, h: 1000 }, vp, { x: 1000, y: 500 }, { left: 0, top: 0 })
    expect(s).toEqual({ left: 1000, top: 500 })
  })

  it('accounts for existing scroll when already overflowing', () => {
    // already zoomed (content 2000 wide, scrolled 500) → zoom to 4000 about centre keeps centre point
    const s = anchoredScroll({ w: 2000, h: 1000 }, { w: 4000, h: 2000 }, vp, { x: 500, y: 250 }, { left: 500, top: 250 })
    // content point under centre before = 500+500=1000 (frac 0.5) → after 0.5*4000 - 500 = 1500
    expect(s).toEqual({ left: 1500, top: 750 })
  })
})

describe('sortMovies', () => {
  it('orders newest-first, name as tiebreak, without mutating input', () => {
    const list: MovieEntry[] = [
      { name: 'b.mp4', size: 1, mtime: 100 },
      { name: 'a.mp4', size: 1, mtime: 200 },
      { name: 'c.mp4', size: 1, mtime: 200 },
    ]
    const sorted = sortMovies(list)
    expect(sorted.map(m => m.name)).toEqual(['a.mp4', 'c.mp4', 'b.mp4'])
    expect(list[0].name).toBe('b.mp4')   // original untouched
  })
})

describe('movieRows', () => {
  const movies: MovieEntry[] = [
    { name: 'big_old.mp4',  size: 2_000_000, mtime: 1_000 },
    { name: 'small_new.mp4', size:   900_000, mtime: 9_000 },
  ]
  const rows = movieRows(movies, b => `${Math.round(b / 1000)} kB`, t => `at ${t}`)

  it('carries a display string AND the raw value each formatted column sorts by', () => {
    // the point of the raw fields: "900 kB" sorts ABOVE "2000 kB" as text, and a locale date sorts
    // alphabetically by month. The table must sort on `size`/`mtime`, never on what it renders.
    expect(rows[0]).toEqual({ name: 'big_old.mp4', label: 'big_old',
                              sizeText: '2000 kB', size: 2_000_000,
                              timeText: 'at 1000', mtime: 1_000,
                              // a pre-registry movie carries the registry fields at their "unset"
                              // values, so the table reads ONE row shape and never has to branch
                              starred: false, tags: [], tagText: '', producedBy: '',
                              renamed: false, configStale: false,
                              hasConfig: false, configKind: '',
                              // …including the image join, which resolves to nothing when no images
                              // were handed in — the page reads one row shape either way
                              imageUid: '', imageName: '',
                              imageChannels: [], movieChannels: [], attr: {} })
    expect(rows[1].size).toBe(900_000)
    expect(rows[1].mtime).toBe(9_000)
  })

  // What the edit action keys off (Phase 6): the row says whether there is a config and which page
  // owns its kind, so the table never looks a movie up a second time to decide whether to offer it.
  it('carries whether a generation config was banked, and of which kind', () => {
    const [r] = movieRows([{ name: 'a.mp4', size: 1, mtime: 1, hasConfig: true, configKind: 'keyframes' }],
                          String, String)
    expect(r.hasConfig).toBe(true)
    expect(r.configKind).toBe('keyframes')
  })

  it('keys each row by the FILE name (what the player streams), not the label', () => {
    expect(rows.map(r => r.name)).toEqual(['big_old.mp4', 'small_new.mp4'])
    expect(rows.map(r => r.label)).toEqual(['big_old', 'small_new'])
  })

  it('preserves the order it is given — sortMovies decides the default, the table re-sorts', () => {
    expect(movieRows(sortMovies(movies), String, String).map(r => r.name))
      .toEqual(['small_new.mp4', 'big_old.mp4'])     // newest first
  })
})

// The join that lets the movie list show an image's channels and attributes (docs/UI.md → Movies).
// Everything below is about movies recorded BEFORE the registry banked a uid, which is all of them in
// an existing project — so the filename fallback is the part that has to be right.
describe('resolveMovieImageUid', () => {
  const images: MovieImage[] = [
    { uid: 'zolIMa', name: 'ctrl 4h' },
    { uid: 'fXgbTl', name: 'treated 4h' },
  ]

  it('prefers the uid the registry banked', () => {
    expect(resolveMovieImageUid('anything.mp4', 'fXgbTl', images)).toBe('fXgbTl')
  })

  it('finds the uid a BATCH filename terminates with, around attrs and a user suffix', () => {
    // _movie_basename: <attr1>_<attr2>_..._<uid>[_suffix].mp4
    expect(resolveMovieImageUid('control_4h_zolIMa.mp4', '', images)).toBe('zolIMa')
    expect(resolveMovieImageUid('control_4h_zolIMa_corrected.mp4', '', images)).toBe('zolIMa')
  })

  it('finds the image a VIEWER filename STARTS with — the recorders name from opposite ends', () => {
    // _movie_named_path: <safe image name>[_suffix][_animation].mp4
    expect(resolveMovieImageUid('ctrl_4h.mp4', '', images)).toBe('zolIMa')
    expect(resolveMovieImageUid('ctrl_4h_animation.mp4', '', images)).toBe('zolIMa')
    expect(resolveMovieImageUid('treated_4h_AF_animation.mp4', '', images)).toBe('fXgbTl')
  })

  it('takes the LONGEST name match, so a prefix does not beat the image actually named', () => {
    const two: MovieImage[] = [{ uid: 'aaa', name: 'cell' }, { uid: 'bbb', name: 'cell 2' }]
    expect(resolveMovieImageUid('cell_2.mp4', '', two)).toBe('bbb')
  })

  it('refuses to guess between images that share a name', () => {
    // two images CAN share a name; labelling a movie with the wrong one's attributes is worse than
    // labelling it with none
    const dupes: MovieImage[] = [{ uid: 'aaa', name: 'day 1' }, { uid: 'bbb', name: 'day 1' }]
    expect(resolveMovieImageUid('day_1.mp4', '', dupes)).toBe('')
  })

  it('matches nothing rather than something for a name that is in no filename', () => {
    expect(resolveMovieImageUid('some_other_movie.mp4', '', images)).toBe('')
  })

  it('keeps a banked uid whose image has since been deleted — that is still the truthful answer', () => {
    expect(resolveMovieImageUid('some_other_movie.mp4', 'gone42', images)).toBe('gone42')
  })
})

describe('movieChannelCells', () => {
  const image = ['DAPI', 'CD8', 'SHG']

  it("shows the image's channels in their own slots", () => {
    expect(movieChannelCells(image, ['CD8'], 3, 'image')).toEqual(['DAPI', 'CD8', 'SHG'])
  })

  it('blanks the slots a movie does not show, so a column stays comparable down the list', () => {
    // reading down column 2 then answers "which of these movies has CD8 in it"
    expect(movieChannelCells(image, ['CD8', 'SHG'], 3, 'movie')).toEqual(['', 'CD8', 'SHG'])
  })

  it('matches channel names leniently — the recorder banks what the napari layer was called', () => {
    expect(movieChannelCells(image, [' cd8 '], 3, 'movie')).toEqual(['', 'CD8', ''])
  })

  it('keeps a shown channel that matches no slot, past the image\'s own', () => {
    // the image's channel names were edited after the recording; dropping it would silently lose
    // what the registry actually banked
    expect(movieChannelCells(image, ['CD8', 'oldName'], 4, 'movie'))
      .toEqual(['', 'CD8', '', 'oldName'])
  })

  it('pads to the column count so every row has a cell per column', () => {
    expect(movieChannelCells(['DAPI'], [], 3, 'image')).toEqual(['DAPI', '', ''])
  })
})

describe('movieChannelCount', () => {
  it('is the widest image in image mode', () => {
    expect(movieChannelCount([{ imageChannels: ['a', 'b'], movieChannels: [] },
                              { imageChannels: ['a'], movieChannels: ['x', 'y', 'z'] }], 'image')).toBe(2)
  })

  it('makes room for shown channels that match no slot, but only in movie mode', () => {
    const rows = [{ imageChannels: ['a', 'b'], movieChannels: ['a', 'gone'] }]
    expect(movieChannelCount(rows, 'movie')).toBe(3)
    expect(movieChannelCount(rows, 'image')).toBe(2)
  })
})

describe('movieRows — the image join', () => {
  const images: MovieImage[] = [
    { uid: 'zolIMa', name: 'ctrl 4h', channelNames: ['DAPI', 'CD8'],
      attr: { Treatment: 'control', Timepoint: '4h' } },
    { uid: 'fXgbTl', name: 'treated 4h', channelNames: ['DAPI', 'CD8', 'SHG'],
      attr: { Treatment: 'anti-PD1' } },
  ]
  const rows = movieRows([{ name: 'ctrl_4h.mp4', size: 1, mtime: 2, channels: ['CD8'] },
                          { name: 'x_fXgbTl.mp4', size: 1, mtime: 1, imageUid: 'fXgbTl' }],
                         String, String, images)

  it('carries the resolved image, its channels and its attributes onto the row', () => {
    expect(rows[0].imageUid).toBe('zolIMa')
    expect(rows[0].imageName).toBe('ctrl 4h')
    expect(rows[0].imageChannels).toEqual(['DAPI', 'CD8'])
    expect(rows[0].movieChannels).toEqual(['CD8'])
    expect(rows[0].attr).toEqual({ Treatment: 'control', Timepoint: '4h' })
  })

  it('flattens each attribute as `attr:<key>` — SelectionTable sorts by reading the key off the row', () => {
    expect(rows[0]['attr:Treatment']).toBe('control')
    expect(rows[1]['attr:Treatment']).toBe('anti-PD1')
    // an image without the attribute simply lacks the field; sortRows puts blanks last
    expect(rows[1]['attr:Timepoint']).toBeUndefined()
  })

  // the union of keys across rows is `attrKeysOf` (utils/attrFilter.ts) — one implementation, shared
  // with the image table's own attribute filter, so a movie row is just another `AttrBearing`
})
