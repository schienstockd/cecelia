import { describe, it, expect } from 'vitest'
import { discoverObsMeasures, obsMeasureLabel, isIdLikeColumn, distinctValueNames, mergeColumnSets, type ObsMeasurePattern } from './obsMeasures'

// the patterns the Spatial page's spec declares
const PATTERNS: ObsMeasurePattern[] = [
  { match: '.cell.min_distance#', label: 'distance to' },
  { match: '.cell.contact#', label: 'contact with' },
  { match: '.cell.is.aggregate', label: 'aggregated' },
  { match: 'spatial.comp.', label: 'composition' },
]

// a realistic obs set after running contacts + aggregates + region clustering
const OBS = [
  'label', 'centroid_x', 'centroid_y',
  'flow.cell.contact#flow.T_qc',
  'flow.cell.min_distance#flow.T_qc',
  'flow.cell.contact_id#flow.T_qc',
  'flow.cell.is.aggregate',
  'flow.cell.aggregate.id',
  'spatial.comp.B_qc__tracked.immune',
  'spatial.comp.T_qc__tracked.immune',
  'regions.immune',
  'live.cell.speed',
]

describe('discoverObsMeasures', () => {
  it('finds the run-dependent spatial measures', () => {
    expect(discoverObsMeasures(OBS, PATTERNS)).toEqual([
      'flow.cell.min_distance#flow.T_qc',
      'flow.cell.contact#flow.T_qc',
      'flow.cell.is.aggregate',
      'spatial.comp.B_qc__tracked.immune',
      'spatial.comp.T_qc__tracked.immune',
    ])
  })

  it('EXCLUDES id-like columns that share a measure prefix', () => {
    // `contact_id#…` is the nearest cell's LABEL and `aggregate.id` the aggregate NUMBER — averaging
    // either is meaningless, and both sit right beside the real measures
    const found = discoverObsMeasures(OBS, PATTERNS)
    expect(found).not.toContain('flow.cell.contact_id#flow.T_qc')
    expect(found).not.toContain('flow.cell.aggregate.id')
    expect(isIdLikeColumn('flow.cell.contact_id#flow.T_qc')).toBe(true)
    expect(isIdLikeColumn('flow.cell.aggregate.id')).toBe(true)
    expect(isIdLikeColumn('flow.cell.min_distance#flow.T_qc')).toBe(false)
  })

  it('ignores unrelated obs columns', () => {
    const found = discoverObsMeasures(OBS, PATTERNS)
    for (const c of ['label', 'centroid_x', 'regions.immune', 'live.cell.speed']) {
      expect(found).not.toContain(c)
    }
  })

  it('orders by PATTERN first (spec controls the family order), alphabetical within one', () => {
    const shuffled = ['spatial.comp.z.x', 'spatial.comp.a.x', 'flow.cell.contact#b', 'flow.cell.contact#a']
    expect(discoverObsMeasures(shuffled, [
      { match: '.cell.contact#' }, { match: 'spatial.comp.' },
    ])).toEqual(['flow.cell.contact#a', 'flow.cell.contact#b', 'spatial.comp.a.x', 'spatial.comp.z.x'])
  })

  it('never returns a column twice when two patterns overlap', () => {
    const overlapping = [{ match: '.cell.' }, { match: '.cell.contact#' }]
    const found = discoverObsMeasures(['flow.cell.contact#a'], overlapping)
    expect(found).toEqual(['flow.cell.contact#a'])
  })

  it('is empty before the spatial tasks have run — the honest "nothing yet" state', () => {
    expect(discoverObsMeasures(['label', 'centroid_x'], PATTERNS)).toEqual([])
    expect(discoverObsMeasures([], PATTERNS)).toEqual([])
  })

  it('is empty when a spec declares no patterns', () => {
    expect(discoverObsMeasures(OBS, [])).toEqual([])
  })
})

describe('against REAL column names', () => {
  // Copied verbatim from project 4kS67f / LUkCpP / labelProps/B.h5ad after running cellContacts +
  // detectAggregates + clustRegions. Note the prefix is `live.` — the INPUT population's pop type, not
  // `flow.`: contacts on a tracked population write `live.cell.*`. That is exactly why the patterns
  // match on `.cell.contact#` rather than anchoring to a pop type.
  const REAL = [
    'label', 'centroid_x', 'centroid_y', 'centroid_t', 'track_id',
    'spatial.comp.B_qc__tracked.immune',
    'spatial.comp.T_qc__tracked.immune',
    'spatial.comp.other.immune',
    'live.cell.contact#live.T_qc__tracked',
    'live.cell.min_distance#live.T_qc__tracked',
    'live.cell.contact_id#live.T_qc__tracked',
    'live.cell.is.aggregate',
    'live.cell.aggregate.id',
    'regions.immune',
  ]

  it('offers every real spatial measure and no identifier', () => {
    expect(discoverObsMeasures(REAL, PATTERNS)).toEqual([
      'live.cell.min_distance#live.T_qc__tracked',
      'live.cell.contact#live.T_qc__tracked',
      'live.cell.is.aggregate',
      // alphabetical within the family, so the catch-all "other" sorts between the basis populations
      // rather than last. Predictable beats clever, and the label makes each row unambiguous.
      'spatial.comp.B_qc__tracked.immune',
      'spatial.comp.other.immune',
      'spatial.comp.T_qc__tracked.immune',
    ])
  })

  it('labels them readably', () => {
    expect(obsMeasureLabel('live.cell.min_distance#live.T_qc__tracked', PATTERNS))
      .toBe('distance to live.T_qc__tracked')
    expect(obsMeasureLabel('live.cell.is.aggregate', PATTERNS)).toBe('aggregated')
    expect(obsMeasureLabel('spatial.comp.other.immune', PATTERNS)).toBe('composition other.immune')
  })
})

describe('obsMeasureLabel', () => {
  it('shows the part that VARIES, not the family repeated on every row', () => {
    expect(obsMeasureLabel('flow.cell.min_distance#flow.T_qc', PATTERNS)).toBe('distance to flow.T_qc')
    expect(obsMeasureLabel('flow.cell.contact#flow.T_qc', PATTERNS)).toBe('contact with flow.T_qc')
    expect(obsMeasureLabel('spatial.comp.B_qc__tracked.immune', PATTERNS)).toBe('composition B_qc__tracked.immune')
  })

  it('uses the bare label when nothing varies', () => {
    expect(obsMeasureLabel('flow.cell.is.aggregate', PATTERNS)).toBe('aggregated')
  })

  it('falls back to the raw column when unclaimed or unlabelled', () => {
    expect(obsMeasureLabel('live.cell.speed', PATTERNS)).toBe('live.cell.speed')
    expect(obsMeasureLabel('flow.cell.contact#x', [{ match: '.cell.contact#' }]))
      .toBe('flow.cell.contact#x')
  })
})

// The order-dependence bug: the panel read columns from the FIRST ticked target only, so which measures
// were on offer depended on the tick order. Both helpers exist to make the list order-INDEPENDENT.
describe('distinctValueNames', () => {
  it('keeps first-ticked order and de-duplicates', () => {
    expect(distinctValueNames([
      { valueName: 'B' }, { valueName: 'B' }, { valueName: 'T' }, { valueName: 'B' },
    ])).toEqual(['B', 'T'])
  })

  it('drops empty names and handles an empty selection', () => {
    expect(distinctValueNames([{ valueName: '' }, { valueName: 'T' }])).toEqual(['T'])
    expect(distinctValueNames([])).toEqual([])
  })
})

describe('mergeColumnSets', () => {
  // B's h5ad carries "distance to T"; T's carries "distance to B". Reading only one hid the other.
  const B = {
    columns: ['area', 'extent'], channels: ['mean_intensity_0'],
    obsColumns: ['label', 'live.cell.min_distance#live.T_qc_tracked'], temporalColumns: ['t'],
  }
  const T = {
    columns: ['area', 'solidity'], channels: ['mean_intensity_0'],
    obsColumns: ['label', 'live.cell.min_distance#live.B_qc_tracked'], temporalColumns: ['t'],
  }

  it('unions across segmentations so BOTH targets are discoverable', () => {
    const m = mergeColumnSets([B, T])
    expect(m.obsColumns).toEqual([
      'label',
      'live.cell.min_distance#live.T_qc_tracked',
      'live.cell.min_distance#live.B_qc_tracked',
    ])
    expect(discoverObsMeasures(m.obsColumns, PATTERNS)).toEqual([
      'live.cell.min_distance#live.B_qc_tracked',
      'live.cell.min_distance#live.T_qc_tracked',
    ])
  })

  it('is order-independent — the whole point', () => {
    expect(new Set(mergeColumnSets([B, T]).obsColumns))
      .toEqual(new Set(mergeColumnSets([T, B]).obsColumns))
  })

  it('de-duplicates every list and tolerates a failed request (empty object)', () => {
    const m = mergeColumnSets([B, {}, T])
    expect(m.columns).toEqual(['area', 'extent', 'solidity'])
    expect(m.channels).toEqual(['mean_intensity_0'])
    expect(m.temporalColumns).toEqual(['t'])
  })

  it('returns empty lists for no parts', () => {
    expect(mergeColumnSets([])).toEqual({ columns: [], channels: [], obsColumns: [], temporalColumns: [] })
  })
})
