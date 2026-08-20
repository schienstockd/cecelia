import { describe, it, expect } from 'vitest'
import {
  pathPoints, pathDomain, focusPoint, gapGeometry, gapHint,
  type TrackPathMap,
  normalizeTracks, displacementVectors, pathCsvRows, trackCountNote, groupedPathPoints, trackEndpoints,
} from './trackPaths'

// two tracks in a straight line along x: A runs 0→2, B carries on 3→5
const straight: TrackPathMap = {
  '1': { t: [0, 1, 2], x: [0, 1, 2], y: [0, 0, 0], label: [1, 2, 3] },
  '2': { t: [3, 4, 5], x: [3, 4, 5], y: [0, 0, 0], label: [4, 5, 6] },
}

describe('pathPoints', () => {
  it('flattens the requested tracks in time order', () => {
    const pts = pathPoints(straight, [1])
    expect(pts.map(p => p.t)).toEqual([0, 1, 2])
    expect(pts.map(p => p.i)).toEqual([0, 1, 2])
    expect(pts.every(p => p.track === '1')).toBe(true)
  })

  it('accepts numeric or string ids', () => {
    expect(pathPoints(straight, [1]).length).toBe(pathPoints(straight, ['1']).length)
  })

  it('skips a track with no geometry rather than throwing', () => {
    // the worklist can reference a track the geometry cap left out — a missing thumbnail beats a
    // broken page
    expect(pathPoints(straight, [1, 999]).map(p => p.track)).toEqual(['1', '1', '1'])
    expect(pathPoints({}, [1])).toEqual([])
    expect(pathPoints({ '3': { t: [], x: [], y: [], label: [] } }, [3])).toEqual([])
  })

  it('tolerates a missing y (degenerate 1-D case)', () => {
    const oneD: TrackPathMap = { '1': { t: [0, 1], x: [0, 5], y: [], label: [1, 2] } }
    expect(pathPoints(oneD, [1]).map(p => p.y)).toEqual([0, 0])
  })
})

describe('pathDomain', () => {
  it('is square — equal x and y extents', () => {
    // a straight run along x must not be stretched to fill the box, or a straight path reads as a
    // diagonal and the shape judgement the thumbnail exists for is destroyed
    const d = pathDomain(pathPoints(straight, [1, 2]))!
    expect(d.x[1] - d.x[0]).toBeCloseTo(d.y[1] - d.y[0], 10)
  })

  it('centres on the data', () => {
    const d = pathDomain(pathPoints(straight, [1, 2]))!
    expect((d.x[0] + d.x[1]) / 2).toBeCloseTo(2.5, 10)   // x spans 0..5
    expect((d.y[0] + d.y[1]) / 2).toBeCloseTo(0, 10)
  })

  it('pads by a fraction of the larger extent', () => {
    const pts = pathPoints(straight, [1, 2])           // x extent 5
    expect(pathDomain(pts, { pad: 0 })!.x).toEqual([0, 5])
    const p = pathDomain(pts, { pad: 0.1 })!
    expect(p.x[1] - p.x[0]).toBeCloseTo(6, 10)         // 5 * 1.2
  })

  it('never collapses to zero width on a stationary or single point', () => {
    const still: TrackPathMap = { '1': { t: [0, 1], x: [7, 7], y: [7, 7], label: [1, 2] } }
    const d = pathDomain(pathPoints(still, [1]), { pad: 0, minSpan: 2 })!
    expect(d.x[1] - d.x[0]).toBeCloseTo(2, 10)
    const one: TrackPathMap = { '1': { t: [0], x: [3], y: [4], label: [1] } }
    expect(pathDomain(pathPoints(one, [1]))!.x[1]).toBeGreaterThan(3)
  })

  it('is null with nothing to show', () => {
    expect(pathDomain([])).toBeNull()
  })
})

describe('focusPoint', () => {
  it('finds the point at a timepoint, scoped to one track', () => {
    const pts = pathPoints(straight, [1, 2])
    expect(focusPoint(pts, 4, 2)).toMatchObject({ track: '2', t: 4, x: 4 })
  })

  it('falls back to the NEAREST timepoint rather than nothing', () => {
    // a split's atT always exists, but a gap's focus is a track end and the caller may ask for a
    // frame that track never had
    expect(focusPoint(pathPoints(straight, [1]), 99)!.t).toBe(2)
  })

  it('is null when there are no points', () => {
    expect(focusPoint([], 0)).toBeNull()
  })
})

describe('gapGeometry', () => {
  it('reports the two ends and the distance between them', () => {
    const g = gapGeometry(straight, 1, 2)!
    expect(g.from).toMatchObject({ track: '1', t: 2 })
    expect(g.to).toMatchObject({ track: '2', t: 3 })
    expect(g.distance).toBeCloseTo(1, 10)
  })

  it('cosine ~1 when A carried straight on into B', () => {
    expect(gapGeometry(straight, 1, 2)!.cosine).toBeCloseTo(1, 10)
  })

  it('cosine ~-1 when B lies BEHIND A — same distance, opposite answer', () => {
    // this is the whole point of showing geometry: 1 µm apart either way, but one is the same cell
    // continuing and the other is a different cell behind it
    const behind: TrackPathMap = {
      '1': { t: [0, 1, 2], x: [0, 1, 2], y: [0, 0, 0], label: [1, 2, 3] },
      '2': { t: [3, 4], x: [1, 0], y: [0, 0], label: [4, 5] },
    }
    const g = gapGeometry(behind, 1, 2)!
    expect(g.distance).toBeCloseTo(1, 10)
    expect(g.cosine).toBeCloseTo(-1, 10)
  })

  it('cosine is null when A has no direction to speak of', () => {
    const single: TrackPathMap = {
      '1': { t: [0], x: [0], y: [0], label: [1] },
      '2': { t: [1, 2], x: [1, 2], y: [0, 0], label: [2, 3] },
    }
    expect(gapGeometry(single, 1, 2)!.cosine).toBeNull()
  })

  it('is null when either track is missing', () => {
    expect(gapGeometry(straight, 1, 999)).toBeNull()
    expect(gapGeometry(straight, 999, 1)).toBeNull()
  })
})

describe('gapHint', () => {
  it('maps a heading to one instruction, not a number to interpret', () => {
    expect(gapHint(1)).toMatch(/same cell/)
    expect(gapHint(-1)).toMatch(/different cell/)
    expect(gapHint(0)).toMatch(/check the image/i)
    expect(gapHint(null)).toMatch(/too short/i)
  })

  it('never renders a raw cosine', () => {
    for (const c of [1, 0.7, 0, -0.7, -1, null]) {
      expect(gapHint(c)).not.toMatch(/[0-9]/)
    }
  })
})

describe('normalizeTracks (star / rose plot)', () => {
  it('translates every track to the origin', () => {
    const pts = normalizeTracks(pathPoints(straight, [1, 2]))
    const firstOf = (t: string) => pts.find(p => p.track === t)!
    expect(firstOf('1')).toMatchObject({ x: 0, y: 0 })
    expect(firstOf('2')).toMatchObject({ x: 0, y: 0 })
  })

  it('preserves the SHAPE — only absolute position is discarded', () => {
    const raw = pathPoints(straight, [2])
    const norm = normalizeTracks(raw)
    // consecutive step lengths are identical before and after
    for (let i = 1; i < raw.length; i++) {
      const dRaw = Math.hypot(raw[i].x - raw[i - 1].x, raw[i].y - raw[i - 1].y)
      const dNorm = Math.hypot(norm[i].x - norm[i - 1].x, norm[i].y - norm[i - 1].y)
      expect(dNorm).toBeCloseTo(dRaw, 10)
    }
  })

  it('keeps t, label and index untouched', () => {
    const norm = normalizeTracks(pathPoints(straight, [1]))
    expect(norm.map(p => p.t)).toEqual([0, 1, 2])
    expect(norm.map(p => p.label)).toEqual([1, 2, 3])
    expect(norm.map(p => p.i)).toEqual([0, 1, 2])
  })

  it('does not mutate the input', () => {
    const raw = pathPoints(straight, [2])
    const before = raw.map(p => p.x)
    normalizeTracks(raw)
    expect(raw.map(p => p.x)).toEqual(before)
  })

  it('is empty for empty input', () => {
    expect(normalizeTracks([])).toEqual([])
  })
})

describe('displacementVectors', () => {
  it('gives one net vector per track, with a compass bearing', () => {
    const v = displacementVectors(pathPoints(straight, [1, 2]))
    expect(v).toHaveLength(2)
    const a = v.find(x => x.track === '1')!
    expect(a).toMatchObject({ x: 2, y: 0, angle: 0 })
    expect(a.distance).toBeCloseTo(2, 10)
  })

  it('bearings are in [0, 360) — never negative', () => {
    const down: TrackPathMap = { '1': { t: [0, 1], x: [0, 0], y: [0, -5], label: [1, 2] } }
    const v = displacementVectors(pathPoints(down, [1]))[0]
    expect(v.angle).toBeCloseTo(270, 10)
    expect(v.angle).toBeGreaterThanOrEqual(0)
  })

  it('a cell that returned to its start has zero displacement, not zero path', () => {
    const loop: TrackPathMap = { '1': { t: [0, 1, 2], x: [0, 9, 0], y: [0, 0, 0], label: [1, 2, 3] } }
    expect(displacementVectors(pathPoints(loop, [1]))[0].distance).toBeCloseTo(0, 10)
  })

  it('is unchanged by normalizing first — displacement is translation-invariant', () => {
    const raw = displacementVectors(pathPoints(straight, [1, 2]))
    const norm = displacementVectors(normalizeTracks(pathPoints(straight, [1, 2])))
    expect(norm).toEqual(raw)
  })
})

describe('pathCsvRows', () => {
  it('emits one row per point, in draw order', () => {
    const rows = pathCsvRows(pathPoints(straight, [1]))
    expect(rows.map(r => r.t)).toEqual([0, 1, 2])
    expect(rows[0]).toMatchObject({ track: '1', x: 0, y: 0, label: 1 })
  })

  it('repeats the per-track colour value on every row of that track', () => {
    const rows = pathCsvRows(pathPoints(straight, [1, 2]), { '1': 4.5, '2': 9 }, 'speed')
    expect(rows.filter(r => r.track === '1').every(r => r.speed === 4.5)).toBe(true)
    expect(rows.find(r => r.track === '2')!.speed).toBe(9)
  })

  it('omits the value column entirely when nothing is coloured', () => {
    expect(Object.keys(pathCsvRows(pathPoints(straight, [1]))[0])).not.toContain('value')
  })

  it('leaves a track with no value blank rather than dropping the row', () => {
    const rows = pathCsvRows(pathPoints(straight, [1, 2]), { '1': 4.5 }, 'speed')
    expect(rows).toHaveLength(6)
    expect(rows.find(r => r.track === '2')!.speed).toBe('')
  })
})

describe('trackCountNote', () => {
  it('says what is missing when the plot is capped', () => {
    expect(trackCountNote(500, 1203)).toMatch(/500 of 1203/)
  })

  it('is empty when everything is shown', () => {
    expect(trackCountNote(374, 374)).toBe('')
    expect(trackCountNote(0, 0)).toBe('')
  })
})

// ── the cohort shape: several groups drawn in one plot ────────────────────────
describe('groupedPathPoints', () => {
  const groups = [
    { key: 'wt', label: 'WT', paths: straight, values: { '1': 4.5 } },
    { key: 'ko', label: 'MerTK', paths: straight, values: { '1': 1.2 } },
  ]

  // two groups can hold the SAME track id — the same movie under two populations, or two movies each
  // with a track 17. `z: 'track'` would then draw one polyline zig-zagging between them.
  it('namespaces the track key per group, and keeps the real id beside it', () => {
    const rows = groupedPathPoints(groups)
    expect(new Set(rows.map(r => r.track))).toEqual(new Set(['wt#1', 'wt#2', 'ko#1', 'ko#2']))
    expect(new Set(rows.map(r => r.id))).toEqual(new Set(['1', '2']))
  })

  it('carries the group key (facet channel) and label (colour channel)', () => {
    const rows = groupedPathPoints(groups)
    expect(rows.filter(r => r.g === 'wt').every(r => r.gl === 'WT')).toBe(true)
    expect(rows.filter(r => r.g === 'ko').every(r => r.gl === 'MerTK')).toBe(true)
  })

  it('joins each group\'s OWN colour values', () => {
    const rows = groupedPathPoints(groups)
    expect(rows.find(r => r.g === 'wt' && r.id === '1')!.v).toBe(4.5)
    expect(rows.find(r => r.g === 'ko' && r.id === '1')!.v).toBe(1.2)
    expect(rows.find(r => r.id === '2')!.v).toBeNull()
  })

  // normalising must happen WITHIN a group, per track — not across the concatenation
  it('normalises per track when asked (the star transform)', () => {
    const rows = groupedPathPoints(groups, { normalise: true })
    for (const k of ['wt#1', 'wt#2', 'ko#1', 'ko#2']) {
      const first = rows.find(r => r.track === k)!
      expect(first).toMatchObject({ x: 0, y: 0 })
    }
  })

  it('falls back to the key when a group has no label', () => {
    expect(groupedPathPoints([{ key: 'k', paths: straight }])[0].gl).toBe('k')
  })

  it('is empty for no groups', () => {
    expect(groupedPathPoints([])).toEqual([])
  })
})

describe('pathCsvRows over groups', () => {
  const rows = () => groupedPathPoints([
    { key: 'wt', label: 'WT', paths: straight, values: { '1': 4.5, '2': 9 } },
    { key: 'ko', label: 'MerTK', paths: straight },
  ])

  // a cohort export is useless without the arm each row came from
  it('adds a group column, and exports the REAL track id', () => {
    const csv = pathCsvRows(rows(), {}, 'speed')
    expect(csv[0]).toMatchObject({ group: 'WT', track: '1' })
    expect(csv.some(r => r.group === 'MerTK')).toBe(true)
    expect(csv.every(r => r.track === '1' || r.track === '2')).toBe(true)
  })

  it('takes the value off the row, so each group keeps its own', () => {
    const csv = pathCsvRows(rows(), {}, 'speed')
    expect(csv.find(r => r.group === 'WT' && r.track === '1')!.speed).toBe(4.5)
    expect(csv.find(r => r.group === 'MerTK' && r.track === '1')!.speed).toBe('')
  })

  it('a single-group export keeps the old shape — no group column', () => {
    expect(Object.keys(pathCsvRows(pathPoints(straight, [1]))[0])).not.toContain('group')
  })
})

describe('displacementVectors carries the row through', () => {
  it('keeps the group and colour value on the arrow', () => {
    const pts = groupedPathPoints([{ key: 'wt', label: 'WT', paths: straight, values: { '1': 4.5 } }],
                                  { normalise: true })
    const v = displacementVectors(pts).find(r => r.id === '1')!
    expect(v).toMatchObject({ g: 'wt', gl: 'WT', v: 4.5 })
    expect(v.distance).toBeCloseTo(2, 10)
  })
})

describe('trackEndpoints', () => {
  const grp = (paths: Record<string, { t: number[]; x: number[]; y: number[]; label: number[] }>) =>
    groupedPathPoints([{ key: 'g', label: 'g', paths, values: {} }])

  it('finds the first and last point of every track', () => {
    const { starts, ends } = trackEndpoints(grp({
      '1': { t: [0, 1, 2], x: [0, 1, 2], y: [0, 0, 0], label: [1, 2, 3] },
      '2': { t: [0, 1], x: [5, 6], y: [1, 1], label: [4, 5] },
    }))
    expect(starts.map(p => p.x)).toEqual([0, 5])
    expect(ends.map(p => p.x)).toEqual([2, 6])
  })

  it('a one-point track is both its own start and its own end', () => {
    const { starts, ends } = trackEndpoints(grp({ '9': { t: [4], x: [3], y: [3], label: [7] } }))
    expect(starts).toHaveLength(1)
    expect(ends).toHaveLength(1)
    expect(starts[0]).toEqual(ends[0])
  })

  // keys are namespaced by group, so the same track id in two groups keeps two endpoints rather than
  // collapsing into one — the same reason `z: 'track'` uses the namespaced key
  it('does not merge the same track id across groups', () => {
    const pts = groupedPathPoints([
      { key: 'a', label: 'a', paths: { '1': { t: [0, 1], x: [0, 1], y: [0, 0], label: [1, 2] } }, values: {} },
      { key: 'b', label: 'b', paths: { '1': { t: [0, 1], x: [9, 8], y: [0, 0], label: [3, 4] } }, values: {} },
    ])
    const { starts, ends } = trackEndpoints(pts)
    expect(starts).toHaveLength(2)
    expect(ends.map(p => p.x).sort()).toEqual([1, 8])
  })

  it('is empty for no points', () => {
    expect(trackEndpoints([])).toEqual({ starts: [], ends: [] })
  })
})
