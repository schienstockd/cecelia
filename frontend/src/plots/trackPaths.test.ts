import { describe, it, expect } from 'vitest'
import {
  pathPoints, pathDomain, focusPoint, gapGeometry, gapHint,
  type TrackPathMap,
  normalizeTracks, displacementVectors, pathCsvRows, trackCountNote, trackEndpoints,
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

describe('trackEndpoints', () => {
  const paths = {
    '1': { t: [0, 1, 2], x: [0, 1, 2], y: [0, 0, 0], label: [1, 2, 3] },
    '2': { t: [0, 1], x: [5, 6], y: [1, 1], label: [4, 5] },
  }

  it('finds the first and last point of every track', () => {
    const { starts, ends } = trackEndpoints(pathPoints(paths, ['1', '2']))
    expect(starts.map(p => [p.track, p.x])).toEqual([['1', 0], ['2', 5]])
    expect(ends.map(p => [p.track, p.x])).toEqual([['1', 2], ['2', 6]])
  })

  it('a one-point track is both its own start and its own end', () => {
    const one = { '9': { t: [4], x: [3], y: [3], label: [7] } }
    const { starts, ends } = trackEndpoints(pathPoints(one, ['9']))
    expect(starts).toHaveLength(1)
    expect(ends).toHaveLength(1)
    expect(starts[0]).toEqual(ends[0])
  })

  it('is empty for no points', () => {
    expect(trackEndpoints([])).toEqual({ starts: [], ends: [] })
  })
})
