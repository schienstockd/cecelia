/**
 * The neighbour-graph figure: each column's edges follow the method's DEFINITION (expected values
 * from the definition — Euler's count, nearest-first, within-r, the window grid — not from the code's
 * own output), and the captions read the form's current values.
 */
import { describe, expect, it } from 'vitest'
import {
  CELLS, FOCUS, GRAPH_METHODS, delaunayEdges, graphColumn, knnEdges, nearest, neighbourGraphFigure,
  ownWindow, radiusEdges,
} from './neighbourGraphVis'
import { PARAM_FIGURES } from './paramFigures'
import type { AdvisorContext } from './paramAdvisors'
import spec from '../../../app/src/tasks/spatialAnalysis/cellNeighbours.json'

const d = (i: number, j: number) => Math.hypot(CELLS[i][0] - CELLS[j][0], CELLS[i][1] - CELLS[j][1])
const P = { radius: 30, k: 6, windowSize: 50 }

describe('delaunayEdges', () => {
  it('a triangle around one interior point: the 3 sides + 3 spokes', () => {
    const e = delaunayEdges([[0, 0], [10, 0], [5, 9], [5, 3]])
    expect(e).toHaveLength(6)
  })

  it('the schematic field triangulates to 3n − 3 − h edges (Euler; 14 cells, 9 on the hull)', () => {
    expect(delaunayEdges(CELLS)).toHaveLength(3 * 14 - 3 - 9)
  })
})

describe('kNN / radius', () => {
  it("the focus cell's k nearest are all closer than every cell it skipped", () => {
    const kn = nearest(CELLS, FOCUS, 4)
    expect(kn).toHaveLength(4)
    const far = CELLS.map((_, i) => i).filter(i => i !== FOCUS && !kn.includes(i))
    expect(Math.max(...kn.map(i => d(FOCUS, i)))).toBeLessThanOrEqual(Math.min(...far.map(i => d(FOCUS, i))))
  })

  it('k is capped at n − 1 — every other cell, no more', () => {
    expect(nearest(CELLS, FOCUS, 99)).toHaveLength(CELLS.length - 1)
    expect(knnEdges(CELLS, 99)).toHaveLength((14 * 13) / 2)
  })

  it('radius links exactly the pairs within r', () => {
    const e = radiusEdges(CELLS, 30)
    for (let i = 0; i < CELLS.length; i++) for (let j = i + 1; j < CELLS.length; j++) {
      expect(e.some(x => x[0] === i && x[1] === j)).toBe(d(i, j) <= 30)
    }
  })
})

describe('ownWindow — spatial_utils.window_grid', () => {
  it('a window of side S on an S/2 grid, centred on the tile holding x', () => {
    // S = 50 → tile 25; x = 30 is in tile 1 = [25, 50); its window reaches half a tile either side
    expect(ownWindow(30, 50)).toEqual([12.5, 62.5])
    expect(ownWindow(0, 50)).toEqual([-12.5, 37.5])
  })
})

describe('graphColumn', () => {
  it('Delaunay dashes (cuts) exactly the triangulation edges longer than the radius', () => {
    const { scene } = graphColumn('delaunay', P)
    const len = (r: { points: { x: number; y: number }[] }) =>
      Math.hypot(r.points[0].x - r.points[1].x, r.points[0].y - r.points[1].y) * 100
    const cut = scene.ribbons.filter(r => r.dashed)
    const kept = scene.ribbons.filter(r => !r.dashed)
    expect(cut.length).toBeGreaterThan(0)
    cut.forEach(r => expect(len(r)).toBeGreaterThan(30))
    kept.forEach(r => expect(len(r)).toBeLessThanOrEqual(30 + 1e-9))
    expect(cut.length + kept.length).toBe(delaunayEdges(CELLS).length)
    // a radius past every edge cuts nothing
    expect(graphColumn('delaunay', { ...P, radius: 200 }).scene.ribbons.some(r => r.dashed)).toBe(false)
  })

  it("kNN colours the focus cell's own k links", () => {
    const { scene } = graphColumn('knn', { ...P, k: 3 })
    expect(scene.ribbons.filter(r => r.colour === 'var(--cc-accent)')).toHaveLength(3)
    expect(scene.points.filter(p => p.colour === 'var(--cc-accent)')).toHaveLength(4) // focus + 3
  })

  it('windows draw no cell-to-cell edge — only the grid and two window outlines', () => {
    const { scene } = graphColumn('windows', P)
    const atCell = (pt: { x: number; y: number }) =>
      CELLS.some(c => Math.abs(c[0] / 100 - pt.x) < 1e-9 && Math.abs(c[1] / 100 - pt.y) < 1e-9)
    expect(scene.ribbons.some(r => r.points.length === 2 && atCell(r.points[0]) && atCell(r.points[1]))).toBe(false)
    expect(scene.ribbons.filter(r => r.points.length === 5)).toHaveLength(2)
  })

  it('captions read the current values', () => {
    const p = { radius: 42, k: 4, windowSize: 80 }
    expect(GRAPH_METHODS.map(m => graphColumn(m, p).text))
      .toEqual(['Mesh, cut > 42 µm', '4 nearest each', 'All within 42 µm', '80 µm squares'])
  })
})

describe('neighbourGraph figure', () => {
  it('one scene row, a column per option in cellNeighbours.json, fed by the form values', () => {
    const { vis } = neighbourGraphFigure(P)
    const param = spec.params.find(p => p.key === 'neighbourMethod') as
      { figure?: string; options: { value: string }[] }
    expect(param.figure).toBe('neighbourGraph')
    expect(vis.columns).toEqual(param.options.map(o => o.value))
    expect(vis.rows).toHaveLength(1)
    expect(vis.rows[0].role).toBe('scene')
    expect(vis.rows[0].cells.every(c => c.scene)).toBe(true)

    const def = PARAM_FIGURES.neighbourGraph({
      images: [], values: { neighbourRadius: 15, nNeighbours: 2, windowSize: 20 },
    } as unknown as AdvisorContext)
    expect(def.headings).toHaveLength(4)
    expect(def.vis.rows[0].cells.map(c => c.text))
      .toEqual(['Mesh, cut > 15 µm', '2 nearest each', 'All within 15 µm', '20 µm squares'])
  })
})
