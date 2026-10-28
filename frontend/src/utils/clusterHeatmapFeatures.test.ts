import { describe, it, expect } from 'vitest'
import { clusterFeatureOptions, reconcileHeatmapFeatures, heatmapRequestFeatures } from './clusterHeatmapFeatures'

// Column names from real runs: a cell run (channel columns) and a track run (hPQXrK `behaviour`).
const CELL = ['mean_intensity_0', 'mean_intensity_1', 'area']
const TRACK = ['live.cell.speed.mean', 'live.cell.angle.mean', 'live.cell.hmm.state.movement.1']
const HMM_RE = /live\.cell\.hmm\.(state|transitions)\b/

describe('clusterFeatureOptions', () => {
  const base = { clusterFeatures: { behaviour: TRACK }, fallback: ['live.track.speed'], suffix: 'behaviour' }

  it('offers the run’s recorded features once loaded for this popType', () => {
    expect(clusterFeatureOptions({ ...base, loadedPopType: 'trackclust', popType: 'trackclust', exclude: HMM_RE }))
      .toEqual(['live.cell.speed.mean', 'live.cell.angle.mean'])
  })

  it('offers nothing while the metadata on hand is for another popType (board cells → tracks switch)', () => {
    const cellsLoaded = { clusterFeatures: { default: CELL }, fallback: CELL, suffix: 'default' }
    expect(clusterFeatureOptions({ ...cellsLoaded, loadedPopType: 'clust', popType: 'trackclust' })).toEqual([])
  })

  it('falls back to the table columns when the run predates feature tracking', () => {
    expect(clusterFeatureOptions({ ...base, suffix: 'old', loadedPopType: 'trackclust', popType: 'trackclust' }))
      .toEqual(['live.track.speed'])
  })
})

describe('reconcileHeatmapFeatures', () => {
  it('seeds every option when nothing was picked', () => {
    expect(reconcileHeatmapFeatures(undefined, TRACK)).toEqual(TRACK)
  })
  it('keeps an explicit empty pick', () => {
    expect(reconcileHeatmapFeatures([], TRACK)).toEqual([])
  })
  it('reseeds when the pick came from another run (cell features on a track run)', () => {
    expect(reconcileHeatmapFeatures(CELL, TRACK)).toEqual(TRACK)
  })
  it('drops only the columns a re-run no longer has', () => {
    expect(reconcileHeatmapFeatures(['live.cell.angle.mean', 'gone'], TRACK)).toEqual(['live.cell.angle.mean'])
  })
})

describe('heatmapRequestFeatures', () => {
  it('never requests a column the run lacks', () => {
    expect(heatmapRequestFeatures([...CELL, 'live.cell.speed.mean'], TRACK)).toEqual(['live.cell.speed.mean'])
  })
  it('requests nothing while the options are unknown', () => {
    expect(heatmapRequestFeatures(CELL, [])).toEqual([])
  })
})
