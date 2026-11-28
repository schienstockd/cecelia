import { describe, it, expect } from 'vitest'
import { popRuns, runKey, runShown, shownRuns, withShownRuns } from './popRuns'

// two clustering runs on one segmentation, plus a gate — the shape `/api/plots/populations` and the
// viewer overlays send (`run` = the filter measure's suffix, "" for a gate)
const pops = [
  { path: '/qc', run: '' },
  { path: '/Epithelial', run: 'tissues' },
  { path: '/Stromal', run: 'tissues' },
  { path: '/Epithelial (AF)', run: 'tissues_af' },
  { path: '/old' },                                   // an older server: no run field
]

describe('popRuns', () => {
  it('lists each run once, in first-seen order, ignoring pops with no run', () => {
    expect(popRuns(pops)).toEqual(['tissues', 'tissues_af'])
    expect(popRuns([{ run: '' }, {}])).toEqual([])
  })

  it('shows every run until one is hidden', () => {
    const none = new Set<string>()
    expect(shownRuns(none, 'cells', 'clust', ['tissues', 'tissues_af'])).toEqual(['tissues', 'tissues_af'])
  })

  it('hides exactly the runs left out, only for that segmentation and pop type', () => {
    const h = withShownRuns(new Set(), 'cells', 'clust', ['tissues', 'tissues_af'], ['tissues_af'])
    expect([...h]).toEqual([runKey('cells', 'clust', 'tissues')])
    expect(runShown(h, 'cells', 'clust', 'tissues')).toBe(false)
    expect(runShown(h, 'cells', 'clust', 'tissues_af')).toBe(true)
    expect(runShown(h, 'nuclei', 'clust', 'tissues')).toBe(true)      // another segmentation
    expect(runShown(h, 'cells', 'region', 'tissues')).toBe(true)      // another pop type
  })

  it('never hides a pop with no run', () => {
    const h = new Set([runKey('cells', 'flow', '')])
    expect(runShown(h, 'cells', 'flow', '')).toBe(true)
    expect(runShown(h, 'cells', 'flow', undefined)).toBe(true)
  })

  it('re-showing a run takes it off the hidden set', () => {
    const h = withShownRuns(new Set([runKey('cells', 'clust', 'tissues')]), 'cells', 'clust',
                            ['tissues', 'tissues_af'], ['tissues', 'tissues_af'])
    expect(h.size).toBe(0)
  })

  it('refuses to hide every run', () => {
    const before = new Set([runKey('cells', 'clust', 'tissues')])
    const h = withShownRuns(before, 'cells', 'clust', ['tissues', 'tissues_af'], [])
    expect([...h]).toEqual([...before])
  })
})
