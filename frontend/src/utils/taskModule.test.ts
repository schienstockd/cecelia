import { describe, it, expect } from 'vitest'
import { moduleKeyFromFun, moduleIdFromFun, moduleColor, MODULE_COLORS } from './taskModule'

// The module key answers "which page owns this task", and it is compared against the string a page
// passes as `module=`. It used to exist three times — here, in runningTasks and inline in the tasks
// store — which is how the mismatches below survived.
describe('moduleKeyFromFun', () => {
  it('strips the category suffix', () => {
    expect(moduleKeyFromFun('cleanupImages.smooth')).toBe('cleanup')
    expect(moduleKeyFromFun('segment.cellpose')).toBe('segment')
    expect(moduleKeyFromFun('testTasks.image_task')).toBe('test')
  })

  it('preserves case, because page keys are camelCase', () => {
    // The old derivation lowercased, so `clustPops` (the string ClusterCellsModule passes as
    // `module=`) never matched the derived `clustpops` and those tasks could not adopt onto their
    // own page. Every multi-word page had the same silent mismatch.
    expect(moduleKeyFromFun('clustPops.cluster')).toBe('clustPops')
    expect(moduleKeyFromFun('clustTracks.cluster')).toBe('clustTracks')
    expect(moduleKeyFromFun('clustRegions.cluster')).toBe('clustRegions')
    expect(moduleKeyFromFun('spatialAnalysis.cellNeighbours')).toBe('spatialAnalysis')
    expect(moduleKeyFromFun('opticalFlow.train')).toBe('opticalFlow')
  })

  it('sends both import and export to the Manage images page', () => {
    // One page hosts several categories. Without the explicit map, `exportImages` would strip to
    // 'export' — a module with no page — so an export running in another tab would never adopt.
    expect(moduleKeyFromFun('importImages.omezarr')).toBe('manageImages')
    expect(moduleKeyFromFun('exportImages.ome_tiff')).toBe('manageImages')
  })

  it('falls back to chain for a bare fun_name', () => {
    expect(moduleKeyFromFun('')).toBe('chain')
  })
})

describe('moduleIdFromFun', () => {
  it('is the key lowercased, and every id it yields has a colour', () => {
    expect(moduleIdFromFun('clustPops.cluster')).toBe('clustpops')
    expect(moduleIdFromFun('exportImages.ome_tiff')).toBe('manageimages')
    // The colour map is keyed lowercase; a key whose lowercase form is missing renders grey, which
    // is how the Manage images rename could have silently dropped that page's accent colour.
    for (const fun of ['importImages.omezarr', 'exportImages.ome_tiff', 'cleanupImages.smooth',
                       'segment.cellpose', 'clustPops.cluster', 'spatialAnalysis.cellNeighbours']) {
      expect(MODULE_COLORS[moduleIdFromFun(fun)], fun).toBeDefined()
    }
  })

  it('falls back to grey for an unknown module', () => {
    expect(moduleColor('nope')).toBe('#52525b')
  })
})
