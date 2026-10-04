import { describe, it, expect } from 'vitest'
import {
  versionCounts, labelCounts, orderDefaultLast,
  survivingVersions, resolveNewActive, unimportsImage,
  survivorCounts, activeMismatches, partialNames,
  runGroups, alsoDeleted, runId, runRef, type AnalysisRunInfo,
} from './imageDelete'

const IMG = (versions: string[], labels: string[] = []) => ({
  filepaths: Object.fromEntries(versions.map(v => [v, `${v}.ome.zarr`])),
  labels:    Object.fromEntries(labels.map(l => [l, [`${l}.zarr`]])),
})

describe('versionCounts', () => {
  it('is the UNION, with how many images carry each name', () => {
    // the case that drove this: an intersection would hide driftCorrected entirely, so it could not be
    // deleted at all until the selection was narrowed
    expect(versionCounts([IMG(['default', 'driftCorrected']), IMG(['default'])]))
      .toEqual([{ name: 'default', count: 2 }, { name: 'driftCorrected', count: 1 }])
  })

  it('puts default first', () => {
    expect(versionCounts([IMG(['driftCorrected', 'default'])]).map(c => c.name))
      .toEqual(['default', 'driftCorrected'])
  })

  it('offers names the selection does not share', () => {
    expect(versionCounts([IMG(['a']), IMG(['b'])]))
      .toEqual([{ name: 'a', count: 1 }, { name: 'b', count: 1 }])
  })

  it('is empty for no images, and for an image with no versions', () => {
    expect(versionCounts([])).toEqual([])
    expect(versionCounts([{ filepaths: null }])).toEqual([])
  })
})

describe('labelCounts', () => {
  it('unions label sets the same way', () => {
    expect(labelCounts([IMG([], ['A', 'B']), IMG([], ['B'])]))
      .toEqual([{ name: 'A', count: 1 }, { name: 'B', count: 2 }])
    expect(labelCounts([{ labels: undefined }])).toEqual([])
  })
})

describe('orderDefaultLast', () => {
  it('moves default to the end so the un-import lands at the end of the loop', () => {
    expect(orderDefaultLast(['default', 'af', 'cp'])).toEqual(['af', 'cp', 'default'])
  })

  it('leaves a list without default alone, and is a no-op on empty', () => {
    expect(orderDefaultLast(['af', 'cp'])).toEqual(['af', 'cp'])
    expect(orderDefaultLast([])).toEqual([])
  })
})

describe('survivingVersions', () => {
  it('is the set difference', () => {
    expect(survivingVersions(['default', 'af', 'cp'], ['default', 'af'])).toEqual(['cp'])
    expect(survivingVersions(['default'], [])).toEqual(['default'])
  })
})

describe('resolveNewActive', () => {
  it('keeps the preferred version when it survives on this image', () => {
    expect(resolveNewActive(['default', 'cp'], ['default'], 'cp')).toBe('cp')
  })

  it('falls back to this image own active when the preferred one is not registered here', () => {
    // the union bug this exists for: the user picks `cp` from the union, but THIS image only has
    // default + af — writing `cp` into its _active would name a version that never existed
    expect(resolveNewActive(['default', 'af'], ['af'], 'cp', 'default')).toBe('default')
  })

  it('falls back to the first survivor when neither preferred nor current survives', () => {
    expect(resolveNewActive(['default', 'af'], ['default'], 'cp', 'default')).toBe('af')
  })

  it('is empty when nothing survives — the image un-imports', () => {
    expect(resolveNewActive(['default'], ['default'], 'default')).toBe('')
  })
})

describe('unimportsImage', () => {
  it('flags a removal that takes every version', () => {
    expect(unimportsImage(['default', 'cp'], ['default', 'cp'])).toBe(true)
    expect(unimportsImage(['default', 'cp'], ['cp'])).toBe(false)
    expect(unimportsImage([], [])).toBe(false)
  })
})

describe('survivorCounts', () => {
  it('counts which versions each image would keep', () => {
    expect(survivorCounts([IMG(['default', 'cp']), IMG(['default'])], ['default']))
      .toEqual([{ name: 'cp', count: 1 }])
  })

  it('offers default first when it survives', () => {
    expect(survivorCounts([IMG(['default', 'cp']), IMG(['default', 'cp'])], ['cp']))
      .toEqual([{ name: 'default', count: 2 }])
  })
})

describe('activeMismatches', () => {
  it('counts images that survive but lack the chosen active version', () => {
    // remove `default` from both: A keeps cp, B keeps af. Choosing cp cannot be honoured on B, which
    // still has a version — so it is a real conflict, not an un-import.
    expect(activeMismatches([IMG(['default', 'cp']), IMG(['default', 'af'])], ['default'], 'cp')).toBe(1)
  })

  it('is zero when every surviving image has the chosen version', () => {
    expect(activeMismatches([IMG(['default', 'cp']), IMG(['default', 'cp'])], ['default'], 'cp')).toBe(0)
  })

  it('does not count an image that un-imports — there is no active to set', () => {
    expect(activeMismatches([IMG(['default'])], ['default'], 'cp')).toBe(0)
  })

  it('does not count an image with no versions at all', () => {
    expect(activeMismatches([{ filepaths: null }], ['default'], 'cp')).toBe(0)
  })
})

describe('partialNames', () => {
  it('lists the names that are not on every selected image', () => {
    expect(partialNames([{ name: 'A', count: 1 }, { name: 'B', count: 3 }], 3)).toEqual(['A'])
    expect(partialNames([{ name: 'B', count: 3 }], 3)).toEqual([])
  })
})

const RUN = (kind: string, key: string, valueName = '', invalidates: string[] = []): AnalysisRunInfo =>
  ({ kind, key, valueName, label: valueName ? `${valueName} ${key}` : key, detail: '', valueNames: [], invalidates })
const KINDS = [{ kind: 'tracks', label: 'Tracks' }, { kind: 'hmm', label: 'HMM' },
               { kind: 'graphs', label: 'Neighbour graphs' }]

describe('runId / runRef', () => {
  it('round-trips a run identity, whatever characters the parts hold', () => {
    const r = { kind: 'contacts', key: 'live#flow.a+b', valueName: 'P14' }
    expect(runRef(runId(r))).toEqual(r)
  })
})

describe('runGroups', () => {
  it('unions runs across images with a per-run image count, in kind order, dropping empty kinds', () => {
    const g = runGroups(KINDS, {
      A: [RUN('hmm', 'movement'), RUN('tracks', 'whole_seg', 'P14')],
      B: [RUN('hmm', 'movement')],
    })
    expect(g.map(x => x.kind)).toEqual(['tracks', 'hmm'])
    expect(g[1].runs[0].count).toBe(2)
    expect(g[0].runs[0].count).toBe(1)
  })

  it('keeps one track set per segmentation — same source key, different valueName', () => {
    const g = runGroups(KINDS, { A: [RUN('tracks', 'whole_seg', 'P14'), RUN('tracks', 'whole_seg', 'OTI')] })
    expect(g[0].runs).toHaveLength(2)
  })
})

describe('alsoDeleted', () => {
  const groups = runGroups(KINDS, {
    A: [RUN('tracks', 'whole_seg', 'P14', ['HMM movement']), RUN('hmm', 'movement')],
    B: [RUN('tracks', 'whole_seg', 'P14', ['HMM movement', 'Track clusters default'])],
  })
  const tracks = groups[0].runs[0].id
  const hmm = groups[1].runs[0].id

  it('names what a picked track set takes with it, unioned across images', () => {
    expect(alsoDeleted(groups, [tracks])).toEqual(['HMM movement', 'Track clusters default'])
  })

  it('does not repeat a dependent the user already picked', () => {
    expect(alsoDeleted(groups, [tracks, hmm])).toEqual(['Track clusters default'])
  })

  it('is empty when nothing picked cascades', () => {
    expect(alsoDeleted(groups, [hmm])).toEqual([])
  })
})
