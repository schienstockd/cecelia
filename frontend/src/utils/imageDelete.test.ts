import { describe, it, expect } from 'vitest'
import {
  versionCounts, labelCounts, orderDefaultLast,
  survivingVersions, resolveNewActive, unimportsImage,
} from './imageDelete'

const IMG = (versions: string[], labels: string[] = []) => ({
  filepaths: Object.fromEntries(versions.map(v => [v, `${v}.ome.zarr`])),
  labels:    Object.fromEntries(labels.map(l => [l, [`${l}.zarr`]])),
})

describe('versionCounts', () => {
  it('is the UNION, with how many images carry each name', () => {
    // the case that drove this: an intersection would hide cpCorrected entirely, so it could not be
    // deleted at all until the selection was narrowed
    expect(versionCounts([IMG(['default', 'cpCorrected']), IMG(['default'])]))
      .toEqual([{ name: 'default', count: 2 }, { name: 'cpCorrected', count: 1 }])
  })

  it('puts default first', () => {
    expect(versionCounts([IMG(['cpCorrected', 'default'])]).map(c => c.name))
      .toEqual(['default', 'cpCorrected'])
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
