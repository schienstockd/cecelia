import { describe, it, expect } from 'vitest'
import { copyWarningLines, copySkippedLines, copyWarningAction, missingColumnsReason } from './gatingCopyWarnings'

const names: Record<string, string> = { a: 'img2', b: 'img3' }
const nameOf = (uid: string) => names[uid] ?? uid

describe('copyWarningLines', () => {
  it('one line per target: columns + count of emptied populations', () => {
    const lines = copyWarningLines({
      a: [{ column: 'clusters.immune', pops: ['/live/immune', '/live/immune/T', '/live/immune/B', '/live/immune/NK'] }],
      b: [{ column: 'clusters.immune', pops: ['/immune'] }, { column: 'cd11c', pops: ['/dc', '/immune'] }],
    }, nameOf)
    expect(lines.map(l => l.text)).toEqual([
      'img2: no clusters.immune — 4 populations will be empty',
      'img3: no clusters.immune, cd11c — 2 populations will be empty',
    ])
    expect(lines[0].pops).toEqual(['immune', 'T', 'B', 'NK'])
  })
  it('nothing missing → no lines', () => {
    expect(copyWarningLines(undefined, nameOf)).toEqual([])
    expect(copyWarningLines({}, nameOf)).toEqual([])
    expect(copyWarningLines({ a: [] }, nameOf)).toEqual([])
  })
  it('singular', () => {
    expect(copyWarningLines({ a: [{ column: 'x', pops: ['/p'] }] }, nameOf)[0].text)
      .toBe('img2: no x — 1 population will be empty')
  })
})

describe('copyWarningAction', () => {
  it('cluster columns → run the clustering', () => {
    expect(copyWarningAction({ a: [{ column: 'clusters.immune', pops: ['/p'] }] })).toMatch(/^Run the clustering/)
  })
  it('any other column → add the measure', () => {
    expect(copyWarningAction({ a: [{ column: 'clusters.immune', pops: [] }, { column: 'cd11c', pops: [] }] }))
      .toMatch(/^Add those measures/)
  })
})

describe('missingColumnsReason', () => {
  it('is empty when nothing is missing', () => {
    expect(missingColumnsReason(undefined)).toBe('')
    expect(missingColumnsReason([])).toBe('')
  })
  it('names the missing columns, through the display label', () => {
    expect(missingColumnsReason(['clusters.immune'])).toBe('Empty: no clusters.immune on this image')
    const label = (c: string) => (c === 'mean_intensity_2' ? 'CD11c' : c)
    expect(missingColumnsReason(['clusters.immune', 'mean_intensity_2'], label))
      .toBe('Empty: no clusters.immune, CD11c on this image')
  })
})

describe('copySkippedLines', () => {
  const name = (u: string) => ({ a: 'Mouse 1' } as Record<string, string>)[u] ?? u
  it('is empty when nothing was refused', () => {
    expect(copySkippedLines(undefined, name)).toEqual([])
  })
  it('says which image was refused and why; the detail goes to the hover', () => {
    const [l] = copySkippedLines({ a: 'channel order differs — mean_intensity_0 is CD8 here, CD4 in the source' }, name)
    expect(l.text).toBe('Mouse 1: not copied, channel order differs')
    expect(l.pops).toEqual(['mean_intensity_0 is CD8 here, CD4 in the source'])
  })
  it('keeps a reason without detail whole', () => {
    expect(copySkippedLines({ b: 'already gated' }, name)[0]).toEqual({ uid: 'b', text: 'b: not copied, already gated', pops: [] })
  })
})
