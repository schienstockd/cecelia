import { describe, it, expect } from 'vitest'
import { copyWarningLines, copyWarningAction } from './gatingCopyWarnings'

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
