import { describe, it, expect } from 'vitest'
import { copyWarningLines, copySkippedLines, copyRemappedLines, renamePanelColumns, copyWarningAction, missingColumnsReason } from './gatingCopyWarnings'

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
    const [l] = copySkippedLines({ a: "channels don't match — CD4 (mean_intensity_0) not on this image" }, name)
    expect(l.text).toBe("Mouse 1: not copied, channels don't match")
    expect(l.pops).toEqual(['CD4 (mean_intensity_0) not on this image'])
  })
  it('keeps a reason without detail whole', () => {
    expect(copySkippedLines({ b: 'already gated' }, name)[0]).toEqual({ uid: 'b', text: 'b: not copied, already gated', pops: [] })
  })
})

describe('copyRemappedLines', () => {
  const name = (u: string) => ({ a: 'Mouse 1' } as Record<string, string>)[u] ?? u
  it('is empty when every target had the same channel order', () => {
    expect(copyRemappedLines(undefined, name)).toEqual([])
    expect(copyRemappedLines({ a: [] }, name)).toEqual([])
  })
  it('notes the image, names each matched channel once, and is not a warning', () => {
    const [l] = copyRemappedLines({ a: [
      { from: 'mean_intensity_0', to: 'mean_intensity_1', channel: 'CD4' },
      { from: 'nuc_mean_intensity_0', to: 'nuc_mean_intensity_1', channel: 'CD4' },
      { from: 'mean_intensity_1', to: 'mean_intensity_0', channel: 'CD8' }] }, name)
    expect(l).toEqual({ uid: 'a', text: 'Mouse 1: channels matched by name', pops: ['CD4', 'CD8'], info: true })
  })
})

describe('renamePanelColumns', () => {
  const swap = [{ from: 'mean_intensity_0', to: 'mean_intensity_1', channel: 'CD4' },
                { from: 'mean_intensity_1', to: 'mean_intensity_0', channel: 'CD8' }]
  it("points a copied plot's axes at the target's columns for the same channels", () => {
    const st: Record<string, unknown>[] = [
      { x: 'mean_intensity_0', y: 'area', z: 'mean_intensity_1', xt: 'logicle' },
      { channels: ['mean_intensity_1', 'mean_intensity_0', 'area'] }]
    renamePanelColumns(st, swap)
    expect(st[0]).toEqual({ x: 'mean_intensity_1', y: 'area', z: 'mean_intensity_0', xt: 'logicle' })
    expect(st[1].channels).toEqual(['mean_intensity_0', 'mean_intensity_1', 'area'])
  })
  it('leaves a layout alone when nothing was remapped', () => {
    const st: Record<string, unknown>[] = [{ x: 'mean_intensity_0' }]
    renamePanelColumns(st, undefined)
    expect(st[0].x).toBe('mean_intensity_0')
  })
})
