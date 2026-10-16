/**
 * The claims the AF roles figure makes, pinned:
 *   1. An AF channel removes the AF; the same channel listed as competing would leave a good share.
 *   2. Each marker keeps its own cell — the leak into the AF channel is fitted and removed, so a bright
 *      cell is not hollowed.
 *   3. The figure follows the selection: a column per picked channel plus one per corrected marker.
 *   4. The verdict agrees with the numbers drawn.
 */
import { describe, expect, it } from 'vitest'
import { afRolesFigure, afScene, correctAll, keptPct, MAX_MARKERS } from './afVis'

const one = { markers: ['CART'], afs: ['AF'], competing: [], exclusive: true }
const two = { markers: ['GFP', 'TOM'], afs: ['AF'], competing: ['SHG'], exclusive: true }

describe('afVis', () => {
  it('an AF channel removes the AF; as competing a good share would stay', () => {
    const scene = afScene(one)
    expect(keptPct(correctAll(scene)[0], scene.markers[0], scene.granule)).toBeLessThan(10)
    expect(keptPct(correctAll(scene, true)[0], scene.markers[0], scene.granule)).toBeGreaterThan(25)
  })

  it('every marker keeps its own cell', () => {
    // alone it loses only the AF holes; with a second marker and a competing signal it also shares
    // the voxels they overlap, which is what those roles are for
    for (const [inp, floor] of [[one, 90], [two, 70]] as const) {
      const scene = afScene(inp)
      correctAll(scene).forEach((o, i) =>
        expect(keptPct(o, scene.markers[i], scene.cells[i])).toBeGreaterThan(floor))
    }
  })

  it('one column per picked channel plus one per corrected marker', () => {
    const f = afRolesFigure(two)
    expect(f.headings).toEqual(['GFP', 'TOM', 'AF', 'SHG', 'GFP corrected', 'TOM corrected'])
    expect(f.vis.columns).toHaveLength(6)
    for (const r of f.vis.rows) expect(r.cells).toHaveLength(6)
  })

  it('caps the columns and falls back to a generic marker', () => {
    const many = afRolesFigure({ markers: ['a', 'b', 'c', 'd', 'e'], afs: [], competing: [],
                                 exclusive: true })
    expect(many.headings.filter(h => h.endsWith('corrected'))).toHaveLength(MAX_MARKERS)
    expect(afRolesFigure({ markers: [], afs: [], competing: [], exclusive: true }).headings[0])
      .toBe('Marker')
  })

  it('cells carrying two markers are shared out when "different cell types" is off', () => {
    const scene = afScene({ ...two, exclusive: false })
    const [gfp, tom] = correctAll(scene)
    // the double-positive cell sits at (19, 24) in both markers; each keeps part of it
    expect(gfp[19][24]).toBeGreaterThan(0)
    expect(tom[19][24]).toBeGreaterThan(0)
  })

  it('the verdict is read off the drawn numbers', () => {
    const f = afRolesFigure(one)
    const left = f.vis.rows.find(r => r.key === 'keptAf')!.cells[2].text
    expect(f.note).toContain(left)
    expect(afRolesFigure({ ...one, afs: [] }).note).toMatch(/^No AF channel/)
  })
})
