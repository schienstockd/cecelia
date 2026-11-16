import { describe, it, expect } from 'vitest'
import { setting, describeSettings, checkSettingsPatch, type PlotSettings } from './plotSettings'

function umapLike() {
  const colourBy = { value: 'cluster' as unknown }, pops = { value: [] as unknown }, bins = { value: 30 as unknown }
  const s: PlotSettings = {
    colourBy: setting(colourBy, () => ['cluster', 'population', 'attribute']),
    colourPops: setting(pops, async () => ['live::B/T', 'live::B/B'], true),
    bins: setting(bins),
  }
  return { s, colourBy, pops, bins }
}

describe('describeSettings', () => {
  it('reports each value, its options, and multi', async () => {
    const { s } = umapLike()
    expect(await describeSettings(s)).toEqual({
      colourBy: { value: 'cluster', options: ['cluster', 'population', 'attribute'] },
      colourPops: { value: [], options: ['live::B/T', 'live::B/B'], multi: true },
      bins: { value: 30 },
    })
  })
})

describe('checkSettingsPatch', () => {
  it('accepts offered values, in the order given', async () => {
    const { s } = umapLike()
    const r = await checkSettingsPatch(s, { colourPops: ['live::B/T'], colourBy: 'population' })
    expect(r.apply).toEqual([['colourPops', ['live::B/T']], ['colourBy', 'population']])
    expect(r.rejected).toEqual({})
  })
  it('refuses an undeclared key, a value not offered, and a wrong shape — and writes nothing', async () => {
    const { s, colourBy } = umapLike()
    const r = await checkSettingsPatch(s, { kind: 'x', colourBy: 'rainbow', colourPops: 'live::B/T', bins: 'many' })
    expect(r.apply).toEqual([])
    expect(Object.keys(r.rejected).sort()).toEqual(['bins', 'colourBy', 'colourPops', 'kind'])
    expect(r.rejected.colourBy).toContain('one of: cluster, population, attribute')
    expect(colourBy.value).toBe('cluster')
  })
  it('refuses a list member that is not offered', async () => {
    const { s } = umapLike()
    expect((await checkSettingsPatch(s, { colourPops: ['live::B/T', 'live::X'] })).rejected.colourPops).toBe('not offered: live::X')
  })
  it('free setting takes a value of its current type', async () => {
    const { s } = umapLike()
    expect((await checkSettingsPatch(s, { bins: 50 })).apply).toEqual([['bins', 50]])
  })
  it('refuses a patch that is not an object', async () => {
    const { s } = umapLike()
    expect((await checkSettingsPatch(s, ['colourBy'])).rejected).toHaveProperty('')
  })
})
