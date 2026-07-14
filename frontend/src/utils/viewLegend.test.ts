import { describe, it, expect } from 'vitest'
import { channelLegend, viewLegendSections } from './viewLegend'

describe('channelLegend', () => {
  it('maps visible single-hue channel layers to swatches', () => {
    const legend = channelLegend({
      gBT: { colormap: 'green', visible: true },
      SHG: { colormap: 'gray', visible: true },
    })
    expect(legend).toEqual([
      { label: 'gBT', colour: '#00ff00' },
      { label: 'SHG', colour: '#d4d4d4' },
    ])
  })

  it('skips hidden layers and non-channel (continuous / unknown) colormaps', () => {
    const legend = channelLegend({
      gBT: { colormap: 'green', visible: false },   // hidden
      heat: { colormap: 'viridis', visible: true }, // continuous → no channel colour
      lbl: { visible: true },                        // no colormap
    })
    expect(legend).toEqual([])
  })

  it('tolerates missing / empty layers', () => {
    expect(channelLegend(undefined)).toEqual([])
    expect(channelLegend(null)).toEqual([])
    expect(channelLegend({})).toEqual([])
  })
})

describe('viewLegendSections', () => {
  it('keeps non-empty sections in channel → population → colour-by order', () => {
    const secs = viewLegendSections({
      colourBy: [{ label: 'Directed', colour: '#ff1493' }],
      channels: [{ label: 'gBT', colour: '#00ff00' }],
      populations: [{ label: 'T cells', colour: '#00bfff' }],
    })
    expect(secs.map(s => s.title)).toEqual(['Channels', 'Populations', 'Colour by'])
  })

  it('drops empty groups (no bare headings)', () => {
    const secs = viewLegendSections({ channels: [{ label: 'gBT', colour: '#00ff00' }], populations: [] })
    expect(secs).toHaveLength(1)
    expect(secs[0].title).toBe('Channels')
  })
})
