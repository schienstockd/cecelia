import { describe, it, expect } from 'vitest'
import {
  manyChannels, cycleGrid, filterGrid, channelList, filterChannelList, summariseStages, MANY_CHANNELS,
} from './acquisitionChannelTable'
import { lightPathStages, type LightPathImage } from './acquisitionLightPath'
import type { AcquisitionSettings } from './acquisitionSettings'
import REAL from './acquisitionChannelTable.fixture.json'
import FV4000 from './acquisitionLightPath.fixture.json'

const cyclic = REAL.cyclic as unknown as AcquisitionSettings
const noXpd = REAL.cyclicNoXpd as unknown as AcquisitionSettings
const spectral = REAL.spectral as unknown as AcquisitionSettings

const cellTexts = (acq: AcquisitionSettings) =>
  cycleGrid(acq)!.rows.map(r => [r.cycle, r.kind, ...cycleGrid(acq)!.columns.map(c => r.cells[c.key]?.text ?? '')])

describe('manyChannels', () => {
  it('keeps the per-stage lines up to eight channels and summarises above', () => {
    expect(MANY_CHANNELS).toBe(8)
    expect(manyChannels(cyclic)).toBe(false)     // 7 channels
    expect(manyChannels(spectral)).toBe(true)    // 33 channels
    expect(manyChannels(null)).toBe(false)
  })
})

describe('cycleGrid — real PhenoCycler run with its .xpd', () => {
  it('has one column per excitation band, in wavelength order, named by the dyes in it', () => {
    expect(cycleGrid(cyclic)!.columns.map(c => c.label)).toEqual(['DAPI', 'ATTO550', 'AF647', 'AF750'])
  })

  it('has one row per planned cycle; the cycles with no marker are the run’s blanks', () => {
    expect(cellTexts(cyclic)).toEqual([
      [1, 'blank', 'DAPI · 1 ms', '', '', ''],
      [2, 'markers', '', 'Podoplanin · 150 ms', 'CD68 · 100 ms', 'SMA · 100 ms'],
      [3, 'markers', '', 'CD45 · 150 ms', 'HIF1A · 150 ms', 'HLA-DR · 150 ms'],
      [4, 'blank', '', '', '', ''],
    ])
  })

  it('uses the image’s own channel names', () => {
    const names = (cyclic.channels ?? []).map(c => (c.name === 'CD68' ? 'Macrophage' : c.name ?? ''))
    const cell = cycleGrid(cyclic, names)!.rows[1].cells
    expect(Object.values(cell).map(c => c?.text)).toContain('Macrophage · 100 ms')
  })

  it('flags a cell whose exposure differs from the run plan, and only that one', () => {
    expect(cycleGrid(cyclic)!.rows.flatMap(r => Object.values(r.cells)).some(c => c?.offPlan)).toBe(false)
    const acq = { ...cyclic, channels: cyclic.channels!.map(c => (c.name === 'CD68' ? { ...c, plannedExposureMs: 150 } : c)) }
    const cells = cycleGrid(acq)!.rows.flatMap(r => Object.values(r.cells)).filter(c => c?.offPlan)
    expect(cells.map(c => [c!.text, c!.tip])).toEqual([['CD68 · 100 ms', 'Planned 150 ms · RX015 · BX015 · KP1']])
  })

  it('is null when no channel has a cycle', () => {
    expect(cycleGrid(spectral)).toBeNull()
  })
})

describe('cycleGrid — the same run without its .xpd', () => {
  it('does not call a cycle blank when the file cannot say', () => {
    expect(cellTexts(noXpd).map(r => r.slice(0, 2))).toEqual([[1, 'empty'], [2, 'markers'], [3, 'markers']])
  })
})

describe('filterGrid', () => {
  it('keeps the rows with a matching marker, dims the rest, and drops the blanks', () => {
    const g = filterGrid(cycleGrid(cyclic)!, 'cd')
    expect(g.rows.map(r => r.cycle)).toEqual([2, 3])
    const row2 = g.rows[0].cells
    expect(Object.values(row2).filter(c => c).map(c => [c!.text.split(' ')[0], c!.match])).toEqual([
      ['Podoplanin', false], ['CD68', true], ['SMA', false],
    ])
  })

  it('matches the reporter and dye too, and an empty filter changes nothing', () => {
    expect(filterGrid(cycleGrid(cyclic)!, 'rx062').rows.map(r => r.cycle)).toEqual([3])
    expect(filterGrid(cycleGrid(cyclic)!, '  ')).toEqual(cycleGrid(cyclic))
  })
})

describe('channelList — real 33-channel lambda stack (no cycles)', () => {
  it('has one row per channel: # · name · ex · em · detector', () => {
    const rows = channelList(spectral)
    expect(rows).toHaveLength(33)
    expect(rows[0]).toEqual({ id: '1', order: 1, name: '415', ex: 'HeNe633 + DPSS 561-10 + ArgonRemote + Diode 405-30',
                              em: '—', detector: 'HV 630 · gain 1' })
    expect(rows[32].name).toBe('T PMT')
  })

  it('filters by name', () => {
    expect(filterChannelList(channelList(spectral), 'pmt').map(r => r.order)).toEqual([33])
  })
})

describe('summariseStages', () => {
  const img = (acq: AcquisitionSettings): LightPathImage => ({ acquisition: acq })
  const summary = (acq: AcquisitionSettings, sel: number | null = null) => {
    const stages = summariseStages(lightPathStages(img(acq), { laserTargets: false }), acq, sel)
    return Object.fromEntries(stages.map(s => [s.key, s.items.map(i => i.text)]))
  }

  it('says what the channels span instead of one line each (widefield)', () => {
    const s = summary(cyclic)
    expect(s.illumination).toEqual(['XCiteMultiBandLed', '4 excitation bands'])
    expect(s.filters).toEqual(['2 filter sets · 4 emission bands'])
    expect(s.camera).toEqual(['Sony IMX421-based camera · binning 2×2', 'exposure 1–150 ms · gain 1'])
    expect(s.stack).toEqual(['3 cycles imaged (4 planned, 2 blank)', 'Autofluorescence subtracted by the scanner'])
  })

  it('adds the selected channel’s own lines back', () => {
    const order = cyclic.channels!.find(c => c.name === 'CD68')!.order
    const s = summary(cyclic, order)
    expect(s.illumination).toEqual(['XCiteMultiBandLed', '4 excitation bands', 'CD68 ex 594-653 nm'])
    expect(s.camera).toContain('CD68 · 100 ms · gain 1')
    expect(s.stack).toContain('CD68 cycle 2')
  })

  it('summarises the detectors of a lambda stack, and lasers lose their 33-name target list', () => {
    const s = summary(spectral)
    expect(s.detectors).toEqual(['HV 300–630 · gain 1'])
    expect(s.laser[0]).toBe('HeNe633 · 633 nm')
    expect(s.filters).toEqual(['Dichroics not recorded', 'Emission filters not recorded'])
  })

  it('points at the table for the measured per-channel signal, and shows the selected channel’s', () => {
    const real = FV4000 as unknown as LightPathImage
    const sample = (sel: number | null) => summariseStages(lightPathStages(real), real.acquisition, sel)
      .find(s => s.key === 'sample')!.items.map(i => i.text)
    expect(sample(null)).toEqual(['60 µm deep · 31 planes', 'Measured per channel: select one below'])
    expect(sample(2)).toHaveLength(2)
    expect(sample(2)[1]).toMatch(/brightest 1 %/)
  })
})

describe('filter words', () => {
  it('needs every word somewhere in the cell, like the other list searches', () => {
    expect(filterGrid(cycleGrid(cyclic)!, 'cd68 kp1').rows.map(r => r.cycle)).toEqual([2])
  })
})
