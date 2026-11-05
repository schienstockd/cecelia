import { describe, it, expect } from 'vitest'
import { dataPathChannels, dataPathNotes, firstDataPathChannel, hasDataPath, sparkPath } from './acquisitionDataPath'
import type { LightPathImage } from './acquisitionLightPath'
import REAL from './acquisitionLightPath.fixture.json'

const real = REAL as unknown as LightPathImage
const chans = () => dataPathChannels(real)
const texts = (i: number) => chans()[i].items.map(it => it.text)

describe('dataPathChannels — real FV4000 store (no clipping, photon-counted zeros)', () => {
  it('lists every probed channel by the image’s own name', () => {
    expect(chans().map(c => c.name)).toEqual(['SHG', 'nuc-GFP', 'mem-TOM', 'CD169-Kat'])
    expect(hasDataPath(real)).toBe(true)
  })

  it('pairs the store depth with the depth the microscope file records', () => {
    expect(texts(2)[0]).toBe('16-bit store · 16-bit recorded')
    expect(chans()[2].items[0].source).toBe('measured')
  })

  it('gives the range of non-zero voxels', () => {
    expect(texts(2)[1]).toBe('non-zero p1 21 · median 34 · p99.9 191 · max 550')
  })

  it('says no pile-up was found', () => {
    expect(texts(2)).toContain('no ceiling pile-up detected')
  })

  it('notes once, from the file, that summed frames can exceed one detector read', () => {
    expect(dataPathNotes(real)).toEqual([{ text: 'Frames summed ×2: values can exceed one detector read', source: 'file' }])
    expect(dataPathNotes({})).toEqual([])
  })

  it('says the zeros are voxels with no photon, with the evidence, and gives photons instead of an SNR', () => {
    expect(texts(2)).toContain('86 % of voxels at 0: likely no photon detected (photon-counted: nothing from 1 to 18, then a peak at 32)')
    expect(texts(2).at(-1)).toBe('photons per non-zero voxel: median 1.06 · p99.9 5.97 (~32 per photon)')
    expect(texts(2).some(t => t.includes('Noise floor'))).toBe(false)
  })

  it('carries the histogram and the per-depth series; time only when there are several frames', () => {
    const c = chans()[2]
    expect(c.hist).toHaveLength(32)
    expect(c.byZ).toHaveLength(31)
    expect(c.byT).toHaveLength(3)
  })
})

describe('dataPathChannels — what is not there', () => {
  it('lists a clipped channel with its ceiling and share of signal', () => {
    const f = real.cleanupFacts!.channels!.map(c => (c.index === 1 ? { ...c, saturated: true, topValue: 4095, clippedPct: 0.004 } : c))
    expect(dataPathChannels({ ...real, cleanupFacts: { channels: f } })[1].items.map(i => i.text))
      .toContain('clipped at 4095 (detected pile-up): 0.004 % of signal')
  })

  it('names a background clipped at 0 and keeps the noise floor as not measurable', () => {
    const f = real.cleanupFacts!.channels!.map(c => ({
      ...c,
      floor: { kind: 'clipped' as const, firstValue: 1, photonStep: null, photons: null, evidence: 'a spike at 0 over a background that runs on from 1' },
      noise: { ...c.noise!, untestable: 'background clipped at 0' },
    }))
    const t = dataPathChannels({ ...real, cleanupFacts: { channels: f } })[2].items
    expect(t.map(i => i.text)).toContain('86 % of voxels at 0: likely background clipped at 0 (a spike at 0 over a background that runs on from 1)')
    expect(t.at(-1)).toEqual({ text: 'Noise floor not measurable: background clipped at 0', source: 'missing' })
  })

  it('gives no cause for the zeros when the probe could not tell', () => {
    const f = real.cleanupFacts!.channels!.map(c => ({ ...c, floor: { ...c.floor!, kind: 'indeterminate' as const } }))
    expect(dataPathChannels({ ...real, cleanupFacts: { channels: f } })[2].items.map(i => i.text)).toContain('86 % of voxels at 0')
  })

  it('gives SNR and background when the noise floor is measurable', () => {
    const f = real.cleanupFacts!.channels!.map(c => ({ ...c, floor: null, noise: { bgLevel: 120, bgMedian: 100, bgSpread: 10.2, snr: 49.6, untestable: null } }))
    expect(dataPathChannels({ ...real, cleanupFacts: { channels: f } })[0].items.at(-1))
      .toEqual({ text: 'SNR 50 · background 100 ± 10', source: 'measured' })
  })

  it('is empty for an image never probed', () => {
    expect(dataPathChannels({})).toEqual([])
    expect(hasDataPath({})).toBe(false)
  })
})

describe('channels that need a look', () => {
  it('flags nothing on the real store: photon-counted zeros are not a loss', () => {
    expect(chans().every(c => c.attention.length === 0)).toBe(true)
    expect(firstDataPathChannel(chans())).toBe(0)
  })

  it('flags top clipping only where QC raised it, plus a clipped floor; opens the first flagged channel', () => {
    const f = real.cleanupFacts!.channels!.map(c =>
      c.index === 2 ? { ...c, saturated: true, topValue: 4095, floor: { ...c.floor!, kind: 'clipped' as const } }
        : c.index === 3 ? { ...c, saturated: true, topValue: 352 }  // probed before QC had a finding for it
        : c.index === 1 ? { ...c, saturated: true, topValue: 4095 } : c)
    const qc = { 'importImages.omezarr/default': { findings: [
      { level: 'warn' as const, code: 'import.channel_saturated', short: '', long: '', detail: { channel: 2 } },
      // below the 1 % warn level QC still reports the clipping, as `info` — the channel is still marked
      { level: 'info' as const, code: 'import.channel_saturated', short: '', long: '', detail: { channel: 1 } }] } }
    const cs = dataPathChannels({ ...real, cleanupFacts: { channels: f }, qc })
    expect(cs[2].attention).toEqual(['clipped at 4095', 'background clipped at 0'])
    expect(cs[1].attention).toEqual(['clipped at 4095'])
    expect(cs[3].attention).toEqual([])
    expect(firstDataPathChannel(cs)).toBe(1)   // the first flagged one, warn or info
  })

  it('has no channel to open on an unprobed image', () => {
    expect(firstDataPathChannel([])).toBe(null)
  })
})

describe('sparkPath', () => {
  it('draws a line across the box, highest value at the top', () => {
    expect(sparkPath([0, 5, 10], 20, 10)).toBe('M0 10 L10 5 L20 0')
  })
  it('draws one bar per value', () => {
    expect(sparkPath([10, 0], 4, 10, { bars: true })).toBe('M1 10 V0 M3 10 V10')
  })
  it('keeps small bins visible on a log scale', () => {
    const d = sparkPath([1e9, 10], 4, 10, { bars: true, log: true })
    expect(d).toMatch(/^M1 10 V0 M3 10 V8\.\d+$/)
  })
  it('stays flat when every value is 0', () => {
    expect(sparkPath([0, 0], 10, 4)).toBe('M0 4 L10 4')
  })
})
