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

  it('says on the channel itself when the scanner processed it and its photons were not measured', () => {
    const img = { cleanupFacts: { channels: [
      { index: 0, name: 'DAPI', zeroPct: 0, clippedPct: 0, photons: { notMeasured: null } },
      { index: 1, name: 'CD8', zeroPct: 0, clippedPct: 0, photons: { notMeasured: 'autofluorescence subtracted by the scanner' } },
    ] } } as unknown as LightPathImage
    const [dapi, cd8] = dataPathChannels(img)
    expect(dapi.items.some(i => i.text.startsWith('Photons not measurable'))).toBe(false)
    expect(cd8.items).toContainEqual({ text: 'Photons not measurable: autofluorescence subtracted by the scanner', source: 'missing' })
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

  it('puts each depth / time finding on its channel, with the channel\'s own number', () => {
    // detail shape as qc/profile.jl writes it (`perChannel` rows of channel + the trend key)
    const qc = { 'importImages.omezarr/default': { findings: [
      { level: 'info' as const, code: 'import.signal_fades', short: '', long: '',
        detail: { channels: [1, 2], perChannel: [{ channel: 1, fade: 0.428 }, { channel: 2, fade: 0.456 }] } },
      { level: 'info' as const, code: 'import.clipped_deep', short: '', long: '',
        detail: { channels: [2], perChannel: [{ channel: 2, clippedDeepShare: 0.71 }] } },
      { level: 'info' as const, code: 'import.darker_with_depth', short: '', long: '',
        detail: { channels: [0], perChannel: [{ channel: 0, depthChange: -0.4 }] } }] } }
    const cs = dataPathChannels({ ...real, qc })
    expect(cs[1].attention).toEqual(['fades 43% over the movie'])
    expect(cs[2].attention).toEqual(['fades 46% over the movie', '71% of clipping in the deeper half'])
    expect(cs[0].attention).toEqual(['40% darker with depth'])
    expect(cs[3].attention).toEqual([])
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

describe('dataPathChannels — a camera has no photon-counted floor', () => {
  // A widefield (DFC365 FX) channel whose histogram the floor heuristic read as photon-counted
  // (cleanupFacts as imported from a DM5500B .lif, trimmed to the fields read here).
  const ch = {
    index: 0, name: 'Channel 1', containerBits: 16, zeroPct: 0.1, clippedPct: 0, saturated: false,
    range: { max: 1781, p50: 75, p1: 22, p999: 639, hist: { binWidth: 55.7, counts: [1, 2] } },
    floor: { kind: 'photon-counted', photonStep: 9, evidence: 'nothing from 1 to 4, then a peak at 9', firstValue: 5,
             photons: { p50: 8.33, p999: 71 } },
    noise: { bgSpread: 10, bgMedian: 57, snr: 3.5, bgLevel: 72, untestable: null },
  }
  const img = (camera: boolean) => ({ cleanupFacts: { channels: [ch] },
    acquisition: camera ? { camera: { name: 'DFC365FX', binning: 2, bitDepth: 12 }, channels: [] } : { channels: [] },
  }) as unknown as LightPathImage

  it('reads the zeros without a cause and gives the SNR, not photons per voxel', () => {
    const t = dataPathChannels(img(true))[0].items.map(i => i.text)
    expect(t).toContain('0.1 % of voxels at 0')
    expect(t.some(x => x.startsWith('SNR 4'))).toBe(true)
    expect(t.join(' ')).not.toContain('photon')
  })

  it('a laser-scanning image keeps the photon-counted reading', () => {
    const t = dataPathChannels(img(false))[0].items.map(i => i.text).join(' ')
    expect(t).toContain('photon-counted')
    expect(t).toContain('photons per non-zero voxel')
  })
})
