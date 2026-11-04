import { describe, it, expect } from 'vitest'
import { lightPathStages, isMultiphoton, type LightPathImage } from './acquisitionLightPath'
import REAL from './acquisitionLightPath.fixture.json'

const real = REAL as unknown as LightPathImage
const stage = (img: LightPathImage, key: string) => lightPathStages(img).find(s => s.key === key)!
const texts = (img: LightPathImage, key: string) => stage(img, key).items.map(i => i.text)

describe('lightPathStages — real FV4000 two-photon stack', () => {
  it('numbers the nine stages in light-path order', () => {
    expect(lightPathStages(real).map(s => `${s.n} ${s.key}`)).toEqual([
      '1 laser', '2 power', '3 scanner', '4 objective', '5 sample',
      '6 filters', '7 detectors', '8 digitisation', '9 stack',
    ])
  })

  it('names the channels each laser excites with the image’s own channel names', () => {
    expect(texts(real, 'laser')).toEqual([
      'MaiTai e HP DS-OL · 900 nm → SHG, nuc-GFP',
      'INSIGHT X3-OL · 1140 nm → mem-TOM, CD169-Kat',
    ])
  })

  it('puts the Bright Z ramp at power control, from the file', () => {
    const power = stage(real, 'power').items
    expect(power.map(i => i.text)).toContain('Bright Z on: MaiTai e HP DS-OL 14.4 % → 21 % over 0 → 60 µm')
    expect(power.every(i => i.source === 'file')).toBe(true)
  })

  it('puts depth from the image metadata and the measured per-depth signal at the sample', () => {
    const sample = stage(real, 'sample').items
    expect(sample[0]).toEqual({ text: '60 µm deep · 31 planes', source: 'ome' })
    const ch = sample.find(i => i.text.startsWith('mem-TOM'))!
    expect(ch.source).toBe('measured')
    expect(ch.text).toMatch(/^mem-TOM brightest 1 %: \d+ at 0 µm → \d+ at 60 µm$/)
  })

  it('keeps the pixel size at the scanner and the frame interval at stack and time', () => {
    expect(stage(real, 'scanner').items).toContainEqual({ text: 'pixel 0.331 µm', source: 'ome' })
    expect(texts(real, 'stack')[0]).toBe('31 planes × 2 µm')
    expect(texts(real, 'stack')[1]).toMatch(/^3 frames · every /)
  })

  it('says what the file does not record instead of leaving a stage blank', () => {
    expect(stage(real, 'filters').items).toEqual([{ text: 'Dichroics and filters not recorded', source: 'missing' }])
  })

  it('lists no clipping when nothing clipped', () => {
    expect(texts(real, 'digitisation')).toEqual(['frame ×2 · sum'])
  })
})

describe('lightPathStages — what is not there', () => {
  it('says power with depth is unknown when the file has no Bright Z block', () => {
    expect(stage({ acquisition: { lasers: [] } }, 'power').items).toEqual(
      [{ text: 'Power with depth not recorded', source: 'missing' }])
  })

  it('says Bright Z off means the same power at every depth', () => {
    const img: LightPathImage = { acquisition: { brightZ: { enabled: false, points: [] } } }
    expect(texts(img, 'power')).toEqual(['Bright Z off: same power at every depth'])
  })

  it('flags a stack whose per-depth signal was never measured', () => {
    const img: LightPathImage = { ...real, cleanupFacts: { channels: real.cleanupFacts!.channels!.map(c => ({ ...c, profile: null })) } }
    expect(stage(img, 'sample').items.at(-1)).toEqual(
      { text: 'Signal by depth not measured yet (next denoise run)', source: 'missing' })
  })

  it('lists clipped channels at digitisation', () => {
    const chans = real.cleanupFacts!.channels!.map(c => (c.index === 1 ? { ...c, clippedPct: 0.25 } : c))
    expect(texts({ ...real, cleanupFacts: { channels: chans } }, 'digitisation'))
      .toContain('nuc-GFP 0.25 % of signal clipped')
  })
})

describe('isMultiphoton', () => {
  it('is true for a near-infrared laser and false for visible lines or nothing', () => {
    expect(isMultiphoton(real.acquisition)).toBe(true)
    expect(isMultiphoton({ lasers: [{ name: '488', wavelengthNm: 488 }, { name: '640', wavelengthNm: 640 }] })).toBe(false)
    expect(isMultiphoton(null)).toBe(false)
  })
})
