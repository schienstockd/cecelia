import { describe, it, expect } from 'vitest'
import { acquisitionCard, hasAcquisitionCard, type CardImage } from './acquisitionCard'
import REAL from './acquisitionLightPath.fixture.json'

// Real output for zolIMa 20armH (FV4000): reader settings + the import probe's measurements.
const real = { ...(REAL as unknown as CardImage), uid: '20armH', name: 'M2c-WT_KAT-SWHL-GFP-Tom-res_0001',
  oriPath: '/mnt/staging/x/M2c-WT_KAT-SWHL-GFP-Tom-res_0001.oir' }
const ctx = { projectUid: 'zolIMa', printedOn: '2026-11-06' }
const card = acquisitionCard(real, ctx)

describe('acquisitionCard — real FV4000 two-photon image', () => {
  it('heads the card with the scope line, source file and both uids', () => {
    expect(card.scope).toBe('FV4000 · software 3.2.1.85 · BX63LF · 2026-07-16 13:44')
    expect(card.sourceFile).toBe('M2c-WT_KAT-SWHL-GFP-Tom-res_0001.oir')
    expect([card.projectUid, card.imageUid, card.printedOn]).toEqual(['zolIMa', '20armH', '2026-11-06'])
  })

  it('lists the settings in light-path order, without per-channel or measured items', () => {
    expect(card.settings.map(r => `${r.n} ${r.label}`)).toEqual([
      '1 Laser', '2 Power', '3 Scanner', '4 Objective', '6 Dichroics', '8 Digitisation', '9 Stack · time'])
    const all = card.settings.flatMap(r => r.items)
    expect(all.every(i => i.source !== 'measured' && i.channel == null)).toBe(true)
    expect(card.settings.find(r => r.label === 'Dichroics')!.items.map(i => i.text))
      .toEqual(['excitation DM: MIRROR', 'IR laser DM: LCDM690-1050', 'mirror cube 1: DM760', 'mirror cube 2: MIRROR'])
    expect(card.settings.find(r => r.label === 'Power')!.items.map(i => i.text))
      .toContain('Bright Z on: MaiTai e HP DS-OL 14.4 % → 21 % over 0 → 60 µm')
  })

  it('has one detector row per channel, with the image’s own channel names and the filters', () => {
    expect(card.detectors.map(d => [d.channel, d.dye, d.detector, d.filter.text])).toEqual([
      ['SHG', 'SHG', 'BNDD1', 'BA410-455'], ['nuc-GFP', 'eGFP', 'BNDD2', 'BA495-540'],
      ['mem-TOM', 'tdTomato', 'RNDD3', 'ET585/40M'], ['CD169-Kat', 'Katushka2S', 'RNDD4', 'ET720/60M']])
  })

  it('keeps the unverified flag on HV recorded under Auto HV, and gain/offset per channel', () => {
    expect(card.autoHV).toEqual({ text: 'Auto HV on: file records HV 0, maybe not the HV used', source: 'file', unverified: true })
    expect(card.detectors.every(d => d.hv === null)).toBe(true)
    expect(card.detectors.map(d => d.gainOffset.text)).toEqual(Array(4).fill('gain 1 · offset 0'))
  })

  it('takes the target from what was measured at import', () => {
    const t = card.targets[2]
    expect(t.channel).toBe('mem-TOM')
    expect(t.clipped).toEqual({ text: '0 %', source: 'measured' })
    expect(t.zeros).toEqual({ text: '86 % · no photon', source: 'measured' })
    expect(t.byDepth.text).toMatch(/^\d[\d,]* → \d[\d,]*$/)
    expect(t.byTime.source).toBe('measured')
    expect(card.targetsMissing).toBe(false)
  })

  it('is deterministic', () => {
    expect(acquisitionCard(real, ctx)).toEqual(card)
  })
})

describe('acquisitionCard — what is not there', () => {
  it('prints unmeasured targets as not measured, never a number', () => {
    const img: CardImage = { ...real, cleanupFacts: { channels: real.cleanupFacts!.channels!.map(c =>
      ({ index: c.index, name: c.name, zeroPct: null, clippedPct: null })) } }
    const c = acquisitionCard(img, ctx)
    expect(c.targets[0]).toEqual({ channel: 'SHG', clipped: { text: 'not measured', source: 'missing' },
      zeros: { text: 'not measured', source: 'missing' }, byDepth: { text: 'not measured', source: 'missing' },
      byTime: { text: 'not measured', source: 'missing' } })
    expect(c.targetsMissing).toBe(true)
  })

  it('says a missing filter is not recorded, and lists HV per channel when Auto HV is off', () => {
    const chans = real.acquisition!.channels!.map((c, i) => ({ ...c, emissionFilter: i === 0 ? null : c.emissionFilter, hv: 600 }))
    const c = acquisitionCard({ ...real, acquisition: { ...real.acquisition, autoHV: false, channels: chans } }, ctx)
    expect(c.detectors[0].filter).toEqual({ text: 'not recorded', source: 'missing' })
    expect(c.autoHV).toBeNull()
    expect(c.detectors.map(d => d.hv)).toEqual(Array(4).fill({ text: '600', source: 'file' }))
  })

  it('prints Bright Z off even when depth points are stored', () => {
    const c = acquisitionCard({ ...real, acquisition: { ...real.acquisition,
      brightZ: { ...real.acquisition!.brightZ!, enabled: false } } }, ctx)
    expect(c.settings.find(r => r.label === 'Power')!.items.map(i => i.text))
      .toContain('Bright Z off: same power at every depth')
  })

  it('prints "No dichroic in the path" when every recorded slot is Empty', () => {
    const empty = real.acquisition!.dichroics!.map(d => ({ ...d, name: 'Empty' }))
    const c = acquisitionCard({ ...real, acquisition: { ...real.acquisition, dichroics: empty } }, ctx)
    expect(c.settings.find(r => r.label === 'Dichroics')!.items).toEqual([{ text: 'No dichroic in the path', source: 'file' }])
  })

  it('only two-photon images get a card', () => {
    expect(hasAcquisitionCard(real)).toBe(true)
    expect(hasAcquisitionCard({ acquisition: { lasers: [{ name: '488', wavelengthNm: 488 }] } })).toBe(false)
    expect(hasAcquisitionCard({})).toBe(false)
  })
})

// Real output: czi_meta.parse_settings on test-data/acquisition/czi_lsm780_nlo_metadata.xml (LSM 780
// two-photon). The .czi reader names no filters or dichroics and records HV per detector.
const CZI = {"source": "czi", "system": "LSM 780, AxioObserver", "softwareVersion": "13.0.1.518", "objective": {"name": "LD C-Apochromat 63x/1.15 W Korr M27", "magnification": 63.0, "na": 1.1500000000000001, "immersion": "water", "refractiveIndex": 1.333}, "lasers": [{"name": "Chameleon", "wavelengthNm": 775.0, "transmissionPct": 2.0}], "brightZ": {"enabled": false, "points": []}, "scanner": {"type": null, "zoom": 1.0, "pixelDwellUs": 0.9848, "integration": {"type": "Average", "count": 4, "average": null}}, "autoHV": null, "pinholeUm": null, "channels": [{"order": 1, "name": "NDD R1", "dyePreset": null, "laser": "Chameleon", "detector": null, "hv": 780.0, "gain": 1.0, "offset": 2.0, "detectorMode": null, "bitDepth": 8}, {"order": 2, "name": "NDD R2", "dyePreset": null, "laser": "Chameleon", "detector": null, "hv": 640.0, "gain": 1.0, "offset": 1.0, "detectorMode": null, "bitDepth": 8}, {"order": 3, "name": "NDD R3", "dyePreset": null, "laser": "Chameleon", "detector": null, "hv": 520.0, "gain": 1.0, "offset": 1.0, "detectorMode": null, "bitDepth": 8}, {"order": 4, "name": "T PMT", "dyePreset": null, "laser": "Chameleon", "detector": null, "hv": 100.0, "gain": 1.0, "offset": 0.0, "detectorMode": null, "bitDepth": 8}], "zStack": {"stepUm": 0.37, "count": 120.0}} as const

describe('acquisitionCard — a two-photon .czi (Zeiss LSM 780)', () => {
  const c = acquisitionCard({ uid: 'z', name: 'nlo', acquisition: CZI as unknown as CardImage['acquisition'] }, ctx)
  it('gets a card, with HV per detector and filters printed as not recorded', () => {
    expect(hasAcquisitionCard({ acquisition: CZI as unknown as CardImage['acquisition'] })).toBe(true)
    expect(c.autoHV).toBeNull()
    expect(c.detectors.map(d => d.hv?.text)).toEqual(['780', '640', '520', '100'])
    expect(c.detectors.every(d => d.filter.source === 'missing')).toBe(true)
    expect(c.settings.find(r => r.label === 'Dichroics')!.items).toEqual([{ text: 'Dichroics not recorded', source: 'missing' }])
  })
  it('names the depth ramp the Zeiss way', () => {
    expect(c.settings.find(r => r.label === 'Power')!.items.map(i => i.text).join(' | ')).toMatch(/Auto Z brightness off/)
  })
})

describe('a widefield card: camera table instead of detectors', () => {
  // `lif_meta` output for a DM5500B image (trimmed to the fields the card reads).
  const wf = { uid: 'w', name: 'speck', channelNames: ['ASC', 'Nuclei'], acquisition: {
    source: 'lif', technology: 'widefield', system: 'DM5500B', lasers: [],
    camera: { name: 'DFC365FX', binning: 2, bitDepth: 12 },
    objective: { name: 'HC PL FLUOTAR 20x/0.50 DRY', magnification: 20, na: 0.5, immersion: 'dry' },
    channels: [
      { order: 1, name: 'New 1', dyePreset: null, laser: null, detector: 'DFC365FX', hv: null, gain: 2, offset: null, filterCube: 'A4', exposureMs: 122.5, illuminationIntensity: 3 },
      { order: 2, name: 'New 2', dyePreset: null, laser: null, detector: 'DFC365FX', hv: null, gain: 2.6, offset: null, filterCube: 'Y5', exposureMs: 122.5, illuminationIntensity: 4 },
    ] } } as const
  const c = acquisitionCard(wf as unknown as Parameters<typeof acquisitionCard>[0], { projectUid: 'p', printedOn: 'today' })

  it('has a card, rows in widefield order, and the camera per channel', () => {
    expect(hasAcquisitionCard(wf as unknown as Parameters<typeof hasAcquisitionCard>[0])).toBe(true)
    expect(c.detectors).toEqual([])
    expect(c.camera!.map(r => [r.channel, r.cube.text, r.exposure.text, r.gain.text, r.lamp.text])).toEqual([
      ['ASC', 'A4', '122.5 ms', '2', '3'], ['Nuclei', 'Y5', '122.5 ms', '2.6', '4']])
    expect(c.settings.map(r => r.label)).not.toContain('Laser')
    expect(c.settings.map(r => r.label)).toContain('Objective')
  })
})
