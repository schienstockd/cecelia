// The settings card laid out per technology, on real reader output (acquisitionCard.fixture.json: the
// readers run on test-data/acquisition), and its pages.
import { describe, it, expect } from 'vitest'
import { acquisitionCard, NONE, type CardImage } from './acquisitionCard'
import { cardPages } from './acquisitionCardPages'
import FIX from './acquisitionCard.fixture.json'

type Acq = CardImage['acquisition']
const ctx = { projectUid: 'p', printedOn: '2026-11-13' }
const card = (acq: unknown, extra: Partial<CardImage> = {}) =>
  acquisitionCard({ uid: 'u', name: 'n', acquisition: acq as Acq, ...extra }, ctx)
const label = (c: ReturnType<typeof card>, l: string) => c.settings.find(r => r.label === l)?.items.map(i => i.text)

describe('confocal, Zeiss tracks (LSM 780, ZEN black)', () => {
  const c = card(FIX.lsm780_multitrack)
  const t = c.detectorTable!
  it('groups channels by track, two channels in track 4', () => {
    expect(t.groupBy).toBe('Track')
    expect(t.groups.map(g => [g.label, g.rows.map(r => r.channel)])).toEqual([
      ['T1', ['Ch2-T1']], ['T2', ['ChS2-T2']], ['T3', ['ChS1-T3']], ['T4', ['Ch1-T4', 'T PMT-T4']]])
  })
  it('gives each track its lines with power, and drops the laser powers from the settings', () => {
    expect(t.groups.map(g => g.lines?.map(l => l.text))).toEqual([['633 nm · 10 %'], ['561 nm · 0.2 %'], ['488 nm · 0.2 %'], ['405 nm · 2 %']])
    expect((label(c, 'Power') ?? []).some(x => /%$/.test(x))).toBe(false)
  })
  it('keeps the band and pinhole per channel', () => {
    const r = t.groups[0].rows[0]
    expect(r.filter.source).toBe('file')
    expect(r.pinhole?.text).toMatch(/AU/)
    expect(t.cols.pinhole).toBe(true)
  })
})

describe('Airyscan (LSM 980)', () => {
  const c = card(FIX.lsm980_airyscan)
  const t = c.detectorTable!
  it('adds the processing mode and the beam splitters per track', () => {
    expect(t.cols.airyscan).toBe(true)
    expect(t.groups.every(g => g.rows[0].airyscan?.text === 'FastAiryScanSheppardSum')).toBe(true)
    expect(t.cols.splitters).toBe(true)
    expect(new Set(t.groups.map(g => g.splitters?.text)).size).toBeGreaterThan(1)
    expect(label(c, 'Emission filters') ?? []).not.toContain('Dichroics not recorded')
  })
  it('the confocal caveat, not the two-photon one', () => {
    expect(c.caveat).toMatch(/pinhole/)
    expect(c.caveat).not.toMatch(/laser age/)
  })
})

describe('a lambda run (LSM 880 spectral)', () => {
  const c = card(FIX.lsm880_spectral)
  it('collapses the 32 wavelength channels into one row, the transmitted channel kept', () => {
    const rows = c.detectorTable!.groups.flatMap(g => g.rows)
    expect(rows.map(r => r.channel)).toEqual(['Lambda 410–695 nm', 'T PMT'])
    expect(rows[0].spans).toBe(32)
    expect(rows[0].filter.text).toBe('410-695 nm')
  })
})

describe('confocal, Leica sequences (SP8)', () => {
  const c = card(FIX.sp8_sequential)
  it('groups by sequence, each with its own line and power', () => {
    const t = c.detectorTable!
    expect(t.groupBy).toBe('Sequence')
    expect(t.groups.map(g => g.label)).toEqual(['1', '2', '3', '4'])
    expect(t.groups.map(g => g.lines?.map(l => l.text))).toEqual(FIX.sp8_sequential.channels.map(ch =>
      ch.laserLines.map(l => `${l.wavelengthNm} nm · ${l.pct} %`)))
    expect(label(c, 'Lasers')!.filter(x => x.startsWith('OPSL 552'))).toHaveLength(1)   // two powers, one laser
    expect(label(c, 'Emission filters')).toContain('Sequential: one sequence per channel')
  })
})

describe('PhenoCycler run', () => {
  const c = card(FIX.phenocycler_cyclic)
  it('one row per marker, by cycle, with reagents and the band; no camera table', () => {
    expect(c.camera).toBeNull()
    expect(c.markers!.map(cy => [cy.cycle, cy.rows.map(m => m.marker)])).toEqual([
      [1, ['DAPI']], [2, ['Podoplanin', 'SMA', 'CD68']], [3, ['CD45', 'HLA-DR', 'HIF1A']]])
    const sma = c.markers![1].rows[1]
    const src = FIX.phenocycler_cyclic.channels.find(ch => ch.name === 'SMA')!
    expect([sma.clone.text, sma.reagent.text]).toEqual([src.clone, [src.reporter, src.barcode].filter(Boolean).join(' · ')])
    expect(sma.band.text).toMatch(/^\d+-\d+ → \d+-\d+$/)
    expect(c.markers![0].rows[0].clone).toEqual(NONE)          // a nuclear stain: a dash, not "not recorded"
  })
  it('names the run, shows a planned exposure only when the run differed', () => {
    expect(c.settings[0]).toMatchObject({ label: 'Run', items: [{ text: 'EXPERIMENT', source: 'file' }] })
    expect(c.markers![1].rows[0].exposure.text).toBe('150 ms')
    const off = card({ ...FIX.phenocycler_cyclic, channels: FIX.phenocycler_cyclic.channels.map((ch, i) => (i === 1 ? { ...ch, exposureMs: 200 } : ch)) })
    expect(off.markers![1].rows[0].exposure.text).toBe('200 ms (planned 150 ms)')
    expect(c.caveat).toMatch(/cycle number/)
  })
  it('a registered image names each scan’s run, and says when none was kept', () => {
    const reg = (cycles: object) => card({ ...FIX.phenocycler_cyclic, cycles })
    expect(reg({ count: 3, registered: true, experiments: ['A', null, 'C'] }).settings[0].items.map(i => i.text))
      .toEqual(['A · C', '1 scan(s) without a run file'])
    expect(reg({ count: 3, registered: true }).settings[0].items).toEqual([{ text: 'Run not recorded', source: 'missing' }])
  })
})

describe('the target table follows the image', () => {
  const facts = { channels: [{ index: 0, name: 'A', zeroPct: 4, clippedPct: 0, floor: { kind: 'photon-counted' as const, firstValue: null, photonStep: null, photons: null, evidence: null } }] }
  it('a widefield snapshot: no depth or time column, zeros without the photon reading', () => {
    const wf = { technology: 'widefield', channels: [{ order: 1, name: 'A', dyePreset: null, laser: null, detector: null, hv: null, gain: 1, offset: null }] }
    const c = card(wf, { cleanupFacts: facts, sizeZ: 1, sizeT: 1 })
    expect(c.targetCols).toEqual({ depth: false, time: false })
    expect(c.targets[0].zeros.text).toBe('4 %')
    expect(c.targetsMissing).toBe(false)
  })
  it('a laser-scanning z-stack keeps the depth column and the photon reading', () => {
    const c = card(FIX.lsm980_airyscan, { cleanupFacts: facts, sizeZ: 50, sizeT: 1 })
    expect(c.targetCols).toEqual({ depth: true, time: false })
    expect(c.targets[0].zeros.text).toBe('4 % · no photon')
  })
  it('no measurements at all is said, not left as an empty table', () => {
    expect(card(FIX.lsm980_airyscan).targetsMissing).toBe(true)
  })
})

describe('pages', () => {
  it('a small card is one page with everything on it', () => {
    const p = cardPages(card(FIX.sp8_sequential))
    expect(p).toHaveLength(1)
    expect(p[0].blocks.map(b => b.kind)).toEqual(['settings', 'channels', 'targets', 'fillIn', 'caveat'])
  })
  it('a long marker table continues on page 2, split between cycles, nothing lost', () => {
    const base = FIX.phenocycler_cyclic
    const channels = Array.from({ length: 14 }, (_, k) => base.channels.slice(1, 4).concat(base.channels[0]).map((ch, i) =>
      ({ ...ch, order: k * 4 + i + 1, cycle: k + 1, name: `${ch.name}-${k + 1}` }))).flat()
    const c = card({ ...base, channels })
    const pages = cardPages(c)
    expect(pages.length).toBeGreaterThan(1)
    const parts = pages.flatMap(p => p.blocks).filter(b => b.kind === 'channels') as { from: number; to: number; cont: boolean }[]
    expect(parts[0].from).toBe(0)
    expect(parts.at(-1)!.to).toBe(14)
    parts.slice(1).forEach((b, i) => { expect(b.from).toBe(parts[i].to); expect(b.cont).toBe(true) })
    expect(pages.at(-1)!.blocks.at(-1)!.kind).toBe('caveat')
  })
})
