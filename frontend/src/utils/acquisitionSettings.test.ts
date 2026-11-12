import { describe, it, expect } from 'vitest'
import { acquiredAtText, microscopeLine, acquisitionNotice, acquisitionRows, depthRampName, framesShortOf, microscopeName, type AcquisitionSettings, type MicroscopeRequest } from './acquisitionSettings'

// Shapes copied from `oir_meta.parse_settings` on test-data/acquisition/oir_*_imageProperties.xml.
const FV4000: AcquisitionSettings = {
  system: 'FV4000', softwareVersion: '3.2.1.85',
  objective: { name: 'XLPLN25XWM', magnification: 25.0, na: 1.05, immersion: 'water' },
  lasers: [{ name: 'MaiTai e HP DS-OL', wavelengthNm: 900.0, transmissionPct: 14.4 }],
  brightZ: { enabled: true, points: [
    { zFromStartUm: 0.0, transmissionPct: { 'MaiTai e HP DS-OL': 14.4 } },
    { zFromStartUm: 60.0, transmissionPct: { 'MaiTai e HP DS-OL': 21.0 } }] },
  scanner: { type: 'Resonant', zoom: 1.5, pixelDwellUs: 0.033, integration: { type: 'Frame', count: 2.0, average: false } },
  autoHV: true, pinholeUm: 50.0,
  channels: [{ order: 1, name: 'CH1', dyePreset: 'SHG', laser: 'MaiTai e HP DS-OL', detector: 'BNDD1', hv: 0.0, gain: 1.0, offset: 0.0 }],
}

const row = (rows: { k: string; v: string }[], k: string) => rows.find(r => r.k === k)?.v

describe('acquisitionRows', () => {
  it('shows the recorded settings, one row per thing', () => {
    const rows = acquisitionRows(FV4000)
    expect(row(rows, 'Microscope')).toBe('FV4000 · software 3.2.1.85')
    expect(row(rows, 'Objective')).toBe('XLPLN25XWM · 25× · NA 1.05 · water')
    expect(row(rows, 'Laser')).toBe('MaiTai e HP DS-OL · 900 nm · 14.4 %')
    expect(row(rows, 'Scanner')).toBe('resonant · zoom 1.5× · dwell 0.033 µs')
    expect(row(rows, 'Integration')).toBe('frame ×2 · sum')
    // Auto HV is on: the recorded HV 0 is not listed as the detector's setting
    expect(row(rows, 'CH1')).toBe('SHG · MaiTai e HP DS-OL · BNDD1')
    expect(row(rows, 'Auto HV')).toBe('on')
    const manual = acquisitionRows({ ...FV4000, autoHV: false })
    expect(row(manual, 'CH1')).toBe('SHG · MaiTai e HP DS-OL · BNDD1 · HV 0')
  })

  it('Bright Z on shows the ramp; off says off even when points are stored', () => {
    expect(row(acquisitionRows(FV4000), 'Bright Z')).toBe('on · MaiTai e HP DS-OL 14.4 → 21 % over 0 → 60 µm')
    const off = { ...FV4000, brightZ: { ...FV4000.brightZ!, enabled: false } }
    expect(row(acquisitionRows(off), 'Bright Z')).toBe('off')
  })

  it('a ramp with a mid-stack peak shows every point', () => {
    const bz = { enabled: true, points: [
      { zFromStartUm: 0, transmissionPct: { L: 10 } },
      { zFromStartUm: 30, transmissionPct: { L: 40 } },
      { zFromStartUm: 60, transmissionPct: { L: 20 } }] }
    expect(row(acquisitionRows({ ...FV4000, brightZ: bz }), 'Bright Z')).toBe('on · L 10 → 40 → 20 % over 0 → 30 → 60 µm')
  })

  it('no settings, no rows', () => {
    expect(acquisitionRows(null)).toEqual([])
    expect(acquisitionRows(undefined)).toEqual([])
  })

  it('integration "None" reads as none', () => {
    const s = { ...FV4000, scanner: { ...FV4000.scanner!, integration: { type: 'None', count: 2 } } }
    expect(row(acquisitionRows(s), 'Integration')).toBe('none')
  })

  it('a multi-phase file says only the first phase is shown', () => {
    expect(row(acquisitionRows({ ...FV4000, phaseCount: 2 }), 'Phases')).toBe('2 · first shown')
    expect(row(acquisitionRows(FV4000), 'Phases')).toBeUndefined()
  })
})

describe('acquisitionNotice — the section never vanishes silently', () => {
  const url = (req: MicroscopeRequest) => `issue:${JSON.stringify(req)}`
  const acq = { system: 'FVMPE-RS', softwareVersion: '2.6.1.243' }

  it('says nothing more under a diagram', () => {
    expect(acquisitionNotice(acq, null, true, url)).toBeNull()
  })
  it('asks for the microscope when its settings are read but its setup has no diagram yet', () => {
    expect(acquisitionNotice(acq, null, false, url)).toEqual({
      text: 'No light-path diagram for this setup yet',
      action: { label: 'Request your microscope',
                url: `issue:${JSON.stringify({ what: 'diagram', system: 'FVMPE-RS', softwareVersion: '2.6.1.243' })}` } })
  })
  it('asks for the microscope when the file type has no reader', () => {
    expect(acquisitionNotice(null, { reason: 'format', ext: '.czi' }, false, url)).toEqual({
      text: 'Settings are not read from .czi files yet',
      action: { label: 'Request your microscope', url: `issue:${JSON.stringify({ what: 'settings', ext: '.czi' })}` } })
  })
  it('names the other reasons, and an image never looked at', () => {
    expect(acquisitionNotice(null, { reason: 'source' }, false, url)?.text).toBe('Source file not reachable: refresh metadata when it is')
    expect(acquisitionNotice(null, { reason: 'other-stack' }, false, url)?.text).toMatch(/another stack/)
    expect(acquisitionNotice(null, null, false, url)?.text).toBe('Not read yet: refresh metadata')
  })
})

describe('microscope name and a stopped-early time-lapse', () => {
  it('prints the acquisition time as the scope PC recorded it', () => {
    expect(acquiredAtText({ acquiredAt: '2026-07-16T13:44:05.228+10:00' })).toBe('2026-07-16 13:44')
    expect(acquiredAtText({})).toBeNull()
    expect(acquiredAtText({ acquiredAt: 'garbage' })).toBeNull()
  })
  it('heads the diagram with system, software, stand and date', () => {
    expect(microscopeLine({ system: 'FV4000', softwareVersion: '3.2.1.85', microscopeBody: 'BX63LF',
                            acquiredAt: '2026-07-16T13:44:05.228+10:00' })).toBe('FV4000 · software 3.2.1.85 · BX63LF · 2026-07-16 13:44')
    expect(microscopeLine({ microscopeBody: 'BX63LF' })).toBeNull()
  })
  it('names the system with its software', () => {
    expect(microscopeName({ system: 'FVMPE-RS', softwareVersion: '2.6.1.243' })).toBe('FVMPE-RS · software 2.6.1.243')
    expect(microscopeName({})).toBeNull()
  })
  it('says how many of the planned frames were acquired', () => {
    expect(framesShortOf({ timelapse: { count: 45, acquired: 21 } })).toBe('21 of 45 planned frames')
    expect(framesShortOf({ timelapse: { count: 45 } })).toBeNull()
    expect(acquisitionRows({ system: 'X', timelapse: { count: 45, acquired: 21 } }))
      .toContainEqual({ k: 'Time-lapse', v: '21 of 45 planned frames (stopped early or cropped)' })
  })
})

describe('a Zeiss .czi names the depth ramp its own way', () => {
  // Shape copied from `czi_meta.parse_settings` on test-data/acquisition/czi_lsm780_nlo_metadata.xml.
  const czi: AcquisitionSettings = {
    source: 'czi', system: 'LSM 780, AxioObserver', softwareVersion: '13.0.1.518',
    lasers: [{ name: 'Chameleon', wavelengthNm: 775, transmissionPct: 2 }],
    brightZ: { enabled: false, points: [] },
  }
  it('says Auto Z brightness for Zeiss and Bright Z otherwise', () => {
    expect(depthRampName(czi)).toBe('Auto Z brightness')
    expect(depthRampName({ ...czi, source: 'oir' })).toBe('Bright Z')
    expect(depthRampName(null)).toBe('Bright Z')
    expect(acquisitionRows(czi)).toContainEqual({ k: 'Auto Z brightness', v: 'off' })
  })
})

describe('a Leica widefield .lif: camera, cube, exposure, gain', () => {
  // What `lif_meta.parse_settings` returns for Image002 of test-data/acquisition/lif_dm5500b_* (trimmed
  // to the fields the rows read).
  const lif: AcquisitionSettings = {
    source: 'lif', technology: 'widefield', system: 'DM5500B', softwareVersion: 'LAS X 3.4.2.18368',
    acquiredAt: '2026-07-09T04:31:57+00:00',
    objective: { name: 'HC PL FLUOTAR 20x/0.50 DRY', magnification: 20, na: 0.5, immersion: 'dry' },
    lasers: [], camera: { name: 'DFC365FX', binning: 2, bitDepth: 12, sensorPixelUm: 6.45 },
    channels: [
      { order: 1, name: 'New 1', dyePreset: null, laser: null, detector: 'DFC365FX', hv: null, gain: 1.998573, offset: null,
        bitDepth: 12, filterCube: 'A4', exposureMs: 122.536, illuminationIntensity: 3 },
    ],
  }

  it('shows the camera once and each channel by cube, exposure, gain and lamp step', () => {
    const rows = acquisitionRows(lif)
    expect(rows.find(r => r.k === 'Camera')?.v).toBe('DFC365FX · binning 2×2 · 12 bit')
    // the camera row names the detector, so the channel doesn't repeat it
    expect(rows.find(r => r.k === 'New 1')!.v).toBe('cube A4 · 122.536 ms · gain 1.999 · lamp 3')
    expect(rows.some(r => r.k === 'Laser')).toBe(false)
  })

  it('qptiff: light source, processing and both bands per channel; a zone-less time stays local', () => {
    const qp: AcquisitionSettings = { source: 'qptiff', technology: 'widefield', system: 'PhenoImager Fusion',
      softwareVersion: 'Fusion 2.4.1', acquiredAt: '2026-11-05T09:13:34', lightSource: 'XCiteMultiBandLed',
      processing: 'OffsetAndShading', saturationProtection: 'EnhancedDynamicRange', lasers: [],
      camera: { name: 'Sony IMX421-based camera', binning: 1, bitDepth: 14, sensorPixelUm: null },
      channels: [{ order: 2, name: 'ATTO 550', dyePreset: null, laser: null, detector: 'Sony IMX421-based camera', hv: null,
                   gain: 1, offset: 0, filterCube: 'DAPI / ATTO550 / AF750', excitationFilter: '533-559 nm',
                   emissionFilter: '570-640 nm', exposureMs: 393.21 }] }
    const rows = acquisitionRows(qp)
    expect(rows.find(r => r.k === 'Light source')?.v).toBe('XCiteMultiBandLed')
    expect(rows.find(r => r.k === 'Processing')?.v).toBe('OffsetAndShading')
    expect(rows.find(r => r.k === 'ATTO 550')!.v)
      .toBe('cube DAPI / ATTO550 / AF750 · ex 533-559 nm · em 570-640 nm · 393.21 ms · gain 1')
    expect(acquiredAtText(qp)).toBe('2026-11-05 09:13')
  })

  it('PhenoCycler run: cycles, reagents per marker, and autofluorescence already subtracted', () => {
    // values as qptiff_meta reads a Fusion 2.4.0 run with its .xpd
    const run: AcquisitionSettings = { source: 'qptiff', technology: 'widefield', modes: ['tiled', 'cyclic'], lasers: [],
      camera: { name: 'Sony IMX421-based camera', binning: 1, bitDepth: 14 },
      cycles: { count: 3, planned: 4, blank: 2, experiment: 'EXPERIMENT' },
      channels: [
        { order: 1, name: 'DAPI', dyePreset: 'DAPI', laser: null, detector: 'cam', hv: null, gain: 1, offset: 0, cycle: 1, afSubtracted: true, exposureMs: 1 },
        { order: 4, name: 'CD68', dyePreset: 'AF647', laser: null, detector: 'cam', hv: null, gain: 1, offset: 0, cycle: 2, afSubtracted: true,
          reporter: 'RX015', clone: 'KP1', barcode: 'BX015', exposureMs: 100, plannedExposureMs: 100 },
      ] }
    const rows = acquisitionRows(run)
    expect(rows.find(r => r.k === 'Cycles')?.v).toBe('3 imaged · 4 planned · 2 blank · EXPERIMENT')
    expect(rows.find(r => r.k === 'Autofluorescence')?.v).toBe('subtracted by the scanner')
    expect(rows.find(r => r.k === 'CD68')!.v).toBe('cycle 2 · AF647 · RX015 · clone KP1 · 100 ms · gain 1')
  })

  it('marks a UTC-only time stamp as UTC', () => {
    expect(acquiredAtText(lif)).toBe('2026-07-09 04:31 UTC')
    expect(acquiredAtText({ acquiredAt: '2026-07-16T13:44:00+10:00' })).toBe('2026-07-16 13:44')
  })

  it('a laser-scanning channel still shows no camera gain', () => {
    const rows = acquisitionRows({ channels: [{ order: 1, name: 'Ch1', dyePreset: null, laser: 'MaiTai', detector: 'PMT1', hv: 550, gain: 1, offset: 0 }] })
    expect(rows[0].v).toBe('MaiTai · PMT1 · HV 550')
  })
})
