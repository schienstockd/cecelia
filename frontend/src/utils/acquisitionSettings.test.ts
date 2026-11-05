import { describe, it, expect } from 'vitest'
import { acquisitionNotice, acquisitionRows, framesShortOf, microscopeName, type AcquisitionSettings } from './acquisitionSettings'

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
    expect(row(rows, 'CH1')).toBe('SHG · MaiTai e HP DS-OL · BNDD1 · HV 0')
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
  const url = (ext: string) => `issue:${ext}`
  const acq = { system: 'FVMPE-RS', softwareVersion: '2.6.1.243' }

  it('says nothing more under a diagram', () => {
    expect(acquisitionNotice(acq, null, true, url)).toBeNull()
  })
  it('says why there is no diagram for settings that are not two-photon', () => {
    expect(acquisitionNotice(acq, null, false, url)?.text).toBe('Light-path diagram: two-photon only so far')
  })
  it('asks for the microscope when the file type has no reader', () => {
    expect(acquisitionNotice(null, { reason: 'format', ext: '.czi' }, false, url)).toEqual({
      text: 'Settings are not read from .czi files yet', action: { label: 'Request your microscope', url: 'issue:.czi' } })
  })
  it('names the other reasons, and an image never looked at', () => {
    expect(acquisitionNotice(null, { reason: 'source' }, false, url)?.text).toBe('Source file not reachable: refresh metadata when it is')
    expect(acquisitionNotice(null, { reason: 'other-stack' }, false, url)?.text).toMatch(/another stack/)
    expect(acquisitionNotice(null, null, false, url)?.text).toBe('Not read yet: refresh metadata')
  })
})

describe('microscope name and a stopped-early time-lapse', () => {
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
