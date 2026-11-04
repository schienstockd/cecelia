import { describe, it, expect } from 'vitest'
import { acquisitionRows, type AcquisitionSettings } from './acquisitionSettings'

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
