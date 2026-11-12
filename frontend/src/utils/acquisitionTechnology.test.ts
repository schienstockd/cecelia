import { describe, it, expect } from 'vitest'
import { acquisitionTechnology, lightPathStages, TECHNOLOGY_STAGES, type LightPathImage, type Technology } from './acquisitionLightPath'
import type { AcquisitionSettings } from './acquisitionSettings'

// The knowledge files the assistant reads: a technology's diagram stages must be its knowledge stages,
// so a symptom names the stage the user is looking at. `extends` + `stageOrder` as server.py resolves.
interface KnowledgeFile { technology: string; extends?: string; stageOrder?: string[]; stages: { key: string }[] }
const FILES = import.meta.glob('../../../mcp/cecelia_mcp/acquisition_knowledge/technologies/*.json',
                               { eager: true, import: 'default' }) as Record<string, KnowledgeFile>
const knowledge: Record<string, KnowledgeFile> = Object.fromEntries(Object.values(FILES).map(t => [t.technology, t]))
const stageKeys = (tech: string): string[] => {
  const t = knowledge[tech]
  return t.stageOrder ?? (t.extends ? stageKeys(t.extends) : t.stages.map(s => s.key))
}

describe('every technology the diagram draws has knowledge with the same stages', () => {
  for (const tech of Object.keys(TECHNOLOGY_STAGES) as Technology[]) {
    it(tech, () => {
      expect(knowledge[tech], `no knowledge file for ${tech}`).toBeTruthy()
      expect(TECHNOLOGY_STAGES[tech].map(([k]) => k)).toEqual(stageKeys(tech))
    })
  }
})

describe('acquisitionTechnology', () => {
  it('takes what the reader recorded', () => {
    expect(acquisitionTechnology({ technology: 'widefield' })).toBe('widefield')
    expect(acquisitionTechnology({ technology: 'airyscan' })).toBe('airyscan')
  })
  it('falls back to the near-infrared rule for settings read before readers recorded it', () => {
    expect(acquisitionTechnology({ lasers: [{ name: 'MaiTai', wavelengthNm: 900 }] })).toBe('two-photon')
    expect(acquisitionTechnology({ lasers: [{ name: 'OPSL', wavelengthNm: 488 }] })).toBeNull()
    expect(acquisitionTechnology(null)).toBeNull()
  })
})

describe('lightPathStages per technology', () => {
  // What `lif_meta` returns for the SP8 fixture's WT image (trimmed to the fields the stages read).
  const sp8: AcquisitionSettings = {
    source: 'lif', technology: 'confocal', system: 'TCS SP8', sequential: true, pinholeAiry: 1, pinholeUm: 65.27,
    objective: { name: 'HC PL APO CS2 40x/1.30 OIL', magnification: 40, na: 1.3, immersion: 'oil' },
    lasers: [{ name: 'OPSL 552', wavelengthNm: 552, transmissionPct: 10 }],
    channels: [{ order: 1, name: 'dTomato', dyePreset: 'dTomato', laser: 'OPSL 552', detector: 'PMT 2', hv: 721, gain: null,
                 offset: 0, bitDepth: 8, emissionFilter: '561-617 nm' }],
  }
  it('confocal: a pinhole stage, sequential noted with the filters', () => {
    const st = lightPathStages({ acquisition: sp8 } as LightPathImage)
    expect(st.map(s => s.key)).toContain('pinhole')
    expect(st.find(s => s.key === 'pinhole')!.items[0].text).toBe('1 AU · 65.27 µm')
    expect(st.find(s => s.key === 'filters')!.items[0].text).toBe('Sequential: one sequence per channel')
  })

  it('widefield: illumination, cube and camera instead of lasers and detectors', () => {
    const wf: AcquisitionSettings = { technology: 'widefield', camera: { name: 'DFC365FX', binning: 2 }, lasers: [],
      channels: [{ order: 1, name: 'New 1', dyePreset: null, laser: null, detector: 'DFC365FX', hv: null, gain: 2, offset: null,
                   filterCube: 'A4', exposureMs: 122.5, illuminationIntensity: 3 }] }
    const st = lightPathStages({ acquisition: wf, channelNames: ['ASC'] } as LightPathImage)
    expect(st.map(s => s.key)).toEqual(['illumination', 'filters', 'objective', 'sample', 'camera', 'digitisation', 'stack'])
    expect(st.find(s => s.key === 'filters')!.items[0].text).toBe('ASC cube A4')
    expect(st.find(s => s.key === 'camera')!.items.map(i => i.text)).toEqual(['DFC365FX · binning 2×2', 'ASC · 122.5 ms · gain 2'])
    expect(st.find(s => s.key === 'illumination')!.items[0].text).toBe('ASC intensity 3')
  })

  it('widefield slide scanner (qptiff): light source, excitation and emission bands', () => {
    // values as qptiff_meta reads a Fusion 2.4.1 scan
    const qp: AcquisitionSettings = { source: 'qptiff', technology: 'widefield', lightSource: 'XCiteMultiBandLed',
      camera: { name: 'Sony IMX421-based camera', binning: 1, bitDepth: 14 }, lasers: [],
      channels: [{ order: 1, name: 'DAPI', dyePreset: null, laser: null, detector: 'Sony IMX421-based camera', hv: null,
                   gain: 1, offset: 0, filterCube: 'DAPI / ATTO550 / AF750', excitationFilter: '377-405 nm',
                   emissionFilter: '440-466 nm', exposureMs: 1.5 }] }
    const st = lightPathStages({ acquisition: qp } as LightPathImage)
    expect(st.find(s => s.key === 'illumination')!.items.map(i => i.text)).toEqual(['XCiteMultiBandLed', 'DAPI ex 377-405 nm'])
    expect(st.find(s => s.key === 'filters')!.items[0].text).toBe('DAPI cube DAPI / ATTO550 / AF750 · em 440-466 nm')
  })

  it('brightfield has no light path yet: no technology, so no diagram', () => {
    expect(acquisitionTechnology({ technology: 'brightfield' })).toBeNull()
  })

  it('cyclic: the stack stage lists the cycles and each marker\'s cycle', () => {
    const run: AcquisitionSettings = { technology: 'widefield', modes: ['tiled', 'cyclic'], cycles: { count: 3, blank: 2 },
      channels: [{ order: 2, name: 'Podoplanin', dyePreset: null, laser: null, detector: null, hv: null, gain: 1, offset: 0, cycle: 2 }] }
    const st = lightPathStages({ acquisition: run } as LightPathImage)
    expect(st.find(s => s.key === 'stack')!.items.map(i => i.text))
      .toEqual(['Stitched from tiles', '3 cycles imaged (2 blank)', 'Podoplanin cycle 2'])
  })

  it('airyscan: the processing stage names each channel\'s mode', () => {
    const ary: AcquisitionSettings = { technology: 'airyscan', channels: [{ order: 1, name: 'AF647-T1', dyePreset: null, laser: null,
      detector: null, hv: 700, gain: 1, offset: 0, airyscan: 'FastAiryScanSheppardSum' }] }
    const st = lightPathStages({ acquisition: ary } as LightPathImage)
    expect(st.find(s => s.key === 'processing')!.items[0].text).toBe('AF647-T1 FastAiryScanSheppardSum')
    expect(st.find(s => s.key === 'detectors')!.title).toBe('Array detector')
  })
})
