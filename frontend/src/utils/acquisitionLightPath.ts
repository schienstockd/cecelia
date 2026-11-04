/**
 * The two-photon light-path diagram's content (`AcquisitionLightPath.vue`): this image's values placed
 * at the stage of the acquisition where each one acts — laser → power control → scanner → objective →
 * sample → filters → detectors → digitisation → stack and time — each tagged with where the value came
 * from. Numbers only; nothing here judges them (findings need thresholds set on real bad images,
 * docs/todo/ACQUISITION_ADVISOR_PLAN.md → P4).
 */

import { fmtNum, shortUnit, formatTimeIncrement } from './physicalSize'
import type { AcquisitionSettings } from './acquisitionSettings'

/** Where a value came from — drawn as a shape AND a colour, so it reads without colour vision. */
export type LightPathSource = 'ome' | 'file' | 'measured' | 'missing'

export const LIGHT_PATH_SOURCES: { key: LightPathSource; label: string }[] = [
  { key: 'ome', label: 'image metadata' },
  { key: 'file', label: 'microscope file' },
  { key: 'measured', label: 'measured at import' },
  { key: 'missing', label: 'not recorded' },
]

export interface LightPathItem { text: string; source: LightPathSource }
export interface LightPathStage { n: number; key: string; title: string; items: LightPathItem[] }

/** Per depth / per frame numbers from the import probe (`intensity_utils.PlaneProfile.summary`). */
export interface PlaneProfileAxis { p50: number[]; p99: number[]; zeroFrac: number[]; clipped: number[] }
/** Range and noise of a channel from the import probe (`intensity_utils.range_stats` / `noise_stats`). */
export interface ChannelRange { p1: number | null; p50: number | null; p999: number | null; max: number; hist: { binWidth: number; counts: number[] } }
export interface ChannelNoise {
  bgLevel: number | null; bgMedian: number | null; bgSpread: number | null; snr: number | null
  untestable: string | null; estimator?: string
}
export interface CleanupFactsChannel {
  index: number; name: string; zeroPct: number | null; clippedPct: number | null
  profile?: { byZ: PlaneProfileAxis; byT: PlaneProfileAxis } | null
  containerBits?: number | null; topValue?: number | null; saturated?: boolean | null
  range?: ChannelRange | null; noise?: ChannelNoise | null
}

/** The slice of the image payload the diagram reads. */
export interface LightPathImage {
  acquisition?: AcquisitionSettings | null
  channelNames?: string[]
  sizeZ?: number | null
  sizeT?: number | null
  physicalSizeX?: number | null
  physicalSizeZ?: number | null
  physicalSizeUnit?: string | null
  timeIncrement?: number | null
  timeIncrementUnit?: string | null
  cleanupFacts?: { channels?: CleanupFactsChannel[] } | null
}

/** Shortest excitation wavelength that still means multiphoton: one-photon lines stop near 640 nm,
 *  two-photon Ti:sapphire / OPO sources start near 680 nm. */
const MULTIPHOTON_MIN_NM = 680

/** Whether the diagram applies: a recorded laser in the near infrared. Only two-photon is drawn. */
export function isMultiphoton(acq: AcquisitionSettings | null | undefined): boolean {
  return (acq?.lasers ?? []).some(l => l.wavelengthNm != null && l.wavelengthNm >= MULTIPHOTON_MIN_NM)
}

const n = (x: number | null | undefined): string => (x == null ? '—' : fmtNum(x))
const join = (parts: (string | null | undefined | false)[]) => parts.filter(Boolean).join(' · ')
const item = (text: string, source: LightPathSource): LightPathItem => ({ text, source })

/** "1,240 at 0 µm → 310 at 60 µm" — p99 at the first and last plane of an axis. */
function endsText(p99: number[], at: (i: number) => string): string | null {
  if (p99.length < 2) return null
  const last = p99.length - 1
  return `${n(Math.round(p99[0]))} at ${at(0)} → ${n(Math.round(p99[last]))} at ${at(last)}`
}

export function lightPathStages(img: LightPathImage): LightPathStage[] {
  const acq = img.acquisition ?? {}
  const chans = acq.channels ?? []
  const facts = img.cleanupFacts?.channels ?? []
  const unit = shortUnit(img.physicalSizeUnit) || 'µm'
  const zStep = img.physicalSizeZ ?? null
  const sizeZ = img.sizeZ ?? 1
  const sizeT = img.sizeT ?? 1
  // The file's channel `order` is 1-based and matches the store's channels (the reader attaches the
  // settings only when the counts agree), so the user's names apply.
  const chName = (c: { order: number; name: string | null }) =>
    img.channelNames?.[c.order - 1] || c.name || `Channel ${c.order}`
  const depthAt = (i: number) => (zStep == null ? `plane ${i + 1}` : `${n(+(i * zStep).toFixed(2))} ${unit}`)

  // 1 · laser — which channels each one excites
  const laser = (acq.lasers ?? []).map(l => {
    const excites = chans.filter(c => c.laser === l.name).map(chName)
    const what = join([l.name, l.wavelengthNm != null && `${n(l.wavelengthNm)} nm`])
    return item(excites.length > 0 ? `${what} → ${excites.join(', ')}` : what, 'file')
  })

  // 2 · power control — transmission and whether it follows depth
  const power: LightPathItem[] = (acq.lasers ?? [])
    .filter(l => l.transmissionPct != null)
    .map(l => item(`${l.name} ${n(l.transmissionPct)} %`, 'file'))
  const bz = acq.brightZ
  if (bz?.enabled === true) {
    const pts = bz.points ?? []
    const lasers = Object.keys(pts[0]?.transmissionPct ?? {})
    for (const l of lasers) {
      power.push(item(`Bright Z on: ${l} ${pts.map(p => `${n(p.transmissionPct[l])} %`).join(' → ')} over ` +
                      `${pts.map(p => n(p.zFromStartUm)).join(' → ')} µm`, 'file'))
    }
    if (lasers.length === 0) power.push(item('Bright Z on', 'file'))
  } else if (bz?.enabled === false) {
    power.push(item('Bright Z off: same power at every depth', 'file'))
  } else {
    power.push(item('Power with depth not recorded', 'missing'))
  }

  // 3 · scanner
  const s = acq.scanner
  const scanner: LightPathItem[] = []
  if (s) scanner.push(item(join([s.type?.toLowerCase(), s.zoom != null && `zoom ${n(s.zoom)}×`,
                                 s.pixelDwellUs != null && `dwell ${n(s.pixelDwellUs)} µs`]), 'file'))
  if (img.physicalSizeX != null) scanner.push(item(`pixel ${n(img.physicalSizeX)} ${unit}`, 'ome'))

  // 4 · objective
  const o = acq.objective
  const objective = o ? [item(join([o.name, o.magnification != null && `${n(o.magnification)}×`,
                                    o.na != null && `NA ${n(o.na)}`, o.immersion]), 'file')]
                      : [item('Objective not recorded', 'missing')]

  // 5 · sample — how deep, and how the signal changes with depth
  const sample: LightPathItem[] = []
  if (sizeZ > 1) sample.push(item(`${depthAt(sizeZ - 1)} deep · ${sizeZ} planes`, 'ome'))
  for (const f of facts) {
    const ends = f.profile ? endsText(f.profile.byZ.p99, depthAt) : null
    if (ends) sample.push(item(`${f.name} brightest 1 %: ${ends}`, 'measured'))
  }
  if (sizeZ > 1 && !facts.some(f => f.profile)) {
    sample.push(item('Signal by depth not measured yet: refresh metadata', 'missing'))
  }

  // 6 · dichroics and filters — the file names neither
  const filters = [item('Dichroics and filters not recorded', 'missing')]

  // 7 · detectors
  const detectors = chans.map(c => item(join([chName(c), c.dyePreset, c.detector,
    c.hv != null && `HV ${n(c.hv)}`, c.gain != null && `gain ${n(c.gain)}`,
    c.offset != null && `offset ${n(c.offset)}`]), 'file'))
  if (acq.autoHV != null) detectors.push(item(`Auto HV ${acq.autoHV ? 'on' : 'off'}`, 'file'))

  // 8 · digitisation — integration, and voxels at the top of the range
  const digit: LightPathItem[] = []
  const integ = s?.integration
  if (integ?.type) {
    const how = integ.average === false ? 'sum' : integ.average === true ? 'average' : null
    digit.push(item(integ.type === 'None' ? 'No integration'
      : join([`${integ.type.toLowerCase()} ×${n(integ.count)}`, how]), 'file'))
  }
  for (const f of facts) {
    if (f.clippedPct != null && f.clippedPct > 0) digit.push(item(`${f.name} ${n(f.clippedPct)} % of signal clipped`, 'measured'))
  }

  // 9 · stack and time
  const stack: LightPathItem[] = []
  if (sizeZ > 1 && zStep != null) stack.push(item(`${sizeZ} planes × ${n(zStep)} ${unit}`, 'ome'))
  if (sizeT > 1) {
    const every = img.timeIncrement ? formatTimeIncrement(img.timeIncrement, img.timeIncrementUnit) : null
    stack.push(item(join([`${sizeT} frames`, every && `every ${every}`]), img.timeIncrement ? 'ome' : 'missing'))
    for (const f of facts) {
      const ends = f.profile ? endsText(f.profile.byT.p99, i => `frame ${i + 1}`) : null
      if (ends) stack.push(item(`${f.name} brightest 1 %: ${ends}`, 'measured'))
    }
  }

  const stages: [string, string, LightPathItem[]][] = [
    ['laser', 'Laser', laser], ['power', 'Power control', power], ['scanner', 'Scanner', scanner],
    ['objective', 'Objective', objective], ['sample', 'Sample', sample],
    ['filters', 'Dichroics · filters', filters], ['detectors', 'Detectors', detectors],
    ['digitisation', 'Digitisation', digit], ['stack', 'Stack · time', stack],
  ]
  return stages.map(([key, title, items], i) => ({ n: i + 1, key, title, items }))
}
