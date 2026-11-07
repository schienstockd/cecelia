/**
 * The two-photon light-path diagram's content (`AcquisitionLightPath.vue`): this image's values placed
 * at the stage of the acquisition where each one acts — laser → power control → scanner → objective →
 * sample → filters → detectors → digitisation → stack and time — each tagged with where the value came
 * from. Numbers only; nothing here judges them (findings need thresholds set on real bad images,
 * docs/todo/ACQUISITION_ADVISOR_PLAN.md → P4).
 */

import type { CciaImage } from '../stores/project'
import { fmtNum, shortUnit, formatTimeIncrement } from './physicalSize'
import { depthRampName, framesShortOf, type AcquisitionSettings } from './acquisitionSettings'

/** Where a value came from — drawn as a shape AND a colour, so it reads without colour vision. */
export type LightPathSource = 'ome' | 'file' | 'measured' | 'missing'

export const LIGHT_PATH_SOURCES: { key: LightPathSource; label: string }[] = [
  { key: 'ome', label: 'image metadata' },
  { key: 'file', label: 'microscope file' },
  { key: 'measured', label: 'measured at import' },
  { key: 'missing', label: 'not recorded' },
]

export interface LightPathItem {
  text: string; source: LightPathSource
  /** The channel (`order`, 1-based) a per-channel item belongs to — the settings card puts those in its
   *  detector table instead of the settings list. */
  channel?: number
  /** As recorded, but what it means is our reading, not checked with the vendor or a facility. */
  unverified?: true
}
export interface LightPathStage { n: number; key: string; title: string; items: LightPathItem[] }

/** Per depth / per frame numbers from the import probe (`intensity_utils.PlaneProfile.summary`). */
export interface PlaneProfileAxis { p50: number[]; p99: number[]; zeroFrac: number[]; clipped: number[] }
/** Range and noise of a channel from the import probe (`intensity_utils.range_stats` / `noise_stats`). */
export interface ChannelRange { p1: number | null; p50: number | null; p999: number | null; max: number; hist: { binWidth: number; counts: number[] } }
export interface ChannelNoise {
  bgLevel: number | null; bgMedian: number | null; bgSpread: number | null; snr: number | null
  untestable: string | null; estimator?: string
}
/** What a channel's exact zeros are (`intensity_utils.floor_stats`); `kind` mirrors `FLOOR_KINDS` there. */
export interface ChannelFloor {
  kind: 'photon-counted' | 'clipped' | 'indeterminate' | 'none'
  firstValue: number | null; photonStep: number | null
  photons: { p50: number; p999: number } | null; evidence: string | null
}
export interface CleanupFactsChannel {
  index: number; name: string; zeroPct: number | null; clippedPct: number | null
  profile?: { byZ: PlaneProfileAxis; byT: PlaneProfileAxis } | null
  containerBits?: number | null; topValue?: number | null; saturated?: boolean | null
  range?: ChannelRange | null; floor?: ChannelFloor | null; noise?: ChannelNoise | null
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
  /** QC sidecars, as on the image payload (`lib/qc.ts` reads them). */
  qc?: CciaImage['qc']
}

/** Shortest excitation wavelength that still means multiphoton: one-photon lines stop near 640 nm,
 *  two-photon Ti:sapphire / OPO sources start near 680 nm. */
const MULTIPHOTON_MIN_NM = 680

/** Whether the diagram applies: a recorded laser in the near infrared. Only two-photon is drawn. */
export function isMultiphoton(acq: AcquisitionSettings | null | undefined): boolean {
  return (acq?.lasers ?? []).some(l => l.wavelengthNm != null && l.wavelengthNm >= MULTIPHOTON_MIN_NM)
}

/** The vendor's light-path slot names (`oir_meta._DICHROIC_SLOTS`), as the diagram labels them. */
const DICHROIC_SLOT_LABEL: Record<string, string> = {
  excitationDM: 'excitation DM', dmSlider: 'DM slider', irLaserDichroic: 'IR laser DM',
  firstMirrorCube: 'mirror cube 1', secondMirrorCube: 'mirror cube 2',
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

/** Auto HV on, and the HV the file records anyway (0 on the FV4000, 550 on the FVMPE-RS) — which may
 *  not be the HV used, so it is said here once instead of as each detector's setting. */
export function autoHVItem(chans: { hv: number | null }[]): LightPathItem {
  const hvs = [...new Set(chans.map(c => c.hv).filter((v): v is number => v != null))]
  return hvs.length > 0
    ? { text: `Auto HV on: file records HV ${hvs.map(n).join(', ')}, maybe not the HV used`, source: 'file', unverified: true }
    : item('Auto HV on', 'file')
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
  const ramp = depthRampName(acq)
  if (bz?.enabled === true) {
    const pts = bz.points ?? []
    const lasers = Object.keys(pts[0]?.transmissionPct ?? {})
    for (const l of lasers) {
      power.push(item(`${ramp} on: ${l} ${pts.map(p => `${n(p.transmissionPct[l])} %`).join(' → ')} over ` +
                      `${pts.map(p => n(p.zFromStartUm)).join(' → ')} µm`, 'file'))
    }
    if (lasers.length === 0) power.push(item(`${ramp} on: power changes with depth (ramp not read)`, 'file'))
  } else if (bz?.enabled === false) {
    power.push(item(`${ramp} off: same power at every depth`, 'file'))
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

  // 6 · dichroics and filters — named where the file records them
  // The reader keeps Empty slots, so "every slot empty" (recorded) differs from no slot found ([]).
  const slots = acq.dichroics ?? []
  const filled = slots.filter(d => d.name.toLowerCase() !== 'empty')
  const filters: LightPathItem[] = filled.map(d => item(`${DICHROIC_SLOT_LABEL[d.slot] ?? d.slot}: ${d.name}`, 'file'))
  if (slots.length > 0 && filled.length === 0) filters.push(item('No dichroic in the path', 'file'))
  for (const c of chans) {
    filters.push({ ...(c.emissionFilter ? item(`${chName(c)} ${c.emissionFilter}`, 'file')
                                         : item(`${chName(c)} filter not recorded`, 'missing')), channel: c.order })
  }
  if (filters.length === 0) filters.push(item('Dichroics and filters not recorded', 'missing'))
  else if (slots.length === 0) filters.unshift(item('Dichroics not recorded', 'missing'))

  // 7 · detectors — under Auto HV the recorded HV is not a setting to copy (FV4000 records 0), so it
  // moves to the Auto HV line instead of reading as each detector's value
  const autoHV = acq.autoHV === true
  const detectors: LightPathItem[] = chans.map(c => ({ ...item(join([chName(c), c.dyePreset, c.detector,
    !autoHV && c.hv != null && `HV ${n(c.hv)}`, c.gain != null && `gain ${n(c.gain)}`,
    c.offset != null && `offset ${n(c.offset)}`]), 'file'), channel: c.order }))
  if (autoHV) {
    detectors.push(autoHVItem(chans))
  } else if (acq.autoHV === false) {
    detectors.push(item('Auto HV off', 'file'))
  }

  // 8 · digitisation — integration, and voxels at the top of the range
  const digit: LightPathItem[] = []
  const integ = s?.integration
  if (integ?.type) {
    const how = integ.average === false ? 'sum' : integ.average === true ? 'average' : null
    // "None" with a count > 1 is what the FVMPE-RS file records; print both rather than pick one.
    const odd = integ.type === 'None' && integ.count != null && integ.count > 1
    digit.push({ ...item(integ.type === 'None'
      ? join(['No integration', odd && `count ${n(integ.count)} recorded`])
      : join([`${integ.type.toLowerCase()} ×${n(integ.count)}`, how]), 'file'), ...(odd ? { unverified: true as const } : {}) })
  }
  const kal = s?.kalman
  if (kal && (kal.average != null || kal.accumulate != null)) {
    digit.push(item(join(['Kalman', kal.average != null && `average ${n(kal.average)}`,
                          kal.accumulate != null && `accumulate ${n(kal.accumulate)}`]), 'file'))
  }
  const bits = [...new Set(chans.map(c => c.bitDepth).filter((v): v is number => v != null))]
  if (bits.length > 0) digit.push(item(`${bits.map(n).join(', ')} bit`, 'file'))
  for (const f of facts) {
    if (f.clippedPct != null && f.clippedPct > 0) digit.push(item(`${f.name} ${n(f.clippedPct)} % of signal clipped`, 'measured'))
  }

  // 9 · stack and time
  const stack: LightPathItem[] = []
  if (sizeZ > 1 && zStep != null) stack.push(item(`${sizeZ} planes × ${n(zStep)} ${unit}`, 'ome'))
  if (sizeT > 1) {
    const every = img.timeIncrement ? formatTimeIncrement(img.timeIncrement, img.timeIncrementUnit) : null
    stack.push(item(join([`${sizeT} frames`, every && `every ${every}`]), img.timeIncrement ? 'ome' : 'missing'))
    const short = framesShortOf(img.acquisition)
    if (short) stack.push(item(`${short}: stopped early or cropped`, 'file'))
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
