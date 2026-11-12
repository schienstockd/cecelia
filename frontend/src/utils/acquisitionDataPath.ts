/**
 * The data-path panel's content (`AcquisitionDataPath.vue`): what landed in the file, per channel —
 * store depth, range used, clipping at the top, what the zeros are (no photon on a photon-counting
 * detector, or a background clipped at 0), noise floor and SNR or photons per voxel, and the shape of the
 * channel over depth and time. Everything comes from the import probe (`data_path.measure_store`,
 * through `cleanupFacts`), so it is `measured`; the recorded detector depth and integration mode come
 * from the microscope file. A check that cannot run says why. Numbers only, no verdicts
 * (docs/todo/ACQUISITION_ADVISOR_PLAN.md → P4b).
 */

import { fmtNum } from './physicalSize'
import { qcFindings } from '../lib/qc'
import type { CciaImage } from '../stores/project'
import type { LightPathImage, LightPathItem, CleanupFactsChannel, ChannelFloor } from './acquisitionLightPath'

export interface DataPathChannel {
  index: number
  name: string
  /** What on this channel needs a look, one short reason each; empty when nothing measured does. */
  attention: string[]
  items: LightPathItem[]
  /** Compact histogram counts over [0, max] (all voxels), or null when not measured. */
  hist: number[] | null
  /** p99 per z-plane / per frame, or null for a single plane / frame. */
  byZ: number[] | null
  byT: number[] | null
}

const n = (x: number | null | undefined): string => (x == null ? '—' : fmtNum(x))
const pct = (x: number) => (x >= 1 ? fmtNum(Math.round(x * 10) / 10) : fmtNum(+x.toPrecision(2)))

/** Why the zeros are there, with what the call rests on; nothing when it can't be told. A camera
 *  (`acquisition.camera`) never counts photons: a "photon-counted" floor measured on one is a misread
 *  of its histogram, so it is read as no call at all. */
function zeroCause(fl: ChannelFloor | null | undefined, camera: boolean): string {
  if (fl?.kind === 'photon-counted') return camera ? '' : `: likely no photon detected (photon-counted: ${fl.evidence})`
  if (fl?.kind === 'clipped') return `: likely background clipped at 0 (${fl.evidence})`
  return ''
}

function channelItems(f: CleanupFactsChannel, recordedBits: number | null | undefined, camera: boolean): LightPathItem[] {
  const items: LightPathItem[] = []
  const store = f.containerBits != null ? `${f.containerBits}-bit store` : null
  if (store) items.push({ text: recordedBits != null ? `${store} · ${n(recordedBits)}-bit recorded` : store, source: 'measured' })
  const r = f.range
  if (r) items.push({ text: `non-zero p1 ${n(r.p1)} · median ${n(r.p50)} · p99.9 ${n(r.p999)} · max ${n(r.max)}`, source: 'measured' })
  if (f.saturated === true) {
    items.push({ text: `clipped at ${n(f.topValue)} (detected pile-up): ${pct(f.clippedPct ?? 0)} % of signal`, source: 'measured' })
  } else if (f.saturated === false) {
    items.push({ text: 'no ceiling pile-up detected', source: 'measured' })
  }
  if (f.zeroPct != null) items.push({ text: `${pct(f.zeroPct)} % of voxels at 0${zeroCause(f.floor, camera)}`, source: 'measured' })
  // the import skipped the photon fit (and read no photon floor): the scanner processed this channel
  if (f.photons?.notMeasured) items.push({ text: `Photons not measurable: ${f.photons.notMeasured}`, source: 'missing' })
  const fl = f.floor
  if (fl?.kind === 'photon-counted' && fl.photons && !camera) {
    // Counts are the measure here: there is no background distribution to take an SNR against.
    items.push({ text: `photons per non-zero voxel: median ${n(fl.photons.p50)} · p99.9 ${n(fl.photons.p999)} (~${n(fl.photonStep)} per photon)`, source: 'measured' })
    return items
  }
  const nz = f.noise
  if (nz) {
    items.push(nz.snr != null
      ? { text: `SNR ${n(Math.round(nz.snr))} · background ${n(Math.round(nz.bgMedian ?? 0))} ± ${n(Math.round(nz.bgSpread ?? 0))}`, source: 'measured' }
      : { text: `Noise floor not measurable: ${nz.untestable ?? 'unknown'}`, source: 'missing' })
  }
  return items
}

/** Channels the import's QC flagged as clipped (`import.channel_saturated`, any level: qc.jl raises it
 *  for every detected pile-up — `warn` from 1 % of signal, `info` below). QC's rule, not a second one. */
function qcClippedChannels(img: LightPathImage): Set<number> {
  return new Set(qcFindings(img as CciaImage)
    .filter(f => f.code === 'import.channel_saturated' && typeof f.detail?.channel === 'number')
    .map(f => f.detail!.channel as number))
}

/** Per-channel note for each depth / time finding (qc/profile.jl), keyed by the finding's code and
 *  read from its `detail.perChannel` — the panel shows the number on the channel it belongs to. */
const PROFILE_NOTES: Record<string, { key: string; note: (v: number) => string }> = {
  'import.signal_fades': { key: 'fade', note: v => `fades ${pc(v)}% over the movie` },
  'import.background_rises': { key: 'backgroundRise', note: v => `background up ${pc(v - 1)}% over the movie` },
  'import.brighter_with_depth': { key: 'depthChange', note: v => `${pc(v)}% brighter with depth` },
  'import.darker_with_depth': { key: 'depthChange', note: v => `${pc(-v)}% darker with depth` },
  'import.clipped_deep': { key: 'clippedDeepShare', note: v => `${pc(v)}% of clipping in the deeper half` },
}
const pc = (v: number) => Math.round(v * 100)   // a whole percent from a fraction

function qcProfileNotes(img: LightPathImage): Map<number, string[]> {
  const out = new Map<number, string[]>()
  for (const f of qcFindings(img as CciaImage)) {
    const spec = PROFILE_NOTES[f.code]
    const per = f.detail?.perChannel
    if (!spec || !Array.isArray(per)) continue
    for (const row of per as Record<string, unknown>[]) {
      const ch = row.channel
      const v = row[spec.key]
      if (typeof ch !== 'number' || typeof v !== 'number') continue
      out.set(ch, [...(out.get(ch) ?? []), spec.note(v)])
    }
  }
  return out
}

/**
 * Measured losses the analysis cannot undo: clipping at the top (from the QC finding) and a background
 * clipped at 0 (dim signal cut away; no QC finding exists for it, the floor kind is the measurement).
 * A photon-counted floor or a low photon count is not on the list: that is how the detector reports,
 * not a loss, and "low" would need a threshold the question sets. Then the depth / time trends QC
 * flagged on this channel (`profileNotes`): not losses, but they change what an intensity means.
 */
export function channelAttention(f: CleanupFactsChannel, qcClipped: boolean, profileNotes: string[] = []): string[] {
  const out: string[] = []
  if (qcClipped) out.push(`clipped at ${n(f.topValue)}`)
  if (f.floor?.kind === 'clipped') out.push('background clipped at 0')
  return [...out, ...profileNotes]
}

/** The channel to open first: the first that needs a look, else the first. */
export function firstDataPathChannel(chans: DataPathChannel[]): number | null {
  return (chans.find(c => c.attention.length) ?? chans[0])?.index ?? null
}

/** Notes that hold for every channel: summed frames lift the ceiling past one detector read; on a slide
 *  scan, how much of it is tissue — the photon and structure numbers come from patches on it. */
export function dataPathNotes(img: LightPathImage): LightPathItem[] {
  const integ = img.acquisition?.scanner?.integration
  const summed = integ?.type && integ.type !== 'None' && integ.average === false && (integ.count ?? 1) > 1
  const out: LightPathItem[] = summed
    ? [{ text: `Frames summed ×${n(integ!.count)}: values can exceed one detector read`, source: 'file' }]
    : []
  const t = img.cleanupFacts?.tissue
  if (t && typeof t.fraction === 'number') {
    const pct = t.fraction * 100
    const share = `${pct < 1 ? pct.toFixed(1) : Math.round(pct)}% of the scan`
    out.push(t.patches
      ? { text: `Tissue: ${share}; signal measured on it`, source: 'measured' }
      : { text: `Tissue: none found; signal measured on a grid over the scan`, source: 'measured' })
  }
  return out
}

/** One entry per probed channel, in channel order. Empty when the image was never probed. */
export function dataPathChannels(img: LightPathImage): DataPathChannel[] {
  const facts = img.cleanupFacts?.channels ?? []
  const acq = img.acquisition
  const clipped = qcClippedChannels(img)
  const profile = qcProfileNotes(img)
  return facts.map(f => {
    const recorded = acq?.channels?.find(c => c.order === f.index + 1)?.bitDepth
    const z = f.profile?.byZ.p99 ?? []
    const t = f.profile?.byT.p99 ?? []
    return {
      index: f.index,
      name: img.channelNames?.[f.index] || f.name,
      attention: channelAttention(f, clipped.has(f.index), profile.get(f.index)),
      items: channelItems(f, recorded, acq?.camera != null),
      hist: f.range?.hist.counts ?? null,
      byZ: z.length > 1 ? z : null,
      byT: t.length > 1 ? t : null,
    }
  })
}

/** Whether any channel was probed with the per-channel range (the current probe). */
export function hasDataPath(img: LightPathImage): boolean {
  return (img.cleanupFacts?.channels ?? []).some(c => c.range)
}

/**
 * SVG path for a small series in a `w` × `h` box: a line through the points, or bars when `bars` is set
 * (one per value). `log` plots log10(1 + v) so a histogram dominated by one bin still shows the rest.
 * Flat at the bottom when every value is 0.
 */
export function sparkPath(values: number[], w: number, h: number, opts: { log?: boolean; bars?: boolean } = {}): string {
  if (!values.length) return ''
  const v = values.map(x => (opts.log ? Math.log10(1 + Math.max(0, x)) : x))
  const top = Math.max(...v) || 1
  const y = (x: number) => +(h - (x / top) * h).toFixed(2)
  if (opts.bars) {
    const bw = w / v.length
    return v.map((x, i) => `M${+(i * bw + bw / 2).toFixed(2)} ${h} V${y(x)}`).join(' ')
  }
  const dx = v.length > 1 ? w / (v.length - 1) : 0
  return v.map((x, i) => `${i ? 'L' : 'M'}${+(i * dx).toFixed(2)} ${y(x)}`).join(' ')
}
