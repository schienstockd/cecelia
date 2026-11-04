/**
 * The data-path panel's content (`AcquisitionDataPath.vue`): what landed in the file, per channel —
 * store depth, range used, clipping at the top and at zero, noise floor and SNR, and the shape of the
 * channel over depth and time. Everything comes from the import probe (`data_path.measure_store`,
 * through `cleanupFacts`), so it is `measured`; the recorded detector depth and integration mode come
 * from the microscope file. A check that cannot run says why. Numbers only, no verdicts
 * (docs/todo/ACQUISITION_ADVISOR_PLAN.md → P4b).
 */

import { fmtNum } from './physicalSize'
import type { LightPathImage, LightPathItem, CleanupFactsChannel } from './acquisitionLightPath'

export interface DataPathChannel {
  name: string
  items: LightPathItem[]
  /** Compact histogram counts over [0, max] (all voxels), or null when not measured. */
  hist: number[] | null
  /** p99 per z-plane / per frame, or null for a single plane / frame. */
  byZ: number[] | null
  byT: number[] | null
}

const n = (x: number | null | undefined): string => (x == null ? '—' : fmtNum(x))
const pct = (x: number) => (x >= 1 ? fmtNum(Math.round(x * 10) / 10) : fmtNum(+x.toPrecision(2)))

function channelItems(f: CleanupFactsChannel, recordedBits: number | null | undefined): LightPathItem[] {
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
  if (f.zeroPct != null) items.push({ text: `${pct(f.zeroPct)} % of voxels at 0`, source: 'measured' })
  const nz = f.noise
  if (nz) {
    items.push(nz.snr != null
      ? { text: `SNR ${n(Math.round(nz.snr))} · background ${n(Math.round(nz.bgMedian ?? 0))} ± ${n(Math.round(nz.bgSpread ?? 0))}`, source: 'measured' }
      : { text: `Noise floor not measurable: ${nz.untestable ?? 'unknown'}`, source: 'missing' })
  }
  return items
}

/** Notes that hold for every channel: summed frames lift the ceiling past one detector read. */
export function dataPathNotes(img: LightPathImage): LightPathItem[] {
  const integ = img.acquisition?.scanner?.integration
  const summed = integ?.type && integ.type !== 'None' && integ.average === false && (integ.count ?? 1) > 1
  return summed ? [{ text: `Frames summed ×${n(integ!.count)}: values can exceed one detector read`, source: 'file' }] : []
}

/** One entry per probed channel, in channel order. Empty when the image was never probed. */
export function dataPathChannels(img: LightPathImage): DataPathChannel[] {
  const facts = img.cleanupFacts?.channels ?? []
  const acq = img.acquisition
  return facts.map(f => {
    const recorded = acq?.channels?.find(c => c.order === f.index + 1)?.bitDepth
    const z = f.profile?.byZ.p99 ?? []
    const t = f.profile?.byT.p99 ?? []
    return {
      name: img.channelNames?.[f.index] || f.name,
      items: channelItems(f, recorded),
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
