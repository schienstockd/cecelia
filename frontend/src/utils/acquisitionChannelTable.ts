/**
 * The Acquisition section with many channels (a PhenoCycler run, a lambda stack): above
 * `MANY_CHANNELS` the light-path stages say what the channels span ("4 excitation bands",
 * "exposure 1–400 ms") instead of one line per channel, and the per-channel detail moves to one
 * channel table under them — a cycle × dye grid when the reader recorded cycles (`qptiff_meta`),
 * else one row per channel. Selecting a channel puts its own lines back into the stages.
 * Rendered by `components/AcquisitionChannelTable.vue` + `AcquisitionLightPath.vue`.
 */

import { fmtNum } from './physicalSize'
import { matchesQuery } from './textQuery'
import type { AcquisitionChannel, AcquisitionSettings } from './acquisitionSettings'
import type { LightPathItem, LightPathStage } from './acquisitionLightPath'

/** Up to this many channels each stage lists them one per line; above, it summarises. */
export const MANY_CHANNELS = 8

export function manyChannels(acq: AcquisitionSettings | null | undefined): boolean {
  return (acq?.channels?.length ?? 0) > MANY_CHANNELS
}

const join = (parts: (string | null | undefined | false)[]) => parts.filter(Boolean).join(' · ')
const chName = (c: AcquisitionChannel, names?: string[]) => names?.[c.order - 1] || c.name || `Channel ${c.order}`
const distinct = <T>(xs: (T | null | undefined)[]): T[] => [...new Set(xs.filter((x): x is T => x != null && x !== ''))]

/** "1–400" over the recorded values, "150" when they agree, null when none is recorded. */
function range(xs: (number | null | undefined)[]): string | null {
  const v = xs.filter((x): x is number => x != null)
  if (v.length === 0) return null
  const lo = Math.min(...v), hi = Math.max(...v)
  return lo === hi ? fmtNum(lo) : `${fmtNum(lo)}–${fmtNum(hi)}`
}

/** A band's edges as recorded ("533-559 nm" → [533, 559]), null when it is not a range. */
export function bandEnds(band: string | null | undefined): [number, number] | null {
  const m = band?.match(/^\s*(\d+(?:\.\d+)?)\s*-\s*(\d+(?:\.\d+)?)/)
  return m ? [Number(m[1]), Number(m[2])] : null
}

/** Lower edge of a band ("533-559 nm" → 533), to order columns by wavelength. */
const bandStart = (band: string | null | undefined) => {
  const m = band?.match(/^\s*(\d+(?:\.\d+)?)/)
  return m ? Number(m[1]) : Infinity
}

/** The run's exposure differs from the `.xpd` plan for this channel. */
export function exposureOffPlan(c: AcquisitionChannel): boolean {
  return c.exposureMs != null && c.plannedExposureMs != null && c.exposureMs !== c.plannedExposureMs
}

// ── cycle × dye grid ────────────────────────────────────────────────────────────────────────────

export interface GridCell {
  order: number; text: string; offPlan: boolean; tip: string
  /** Matches the filter (true with no filter). */
  match: boolean
  /** Marker, dye and reagent names the filter searches (`matchesQuery`). */
  fields: string[]
}
export interface GridColumn { key: string; label: string }
/** `markers`: imaged with markers. `blank`: one of the run's blank cycles (background only, not in
 *  the image). `empty`: no marker in the image, and the file cannot say the cycle was a blank. */
export interface GridRow { cycle: number; kind: 'markers' | 'blank' | 'empty'; cells: Record<string, GridCell | null> }
export interface ChannelGrid { columns: GridColumn[]; rows: GridRow[] }

/** The nuclear stain the PhenoCycler images with every cycle; a cycle with nothing else has no
 *  marker (`qptiff_meta._xpd_cycles` counts blanks the same way). */
const isNuclear = (c: AcquisitionChannel) => (c.biomarker ?? c.name ?? '').toUpperCase() === 'DAPI'

/** Rows = cycles (1 … planned, or the highest imaged), columns = excitation bands named by the dyes
 *  imaged in them, a cell = marker · exposure. Null when no channel has a cycle. */
export function cycleGrid(acq: AcquisitionSettings | null | undefined, names?: string[]): ChannelGrid | null {
  const chans = (acq?.channels ?? []).filter(c => c.cycle != null)
  if (chans.length === 0) return null
  const colOf = (c: AcquisitionChannel) => c.excitationFilter ?? c.dyePreset ?? '?'
  const keys = distinct(chans.map(colOf)).sort((a, b) => bandStart(a) - bandStart(b))
  const columns = keys.map(key => ({
    key, label: distinct(chans.filter(c => colOf(c) === key).map(c => c.dyePreset)).join(' / ') || key,
  }))
  const last = Math.max(acq?.cycles?.planned ?? 0, ...chans.map(c => c.cycle!))
  const rows: GridRow[] = []
  for (let cycle = 1; cycle <= last; cycle++) {
    const here = chans.filter(c => c.cycle === cycle)
    const cells: Record<string, GridCell | null> = Object.fromEntries(keys.map(k => [k, null]))
    for (const c of here) {
      const name = chName(c, names)
      cells[colOf(c)] = {
        order: c.order, text: join([name, c.exposureMs != null && `${fmtNum(c.exposureMs)} ms`]), offPlan: exposureOffPlan(c),
        tip: join([exposureOffPlan(c) && `Planned ${fmtNum(c.plannedExposureMs!)} ms`, c.reporter, c.barcode, c.clone]) || name,
        match: true, fields: [name, c.biomarker, c.dyePreset, c.reporter, c.barcode, c.clone].filter((f): f is string => !!f),
      }
    }
    rows.push({ cycle, kind: here.some(c => !isNuclear(c)) ? 'markers' : 'empty', cells })
  }
  // A cycle without a marker is a blank only when the .xpd's blank count accounts for all of them —
  // otherwise one of them was not imaged for some other reason, and the file cannot say which.
  const empty = rows.filter(r => r.kind === 'empty')
  if (acq?.cycles?.blank != null && empty.length === acq.cycles.blank) for (const r of empty) r.kind = 'blank'
  return { columns, rows }
}

/** Rows with a cell matching `q` (marker, dye, reporter, barcode, clone); the others in a kept row
 *  are dimmed (`match: false`). A blank `q` returns the grid unchanged. */
export function filterGrid(grid: ChannelGrid, q: string): ChannelGrid {
  if (!q.trim()) return grid
  const rows = grid.rows.map(r => ({
    ...r, cells: Object.fromEntries(Object.entries(r.cells).map(([k, c]) => [k, c && { ...c, match: matchesQuery(c.fields, q) }])),
  })).filter(r => Object.values(r.cells).some(c => c?.match))
  return { columns: grid.columns, rows }
}

// ── one row per channel (no cycles) ─────────────────────────────────────────────────────────────

export interface ChannelListRow { id: string; order: number; name: string; ex: string; em: string; detector: string }

/** # · name · ex · em · detector/exposure, one row per channel, values as recorded ("—" when not). */
export function channelList(acq: AcquisitionSettings | null | undefined, names?: string[]): ChannelListRow[] {
  return (acq?.channels ?? []).map(c => ({
    id: String(c.order), order: c.order, name: chName(c, names),
    ex: c.excitationFilter ?? (c.laserLines?.length ? c.laserLines.map(l => `${fmtNum(l.wavelengthNm)} nm`).join(', ') : c.laser) ?? '—',
    em: c.emissionFilter ?? '—',
    detector: join([c.detector, c.exposureMs != null && `${fmtNum(c.exposureMs)} ms`, c.hv != null && `HV ${fmtNum(c.hv)}`,
                    c.gain != null && `gain ${fmtNum(c.gain)}`]) || '—',
  }))
}

export function filterChannelList(rows: ChannelListRow[], q: string): ChannelListRow[] {
  return rows.filter(r => matchesQuery([r.name], q))
}

// ── summarised stages ───────────────────────────────────────────────────────────────────────────

/** What a stage's per-channel lines add up to, by stage key. */
function stageSummary(key: string, chans: AcquisitionChannel[]): LightPathItem[] {
  const file = (text: string): LightPathItem[] => (text ? [{ text, source: 'file' }] : [])
  const count = (k: number, one: string, many: string) => (k > 0 ? `${k} ${k === 1 ? one : many}` : null)
  const gain = range(chans.map(c => c.gain))
  switch (key) {
    case 'illumination':
      return file(count(distinct(chans.map(c => c.excitationFilter)).length, 'excitation band', 'excitation bands') ?? '')
    case 'filters': {
      const text = join([count(distinct(chans.map(c => c.filterCube)).length, 'filter set', 'filter sets'),
                         count(distinct(chans.map(c => c.emissionFilter)).length, 'emission band', 'emission bands')])
      return text ? file(text) : [{ text: 'Emission filters not recorded', source: 'missing' }]
    }
    case 'camera': {
      const exp = range(chans.map(c => c.exposureMs))
      return file(join([exp && `exposure ${exp} ms`, gain && `gain ${gain}`]))
    }
    case 'detectors': {
      const hv = range(chans.map(c => c.hv))
      return file(join([count(distinct(chans.map(c => c.detector)).length, 'detector', 'detectors'), hv && `HV ${hv}`, gain && `gain ${gain}`]))
    }
    case 'processing':
      return file(distinct(chans.map(c => c.airyscan)).join(', '))
    default:
      return []
  }
}

/**
 * The stages with their per-channel lines replaced by what those lines span, plus the `selected`
 * channel's own lines (its band, cube, exposure, cycle, measured signal). Lines that are not one
 * channel's are kept as they are.
 */
export function summariseStages(stages: LightPathStage[], acq: AcquisitionSettings | null | undefined,
                                selected: number | null): LightPathStage[] {
  const chans = acq?.channels ?? []
  return stages.map(s => {
    const perChannel = s.items.filter(i => i.channel != null)
    if (perChannel.length === 0) return s
    const own = s.items.filter(i => i.channel == null)
    const summary = stageSummary(s.key, chans)
    const measured: LightPathItem[] = perChannel.some(i => i.source === 'measured') && selected == null
      ? [{ text: 'Measured per channel: select one below', source: 'measured' }] : []
    return { ...s, items: [...own, ...summary, ...measured, ...perChannel.filter(i => i.channel === selected)] }
  })
}
