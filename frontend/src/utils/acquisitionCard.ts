/**
 * The printable acquisition settings card (`AcquisitionCardSheet.vue`): one image's settings, for someone
 * at the microscope to acquire it again, plus what landed last time as the target for the first snap.
 * It regroups the metadata dialog's light-path stages (`lightPathStages`) rather than rebuilding them,
 * so the dialog and the card say the same thing; it adds a per-channel table and the fill-in fields.
 * Every technology the diagram draws gets one, laid out the way that microscope is set up:
 * - laser scanning: detectors per channel; grouped by Zeiss track or Leica sequence when the reader
 *   recorded which ran each channel, with each group's laser lines and power (the global power row
 *   then drops them); a lambda run collapses to one row; Airyscan adds its processing mode;
 * - widefield: the camera per channel;
 * - cyclic (PhenoCycler): one row per marker, by cycle, with its reagents and excitation band.
 * Pages: `acquisitionCardPages.ts`. docs/todo/MICROSCOPE_PROFILES_PLAN.md → P6, ACQUISITION_CARD_PLAN.md.
 */

import { fmtNum } from './physicalSize'
import { lightPathStages, acquisitionTechnology, autoHVItem, type LightPathImage, type LightPathItem, type LightPathSource, type Technology } from './acquisitionLightPath'
import { microscopeLine, pinholeText, type AcquisitionChannel } from './acquisitionSettings'
import { bandEnds, exposureOffPlan } from './acquisitionChannelTable'

export interface CardRow { n: number | null; label: string; items: LightPathItem[] }
export interface CardCell { text: string; source: LightPathSource; unverified?: true }
export interface CardDetector {
  channel: string; dye: string; laser: string; detector: string; filter: CardCell
  /** This detector's HV, or null under Auto HV (then `AcquisitionCard.autoHV` spans the column). */
  hv: CardCell | null
  gainOffset: CardCell
  /** The lines on for this channel with their power, when the reader records them per channel. */
  lines: CardCell[] | null
  pinhole: CardCell | null
  /** Zeiss Airyscan processing mode, as the file names it. */
  airyscan: CardCell | null
  /** A lambda run collapsed into this row: how many wavelength channels it stands for. */
  spans?: number
}
/** The channels one track / sequence ran, with what it set for all of them. */
export interface CardDetectorGroup { label: string | null; lines: CardCell[] | null; splitters: CardCell | null; rows: CardDetector[] }
export interface CardDetectorTable {
  /** Set when the reader recorded which Zeiss track or Leica sequence ran each channel. */
  groupBy: 'Track' | 'Sequence' | null
  groups: CardDetectorGroup[]
  cols: { lines: boolean; pinhole: boolean; airyscan: boolean; splitters: boolean }
}
/** A widefield channel's camera settings. `band` is the excitation → emission band when recorded. */
export interface CardCameraChannel { channel: string; cube: CardCell; exposure: CardCell; gain: CardCell; lamp: CardCell; band: CardCell | null }
/** One marker of a cyclic run (PhenoCycler). */
export interface CardMarker { marker: string; clone: CardCell; reagent: CardCell; dye: string; band: CardCell; cube: CardCell; exposure: CardCell }
export interface CardCycle { cycle: number; rows: CardMarker[] }
export interface CardTarget { channel: string; clipped: CardCell; zeros: CardCell; byDepth: CardCell; byTime: CardCell }

export interface AcquisitionCard {
  title: string
  /** "FV4000 · software 3.2.1.85 · BX63LF · 2026-07-16 13:44", or null when the file names no system. */
  scope: string | null
  sourceFile: string | null
  projectUid: string
  imageUid: string
  printedOn: string
  settings: CardRow[]
  /** Every detector row (laser scanning), flat; `detectorTable` holds them grouped as printed. */
  detectors: CardDetector[]
  detectorTable: CardDetectorTable | null
  /** Widefield: per-channel camera settings instead of `detectors` (which is then empty). */
  camera: CardCameraChannel[] | null
  /** Cyclic: markers by cycle, instead of `camera`. */
  markers: CardCycle[] | null
  /** Under Auto HV, one HV cell for every detector: the recorded HV is not a setting to copy. */
  autoHV: CardCell | null
  targets: CardTarget[]
  /** Which target columns mean anything: a single plane has no depth, a single frame no time. */
  targetCols: { depth: boolean; time: boolean }
  /** A slide scan's tissue share, from the import probe. */
  tissue: CardCell | null
  /** Shown under the target table when any number in it was not measured, or none was. */
  targetsMissing: boolean
  fillIn: string[]
  caveat: string
}

/** The slice of the image payload the card reads. */
export interface CardImage extends LightPathImage { uid: string; name: string; oriPath?: string | null }

/** Stages that are not settings rows: sample's measured depth numbers go to the target table, the
 *  detectors to the per-channel table. */
const NOT_SETTINGS = new Set(['sample', 'detectors'])
/** Card labels shorter than the dialog's stage titles. */
const CARD_LABEL: Record<string, string> = { 'Power control': 'Power', 'Dichroics · filters': 'Dichroics' }

/** Where a value came from, as a shape that survives a black-and-white printer — the same shapes as
 *  the dialog's `LightPathSourceGlyph`. */
export const CARD_GLYPH: Record<LightPathSource, string> = { file: '◆', ome: '●', measured: '▲', missing: '○' }

export const CARD_FILL_IN = ['Sample', 'Date', 'Question', 'Operator', 'Notes', 'Unclear on this card']
const CAVEAT_END = 'The settings are where to start; the target is the check.'
/** What makes the same settings give a different image, by how the image was made. */
export const CARD_CAVEATS: Record<Technology | 'cyclic', string> = {
  'two-photon': 'Same settings ≠ same image. Power at the sample changes with alignment and laser age; tissue and depth differ.',
  confocal: 'Same settings ≠ same image. Laser output and alignment drift, and a different zoom or pinhole changes what each detector sees; tissue and staining differ.',
  airyscan: 'Same settings ≠ same image. Laser output and alignment drift, and a different zoom or pinhole changes what each detector sees; tissue and staining differ.',
  widefield: 'Same settings ≠ same image. Light-source output changes with age and alignment; tissue and staining differ.',
  cyclic: 'Same settings ≠ same image. Light-source output ages; antibody lot, staining and cycle number change the signal.',
}

const NOT_MEASURED: CardCell = { text: 'not measured', source: 'missing' }
const NOT_RECORDED: CardCell = { text: 'not recorded', source: 'missing' }
/** Nothing to record here (printed as a bare dash, no source shape). */
export const NONE: CardCell = { text: '—', source: 'file' }
const n = (x: number | null | undefined): string => (x == null ? '—' : fmtNum(x))
const cell = (it: LightPathItem): CardCell => ({ text: it.text, source: it.source, ...(it.unverified ? { unverified: true } : {}) })
const file = (x: string | null | undefined): CardCell => (x ? { text: x, source: 'file' } : NOT_RECORDED)

/** "1,240 → 310" — p99 at the first and last plane / frame; "single plane" when there is one. */
function ends(p99: number[] | undefined, single: string): CardCell {
  if (!p99 || p99.length === 0) return NOT_MEASURED
  if (p99.length === 1) return { text: single, source: 'ome' }
  return { text: `${n(Math.round(p99[0]))} → ${n(Math.round(p99[p99.length - 1]))}`, source: 'measured' }
}

/** What a channel's exact zeros are. On a camera they are background below the offset, so the
 *  photon-counting reading is not printed there. */
const ZERO_KIND: Record<string, string> = { 'photon-counted': 'no photon', clipped: 'clipped at 0', indeterminate: 'cause unclear' }

/** Whether this image gets a card: the same rule as the diagram it is made from. */
export function hasAcquisitionCard(img: LightPathImage): boolean {
  return acquisitionTechnology(img.acquisition) != null
}

function laserLines(c: AcquisitionChannel): CardCell[] | null {
  if (!c.laserLines?.length) return null
  return c.laserLines.map(l => file(l.pct == null ? `${n(l.wavelengthNm)} nm` : `${n(l.wavelengthNm)} nm · ${n(l.pct)} %`))
}

/** "533-559 → 570-640" (nm, in the column header). */
const bandText = (c: AcquisitionChannel) =>
  [c.excitationFilter, c.emissionFilter].map(b => b?.replace(/\s*nm$/, '') ?? '?').join(' → ')

const sameCells = (a: CardCell[] | null, b: CardCell[] | null) => JSON.stringify(a) === JSON.stringify(b)

/** Consecutive wavelength channels of a lambda run (ZEN `SpectralImaging`) set the same way become one
 *  row "Lambda 415–690 nm"; the run's pinhole, which ZEN gives per wavelength, as its range. */
function collapseLambda(rows: CardDetector[], chans: AcquisitionChannel[]): CardDetector[] {
  const out: CardDetector[] = []
  const key = (d: CardDetector) => JSON.stringify([d.detector, d.hv, d.gainOffset, d.lines, d.laser])
  for (let i = 0; i < rows.length;) {
    let j = i
    if (chans[i].acquisitionMode === 'SpectralImaging') {
      while (j + 1 < rows.length && chans[j + 1].acquisitionMode === 'SpectralImaging' && key(rows[j + 1]) === key(rows[i])) j++
    }
    const run = rows.slice(i, j + 1)
    const bands = run.map(d => bandEnds(d.filter.text))
    if (run.length >= 3 && bands.every(b => b)) {
      const lo = Math.round(Math.min(...bands.map(b => b![0]))), hi = Math.round(Math.max(...bands.map(b => b![1])))
      const airy = chans.slice(i, j + 1).map(c => c.pinholeAiry).filter((v): v is number => v != null)
      out.push({ ...run[0], channel: `Lambda ${lo}–${hi} nm`, dye: '—', filter: file(`${lo}-${hi} nm`), spans: run.length,
        pinhole: airy.length ? file(`${n(Math.min(...airy))}–${n(Math.max(...airy))} AU`) : run[0].pinhole })
    } else {
      out.push(...run)
    }
    i = j + 1
  }
  return out
}

function detectorTable(acq: NonNullable<CardImage['acquisition']>, chans: AcquisitionChannel[], rows: CardDetector[]): CardDetectorTable {
  const groupBy = chans.some(c => c.track != null) ? 'Track' : chans.some(c => c.sequence != null) ? 'Sequence' : null
  const keyOf = (c: AcquisitionChannel) => (groupBy === 'Track' ? c.track : groupBy === 'Sequence' ? c.sequence : null) ?? null
  const keys = [...new Set(chans.map(keyOf))].sort((a, b) => (a ?? Infinity) - (b ?? Infinity))
  const shared = (acq.dichroics ?? []).length > 0
  const groups: CardDetectorGroup[] = keys.map(k => {
    const idx = chans.map((c, i) => (keyOf(c) === k ? i : -1)).filter(i => i >= 0)
    const gc = idx.map(i => chans[i])
    const gr = collapseLambda(idx.map(i => rows[i]), gc)
    const lines = gr.every(d => sameCells(d.lines, gr[0].lines)) ? gr[0].lines : null
    const split = [...new Set(gc.flatMap(c => c.beamSplitters ?? []))]
    return {
      label: k == null ? null : groupBy === 'Track' ? `T${k}` : String(k),
      lines: groupBy ? lines : null,
      splitters: !shared && split.length ? file(split.join(' · ')) : null,
      rows: gr,
    }
  })
  const all = groups.flatMap(g => g.rows)
  return {
    groupBy, groups,
    cols: {
      lines: all.some(d => d.lines), pinhole: all.some(d => d.pinhole),
      airyscan: all.some(d => d.airyscan), splitters: groups.some(g => g.splitters),
    },
  }
}

function markerTable(chans: AcquisitionChannel[], chName: (c: AcquisitionChannel) => string): CardCycle[] {
  const cycles = [...new Set(chans.map(c => c.cycle!))].sort((a, b) => a - b)
  return cycles.map(cycle => ({
    cycle,
    rows: chans.filter(c => c.cycle === cycle).map(c => ({
      // a nuclear stain has no clone or reporter: a dash, not "not recorded"
      marker: chName(c), clone: c.clone ? file(c.clone) : NONE, dye: c.dyePreset ?? '—',
      reagent: c.reporter || c.barcode ? file([c.reporter, c.barcode].filter(Boolean).join(' · ')) : NONE,
      band: file(bandText(c)),
      cube: file(c.filterCube),
      exposure: c.exposureMs == null ? NOT_RECORDED
        : file(exposureOffPlan(c) ? `${n(c.exposureMs)} ms (planned ${n(c.plannedExposureMs)} ms)` : `${n(c.exposureMs)} ms`),
    })),
  }))
}

/** Card-only rows a slide scanner records: the run it belongs to and what the scanner did to it. */
function scannerRows(acq: NonNullable<CardImage['acquisition']>): CardRow[] {
  const out: CardRow[] = []
  const cy = acq.cycles
  if (cy) {
    const runs = (cy.experiments ?? []).filter((x): x is string => !!x)
    const items: LightPathItem[] = cy.experiment ? [{ text: cy.experiment, source: 'file' }]
      : runs.length ? [{ text: runs.join(' · '), source: 'file' }]
      : [{ text: cy.registered ? 'Run not recorded' : 'Run file not found beside the scan', source: 'missing' }]
    if (cy.registered && runs.length && runs.length < cy.count) items.push({ text: `${cy.count - runs.length} scan(s) without a run file`, source: 'missing' })
    out.push({ n: null, label: 'Run', items })
  }
  if (acq.processing || acq.saturationProtection) {
    out.push({ n: null, label: 'Processing', items: [
      ...(acq.processing ? [{ text: acq.processing, source: 'file' as const }] : []),
      ...(acq.saturationProtection ? [{ text: `saturation protection: ${acq.saturationProtection}`, source: 'file' as const }] : []),
    ] })
  }
  return out
}

export function acquisitionCard(img: CardImage, ctx: { projectUid: string; printedOn: string }): AcquisitionCard {
  const acq = img.acquisition ?? {}
  const chans = acq.channels ?? []
  const tech = acquisitionTechnology(acq)
  const widefield = tech === 'widefield'
  const cyclic = widefield && chans.some(c => c.cycle != null)
  const chName = (c: AcquisitionChannel) => img.channelNames?.[c.order - 1] || c.name || `Channel ${c.order}`

  const autoHV = acq.autoHV === true ? cell(autoHVItem(chans)) : null
  const detectors: CardDetector[] = widefield ? [] : chans.map(c => ({
    channel: chName(c), dye: c.dyePreset ?? '—', laser: c.laser ?? '—', detector: c.detector ?? '—',
    filter: file(c.emissionFilter),
    hv: autoHV ? null : c.hv != null ? { text: n(c.hv), source: 'file' } : NOT_RECORDED,
    gainOffset: c.gain == null && c.offset == null ? NOT_RECORDED
      : { text: [c.gain != null && `gain ${n(c.gain)}`, c.offset != null && `offset ${n(c.offset)}`].filter(Boolean).join(' · '), source: 'file' },
    lines: laserLines(c),
    pinhole: c.pinholeAiry != null || c.pinholeUm != null ? file(pinholeText(c.pinholeAiry, c.pinholeUm)) : null,
    airyscan: c.airyscan ? file(c.airyscan) : null,
  }))
  const table = widefield ? null : detectorTable(acq, chans, detectors)
  const camera: CardCameraChannel[] | null = widefield && !cyclic ? chans.map(c => ({
    channel: chName(c), cube: file(c.filterCube),
    exposure: file(c.exposureMs != null ? `${n(c.exposureMs)} ms` : null), gain: file(c.gain != null ? n(c.gain) : null),
    lamp: file(c.illuminationIntensity != null ? n(c.illuminationIntensity) : null),
    band: c.excitationFilter ? file(bandText(c)) : null,
  })) : null

  // Settings: every stage item that is neither measured (target table) nor one channel's (channel
  // table); a laser's power too when the channel table carries each channel's power.
  const powerInTable = detectors.some(d => d.lines?.some(l => l.text.includes('%')))
  const settings = [...scannerRows(acq), ...lightPathStages(img).filter(st => !NOT_SETTINGS.has(st.key)).map(st => ({
    n: st.n, label: CARD_LABEL[st.title] ?? st.title,
    // a laser listed once per power it ran at reads as a duplicate once the powers are in the table
    items: st.items.filter((i, k, all) => i.source !== 'measured' && i.channel == null && !(powerInTable && i.laser)
      && all.findIndex(o => o.text === i.text) === k),
  }))].filter(r => r.items.length > 0)

  const facts = img.cleanupFacts?.channels ?? []
  const targets: CardTarget[] = facts.map(f => {
    const c = chans[f.index]
    const kind = !widefield && f.floor?.kind ? ZERO_KIND[f.floor.kind] : null
    return {
      channel: cyclic && c?.cycle != null ? `${f.name} · c${c.cycle}` : f.name,
      clipped: f.clippedPct == null ? NOT_MEASURED : { text: `${n(f.clippedPct)} %`, source: 'measured' },
      zeros: f.zeroPct == null ? NOT_MEASURED : { text: [`${n(f.zeroPct)} %`, kind].filter(Boolean).join(' · '), source: 'measured' },
      byDepth: ends(f.profile?.byZ.p99, 'single plane'),
      byTime: ends(f.profile?.byT.p99, 'single frame'),
    }
  })
  const targetCols = { depth: (img.sizeZ ?? 1) > 1, time: (img.sizeT ?? 1) > 1 }
  const shown = (t: CardTarget) => [t.clipped, t.zeros, ...(targetCols.depth ? [t.byDepth] : []), ...(targetCols.time ? [t.byTime] : [])]
  const tf = img.cleanupFacts?.tissue
  const tissue = tf ? { text: `${n(Math.round(tf.fraction * 1000) / 10)} % of the scan`, source: 'measured' as const } : null

  return {
    title: img.name,
    scope: microscopeLine(acq),
    sourceFile: img.oriPath ? img.oriPath.split(/[\\/]/).pop() ?? null : null,
    projectUid: ctx.projectUid, imageUid: img.uid, printedOn: ctx.printedOn,
    settings, detectors: table ? table.groups.flatMap(g => g.rows) : [], detectorTable: table,
    camera, markers: cyclic ? markerTable(chans, chName) : null, autoHV,
    targets, targetCols, tissue,
    targetsMissing: targets.length === 0 || targets.some(t => shown(t).some(c => c.source === 'missing')),
    fillIn: CARD_FILL_IN,
    caveat: `${CARD_CAVEATS[cyclic ? 'cyclic' : tech ?? 'two-photon']} ${CAVEAT_END}`,
  }
}
