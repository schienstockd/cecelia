/**
 * The printable acquisition settings card (`AcquisitionCard.vue`): one image's settings, for someone
 * at the microscope to acquire it again, plus what landed last time as the target for the first snap.
 * It regroups the metadata dialog's light-path stages (`lightPathStages`) rather than rebuilding them,
 * so the dialog and the card say the same thing; it adds the per-channel detector and target tables
 * and the fill-in fields. Two-photon only, like the diagram (docs/todo/ACQUISITION_CARD_PLAN.md).
 */

import { fmtNum } from './physicalSize'
import { lightPathStages, isMultiphoton, autoHVItem, type LightPathImage, type LightPathItem, type LightPathSource } from './acquisitionLightPath'
import { microscopeLine } from './acquisitionSettings'

export interface CardRow { n: number | null; label: string; items: LightPathItem[] }
export interface CardCell { text: string; source: LightPathSource; unverified?: true }
export interface CardDetector {
  channel: string; dye: string; laser: string; detector: string; filter: CardCell
  /** This detector's HV, or null under Auto HV (then `AcquisitionCard.autoHV` spans the column). */
  hv: CardCell | null
  gainOffset: CardCell
}
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
  detectors: CardDetector[]
  /** Under Auto HV, one HV cell for every detector: the recorded HV is not a setting to copy. */
  autoHV: CardCell | null
  targets: CardTarget[]
  /** Shown under the target table when any number in it was not measured. */
  targetsMissing: boolean
  fillIn: string[]
  caveat: string
}

/** The slice of the image payload the card reads. */
export interface CardImage extends LightPathImage { uid: string; name: string; oriPath?: string | null }

/** Settings stages the card keeps, with the card's label. Sample and its measured depth numbers go to
 *  the target table; detectors to the detector table. */
const SETTINGS_STAGES: [string, string][] = [
  ['laser', 'Laser'], ['power', 'Power'], ['scanner', 'Scanner'], ['objective', 'Objective'],
  ['filters', 'Dichroics'], ['digitisation', 'Digitisation'], ['stack', 'Stack · time'],
]

/** Where a value came from, as a shape that survives a black-and-white printer — the same shapes as
 *  the dialog's `LightPathSourceGlyph`. */
export const CARD_GLYPH: Record<LightPathSource, string> = { file: '◆', ome: '●', measured: '▲', missing: '○' }

export const CARD_FILL_IN = ['Sample', 'Date', 'Question', 'Operator', 'Notes', 'Unclear on this card']
export const CARD_CAVEAT = 'Same settings ≠ same image. Power at the sample changes with alignment and laser age; ' +
  'tissue and depth differ. The settings are where to start; the target is the check.'

const NOT_MEASURED: CardCell = { text: 'not measured', source: 'missing' }
const n = (x: number | null | undefined): string => (x == null ? '—' : fmtNum(x))
const cell = (it: LightPathItem): CardCell => ({ text: it.text, source: it.source, ...(it.unverified ? { unverified: true } : {}) })

/** "1,240 → 310" — p99 at the first and last plane / frame; "single plane" when there is one. */
function ends(p99: number[] | undefined, single: string): CardCell {
  if (!p99 || p99.length === 0) return NOT_MEASURED
  if (p99.length === 1) return { text: single, source: 'ome' }
  return { text: `${n(Math.round(p99[0]))} → ${n(Math.round(p99[p99.length - 1]))}`, source: 'measured' }
}

const ZERO_KIND: Record<string, string> = { 'photon-counted': 'no photon', clipped: 'clipped at 0', indeterminate: 'cause unclear' }

/** Whether this image gets a card: the same rule as the diagram it is made from. */
export function hasAcquisitionCard(img: LightPathImage): boolean {
  return isMultiphoton(img.acquisition)
}

export function acquisitionCard(img: CardImage, ctx: { projectUid: string; printedOn: string }): AcquisitionCard {
  const acq = img.acquisition ?? {}
  const chans = acq.channels ?? []
  const stages = lightPathStages(img)
  // Settings: every stage item that is neither measured (target table) nor one channel's (detector table)
  const settings = SETTINGS_STAGES.map(([key, label]) => {
    const st = stages.find(s => s.key === key)!
    return { n: st.n, label, items: st.items.filter(i => i.source !== 'measured' && i.channel == null) }
  }).filter(r => r.items.length > 0)

  const chName = (order: number, fallback: string | null) => img.channelNames?.[order - 1] || fallback || `Channel ${order}`
  const autoHV = acq.autoHV === true ? cell(autoHVItem(chans)) : null
  const detectors: CardDetector[] = chans.map(c => ({
    channel: chName(c.order, c.name), dye: c.dyePreset ?? '—', laser: c.laser ?? '—', detector: c.detector ?? '—',
    filter: c.emissionFilter ? { text: c.emissionFilter, source: 'file' } : { text: 'not recorded', source: 'missing' },
    hv: autoHV ? null : c.hv != null ? { text: n(c.hv), source: 'file' } : { text: 'not recorded', source: 'missing' },
    gainOffset: c.gain == null && c.offset == null ? { text: 'not recorded', source: 'missing' }
      : { text: [c.gain != null && `gain ${n(c.gain)}`, c.offset != null && `offset ${n(c.offset)}`].filter(Boolean).join(' · '), source: 'file' },
  }))

  const facts = img.cleanupFacts?.channels ?? []
  const targets: CardTarget[] = facts.map(f => ({
    channel: f.name,
    clipped: f.clippedPct == null ? NOT_MEASURED : { text: `${n(f.clippedPct)} %`, source: 'measured' },
    zeros: f.zeroPct == null ? NOT_MEASURED
      : { text: [`${n(f.zeroPct)} %`, f.floor?.kind && ZERO_KIND[f.floor.kind]].filter(Boolean).join(' · '), source: 'measured' },
    byDepth: ends(f.profile?.byZ.p99, 'single plane'),
    byTime: ends(f.profile?.byT.p99, 'single frame'),
  }))
  const targetsMissing = targets.some(t => [t.clipped, t.zeros, t.byDepth, t.byTime].some(c => c.source === 'missing'))

  return {
    title: img.name,
    scope: microscopeLine(acq),
    sourceFile: img.oriPath ? img.oriPath.split(/[\\/]/).pop() ?? null : null,
    projectUid: ctx.projectUid, imageUid: img.uid, printedOn: ctx.printedOn,
    settings, detectors, autoHV, targets, targetsMissing,
    fillIn: CARD_FILL_IN, caveat: CARD_CAVEAT,
  }
}
