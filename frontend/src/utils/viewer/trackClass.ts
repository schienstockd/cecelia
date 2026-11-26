// Track ribbons coloured by a population's sub-populations (docs/todo/TRACK_SUBPOP_GATING_PLAN.md P6):
// "sub-population" paints each step in its cell's sub-population colour (raw flicker), "dominant
// sub-population" paints a whole track in its class colour (`track_classes`, a strict majority over
// the population's threshold), grey where mixed or in none. The class is the SERVER's
// (`GET /api/gating/track-classes`), so the viewer, a recording (`_step_colours`, api/src/overlay_author.jl)
// and the plots share one answer; this file only maps it onto payload rows.

import palettesJson from '../../plots/palettes.json'
import { hexToUnit, type OverlayPayload, type OverlayPop, type TrackColorMode } from '../viewerOverlays'

/** A step / track that no sub-population holds, or a mixed track — `palettes.json` → `trackMixed`. */
export const TRACK_MIXED_HEX: string = palettesJson.trackMixed

/** The modes that need a population's sub-populations. */
export const TRACK_STEP_MODES: readonly TrackColorMode[] = ['subpop', 'class']
export const isStepMode = (m: TrackColorMode) => TRACK_STEP_MODES.includes(m)

/** `GET /api/gating/track-classes` (with `tracks=1`). */
export interface TrackClassInfo {
  pop: string
  threshold: number
  level: number
  maxLevel: number
  children: { path: string; name: string; colour: string; tracks: number }[]
  mixed: number
  total: number
  mixedPct: number | null
  trackIds?: number[]
  /** index into `children`, -1 = mixed */
  classes?: number[]
}

export function trackClassesUrl(q: { projectUid: string; imageUid: string; valueName: string; pop: string }): string {
  const p = new URLSearchParams({ projectUid: q.projectUid, imageUid: q.imageUid, valueName: q.valueName,
                                  pop: q.pop, tracks: '1' })
  return '/api/gating/track-classes?' + p.toString()
}

type Rgb = [number, number, number]

/**
 * A ribbon source's colour per step — `(payload row, track id) → rgb` — for the sub-population modes;
 * null when there is nothing to colour by (another mode, no sub-populations), so the ribbon keeps its
 * population colour. `payload` is the source's own (filtered) payload, whose rows the buffer indexes;
 * `pops` are the layer's populations, which carry each sub-population's labels. A step is keyed
 * (track, frame), as `track_step_children` keys it for a recording: a step two sub-populations share,
 * or two cells of one track in one frame (a merge), takes the first in `info.children` order.
 */
export function stepRgbFor(mode: TrackColorMode, info: TrackClassInfo | undefined, payload: OverlayPayload,
                           pops: readonly Pick<OverlayPop, 'path' | 'labels'>[]): ((row: number, track: number) => Rgb) | null {
  if (!info || !info.children.length || !isStepMode(mode)) return null
  const mixed = hexToUnit(TRACK_MIXED_HEX)
  const colours = info.children.map(c => hexToUnit(c.colour))
  if (mode === 'class') {
    const byTrack = new Map<number, Rgb>()
    const ids = info.trackIds ?? [], cls = info.classes ?? []
    for (let i = 0; i < ids.length; i++) byTrack.set(ids[i], cls[i] >= 0 ? colours[cls[i]] : mixed)
    return (_row, track) => byTrack.get(track) ?? mixed
  }
  const childOf = new Map<number, number>()
  info.children.forEach((c, j) => {
    for (const l of pops.find(p => p.path === c.path)?.labels ?? []) if (!childOf.has(l)) childOf.set(l, j)
  })
  const labels = payload.cells.label ?? [], ts = payload.cells.t ?? [], tracks = payload.cells.track ?? []
  const stepKey = (r: number) => `${tracks[r]}:${Math.round(ts[r])}`
  const step = new Map<string, number>()
  for (let r = 0; r < labels.length; r++) {
    const j = childOf.get(labels[r]); if (j === undefined) continue
    const k = stepKey(r), cur = step.get(k)
    if (cur === undefined || j < cur) step.set(k, j)
  }
  return (row) => { const j = step.get(stepKey(row)); return j === undefined ? mixed : colours[j] }
}

/** The threshold choices offered (a strict majority up). */
export const TRACK_CLASS_THRESHOLDS: readonly number[] = [0.5, 0.6, 0.7, 0.8, 0.9]
