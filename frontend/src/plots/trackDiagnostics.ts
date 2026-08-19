// Pure helpers for the track-diagnostics panel (celltrackR QC battery — see docs/TRACKING.md).
//
// The response is a COHORT: one entry per (images × population) group, each carrying what the
// single-image response used to carry at its top level (`plots/trackGroups.ts` owns what a group is and
// how it was asked for). Every function here therefore reads across groups — a mode is offered if ANY
// group has data for it, the summary line names each group, and the CSV carries a `group` column. The
// alternative was a second set of "…ForGroup" helpers beside the old ones, which is how a panel ends up
// showing one arm of a two-arm comparison.
//
// The VERDICTS are not here. Whether an image drifts, whether its motion is confined, whether the
// volume edge is pulling the tracking — all of that arrives from `GET /api/tracking/diagnostics` as
// findings, computed by the same package roll-up the tracking task banks as QC. Re-deriving any of it
// from the curves in TypeScript would create a second threshold that quietly disagrees with the one in
// the QC doc. What lives here is presentation: which modes have data, the fitted line to draw over the
// MSD points, and the axis/labelling arithmetic.

import type { TrackGroupMeta } from './trackGroups'

export interface DiagCurve { lag: number[]; value: (number | null)[]; sem: (number | null)[]; n: number[] }
export interface DiagCloud { distance: number[]; angle: number[] }

export interface DiagFinding { code: string; level: string; short: string; long: string; detail?: string }

/** One (images × population) group's battery — the whole of the old single-image response. */
export interface DiagGroup extends TrackGroupMeta {
  nTracks?: number
  msd?: DiagCurve
  acor?: DiagCurve
  plane?: DiagCloud & { expected: number; angleNear: number | null; angleFar: number | null; suspect: boolean }
  pairs?: { angle: number[]; distance: number[]; shown: number; total: number
            meanAngleFar: number | null; drifting: boolean
            // the pair scan is O(tracks²) and is skipped above `maxTracks` — see PAIR_SCAN_MAX_TRACKS
            skipped?: boolean; maxTracks?: number }
  drift?: { p: number | null; n: number; meanStep: number[]; drifting: boolean
            stepSpacing: number; alpha: number }
  summary?: { msdSlope: number | null; motionKind: string
              persistenceLag: number | null; nDuplicatePairs: number }
  findings?: DiagFinding[]
}

export interface DiagnosticsResponse {
  valueName: string
  tracked: boolean
  /** groups the server capped away — said out loud, never silently */
  dropped?: number
  groups: DiagGroup[]
}

/** The groups worth drawing (tracked, in server order). */
export const diagGroups = (d: DiagnosticsResponse | null): DiagGroup[] =>
  (d?.groups ?? []).filter(g => g.tracked !== false)

export type DiagMode = 'msd' | 'acor' | 'plane' | 'pairs'

export const DIAG_LABEL: Record<DiagMode, string> = {
  msd: 'Displacement',
  acor: 'Persistence',
  plane: 'Volume edge',
  pairs: 'Track pairs',
}

/**
 * Which modes this image actually has data for.
 *
 * `plane` is 3D-only (celltrackR's `angleToPlane` refuses 2D), and a 2D timelapse is the common case —
 * so the mode is ABSENT rather than an empty box with an explanation in it.
 */
export function availableModes(d: DiagnosticsResponse | null): DiagMode[] {
  const gs = diagGroups(d)
  if (!d?.tracked || !gs.length) return []
  const out: DiagMode[] = []
  const any = (f: (g: DiagGroup) => boolean) => gs.some(f)
  if (any(g => !!g.msd?.lag.length)) out.push('msd')
  if (any(g => !!g.acor?.lag.length)) out.push('acor')
  // ANY group, not every: a 2D movie pooled with a 3D one has no volume-edge profile, and hiding the
  // mode for the whole plot would hide the one arm that can answer the question
  if (any(g => !!g.plane?.angle.length)) out.push('plane')
  // a skipped scan has no cloud AND no verdict — offering the mode would show an empty box that
  // reads as "no pairs are suspicious"
  if (any(g => !!g.pairs?.angle.length && !g.pairs.skipped)) out.push('pairs')
  return out
}

/** The mode to show: the persisted one if it still has data, else the first that does. */
export function resolveMode(d: DiagnosticsResponse | null, wanted?: string): DiagMode | null {
  const have = availableModes(d)
  if (!have.length) return null
  return have.includes(wanted as DiagMode) ? (wanted as DiagMode) : have[0]
}

/** Points with a null value dropped — a "not assessed" lag must not be drawn as zero. */
export function curvePoints(c: DiagCurve | undefined): { lag: number; value: number; sem: number | null; n: number }[] {
  if (!c) return []
  const out: { lag: number; value: number; sem: number | null; n: number }[] = []
  for (let i = 0; i < c.lag.length; i++) {
    const v = c.value[i]
    if (v === null || v === undefined || !Number.isFinite(v)) continue
    const s = c.sem[i]
    out.push({ lag: c.lag[i], value: v, sem: s === null || s === undefined || !Number.isFinite(s) ? null : s, n: c.n[i] })
  }
  return out
}

/**
 * The power law `msd = a · lag^slope` as two endpoints, for drawing the fit the slope came from.
 *
 * The SLOPE is the server's (one least-squares fit, shared with the QC finding); only the intercept is
 * solved here, so the line cannot imply a different exponent from the number printed beside it.
 * Null when there is nothing to fit or the slope is unavailable.
 */
export function msdFitLine(
  points: { lag: number; value: number }[], slope: number | null | undefined,
): { lag: number; value: number }[] | null {
  if (slope === null || slope === undefined || !Number.isFinite(slope)) return null
  const usable = points.filter(p => p.lag > 0 && p.value > 0)
  if (usable.length < 2) return null
  const meanLogX = usable.reduce((s, p) => s + Math.log(p.lag), 0) / usable.length
  const meanLogY = usable.reduce((s, p) => s + Math.log(p.value), 0) / usable.length
  const logA = meanLogY - slope * meanLogX
  const lags = usable.map(p => p.lag)
  const lo = Math.min(...lags), hi = Math.max(...lags)
  return [lo, hi].map(lag => ({ lag, value: Math.exp(logA + slope * Math.log(lag)) }))
}

/**
 * The one-line reading of a mode — what the picture is for, in the terms of the check.
 *
 * Deliberately about the SHAPE and not the numbers: the numbers are on the axes and in the findings.
 */
export function modeHint(mode: DiagMode): string {
  switch (mode) {
    case 'msd':   return 'Slope 1 = random walk, 2 = directed, below 1 = confined'
    case 'acor':  return 'How long a cell keeps its direction — flat at 0 means none'
    case 'plane': return 'Angles dipping only near the edge = tracking artefact'
    case 'pairs': return 'Far-apart pairs should average 90° — lower means the field moves together'
  }
}

/**
 * The reference line a mode is read against, or null.
 *
 * Every one of these is a published expectation, not a house threshold: 1/e for a decorrelation time,
 * 32.7° for unbiased 3D motion (Beltman 2009), 90° for two unrelated cells.
 */
export function referenceLine(mode: DiagMode, d: DiagnosticsResponse | null): { value: number; label: string } | null {
  switch (mode) {
    case 'acor':  return { value: Math.exp(-1), label: '1/e' }
    case 'plane': return { value: diagGroups(d).find(g => g.plane)?.plane?.expected ?? 32.7,
                           label: '32.7° unbiased' }
    case 'pairs': return { value: 90, label: '90° unrelated' }
    default:      return null
  }
}

/** Axis labels per mode — `[x, y]`. Lags are in FRAMES; nothing here invents a time unit. */
export function axisLabels(mode: DiagMode): [string, string] {
  switch (mode) {
    case 'msd':   return ['lag (frames)', 'MSD (µm²)']
    case 'acor':  return ['lag (frames)', 'autocorrelation']
    case 'plane': return ['distance to edge (µm)', 'step angle (°)']
    case 'pairs': return ['distance between tracks (µm)', 'angle between paths (°)']
  }
}

/**
 * The header line: the scalars worth reading before any curve, in words.
 *
 * Empty string when nothing has been computed, so the caller renders no header rather than a row of
 * "—" placeholders.
 */
export function diagnosticsSummary(g: DiagGroup | null): string {
  if (!g || !g.summary) return ''
  const bits: string[] = [`${g.nTracks ?? 0} tracks`]
  const s = g.summary
  if (s.msdSlope !== null && s.msdSlope !== undefined && Number.isFinite(s.msdSlope))
    bits.push(`${s.motionKind} (slope ${s.msdSlope.toFixed(2)})`)
  if (s.persistenceLag !== null && s.persistenceLag !== undefined && Number.isFinite(s.persistenceLag))
    bits.push(`persistence ${s.persistenceLag.toFixed(1)} frames`)
  // A drift p-value is only meaningful with the spacing it was computed at — celltrackR's whole point
  // about this test — so the two are never shown apart.
  if (g.drift && g.drift.p !== null && Number.isFinite(g.drift.p))
    bits.push(`drift p ${g.drift.p < 0.001 ? '<0.001' : g.drift.p.toFixed(3)} @${g.drift.stepSpacing}f`)
  return bits.join(' · ')
}

/**
 * The header line for a COHORT: each group's reading, named.
 *
 * One line rather than a stack of them, because the comparison IS the readout — "WT: random walk (1.02)
 * · MerTK: confined (0.71)" is the sentence the plot exists to support. A single group keeps the bare
 * reading (no label to repeat).
 */
export function cohortSummary(d: DiagnosticsResponse | null): string {
  const gs = diagGroups(d)
  if (!gs.length) return ''
  if (gs.length === 1) return diagnosticsSummary(gs[0])
  return gs.map(g => {
    const line = diagnosticsSummary(g)
    return g.label ? `${g.label}: ${line}` : line
  }).filter(Boolean).join(' · ')
}

/** Every group's findings, each carrying which group raised it (a cohort's arms fail differently). */
export function cohortFindings(d: DiagnosticsResponse | null): (DiagFinding & { group: string })[] {
  return diagGroups(d).flatMap(g => (g.findings ?? []).map(f => ({ ...f, group: g.label })))
}

/** Curve rows for a mode, tagged by group — one array, one mark, `g` as the colour/facet channel. */
export function diagCurveRows(
  d: DiagnosticsResponse | null, mode: DiagMode,
): { lag: number; value: number; sem: number | null; n: number; g: string; label: string }[] {
  if (mode !== 'msd' && mode !== 'acor') return []
  return diagGroups(d).flatMap(g =>
    curvePoints(mode === 'msd' ? g.msd : g.acor).map(p => ({ ...p, g: g.key, label: g.label })))
}

/** Cloud rows for a mode, tagged by group. Same contract as `diagCurveRows`. */
export function diagCloudRows(
  d: DiagnosticsResponse | null, mode: DiagMode,
): { distance: number; angle: number; g: string; label: string }[] {
  if (mode !== 'plane' && mode !== 'pairs') return []
  return diagGroups(d).flatMap(g => {
    const c = mode === 'plane' ? g.plane : g.pairs
    const dist = c?.distance ?? []
    const ang = c?.angle ?? []
    return dist.map((distance, i) => ({ distance, angle: ang[i], g: g.key, label: g.label }))
  })
}

/** The MSD fit line per group, from each group's OWN slope (the server's, never re-fitted here). */
export function diagFitRows(
  d: DiagnosticsResponse | null,
): { lag: number; value: number; g: string }[] {
  return diagGroups(d).flatMap(g => {
    const fit = msdFitLine(curvePoints(g.msd), g.summary?.msdSlope)
    return (fit ?? []).map(p => ({ ...p, g: g.key }))
  })
}

/**
 * What the pair cloud is not showing, or empty. Same rule as the track plot: never a silent cap.
 *
 * Two different absences, and conflating them would be the bug: `skipped` means the O(tracks²) scan
 * did not run at all on this image, so "no duplicate pairs" was never established. A strided cloud
 * merely draws fewer points than it measured.
 */
export function pairCapNote(d: DiagnosticsResponse | null): string {
  const ps = diagGroups(d).map(g => g.pairs).filter(Boolean) as NonNullable<DiagGroup['pairs']>[]
  if (!ps.length) return ''
  const skipped = ps.find(p => p.skipped)
  if (skipped) return `not checked above ${(skipped.maxTracks ?? 0).toLocaleString()} tracks`
  const shown = ps.reduce((a, p) => a + p.shown, 0)
  const total = ps.reduce((a, p) => a + p.total, 0)
  if (total <= shown) return ''
  return `${shown.toLocaleString()} of ${total.toLocaleString()} pairs`
}

/** CSV rows for the shown mode — the numbers behind the picture, one row per drawn point. */
export function diagnosticsCsvRows(mode: DiagMode, d: DiagnosticsResponse | null): Record<string, unknown>[] {
  const gs = diagGroups(d)
  if (!gs.length) return []
  // a `group` column only when there IS more than one — a single-group export keeps the old shape
  const tag = (g: DiagGroup) => (gs.length > 1 ? { group: g.label || g.key } : {})
  if (mode === 'msd' || mode === 'acor') {
    return gs.flatMap(g => curvePoints(mode === 'msd' ? g.msd : g.acor)
      .map(p => ({ ...tag(g), lag: p.lag, value: p.value, sem: p.sem ?? '', n: p.n })))
  }
  return gs.flatMap(g => {
    const c = mode === 'plane' ? g.plane : g.pairs
    const dist = c?.distance ?? []
    const ang = c?.angle ?? []
    return dist.map((distance, i) => ({ ...tag(g), distance, angle: ang[i] }))
  })
}

/**
 * Which segmentation a TRACK view should show, given what the image has.
 *
 * Not `'default'`, and not the active one: both are routinely untracked. On the reference image
 * (`zolIMa/1/fXgbTl`) `default` and the active `three` carry no tracks while `memTom` holds 374, so a
 * view defaulting to either reported "nothing to review" for an image with 31 correction candidates.
 * `trackedValueNames` comes from `/api/gating/channels?popType=track`.
 *
 * Order of preference: a persisted choice that is still tracked (so navigating away and back does not
 * re-point the panel), then the active segmentation if it is tracked, then the first tracked one.
 */
// The picker this feeds is labelled "tracks", not "segmentation" — the list only ever contains
// TRACKED label sets, so on a track plot a segmentation name is the storage detail and the tracks are
// the thing being chosen: "the selection of a segmentation is a bit weird for these plots, because
// what you should be selecting are tracks not segmentation."
export function resolveTrackValueName(
  wanted: string | undefined, tracked: readonly string[], all: readonly string[] = [],
  active?: string,
): string {
  if (wanted && tracked.includes(wanted)) return wanted
  // the ACTIVE segmentation when it is tracked — on an image with two tracked sets ("importTest" and
  // "memTom" on the reference image) "the first one" is an arbitrary pick, and the one the rest of the
  // app is pointed at is the answer the user expects
  if (active && tracked.includes(active)) return active
  if (tracked.length) return tracked[0]
  // nothing is tracked: keep the request (or fall back) so the view can say "not tracked" about a
  // real segmentation rather than about nothing
  return wanted && all.includes(wanted) ? wanted : (all[0] ?? '')
}
