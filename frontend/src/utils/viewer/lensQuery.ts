// "Find regions like this one" from the pinned lens — the pure half (docs/todo/LENS_QUERY_PLAN.md).
// The task is `clustRegions.lensQuery` (app/src/tasks/clustRegions/lensQuery.jl): it writes a per-cell
// score column, `spatial.lens.<name>`, that the viewer then colours by.

/** The task the chip's Find similar runs. */
export const LENS_QUERY_FUN = 'clustRegions.lensQuery'

/** A region run the lens can read: its segmentation and suffix (`regions.<suffix>`). */
export interface LensRegionRun { vn: string; suffix: string }

/** The region runs in a segmentation's cell columns — every `regions.<suffix>`. */
export function regionRunsIn(vn: string, obsColumns: readonly string[]): LensRegionRun[] {
  return obsColumns.filter(c => /^regions\.[^.]+$/.test(c)).map(c => ({ vn, suffix: c.slice('regions.'.length) }))
}

/** The column a lens query writes — twin of Julia `lens_score_col`. */
export function lensScoreColumn(name: string): string {
  return `spatial.lens.${name.trim().replace(/[^A-Za-z0-9_-]+/g, '_')}`
}

/** The next free lens name: lens1, lens2, … — free in every segmentation's columns on the image AND among
 *  the project's lens figures (a column deleted elsewhere can leave its figure), so a new query never
 *  overwrites, or is listed under, another lens. */
export function nextLensName(obsColumns: readonly string[], figureColumns: readonly string[] = []): string {
  const taken = new Set([...obsColumns, ...figureColumns])
  for (let i = 1; ; i++) if (!taken.has(lensScoreColumn(`lens${i}`))) return `lens${i}`
}

/** The task params for a pinned lens (level-0 px) on a region run. */
export function lensQueryParams(run: LensRegionRun, lens: { x: number; y: number; r: number }, t: number | null,
                                name: string): Record<string, unknown> {
  return { valueName: run.vn, regionSuffix: run.suffix, lensX: Math.round(lens.x), lensY: Math.round(lens.y),
           lensR: Math.max(1, Math.round(lens.r)), lensT: t ?? -1, outputName: name }
}

/** A lens figure's lens, as the movie registry lists it (`_clean_lens`, api/src/movies_api.jl): where it
 *  was on `sourceUid` (level-0 px; `t` -1 = every frame), the region run it read and the score column it
 *  wrote on `images`. */
export interface FigureLens {
  sourceUid: string
  x: number; y: number; r: number; t?: number
  column: string
  valueName?: string
  regionSuffix?: string
  images?: string[]
  /** what was on screen at Find similar — reopening restores it */
  look?: LensLook
}

/** What was toggled on screen when Find similar ran — the view (channels, camera, frame) and the
 *  overlay switches the viewer draws from (`drawSpec.ts`). Plain data: it rides on the figure's entry
 *  and is written back on Open (`applyLensLook`). Colour-by is deliberately not in it: scores are shown
 *  by their own toggle, never switched on by opening a lens. */
export interface LensLook {
  view?: unknown
  labelVisible: Record<string, boolean>
  popVisible: Record<string, boolean>
  popEyes: Record<string, Record<string, boolean>>
  ribbonEyes: Record<string, Record<string, boolean>>
  trackVisible: Record<string, boolean>
  graphVisible: Record<string, boolean>
  showGatedTracks: boolean
}

/** The look from the window's draw spec (plain copies — the spec's getters are live). */
export function lensLookOf(s: {
  viewState?: unknown; maskValueName: string; labelNames: readonly string[]; popVisible: Record<string, boolean>
  popEyes: Record<string, Record<string, boolean>>; ribbonEyes: Record<string, Record<string, boolean>>
  trackVisible: Record<string, boolean>; graphVisible: Record<string, boolean>; showGatedTracks: boolean
}): LensLook {
  const copy = <T>(v: T): T => JSON.parse(JSON.stringify(v ?? {}))
  return {
    view: s.viewState ? copy(s.viewState) : undefined,
    labelVisible: Object.fromEntries(s.labelNames.map(n => [n, n === s.maskValueName])),
    popVisible: { ...s.popVisible }, popEyes: copy(s.popEyes), ribbonEyes: copy(s.ribbonEyes),
    trackVisible: { ...s.trackVisible }, graphVisible: { ...s.graphVisible }, showGatedTracks: !!s.showGatedTracks,
  }
}

/** The settings writes a look needs — the settings store satisfies it. */
export interface LensLookTarget {
  setLabelVisibility(imageUid: string, vis: Record<string, boolean>): void
  setPopVisible(setUid: string, popType: string, v: boolean): void
  setPopEyes(imageUid: string, vn: string, eyes: Record<string, boolean>, layer?: 'points' | 'ribbons'): void
  setTrackVisibility(imageUid: string, vis: Record<string, boolean>): void
  setGraphVisibility(imageUid: string, vis: Record<string, boolean>): void
  setShowGatedTracks(setUid: string, v: boolean): void
}

/** Put a lens's look back: the same segmentation, population types and eyes, tracks and graphs. */
export function applyLensLook(look: LensLook, imageUid: string, setUid: string, t: LensLookTarget): void {
  if (Object.keys(look.labelVisible ?? {}).length) t.setLabelVisibility(imageUid, look.labelVisible)
  if (setUid) {
    for (const [pt, v] of Object.entries(look.popVisible ?? {})) t.setPopVisible(setUid, pt, v)
    t.setShowGatedTracks(setUid, !!look.showGatedTracks)
  }
  for (const [vn, eyes] of Object.entries(look.popEyes ?? {})) t.setPopEyes(imageUid, vn, eyes, 'points')
  for (const [vn, eyes] of Object.entries(look.ribbonEyes ?? {})) t.setPopEyes(imageUid, vn, eyes, 'ribbons')
  if (Object.keys(look.trackVisible ?? {}).length) t.setTrackVisibility(imageUid, look.trackVisible)
  if (Object.keys(look.graphVisible ?? {}).length) t.setGraphVisibility(imageUid, look.graphVisible)
}

/** The names lenses were given (their figure's Movies & figures display name), by score column —
 *  every list that shows a lens score names it by these (`lensLabel`). Newest figure wins. */
export function lensNamesOf(movies: readonly { displayName?: string; mtime: number; lens?: FigureLens | null }[]): Record<string, string> {
  const out: Record<string, string> = {}
  for (const m of [...movies].sort((a, b) => a.mtime - b.mtime)) {
    const n = (m.displayName ?? '').trim()
    if (m.lens && n) out[m.lens.column] = n
  }
  return out
}

/** A lens score column as people see it: the name it was given, else `lensN`. */
export const lensLabel = (column: string, names: Record<string, string> = {}): string =>
  names[column] || column.replace(/^spatial\.lens\./, '')

/** A lens figure as the Lenses section lists it. */
export interface LensFigureRow { name: string; label: string; lens: FigureLens
  /** its Movies & figures display name, '' = none (the label is then the lens's own name) */
  displayName: string }

/** The lens figures whose scores are on this image (Movies & figures' entries), newest first. */
export function lensFiguresFor(movies: readonly { name: string; displayName?: string; mtime: number; lens?: FigureLens | null }[],
                               imageUid: string): LensFigureRow[] {
  return movies
    .filter(m => m.lens && (m.lens.images?.length ? m.lens.images : [m.lens.sourceUid]).includes(imageUid))
    .sort((a, b) => b.mtime - a.mtime)
    .map(m => ({ name: m.name, lens: m.lens!, displayName: (m.displayName ?? '').trim(),
                 label: (m.displayName ?? '').trim() || m.lens!.column.replace(/^spatial\.lens\./, '') }))
}

/** What Find similar sends with its figure (`/api/figures/render` → `lens`); the server adds the run's images. */
export function figureLensBody(run: LensRegionRun, lens: { x: number; y: number; r: number }, t: number | null,
                               column: string, look?: LensLook) {
  return { x: Math.round(lens.x), y: Math.round(lens.y), r: Math.max(1, Math.round(lens.r)), t: t ?? -1,
           column, valueName: run.vn, regionSuffix: run.suffix, ...(look ? { look } : {}) }
}

/** The lens figure's crop: the base spec (the viewer's channels and contrast) framing the lens as the
 *  viewer showed it — its diameter plus a hair for the outline (`circles`), at least 16 px. `center` is
 *  [y, x] as the renderer reads it. */
export function lensFigureSpec<S extends { center: [number, number]; sizePx: number }>(
    base: S, lens: { x: number; y: number; r: number }): S & { circles: { x: number; y: number; r: number }[] } {
  return { ...base, center: [Math.round(lens.y), Math.round(lens.x)], sizePx: Math.max(16, Math.round(2.1 * lens.r)),
           circles: [{ x: Math.round(lens.x), y: Math.round(lens.y), r: Math.max(1, Math.round(lens.r)) }] }
}

/** Open a lens figure: the pending view the viewer applies — the view it was found in (channels, camera),
 *  the lens pinned again with its look (`PendingViewState.lens`), on its frame, on its image. Scores are
 *  not shown: that is the Lenses section's toggle. */
export function lensOpenPending(l: FigureLens) {
  return { ...(l.look?.view ? { viewState: l.look.view } : {}),
           lens: { x: l.x, y: l.y, r: l.r, ...(l.look ? { look: l.look } : {}) }, imageUid: l.sourceUid,
           ...(l.t !== undefined && l.t >= 0 ? { focus: { t: l.t } } : {}) }
}
