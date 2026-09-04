/**
 * Figures a param can offer — the registry behind `ParamDef.figure`.
 *
 * The first figure was a segmentation group's column strip, and it was built where it was used, in
 * `ParamRenderer`'s group branch. The second one (smoothing's median vs gated) is not a group, which
 * left two ways to add it: match on the key inside the renderer, or let the SPEC say which figure it
 * wants. Matching on `temporalStat` in the renderer is the same bug `paramVis` already refuses for
 * roles — the picture becomes a second description of the form, free to diverge from it, and the next
 * task that wants a figure has to edit a Vue file to get one.
 *
 * So: `"figure": "smoothMethod"` in the task JSON, resolved here. Same shape as `PARAM_ADVISORS` next
 * door, for the same reason — the judgement is a plain module, unit-tested, and the renderer only
 * mounts what it is handed.
 *
 * A builder gets the same `AdvisorContext` an advisor gets: the selected images and the CURRENT form
 * values. Both matter here — the figure has to redraw when the window changes, and the cost row is
 * the size of the user's own movie.
 */
import type { AdvisorContext } from './paramAdvisors'
import type { ParamDef } from './types'
import type { VisColumns } from './paramVis'
import type { Severity } from '../lib/severity'
import { smoothFigure, smoothSpatialFigure } from './smoothVis'
import { driftFigure } from './driftVis'

/** Everything `ParamFigure.vue` needs. The builder decides all of it, including how big the float is. */
export interface ParamFigureDef {
  vis: VisColumns
  title: string
  /** the button's tooltip — what this particular figure shows */
  tip: string
  headings?: string[]
  note?: string
  noteSeverity?: Severity
  /** `FloatingPanel`'s namespace — per figure, so two of them never fight over one position */
  storageKey: string
  defaultW?: number
  defaultH?: number
}

export type FigureBuilder = (ctx: AdvisorContext) => ParamFigureDef

function num(v: unknown, fallback: number): number {
  const n = typeof v === 'number' ? v : typeof v === 'string' ? Number(v) : NaN
  return Number.isFinite(n) ? n : fallback
}

/**
 * Planes to process: z x t, summed over the selected images. Null when nothing is selected or the
 * metadata has not arrived — `gatedCost` then says "minutes" rather than inventing a number.
 */
function planeCount(ctx: AdvisorContext): number | null {
  const imgs = ctx.images ?? []
  if (!imgs.length) return null
  let total = 0
  for (const i of imgs) {
    const t = i.sizeT ?? 0, z = i.sizeZ ?? 0
    if (!t || !z) return null
    total += t * z
  }
  return total || null
}

export const PARAM_FIGURES: Record<string, FigureBuilder> = {
  /**
   * Smoothing's temporal statistic. The note is NOT the old "not for photon-limited data": that read
   * as measured-and-bad when what was measured is that the median smears moving cells and the gate
   * does not (#554). What stays worth saying at the point of choosing is the trade the figure shows.
   */
  smoothMethod: ctx => {
    const { vis, note } = smoothFigure({
      frames: num(ctx.values?.temporalFrames, 3),
      sigma: num(ctx.values?.spatialSigma, 1),
      planes: planeCount(ctx),
      channels: Math.max(1, (ctx.values?.channels as unknown[] | undefined)?.length ?? 1),
    })
    return {
      vis,
      // The whole point of the figure, said out loud. Two grids that agree ARE the answer at the
      // default window, but only to someone who knows that is what agreement means — so the figure
      // draws the conclusion rather than leaving it to be inferred. No severity: it is a
      // recommendation, not a finding about the user's data.
      note,
      // `<param> — at a glance` is the group figure's pattern, but it truncated to
    // `TEMPORAL STATISTIC — AT A G…` in the header and the half that survived was the informative
    // half. (Re-applied from the reverted compaction commit: the row removals there were not wanted,
    // this was a defect on screen.)
    title: 'Temporal statistic',
      tip: 'Show what each statistic does to a moving spot',
      headings: ['Input', 'Median', 'Gated'],
      storageKey: 'smooth-method-figure',
      defaultW: 340, defaultH: 300,
    }
  },

  /**
   * Smoothing's SPATIAL method. Same construction as `smoothMethod` — a schematic, drawn from the
   * real algorithms at 16x16, and a verdict line read off the frames rather than recomputed from
   * the settings. The verdict crossing follows the same rule the temporal figure follows: what the
   * pictures show is what the line says.
   */
  smoothSpatial: ctx => {
    const { vis, note } = smoothSpatialFigure({
      method: (String(ctx.values?.spatialMethod ?? 'gaussian') === 'bilateral_vst'
                 ? 'bilateral_vst' : 'gaussian'),
      sigma: num(ctx.values?.spatialSigma, 1),
      bilateralColor: num(ctx.values?.bilateralColor, 10),
      bilateralReach: num(ctx.values?.bilateralReach, 3),
      bilateralPolish: num(ctx.values?.bilateralPolish, 0.6),
    })
    return {
      vis,
      note,
      title: 'Spatial method',
      tip: 'Show what each spatial filter does to a sparse punctate field',
      headings: ['Input', 'Gaussian', 'Bilateral (VST)'],
      storageKey: 'smooth-spatial-figure',
      defaultW: 340, defaultH: 260,
    }
  },

  /**
   * Drift correction's estimator. Same construction as `smoothMethod`: a schematic drawn from the
   * real algorithms at 24x24 — a rotating field, translation-only alignment, rigid alignment, and a
   * static "on request" column for the deferred full 6-DOF 3D rigid (option A in
   * `docs/todo/DRIFT_RIGID_PLAN.md` P5).
   *
   * The `Per-frame cap` row reads the current form values, so tuning `driftMaxLag` /
   * `driftMaxAngle` reflects in the figure the same way `smoothVis`' window count does.
   */
  driftEstimator: ctx => {
    const { vis, note } = driftFigure({
      maxLag:      num(ctx.values?.driftMaxLag, 3),
      maxAngleDeg: num(ctx.values?.driftMaxAngle, 5),
    })
    return {
      vis,
      note,
      title: 'Estimator',
      tip: 'Show what each estimator does to a rotating field',
      headings: ['Input', 'Multi-lag', 'Rigid', '3D full'],
      storageKey: 'drift-estimator-figure',
      defaultW: 460, defaultH: 320,
    }
  },
}

/** The figure a param asks for, or undefined. An unknown name draws nothing rather than guessing. */
export function paramFigure(param: ParamDef): FigureBuilder | undefined {
  return param.figure ? PARAM_FIGURES[param.figure] : undefined
}
