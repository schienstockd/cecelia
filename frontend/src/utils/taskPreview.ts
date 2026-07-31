// Pure logic for the task preview — the decisions, kept out of the store/SFC so they are testable
// without a mounted component, a viewer or a GPU.
//
// The preview is expensive (real cellpose per run) and it is showing the user something they will
// judge parameters by, so both questions below have to be answered exactly: WHETHER to run, and WHAT
// the user is told when we don't.

/** What the backend reports about the viewer + worker (`GET /api/preview/status`). */
export interface PreviewStatus {
  alive: boolean
  starting: boolean
  imageUid: string | null
  zarrPath: string | null
  taskDir: string | null
}

/** What the module page knows: the task being configured. */
export interface PreviewContext {
  projectUid: string
  imageUid: string
  /** output value_name for the segmentation being configured */
  valueName: string
  params: Record<string, unknown> | null
}

export type PreviewBlocker =
  | 'off'               // toggle is off
  | 'pinned'            // user pinned the current result; stop chasing the view
  | 'no-context'        // no project/image selected on the page yet
  | 'no-params'         // task params not resolved yet
  | 'no-models'         // params carry no model to run
  | 'image-mismatch'    // the viewer has a different image open
  | 'no-image-open'     // nothing open in the viewer

/**
 * Why a preview must not run right now, or `null` when it may.
 *
 * `image-mismatch` is deliberately a blocker rather than something we resolve by opening the image:
 * the region comes from whatever the viewer is showing, so previewing an image the user is not looking
 * at would compute the wrong area AND move their viewer out from under them. The backend enforces the
 * same rule (`/api/preview/run` returns 409) — this copy exists so the UI can explain it before
 * spending a request, not as the authority.
 */
export function previewBlocker(
  ctx: PreviewContext | null,
  status: PreviewStatus | null,
  opts: { enabled: boolean; pinned: boolean },
): PreviewBlocker | null {
  if (!opts.enabled) return 'off'
  if (opts.pinned) return 'pinned'
  if (!ctx || !ctx.projectUid || !ctx.imageUid) return 'no-context'
  if (!ctx.params) return 'no-params'
  if (!hasPreviewableModel(ctx.params)) return 'no-models'
  if (!status || !status.imageUid) return 'no-image-open'
  if (status.imageUid !== ctx.imageUid) return 'image-mismatch'
  return null
}

/**
 * True when the params carry at least one model the preview can run.
 *
 * The worker previews only `matchAs: "base"` models — the primary segmentation is what you judge, and
 * a nucleus pass doubles the cost for a mask you are not looking at. Params with only non-base models
 * would make the worker raise, so the UI must not send them.
 */
export function hasPreviewableModel(params: Record<string, unknown> | null): boolean {
  const models = (params as { models?: unknown } | null)?.models
  if (!models || typeof models !== 'object') return false
  const entries = Object.values(models as Record<string, unknown>)
  if (entries.length === 0) return false
  return entries.some(m => {
    const matchAs = (m as { matchAs?: unknown } | null)?.matchAs
    return matchAs === undefined || matchAs === null || matchAs === '' || matchAs === 'base'
  })
}

/** One short line, in the imperative where the user has something to do. Empty = say nothing. */
export function blockerMessage(b: PreviewBlocker | null): string {
  switch (b) {
    case 'no-image-open':   return 'Open the image to preview it'
    case 'image-mismatch':  return 'Open this image to preview it'
    case 'no-models':       return 'Add a model to preview'
    // 'off' / 'pinned' / 'no-context' / 'no-params' are states the user chose or can see; a message
    // for those is noise (docs/UI.md — keep UI copy short, say nothing rather than narrate)
    default:                return ''
  }
}

/**
 * The 2D-fallback warning. A preview always runs on ONE z-plane, so in 3D display mode it previews the
 * current plane rather than refusing — and must say so.
 *
 * Short = the problem; detail = what the result does and does not tell you, which is the part that
 * matters here and the part "results may vary" would throw away. Per-plane inference IS the same
 * compute as the run, so everything you tune per-plane is faithful; what a single plane cannot show is
 * z-stitching, which is exactly what determines object counts and z-extents. A user who knows that can
 * still judge diameter from a plane; a user told "may vary" cannot judge anything.
 */
export const FALLBACK_2D_WARN = {
  short: '2D preview only',
  detail: 'Diameter, boundaries and splitting match the run; counts and z-extents will not (no z-stitching)',
} as const

/**
 * The base-model-only warning, for a run that declares a second (nucleus) model.
 *
 * The worker previews only `matchAs: "base"`, so the nucleus pass and the IoU matching step
 * (`_match_nuc_cyto`) do not run — which means for a two-model segmentation the preview is not what the
 * run produces. That has to be SAID, since the whole point of previewing real compute is that you can
 * trust it.
 *
 * Why not just preview both and match? Measured 2026-07-31 on one 590² plane: `_compute_iou_matrix`
 * takes **1.8 s at 100×100 labels and 26.9 s at 400×400** — it is quadratic in cell count. A warm
 * preview is 0.14–0.38 s, so matching would cost 5×–100× the entire preview and get worse the more
 * cells there are. Honest and fast beats complete and unusable. (That cost is also paid per timepoint
 * by every real nuc+cyto RUN — see TODO #00093.)
 *
 * `removeUnmatched` changes the advice, so it changes the text: with it on, matching DELETES base labels
 * that found no nucleus, so the run genuinely finds fewer cells than the preview shows. With it off the
 * base mask is untouched and only the nucleus layer is missing.
 */
export function baseOnlyWarning(params: Record<string, unknown> | null): { short: string; detail: string } {
  const models = (params as { models?: unknown } | null)?.models
  if (!models || typeof models !== 'object') return { short: '', detail: '' }
  const others = Object.values(models as Record<string, unknown>).filter(m => {
    const ma = (m as { matchAs?: unknown } | null)?.matchAs
    return ma !== undefined && ma !== null && ma !== '' && ma !== 'base'
  })
  if (others.length === 0) return { short: '', detail: '' }
  const removeUnmatched = Boolean((params as { removeUnmatched?: unknown }).removeUnmatched)
  return {
    short: 'Base model only',
    detail: removeUnmatched
      ? 'The run also matches nuclei and drops cells without one, so it will find fewer than this'
      : 'The nucleus pass is not previewed; these base masks are what the run produces',
  }
}

/**
 * What a finished preview says about itself. `cells === 0` is genuinely ambiguous — no signal in the
 * region, or a parameter that finds nothing — so it must NOT be reported as "no cells found" alone.
 */
export function previewSummary(
  counts: Record<string, number> | null,
  fallback2d: boolean,
  signal?: { hasSignal?: boolean; noSignalWhy?: string },
): { cells: number | null; text: string; warn: string; warnDetail: string } {
  const cells = counts && typeof counts.base === 'number' ? counts.base : null
  let warn = fallback2d ? FALLBACK_2D_WARN.short : ''
  let warnDetail = fallback2d ? FALLBACK_2D_WARN.detail : ''

  // A zero that means "there is nothing here" must never read as "your parameters found nothing" —
  // on a drift-corrected stack a padded plane returns 0 cells and looks exactly like too large a
  // diameter, so the user retunes against a region that could never produce a mask (TODO #00090).
  // The 2D-fallback warning yields to this one: no point explaining z-stitching for an empty region.
  if (cells === 0 && signal && signal.hasSignal === false) {
    warn = signal.noSignalWhy === 'padding' ? 'No image data here' : 'Region is blank'
    warnDetail = signal.noSignalWhy === 'padding'
      ? 'This part of the corrected stack is padding, not data — move to a plane with signal'
      : 'Every pixel in view is zero — check the channel, or move to where there is signal'
  }

  if (cells === null) return { cells: null, text: '', warn, warnDetail }
  return { cells, text: cells === 1 ? '1 cell' : `${cells} cells`, warn, warnDetail }
}

/** Debounce window, ms. A pan emits events continuously and each run is real cellpose on the GPU. */
export const PREVIEW_DEBOUNCE_MS = 400
