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
 * What a finished preview says about itself. `cells === 0` is genuinely ambiguous — no signal in the
 * region, or a parameter that finds nothing — so it must NOT be reported as "no cells found" alone.
 */
export function previewSummary(
  counts: Record<string, number> | null,
  fallback2d: boolean,
): { cells: number | null; text: string; warn: string; warnDetail: string } {
  const cells = counts && typeof counts.base === 'number' ? counts.base : null
  const warn = fallback2d ? FALLBACK_2D_WARN.short : ''
  const warnDetail = fallback2d ? FALLBACK_2D_WARN.detail : ''
  if (cells === null) return { cells: null, text: '', warn, warnDetail }
  return { cells, text: cells === 1 ? '1 cell' : `${cells} cells`, warn, warnDetail }
}

/** Debounce window, ms. A pan emits events continuously and each run is real cellpose on the GPU. */
export const PREVIEW_DEBOUNCE_MS = 400
