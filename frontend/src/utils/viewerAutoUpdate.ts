// What a finished task does to the open viewer (Preferences → Auto-update, `settings.viewerAutoUpdate`).
// Pure: the app-level `composables/useViewerAutoUpdate.ts` turns the actions into store writes and
// channel publishes (docs/todo/VIEWER_ON_THE_IMAGE_PLAN.md, Decision 4).
import { writtenImageVersion } from './taskResultVersion'

export type ViewerAutoUpdateAction =
  /** refetch overlays (pop counts, colours, tracks) */
  | { kind: 'ping' }
  /** invalidate cached pixels of this image, scoped as narrow as the task says */
  | { kind: 'cacheClear'; valueName?: string; labelValueName?: string }
  /** show this image version */
  | { kind: 'version'; valueName: string }
  /** show only this mask */
  | { kind: 'onlyMask'; valueName: string }

/** `task:status` — done on the open image: refresh overlays, and drop the image's cached pixels
 *  (a re-run overwrites a store in place, keeping its name). `covered` is what the same task's
 *  `task:result` already did — it arrives first — so its overlay ping is not sent twice. The clear is
 *  always sent: the result's was scoped to the mask it wrote, which a viewer showing another mask
 *  rejects, and this unscoped one makes it re-check every store it draws. A second clear costs the
 *  viewer one meta fetch, never a second reload (it queues them, `mergeCacheClearJobs`). */
export function onStatusActions(data: Record<string, unknown>, openUid: string | null,
                                autoUpdate: boolean,
                                covered: ReadonlySet<ViewerAutoUpdateAction['kind']> = new Set()): ViewerAutoUpdateAction[] {
  if (!autoUpdate || String(data.status ?? '') !== 'done') return []
  if (!openUid || String(data.imageUid ?? '') !== openUid) return []
  const out: ViewerAutoUpdateAction[] = []
  if (!covered.has('ping')) out.push({ kind: 'ping' })
  out.push({ kind: 'cacheClear' })
  return out
}

/** `task:result` on the open image: a new image version is shown, a new mask is shown alone, a label
 *  or analysis value name refreshes overlays. A written label store's cache clear does not wait for
 *  Auto-update: stale slabs of a mask on screen are wrong, not just old. */
export function onResultActions(data: Record<string, unknown>, openUid: string | null,
                                autoUpdate: boolean): ViewerAutoUpdateAction[] {
  const imageUid = String(data.imageUid ?? '')
  if (!imageUid || imageUid !== openUid) return []
  const meta = (data.meta ?? {}) as Record<string, unknown>
  const out: ViewerAutoUpdateAction[] = []
  // `meta.valueName` alone is NOT an image version — tracking, clustering … return their LABEL vn
  // there. Only a pixel writer's (valueName, filename) pair is (`writtenImageVersion`).
  const added = writtenImageVersion(meta)
  if (added && autoUpdate) out.push({ kind: 'version', valueName: added }, { kind: 'ping' }, { kind: 'cacheClear', valueName: added })
  else if (meta.valueName && autoUpdate) out.push({ kind: 'ping' })
  const labelValueName = typeof meta.labelValueName === 'string' ? meta.labelValueName : ''
  if (labelValueName) out.push({ kind: 'cacheClear', labelValueName })
  if (labelValueName && autoUpdate) out.push({ kind: 'onlyMask', valueName: labelValueName }, { kind: 'ping' })
  return out
}
