import { ref, onMounted, onUnmounted } from 'vue'
import { useProjectStore, type CciaImage } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useWsStore } from '../stores/ws'
import { useLogStore } from '../stores/log'
import {
  pushLabels, refreshLabels, pushTracks, pushPopulations, pushColourLabels,
} from '../utils/napariOverlays'
import {
  buildAutoShowPlan, activeValueName, createClaimRegistry, CELL_POP_TYPES,
  liveLabelPreviews, shouldRefreshPreview, type LivePreview, type TaskListEntry, trackableValueNames } from '../utils/napariAutoShow'

// Everything that turns an image's REMEMBERED overlay state into actual napari layers: the restore on
// open, the re-push when gating changes, and the one implementation of each overlay request the
// ViewerPanel toggles also go through.
//
// THREE RULES, all learned from real bugs — read before adding a fourth overlay kind:
//
// 1. OWNERSHIP. None of this may live in a component that can be unmounted. It used to live entirely
//    in ViewerPanel.vue, which App.vue mounts behind `v-if="settings.viewerPanelOpen"` — and that
//    floating panel is off by default. With it closed, nothing was subscribed to `napari:opened`, so
//    opening an image restored no overlays at all while the toggles (persisted in localStorage) still
//    read ON — the user had to flip each one off and on to get anything. The same was true of
//    `gating:popmap`: editing a gate did not update the napari overlay unless the panel happened to be
//    open. `useNapariAutoShow()` is mounted ONCE in App.vue so both work regardless.
//
// 2. SEQUENTIAL. The napari bridge drains ONE command at a time (napari_bridge.drain_queue), and its
//    layer reconciliation is not push-order-independent: fired concurrently, a later push's
//    reconciliation races an earlier one and some layers stick while others silently don't appear
//    (the same reason utils/napariOverlays.restoreOverlays awaits each step). Never turn the awaits in
//    `pushAllOverlays` into a Promise.all.
//
// 3. READ `settings`, NEVER A COMPONENT'S REFS. These run off WS events, so no component watcher is
//    guaranteed to have run first. Trusting ViewerPanel's refs is what previously pushed labels
//    against a stale/empty visibility map and skipped branches entirely. The panel persists every
//    toggle to `settings` before pushing, so reading `settings` here is equivalent AND timing-proof.

// ── Shared colour-by legend ──────────────────────────────────────────────────────
// {category value → hex} and {category value → population name}, harvested from whichever push last
// returned a legend. Module-level (not ViewerPanel-local) because the pushes that produce it now run
// app-level: a gate edit with the panel closed must still leave the legend correct for when it opens.
export const colourLegend       = ref<Record<string, string>>({})
export const colourLegendLabels = ref<Record<string, string>>({})
export function resetColourLegend() { colourLegend.value = {}; colourLegendLabels.value = {} }

// Merge a push response's legend in. Best-effort: a response without one (continuous column, hidden
// labels layer) simply leaves the current legend alone.
async function _harvestLegend(res: Response | undefined) {
  if (!res?.ok) return
  try {
    const j = (await res.json()) as { legend?: Record<string, string>; legendLabels?: Record<string, string> }
    if (Object.keys(j.legend ?? {}).length) colourLegend.value = { ...colourLegend.value, ...j.legend }
    if (Object.keys(j.legendLabels ?? {}).length)
      colourLegendLabels.value = { ...colourLegendLabels.value, ...j.legendLabels }
  } catch { /* legend harvest is best-effort */ }
}

// The open image + its remembered per-set display preferences. Null when nothing is open (or the image
// isn't in the loaded project), which every caller treats as "nothing to push".
interface OverlayCtx {
  uid: string; projectUid: string; img: CciaImage; setUid: string | null
  colourBy: string; overrides: Record<string, string>; pointsSize: number; valueName: string
}
function _ctx(): OverlayCtx | null {
  const project     = useProjectStore()
  const projectMeta = useProjectMetaStore()
  const settings    = useSettingsStore()
  const uid        = project.napariImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid) return null
  const img = project.imageByUid(uid)
  if (!img) return null
  const setUid   = project.setUidOfImage(uid)
  const colourBy = setUid ? settings.getColourBy(setUid) : ''
  return {
    uid, projectUid, img, setUid, colourBy,
    overrides:  (setUid && colourBy) ? settings.getColourOverrides(setUid, colourBy) : {},
    pointsSize: setUid ? settings.getPointSize(setUid) : 6,
    valueName:  activeValueName(img),
  }
}

// ── The one implementation of each overlay push ──────────────────────────────────
// ViewerPanel's toggles delegate to these rather than building their own requests: every panel call
// site persists its change to `settings` first, so reading `settings` here is equivalent to reading
// the panel's refs — and it keeps ONE request shape per endpoint (see rule 3).

// The mask outline width (0 = filled) for an image's set — the SAME per-set value the viewer's outline
// slider writes, read here rather than stored twice. Every show-labels push must carry it: the endpoint
// rebuilds the Labels layer and the backend defaults the value to 0, so a push without it refills a
// mask the user had outlined (which is what made recorded movies come out filled). `undefined` when the
// image has no set — then the backend default stands, as it did before.
function _labelContour(uid: string | undefined | null): number | undefined {
  if (!uid) return undefined
  const setUid = useProjectStore().setUidOfImage(uid)
  return setUid ? useSettingsStore().getMovieConfig(setUid).labelContour : undefined
}

// Track ribbons for whichever segmentations are toggled on, plus the gated-track / trackclust masters.
export async function pushTracksNow(): Promise<boolean> {
  const c = _ctx()
  if (!c) return false
  const settings = useSettingsStore()
  const trackVis = settings.getTrackVisibility(c.uid, trackableValueNames(c.img))
  const res = await pushTracks(c.projectUid, c.uid, {
    valueNames:      Object.keys(trackVis).filter(vn => trackVis[vn]),
    showGatedTracks: c.setUid ? settings.getShowGatedTracks(c.setUid) : false,
    showTrackclust:  c.setUid ? settings.getPopVisible(c.setUid, 'trackclust') : false,
    colorBy:         c.colourBy, colourOverrides: c.overrides,
  })
  // the Labels layer may be hidden (then colour-labels returns no legend), so tracks are the only
  // legend source when colouring tracks alone
  if (c.colourBy) await _harvestLegend(res)
  return !!res?.ok
}

// `show` is explicit, not read from settings: the panel's toggle pushes FIRST and only persists if the
// push succeeded, so it must be able to ask for a value settings doesn't hold yet.
export async function pushPopulationsNow(
  popType: string, show: boolean, valueName?: string,
): Promise<boolean> {
  const c = _ctx()
  if (!c) return false
  const res = await pushPopulations(c.projectUid, c.uid, {
    popType, show, valueName, pointsSize: c.pointsSize,
  })
  return !!res?.ok
}

// Recolour the Labels layer by an obs column ('' resets to napari's default label colours).
export async function pushColourLabelsNow(column: string, valueName?: string): Promise<boolean> {
  const c = _ctx()
  if (!c) return false
  if (!column) resetColourLegend()                     // reset → no legend
  const res = await pushColourLabels(c.projectUid, c.uid, {
    valueName: valueName ?? c.valueName, column, colourOverrides: c.overrides,
  })
  if (column) await _harvestLegend(res)                // categorical → {value: hex}
  return !!res?.ok
}

// ── Restore everything on open ───────────────────────────────────────────────────
// Re-push ALL of the image's DATA overlays — labels, branch (skeleton) labels, colour-by, population
// centroid points and track ribbons. Each endpoint re-reads from disk and replaces its own layer in
// place, so this touches NO image pyramid. Also ViewerPanel's data-only reload path.
export async function pushAllOverlays(): Promise<void> {
  const c = _ctx()
  if (!c) return
  const settings = useSettingsStore()
  const log      = useLogStore()

  const plan = buildAutoShowPlan({
    labels:           (c.img.labels ?? {}) as Record<string, string[]>,
    branchLabels:     (c.img.branchLabels ?? {}) as Record<string, string[]>,
    labelVisibility:  settings.getLabelVisibility(c.uid, Object.keys(c.img.labels ?? {})),
    branchVisibility: settings.getBranchVisibility(c.uid, Object.keys(c.img.branchLabels ?? {})),
    trackVisibility:  settings.getTrackVisibility(c.uid, trackableValueNames(c.img)),
    popTypes:         c.setUid ? CELL_POP_TYPES.filter(pt => settings.getPopVisible(c.setUid!, pt)) : [],
    showGatedTracks:  c.setUid ? settings.getShowGatedTracks(c.setUid) : false,
    showTrackclust:   c.setUid ? settings.getPopVisible(c.setUid, 'trackclust') : false,
  })

  const hasLabels = Object.keys(plan.labels).length > 0
  if (hasLabels) {
    const res = await pushLabels({ labels: plan.labels, show: true, cache: settings.napariLabelsCache,
                                   labelContour: _labelContour(c.uid) })
    if (!res?.ok) log.error('Show labels on open failed.', { source: 'napari' })
  }
  if (Object.keys(plan.branchLabels).length) {
    const res = await pushLabels({ branchLabels: plan.branchLabels, show: true, cache: settings.napariLabelsCache })
    if (!res?.ok) log.error('Show branches on open failed.', { source: 'napari' })
  }
  // Apply the remembered colour-by ONLY if this segmentation actually has that column — the preference
  // is per-SET and segmentations differ across a set, so a stale/absent column would recolour (and so
  // hide) napari's distinct default label colours.
  if (hasLabels && c.colourBy && await _hasObsColumn(c.projectUid, c.uid, c.valueName, c.colourBy)) {
    await pushColourLabelsNow(c.colourBy, c.valueName)
  }
  for (const popType of plan.popTypes) {
    await pushPopulationsNow(popType, true)
  }
  if (plan.pushTracks) await pushTracksNow()
}

// Does this segmentation carry `column`? Same endpoint the ViewerPanel colour-by dropdown reads.
// Best-effort: on any failure, report absent (skipping the recolour leaves the labels readable —
// applying a column the h5ad lacks does not).
async function _hasObsColumn(
  projectUid: string, imageUid: string, valueName: string, column: string,
): Promise<boolean> {
  if (!valueName) return false
  try {
    const q = `projectUid=${projectUid}&imageUid=${imageUid}&valueName=${encodeURIComponent(valueName)}`
    const res = await fetch(`/api/gating/channels?${q}`)
    if (!res.ok) return false
    const j = await res.json() as { obsColumns?: string[]; trackColourColumns?: string[] }
    return [...(j.obsColumns ?? []), ...(j.trackColourColumns ?? [])].includes(column)
  } catch { return false }
}

// ── Live update while gating ─────────────────────────────────────────────────────
// The population tree changed for the image open in napari (gate edit, pop add/remove/rename, cell
// selection, dot-size change) — re-push so the overlay tracks the gating.
export async function handleGatingChange(data: Record<string, unknown>): Promise<void> {
  const project = useProjectStore()
  if (String(data.imageUid ?? '') !== project.napariImageUid) return
  const settings = useSettingsStore()
  const setUid   = project.napariImageUid ? project.setUidOfImage(project.napariImageUid) : null
  const popType  = String(data.popType ?? 'flow')
  // track-grained edits (track / trackclust) re-push the RIBBONS (never points — points would be wrong
  // for track_ids and orphaned by the viewer's toggles). Cell-grained edits (flow / clust / region)
  // re-push that pop type's POINT overlay if it's visible.
  if (popType === 'track' || popType === 'trackclust') {
    const on = setUid && (settings.getShowGatedTracks(setUid) || settings.getPopVisible(setUid, 'trackclust'))
    if (on) await pushTracksNow()
  } else if (setUid && settings.getPopVisible(setUid, popType)) {
    await pushPopulationsNow(popType, true, data.valueName as string | undefined)
  }
}

// ── Live preview of a running task's label store ─────────────────────────────────
// A segmentation creates its label zarr at full shape and fills it one timepoint at a time, so it can
// be watched while it runs. `ccid.json` only registers the set on success, so the running task itself
// is the source of truth for what exists (`live_outputs` → GET /api/tasks).
//
// This lives app-level, next to the other pushes, for the same reason they do (rule 1): the tick that
// refreshes a preview arrives on the WS, and ViewerPanel — which renders the rows — is `v-if`'d, so a
// panel-scoped subscriber would stop refreshing the moment the user closed the panel while leaving the
// layer on screen, silently frozen.

// Label stores being written right now for the open image (drives the ViewerPanel rows).
export const livePreviews = ref<LivePreview[]>([])
// Which of them the user has actually asked to see, by value_name.
//
// Deliberately NOT persisted, unlike every other viewer toggle: it describes a store that exists only
// while one task runs. Persisting it would restore a preview for a value_name that may never exist
// again (a cancelled or failed run leaves nothing to register), producing a dead toggle for a layer
// the bridge can only skip. Module-level so it survives the panel being closed and reopened.
export const previewShown = ref<Record<string, boolean>>({})
const _lastRefreshAt: Record<string, number> = {}

function _previewFiles(valueName: string): string[] {
  return livePreviews.value.find(p => p.valueName === valueName)?.files ?? []
}

// Re-read what is in flight and reconcile the previews against it. Called on every task lifecycle
// event rather than polled: `list_tasks()` is a point-in-time snapshot, and the WS already says when
// that snapshot changed.
export async function refreshLivePreviews(): Promise<void> {
  const project  = useProjectStore()
  const imageUid = project.napariImageUid
  if (!imageUid) { livePreviews.value = []; return }
  let tasks: TaskListEntry[] = []
  try {
    const res = await fetch('/api/tasks')
    if (res.ok) tasks = await res.json() as TaskListEntry[]
  } catch { /* a snapshot we couldn't fetch just means no previews offered this round */ }
  const next = liveLabelPreviews(tasks, imageUid)
  const live = new Set(next.map(p => p.valueName))

  // A preview whose task is gone must not stay on screen pointing at a store nobody is writing —
  // hand it over to the finished set where there is one, otherwise just take it down.
  //
  // "Finished" is decided by `img.labels`, which the ws store fills from the task's OWN result meta:
  // `ws_result` is sent before the terminal `ws_status` on purpose (see sockets.jl), so by the time
  // this runs the successful run is already registered. A cancelled or failed run never registers, so
  // it correctly falls through to the plain hide.
  //
  // The promotion deliberately does NOT check `napariUpdateImage`: that setting exists to stop
  // expensive IMAGE-pyramid reloads on task completion, and this is the cheap labels-layer path for a
  // store the user explicitly asked to watch. Leaving them staring at a layer that just disappeared
  // would be the surprising behaviour.
  for (const vn of Object.keys(previewShown.value)) {
    if (!previewShown.value[vn] || live.has(vn)) continue
    const finished = (project.imageByUid(imageUid)?.labels ?? {})[vn] as string[] | undefined
    if (finished?.length) {
      // one request, and the bridge evicts the `(live)` layer as it adds the finished one
      const res = await pushLabels({ labels: { [vn]: finished }, show: true,
                                     cache: useSettingsStore().napariLabelsCache,
                                     labelContour: _labelContour(imageUid) })
      if (res?.ok) {
        const settings = useSettingsStore()
        settings.setLabelVisibility(imageUid,
          { ...settings.getLabelVisibility(imageUid, Object.keys(project.imageByUid(imageUid)?.labels ?? {})),
            [vn]: true })
        continue
      }
    }
    const files = _previewFiles(vn)
    if (files.length) void pushLabels({ labels: { [vn]: files }, show: false, cache: false, preview: true })
  }
  previewShown.value = Object.fromEntries(
    Object.entries(previewShown.value).filter(([vn, on]) => on && live.has(vn)))
  livePreviews.value = next
}

// Show/hide one live preview. Returns the new state so the caller can reflect a failed push.
export async function togglePreview(valueName: string): Promise<boolean> {
  const files = _previewFiles(valueName)
  if (!files.length) return false
  const want = !previewShown.value[valueName]
  const res = await pushLabels({ labels: { [valueName]: files }, show: want, cache: false, preview: true,
                                 labelContour: _labelContour(useProjectStore().napariImageUid) })
  if (!res?.ok) {
    useLogStore().error(`Could not ${want ? 'show' : 'hide'} the live preview for ${valueName}.`,
                        { source: 'napari' })
    return !!previewShown.value[valueName]
  }
  previewShown.value = { ...previewShown.value, [valueName]: want }
  if (want) _lastRefreshAt[valueName] = Date.now()
  return want
}

// Progress tick → re-read the shown previews, throttled (see shouldRefreshPreview: cellpose emits a
// tick per XY tile, and each refresh re-reads label chunks from disk).
function _onProgressTick(): void {
  const shown = livePreviews.value.filter(p => previewShown.value[p.valueName])
  if (!shown.length) return
  const now = Date.now()
  const due: Record<string, string[]> = {}
  for (const p of shown) {
    if (!shouldRefreshPreview(_lastRefreshAt[p.valueName], now)) continue
    _lastRefreshAt[p.valueName] = now
    due[p.valueName] = p.files
  }
  if (Object.keys(due).length) void refreshLabels(due)
}

// ── Opt-out for callers that restore a DIFFERENT view than the remembered toggles ───────────────
const _claims = createClaimRegistry()
// Claim an image's next open (analysis-board zoom-to-source replays a captured frame instead).
export function suppressAutoShowOnce(imageUid: string) { _claims.claim(imageUid) }
// Release the claim when the open never happened (request failed), so the next legitimate open for
// that image is not silently swallowed. No argument drops every claim.
export function releaseAutoShowSuppression(imageUid?: string) { _claims.release(imageUid) }

// Mount ONCE, app-level (App.vue) — see rule 1. Not for use in a page or a floating panel.
export function useNapariAutoShow() {
  const ws = useWsStore()
  const onOpened = (data: Record<string, unknown>) => {
    const uid = String(data?.imageUid ?? '')
    // previews belong to the image that was open; a different image's runs are a different set
    previewShown.value = {}
    void refreshLivePreviews()
    if (uid && _claims.consume(uid)) return
    void pushAllOverlays()
  }
  const onGating = (data: Record<string, unknown>) => { void handleGatingChange(data) }
  // Any task lifecycle change can add or remove a watchable store. Chain nodes are included because a
  // chain-launched segmentation writes exactly the same store — the frontend never sees its params, so
  // the backend's own `live_outputs` snapshot is what makes chain runs previewable at all.
  const onTaskLifecycle = () => { void refreshLivePreviews() }
  const onProgress = () => _onProgressTick()
  onMounted(() => {
    ws.on('napari:opened', onOpened)
    ws.on('gating:popmap', onGating)
    ws.on('task:status', onTaskLifecycle)
    ws.on('chain:node:running', onTaskLifecycle)
    ws.on('chain:node:done', onTaskLifecycle)
    ws.on('chain:node:failed', onTaskLifecycle)
    ws.on('task:progress', onProgress)
    void refreshLivePreviews()   // a run may already be in flight when the app connects
  })
  onUnmounted(() => {
    ws.off('napari:opened', onOpened)
    ws.off('gating:popmap', onGating)
    ws.off('task:status', onTaskLifecycle)
    ws.off('chain:node:running', onTaskLifecycle)
    ws.off('chain:node:done', onTaskLifecycle)
    ws.off('chain:node:failed', onTaskLifecycle)
    ws.off('task:progress', onProgress)
  })
}
