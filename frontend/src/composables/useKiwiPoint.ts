// Kiwi points — perform what a clicked ref means (KIWI_ASSISTANT_PLAN → "Pointing = rendering a ref").
// `utils/kiwiTurn.ts::pointTarget` decides WHAT a click does, as data; this performs it with the
// existing point-out machinery — the same store setters the `viewer:mark` WS handler calls for the
// assistant's `mark_*` tools, the capture refocus path, the router. No new point-out mechanism.
//
// Marks carry `origin: 'claude'` — the provenance the plot consumers already render for an
// assistant's pointer (a Kiwi claim IS the assistant pointing, even though the user clicked it).
// Must be called from `setup`.

import { nextTick } from 'vue'
import { useRouter } from 'vue-router'
import { useViewerStore } from '../stores/viewer'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { usePlotRegistryStore } from '../stores/plotRegistry'
import { openViewerWindow } from '../utils/viewerWindow'
import { fetchCaptureEnvelope } from '../utils/kiwiCaptures'
import { pointTarget, refLabel, fetchPopulationCells } from '../utils/kiwiTurn'
import { useKiwiStore } from '../stores/kiwi'
import { revealPlots } from '../utils/sectionOpen'
import type { KiwiRef, KiwiRefResult } from '../utils/kiwiRef'
import { resolveAnchor } from '../utils/guideAnchor'
import { useCaptureFocus } from './useCaptureFocus'

const MARK_TTL_S = 300
const PLOT_POINT_TTL_S = 8      // "here it is" on a panel — a short bubble, not a standing mark

export function useKiwiPoint() {
  const router = useRouter()
  const viewer = useViewerStore()
  const project = useProjectStore()
  const pm = useProjectMetaStore()
  const plots = usePlotRegistryStore()
  const { focusCapture } = useCaptureFocus()
  const kiwi = useKiwiStore()

  /** Point at `ref`. Resolves to a short reason when there was nothing to do, else ''. No toast for that
   *  (user, 2026-09-24: "that's an error message") — a ref that can't be shown already says why on
   *  hover (`chipState`), so a click that finds nothing just does nothing. `result` is
   *  the resolver's answer when the caller has one — a plot's page comes from it once the panel is gone. */
  async function pointAt(ref: KiwiRef, label = '', result?: KiwiRefResult): Promise<string> {
    const puid = pm.current?.uid ?? ''
    if (!puid) return 'No project open'
    const tgt = pointTarget(ref)
    // the resolver's label when there is one — a claim row passes no label, and "plot" alone said nothing
    const caption = label || (result?.ok && result.label ? result.label : '') || refLabel(ref)
    switch (tgt.action) {
      case 'viewer': {
        if (tgt.tracks) viewer.setTrackHighlight({ imageUid: tgt.imageUid, valueName: tgt.tracks.valueName,
                                                   trackIds: tgt.tracks.ids, label: caption, origin: 'claude' })
        if (tgt.cells) viewer.setPickHighlight({ imageUid: tgt.imageUid, valueName: tgt.cells.valueName,
                                                 labels: tgt.cells.ids, focusId: tgt.cells.ids[0] ?? 0,
                                                 label: caption, origin: 'claude' })
        if (tgt.t != null || tgt.z != null) {
          viewer.setPendingViewState({ imageUid: tgt.imageUid,
                                       focus: { ...(tgt.t != null ? { t: tgt.t } : {}), ...(tgt.z != null ? { z: tgt.z } : {}) } })
        }
        const vn = tgt.tracks?.valueName ?? tgt.cells?.valueName
        openViewerWindow({ projectUid: puid, imageUid: tgt.imageUid, ...(vn ? { valueName: vn } : {}) })
        return ''
      }
      case 'population': {
        // its cells, outlined in the viewer — the same PickHighlight a plot's brushing draws
        const cells = await fetchPopulationCells(puid, ref)
        if (!cells) return 'Couldn’t read that population'
        if (!cells.labelIds.length) return 'That population has no cells'
        viewer.setPickHighlight({ imageUid: tgt.imageUid, valueName: tgt.valueName, labels: cells.labelIds,
                                  focusId: 0, label: caption, origin: 'claude' })
        openViewerWindow({ projectUid: puid, imageUid: tgt.imageUid, valueName: tgt.valueName })
        return ''
      }
      case 'set':
        project.activeSetUid = tgt.setUid
        await router.push('/manage-images')
        return ''
      case 'route':
        if (tgt.rememberFn) localStorage.setItem(`cc-fn:${tgt.rememberFn.module}`, tgt.rememberFn.task)
        await router.push({ path: tgt.path, ...(tgt.query ? { query: tgt.query } : {}) })
        return ''
      case 'capture': {
        const env = await fetchCaptureEnvelope(puid, tgt.captureId)
        if (!env) return 'Capture not found'
        return focusCapture(puid, env) ? '' : 'Nowhere to reopen this capture'
      }
      case 'plot': {
        const last = plots.getLast(tgt.plotId)
        const route = last?.meta.route || result?.route || ''
        if (!route) return 'That plot isn’t open'
        await router.push(route)
        await nextTick()
        revealPlots()          // the page folds its image table so the plot is the first thing in view
        // the page may have to mount the panel first (a board fetches its layout); give it a second,
        // after which a missing anchor means the plot is gone from its page
        const anchor = `plot:${tgt.plotId}`
        let el: HTMLElement | null = null
        for (let i = 0; i < 10 && !el; i++) {
          await nextTick()
          el = resolveAnchor(anchor)
          if (!el) await new Promise(r => setTimeout(r, 100))
        }
        if (!el) return 'That plot isn’t on its page any more'
        el.scrollIntoView({ block: 'nearest' })     // instant: Kiwi measures where it landed, next
        kiwi.notePointed(anchor)
        if (last && tgt.u != null && tgt.v != null) {
          viewer.pushPlotMark({ markerId: `kiwi-${Date.now()}`, family: last.meta.family, plotId: tgt.plotId,
                                u: tgt.u, v: tgt.v, label: caption, ttlSeconds: MARK_TTL_S })
        } else {
          viewer.pushUiMark({ markerId: `kiwi-${Date.now()}`, anchor, label: caption, ttlSeconds: PLOT_POINT_TTL_S })
        }
        return ''
      }
      case 'ui':
        if (tgt.anchor.startsWith('nav:')) await router.push(tgt.anchor.slice(4))
        viewer.pushUiMark({ markerId: `kiwi-${Date.now()}`, anchor: tgt.anchor, label: caption, ttlSeconds: MARK_TTL_S })
        kiwi.notePointed(tgt.anchor)
        return ''
      case 'none':
        return tgt.why
    }
  }

  return { pointAt }
}
