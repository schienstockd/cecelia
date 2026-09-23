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
import { useGatingStore } from '../stores/gating'
import { usePlotRegistryStore } from '../stores/plotRegistry'
import { openViewerWindow } from '../utils/viewerWindow'
import { fetchCaptureEnvelope } from '../utils/kiwiCaptures'
import { pointTarget, refLabel } from '../utils/kiwiTurn'
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

  /** Point at `ref`. Resolves to a short reason when there was nothing to do, else ''. No toast for that
   *  (user, 2026-09-24: "that's an error message") — a ref that can't be shown already says why on
   *  hover (`chipState`), so a click that finds nothing just does nothing. `result` is
   *  the resolver's answer when the caller has one — a plot's page comes from it once the panel is gone. */
  async function pointAt(ref: KiwiRef, label = '', result?: KiwiRefResult): Promise<string> {
    const puid = pm.current?.uid ?? ''
    if (!puid) return 'No project open'
    const tgt = pointTarget(ref)
    const caption = label || refLabel(ref)
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
      case 'gate': {
        // the gating page's image comes from its table selection; the population itself is per-panel
        // state nothing outside the panel can set, so this lands on the image, not the gate
        const sid = project.setUidOfImage(tgt.imageUid)
        if (sid) { project.activeSetUid = sid; project.setImageSelection('gate', sid, [tgt.imageUid]) }
        void useGatingStore().selectImage(tgt.imageUid, tgt.valueName)
        await router.push('/gate')
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
        el.scrollIntoView({ block: 'nearest', behavior: 'smooth' })
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
        return ''
      case 'none':
        return tgt.why
    }
  }

  return { pointAt }
}
