// Open a capture where it was made — THE one refocus path. Was written out twice (Kiwi's recent-
// captures row, the Blackboard's attachment chip) and a Kiwi claim chip would have been the third.
//
//   • plot captures (canvas Share) → hand the envelope to the reshow store and navigate to the module
//     page that owns the canvas; the page mounts CaptureViewSurface over its own plots on arrival
//     (`consumeFor(module)`).
//   • viewer captures → seed `pendingViewState` (it persists to localStorage, so a fresh pop-out
//     mount reads it), then open the pop-out. Modern captures carry `viewStateSnapshot` and restore
//     the exact view; older ones fall into the `focus` path — nudge t / z, leave camera + channels.
//     A t-range (slab capture) refocuses to its first frame.
//
// Must be called from `setup` (it takes the router).

import { useRouter } from 'vue-router'
import { useViewerStore } from '../stores/viewer'
import { useCaptureReshowStore } from '../stores/captureReshow'
import { moduleRouteFor } from '../utils/moduleRoute'
import { openViewerWindow } from '../utils/viewerWindow'
import type { CaptureEnvelope } from '../utils/kiwiCaptures'

export function useCaptureFocus() {
  const router = useRouter()
  const viewer = useViewerStore()
  const reshow = useCaptureReshowStore()

  /** Refocus `env`. Returns false when there is nowhere to go (a plot capture from a page with no
   *  reshow flow and no image, or a capture with no address). */
  function focusCapture(projectUid: string, env: CaptureEnvelope): boolean {
    if (env.surface === 'plot') {
      const mod = (env.address?.plotSpec?.params as { module?: string } | undefined)?.module
      const path = mod ? moduleRouteFor(mod) : null
      if (mod && path) {
        reshow.setPending({ module: mod, envelope: env })
        void router.push(path)
        return true
      }
      // no page we know — fall through: a plot capture that also names an image can still open it
    }
    const a = env.address
    if (!a?.imageUid) return false
    const t = Array.isArray(a.t) ? a.t[0] : a.t
    const marks = env.overlay ?? []
    const overlay = marks.length > 0 ? { captureId: env.captureId, marks: marks as unknown[] } : undefined
    if (env.viewStateSnapshot) {
      viewer.setPendingViewState({ viewState: env.viewStateSnapshot, overlay, imageUid: a.imageUid })
    } else {
      viewer.setPendingViewState({
        focus: { ...(typeof t === 'number' ? { t } : {}), ...(typeof a.z === 'number' ? { z: a.z } : {}) },
        overlay, imageUid: a.imageUid,
      })
    }
    openViewerWindow({ projectUid, imageUid: a.imageUid, ...(a.valueName ? { valueName: a.valueName } : {}) })
    return true
  }

  return { focusCapture }
}
