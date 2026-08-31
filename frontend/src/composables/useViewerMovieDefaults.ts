/**
 * The three movie surfaces (`ViewerPanel`, `AnimationPanel`, `BatchMoviesPanel`) all want the same
 * facts to fill placeholders on their config controls:
 *   * canvas size — what a movie records at when the size fields are blank, shown as the fields'
 *     placeholder so the honest default is visible.
 *   * multiscale levels the open image has — the range the 3D detail control offers (0 or 1 = no
 *     choice to make, so the control hides).
 *   * viewer's current z — what the 2D z-slice slider snaps to when the user hasn't chosen one, so
 *     opening the movie form shows the plane they're already looking at rather than plane 0.
 *
 * All come from the browser volume viewer's own state via `useViewerStore` — canvas from the
 * published `viewState.canvas` (updated on every pan/zoom/resize), level count from `openImage`
 * (updated when the image loads), z from `viewState.dims.current_step[1]`. No poll, no bridge — the
 * popup writes localStorage and this window's store rehydrates through the storage bridge.
 */
import { computed } from 'vue'
import { useViewerStore } from '../stores/viewer'

export function useViewerMovieDefaults() {
  const store = useViewerStore()
  const canvasSizeX     = computed(() => store.viewState?.canvas?.width  ?? null)
  const canvasSizeY     = computed(() => store.viewState?.canvas?.height ?? null)
  const multiscaleLevels = computed(() => store.openImage?.nLevels ?? 0)
  // 2D only — a 3D viewer has no single z, so `null` and the recorder's own 3D branch (which
  // records every plane) is the right default there.
  const viewerZ = computed<number | null>(() => {
    const vs = store.viewState
    if (!vs || vs.dims?.ndisplay === 3) return null
    const step = vs.dims?.current_step
    const z = Array.isArray(step) && step.length >= 2 ? step[1] : null
    return typeof z === 'number' && Number.isFinite(z) ? Math.max(0, Math.round(z)) : null
  })
  return { canvasSizeX, canvasSizeY, multiscaleLevels, viewerZ }
}
