/**
 * The three movie surfaces (`ViewerPanel`, `AnimationPanel`, `BatchMoviesPanel`) all want the same
 * two facts to fill placeholders on their config controls:
 *   * canvas size — what a movie records at when the size fields are blank, shown as the fields'
 *     placeholder so the honest default is visible.
 *   * multiscale levels the open image has — the range the 3D detail control offers (0 or 1 = no
 *     choice to make, so the control hides).
 *
 * Both come from the browser volume viewer's own state via `useViewerStore` — canvas from the
 * published `viewState.canvas` (updated on every pan/zoom/resize), level count from `openImage`
 * (updated when the image loads). No poll, no bridge — the popup writes localStorage and this
 * window's store rehydrates through the storage bridge.
 */
import { computed } from 'vue'
import { useViewerStore } from '../stores/viewer'

export function useViewerMovieDefaults() {
  const store = useViewerStore()
  const canvasSizeX     = computed(() => store.viewState?.canvas?.width  ?? null)
  const canvasSizeY     = computed(() => store.viewState?.canvas?.height ?? null)
  const multiscaleLevels = computed(() => store.openImage?.nLevels ?? 0)
  return { canvasSizeX, canvasSizeY, multiscaleLevels }
}
