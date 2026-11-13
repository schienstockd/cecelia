import { ref, onMounted, onUnmounted, watch } from 'vue'
import { useProjectStore } from '../stores/project'
import { createClaimRegistry } from '../utils/overlayAutoShow'

// The main window's app-level overlay glue: the autoshow claims, keyed off the project store's
// `openImageUid`, and the colour-by legend bag (dead until the panel goes — the legend is dropped,
// docs/todo/VIEWER_ON_THE_IMAGE_PLAN.md, S3). Mounted ONCE in App.vue: never in a component that can
// be unmounted, and reading stores, never a component's refs.

// ── Shared colour-by legend ──────────────────────────────────────────────────────
// {category value → hex} and {category value → population name} — module-level (not
// ViewerPanel-local) because whoever ends up populating them app-side (a route reply, the browser
// viewer's own legend derivation, or the animation-snapshot legend) needs to leave them correct
// whether or not the panel is open. Empty during PR 2; PR 4 rewires the source.
export const colourLegend       = ref<Record<string, string>>({})
export const colourLegendLabels = ref<Record<string, string>>({})
export function resetColourLegend() { colourLegend.value = {}; colourLegendLabels.value = {} }

// Live preview of a running task's label store moved to the viewer window: composables/useLivePreview.ts.

// ── Opt-out for callers that restore a DIFFERENT view than the remembered toggles ───────────────
const _claims = createClaimRegistry()
// Claim an image's next open (analysis-board zoom-to-source replays a captured frame instead).
export function suppressAutoShowOnce(imageUid: string) { _claims.claim(imageUid) }
// Release the claim when the open never happened (request failed), so the next legitimate open for
// that image is not silently swallowed. No argument drops every claim.
export function releaseAutoShowSuppression(imageUid?: string) { _claims.release(imageUid) }

// Mount ONCE, app-level (App.vue) — see rule 1. Not for use in a page or a floating panel.
export function useOverlayAutoShow() {
  const project = useProjectStore()
  let stopWatch: (() => void) | null = null
  onMounted(() => {
    stopWatch = watch(() => project.openImageUid, (uid) => {
      // The `suppressAutoShowOnce` claim stays as a hook for the eventual browser-viewer restore path
      // — consuming the claim keeps the semantic that "this open was handled by whoever set the claim,
      // don't run the default restore".
      if (uid) void _claims.consume(uid)
    })
  })
  onUnmounted(() => { stopWatch?.() })
}
