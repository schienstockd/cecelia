import { onScopeDispose } from 'vue'
import { onViewerImageMeta } from '../lib/viewerImageMetaChannel'

// "Refetch when one of THESE images' display metadata changes" — a channel rename in the image table
// or the metadata panel. The twin of `useDataRefresh` for renames: a rename is a direct POST, not a
// task, so it never bumps the data version and `useDataRefresh` never fires. Anything that copies
// `channelNames` out of a server response (`/api/gating/channels`) keeps the old names until this
// fires — and plot pages are kept alive, so navigating back never re-runs their load either.
//
// Not gated by `autoRefreshOnTask`: a rename changes labels only, never the data under a plot.
// Works in a component or a setup store (both own an effect scope).
export function useImageMetaRefresh(imageUids: () => string[], onRefresh: () => void) {
  const stop = onViewerImageMeta(ev => {
    if (imageUids().includes(ev.imageUid)) onRefresh()
  })
  onScopeDispose(stop)
}
