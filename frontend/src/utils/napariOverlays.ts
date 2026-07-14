// Canonical "re-push these overlays to napari" helper — one place that turns an OverlayPushConfig (+
// colour-by) into the show-tracks / show-populations / colour-labels requests. Used by zoom-to-source to
// restore the tracks/pops a captured frame had (the ViewerPanel drives the same endpoints interactively;
// this is the non-interactive caller so the two don't diverge into separate request-building logic).
import type { OverlayPushConfig } from './overlayLayers'

export interface RestoreOverlaysOpts extends OverlayPushConfig {
  colourBy?: string
  pointsSize?: number
}

export async function restoreOverlays(projectUid: string, imageUid: string, cfg: RestoreOverlaysOpts): Promise<void> {
  const headers = { 'Content-Type': 'application/json' }
  const post = (path: string, body: unknown) =>
    fetch(path, { method: 'POST', headers, body: JSON.stringify(body) }).catch(() => undefined)
  // SEQUENTIAL — the bridge processes one command at a time; firing these in parallel let a later push's
  // layer reconciliation race an earlier one (tracks would stick but points wouldn't). Await each in turn.
  // tracks: one call covers whole-segmentation (_tracked), gated `track` and `trackclust` ribbons
  if (cfg.trackValueNames.length || cfg.showGatedTracks || cfg.showTrackclust) {
    await post('/api/napari/show-tracks', {
      projectUid, imageUid, valueNames: cfg.trackValueNames,
      showGatedTracks: cfg.showGatedTracks, showTrackclust: cfg.showTrackclust, colorBy: cfg.colourBy ?? '',
    })
  }
  // population POINTS, per pop type that was shown
  for (const pt of cfg.popTypes) {
    await post('/api/napari/show-populations', { projectUid, imageUid, popType: pt, show: true, pointsSize: cfg.pointsSize ?? 6 })
  }
  // colour the label mask by the same measure (harmless no-op if there's no labels layer)
  if (cfg.colourBy) {
    await post('/api/napari/colour-labels', { projectUid, imageUid, column: cfg.colourBy })
  }
}
