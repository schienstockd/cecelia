import { useProjectStore } from '../../stores/project'
import { useSettingsStore } from '../../stores/settings'
import { useViewerStore } from '../../stores/viewer'
import { useLogStore } from '../../stores/log'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { openViewerWindow } from '../viewerWindow'
import { buildFocusViewState } from './focusOnCell'

/**
 * Show tracks in the browser viewer, and fly the camera to the union bbox at the middle timepoint.
 *
 * Extracted from `TrackSchemeView.vue:showInViewer` so the correction cockpit and the timeline
 * both drive the viewer the same way — a second copy in the cockpit would drift immediately,
 * and the whole point of the cockpit is that "Show" means the same thing everywhere. Steps:
 *
 *   1. **Ensure the viewer's on this image.** No force-launch — that matches the canvas nav
 *      rule ("don't open the viewer just because a lane was clicked"). Logs a hint and returns
 *      false when no viewer is open.
 *   2. **Enable the segmentation's track visibility** so the highlight has a source to narrow.
 *      Pokes `cc.viewerOverlaysTick` so the popup rebuilds its overlays immediately.
 *   3. **Publish the highlight** via `viewerStore.setTrackHighlight` — the viewer's per-vn
 *      track source drops to just these ids via `filterPayloadByTracks`.
 *   4. **Fit + jump.** ONE fetch of paths (limited to the requested ids), ONE fetch of
 *      geometry (for voxelUm), compute the union bbox + middle-t, `buildFocusViewState` →
 *      `setPendingViewState`. Silent failure here is fine — the highlight is the primary
 *      effect and a missing focus just means the user pans themselves.
 *
 * Returns true iff the highlight was published; false when there's no viewer open or no ids.
 */
export async function showTracksInViewer(
  projectUid: string,
  imageUid: string,
  valueName: string,
  trackIds: readonly number[],
  source: string = 'tracks',
): Promise<boolean> {
  if (!trackIds.length || !projectUid || !imageUid || !valueName) return false

  const project = useProjectStore()
  const projectMeta = useProjectMetaStore()
  const settings = useSettingsStore()
  const viewerStore = useViewerStore()
  const log = useLogStore()

  // 1. Ensure viewer is on this image (no force-launch)
  if (!project.openImageUid) {
    log.info('Open this image in the viewer first.', { source })
    return false
  }
  if (project.openImageUid !== imageUid) {
    openViewerWindow({
      projectUid: projectMeta.current?.uid ?? projectUid,
      imageUid,
      valueName: valueName || undefined,
    })
  }

  // 2. Enable this segmentation's ribbons (the highlight NARROWS; the source has to exist)
  const uid = project.openImageUid
  if (uid) {
    const cur = settings.getTrackVisibility(uid, [valueName])
    if (!cur[valueName]) settings.setTrackVisibility(uid, { ...cur, [valueName]: true })
    if (typeof localStorage !== 'undefined') {
      localStorage.setItem('cc.viewerOverlaysTick', `${uid}:${Date.now()}`)
    }
  }

  // 3. Highlight
  viewerStore.setTrackHighlight({ imageUid, valueName, trackIds: [...trackIds] })

  // 4. Fit + jump — best effort. See TSV showInViewer for the design notes; the bbox is over
  // every requested track's points and t goes to the middle of the union window so the most
  // tracks tend to be alive at the target frame. Any skip carries a reason in the log rail —
  // silent fits that didn't move the camera made this hard to notice, since the highlight
  // still lands and the user assumes the whole Show fired.
  const nTracks = trackIds.length
  let skipReason: string | null = null
  let t = 0
  try {
    const cp = new URLSearchParams({ projectUid, imageUid, ids: trackIds.join(',') })
    if (valueName) cp.set('valueName', valueName)
    // Geometry is per-IMAGE, not per-valueName — voxelUm is a property of the pixels, and a
    // tracked value name (e.g. "flowTom") has no `filepath` field of its own in the CCID (it
    // derives from a base segmentation). Passing a tracked valueName here always 404s the way
    // the handler is written ("No filepath registered"). Omitting valueName ⇒ the handler
    // resolves the ACTIVE image version, which is what we want for the camera fit anyway.
    const geomUrl = `/api/images/geometry?projectUid=${encodeURIComponent(projectUid)}` +
                    `&imageUid=${encodeURIComponent(imageUid)}`
    const [rPaths, rGeom] = await Promise.all([
      fetch(`/api/tracking/paths?${cp}`),
      fetch(geomUrl),
    ])
    if (!rPaths.ok || !rGeom.ok) {
      skipReason = `paths ${rPaths.status}, geom ${rGeom.status}`
    } else {
      const dPaths = await rPaths.json() as {
        groups?: { imageUids?: string[]; paths?: Record<string, { t: number[]; x: number[]; y: number[]; z?: number[] }> }[]
      }
      const dGeom = await rGeom.json() as { voxelUm?: number[] }
      const grp = (dPaths.groups ?? []).find(g => (g.imageUids ?? []).includes(imageUid))
               ?? dPaths.groups?.[0]
      const vu = dGeom.voxelUm
      if (!grp?.paths) {
        skipReason = `paths response has no matching group for ${imageUid}`
      } else if (!vu || vu.length < 2) {
        skipReason = `voxelUm missing on ${imageUid}`
      } else {
        // Pass 1: union time range (for the target t) + union bbox (for the fit).
        let xmin = Infinity, xmax = -Infinity, ymin = Infinity, ymax = -Infinity
        let zmin = Infinity, zmax = -Infinity, tmin = Infinity, tmax = -Infinity
        let anyZ = false
        for (const id of trackIds) {
          const p = grp.paths[String(id)]
          if (!p?.t?.length) continue
          for (let i = 0; i < p.t.length; i++) {
            const x = p.x[i], y = p.y[i], tt = p.t[i]
            if (x < xmin) xmin = x; if (x > xmax) xmax = x
            if (y < ymin) ymin = y; if (y > ymax) ymax = y
            if (tt < tmin) tmin = tt; if (tt > tmax) tmax = tt
            if (p.z) {
              anyZ = true
              const z = p.z[i]
              if (z < zmin) zmin = z; if (z > zmax) zmax = z
            }
          }
        }
        if (!Number.isFinite(xmin)) {
          skipReason = `none of the ${nTracks} tracks are in the paths response`
        } else {
          t = Math.round((tmin + tmax) / 2)
          // Pass 2: centre on where the cells ACTUALLY ARE at t, not on the bbox centre. For each
          // track find its sample nearest to t; average across tracks. A track that had already
          // ended (or hadn't started) at t is silently dropped from the average — the alternative
          // (a stale sample from an earlier/later frame) would drag the camera off the cluster.
          // Was: bbox centre over all time — a track moving across the field of view then landed
          // the camera at its path midpoint, not on the cell at t (Dominik, 2026-09-07).
          let sxUm = 0, syUm = 0, szUm = 0, nContrib = 0, nContribZ = 0
          for (const id of trackIds) {
            const p = grp.paths[String(id)]
            if (!p?.t?.length) continue
            let best = 0, bestDist = Infinity
            for (let i = 0; i < p.t.length; i++) {
              const d = Math.abs(p.t[i] - t)
              if (d < bestDist) { bestDist = d; best = i }
            }
            sxUm += p.x[best]; syUm += p.y[best]; nContrib++
            if (p.z) { szUm += p.z[best]; nContribZ++ }
          }
          const cxUm = nContrib ? sxUm / nContrib : (xmin + xmax) / 2
          const cyUm = nContrib ? syUm / nContrib : (ymin + ymax) / 2
          const czUm = nContribZ ? szUm / nContribZ : (anyZ ? (zmin + zmax) / 2 : null)
          const cx = cxUm / (vu[0] || 1)
          const cy = cyUm / (vu[1] || 1)
          const cz = czUm !== null ? czUm / (vu[2] || 1) : undefined
          const halfWpx = Math.max(1, (xmax - xmin) / 2 / (vu[0] || 1))
          const halfHpx = Math.max(1, (ymax - ymin) / 2 / (vu[1] || 1))
          if (!viewerStore.viewState) {
            skipReason = 'viewer viewState not published yet (is the popup open?)'
          } else {
            const focus = buildFocusViewState(viewerStore.viewState, { t, cx, cy, cz, halfWpx, halfHpx })
            if (focus) viewerStore.setPendingViewState(focus)
            else skipReason = 'buildFocusViewState returned null (viewer state incomplete)'
          }
        }
      }
    }
  } catch (e) {
    skipReason = `fit threw — ${e instanceof Error ? e.message : String(e)}`
  }

  const tracks = `${nTracks} track${nTracks === 1 ? '' : 's'}`
  if (skipReason) log.info(`Highlighting ${tracks} — camera skipped: ${skipReason}.`, { source })
  else log.info(`Highlighting ${tracks} — moved to t=${t}.`, { source })
  return true
}
