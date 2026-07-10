import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'
import { useSettingsStore } from '../stores/settings'
import { useLogStore } from '../stores/log'

// Open (or reload) an image in the napari viewer. The ONE open path — shared by the image-table eye
// button and the gating prev/next navigation — so the request payload, the reload-vs-open decision,
// and the error/status handling live in a single place (not re-implemented per caller). Callers that
// want a per-image loading indicator (the eye's spinner) manage that around this.
export function useNapariOpen() {
  const projectMeta = useProjectMetaStore()
  const project     = useProjectStore()
  const settings    = useSettingsStore()
  const log         = useLogStore()

  async function openInNapari(imageUid: string, setUid: string): Promise<void> {
    const projectUid = projectMeta.current?.uid
    if (!projectUid) return
    // clicking on the ALREADY-open image = reload it. Delegate to ViewerPanel (it owns the overlay
    // logic), which reloads DATA only unless the user ticked reset — no needless pyramid reopen.
    if (project.napariImageUid === imageUid) { project.requestNapariReload(); return }
    const autoProps = settings.napariAutoSaveLayerProps
    try {
      const res = await fetch('/api/napari/open', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          projectUid,
          imageUid,
          autoSaveProps: autoProps,
          autoLoadProps: autoProps,
          show3D:        settings.getShow3D(setUid),   // per-set (only applied where a z-axis exists)
          asDask:        settings.napariAsDask,
        }),
      })
      const body = await res.json().catch(() => ({})) as
        { ok?: boolean; starting?: boolean; message?: string; error?: string }
      if (res.status === 202 && body.starting) {
        log.info(body.message ?? 'Napari is starting…', { source: 'viewer' }); return
      }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
      log.info('Opened image in Napari.', { source: 'viewer' })
    } catch (e) {
      log.error(`Napari: ${e instanceof Error ? e.message : String(e)}`, { source: 'viewer' })
    }
  }

  return { openInNapari }
}
