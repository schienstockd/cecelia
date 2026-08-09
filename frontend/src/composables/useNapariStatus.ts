/**
 * One poll of `GET /api/napari/status`, shared by every component that needs it.
 *
 * Two consumers with different interests read the same payload: ViewerPanel warns when the bridge is
 * STALE (napari is a separate process that survives a backend restart), and the three movie surfaces
 * want the CANVAS SIZE — what a movie records at when no size is asked for, shown as the size fields'
 * placeholder. That is one request, so it is one poller with module-level state and refcounted timer,
 * rather than three components each hitting the route on their own schedule.
 *
 * See docs/NAPARI.md → *Movie output size*.
 */
import { ref, onMounted, onUnmounted } from 'vue'

const bridgeStale = ref(false)
const canvasSizeX = ref<number | null>(null)
const canvasSizeY = ref<number | null>(null)
// multiscale levels the OPEN IMAGE has — the range the 3D detail control offers (0 or 1 = no choice
// to make, so the control hides). See docs/NAPARI.md → *3D detail*.
const multiscaleLevels = ref<number>(0)

let timer: number | undefined
let watchers = 0

/** Fetch once. Exported so a caller can refresh right after an action (e.g. reopening the viewer). */
export async function pollNapariStatus() {
  try {
    const s = await (await fetch('/api/napari/status')).json() as
      { bridgeStale?: boolean; canvasSizeX?: number | null; canvasSizeY?: number | null
        multiscaleLevels?: number | null }
    bridgeStale.value = !!s.bridgeStale
    canvasSizeX.value = s.canvasSizeX ?? null
    canvasSizeY.value = s.canvasSizeY ?? null
    multiscaleLevels.value = s.multiscaleLevels ?? 0
  } catch {
    bridgeStale.value = false            // no bridge → nothing to warn about, and no canvas to report
  }
}

/**
 * Reactive status, polling while at least one component is mounted. The refs are shared, so a second
 * consumer costs no extra requests.
 */
export function useNapariStatus() {
  onMounted(() => {
    watchers += 1
    void pollNapariStatus()
    if (timer === undefined) timer = window.setInterval(pollNapariStatus, 5000)
  })
  onUnmounted(() => {
    watchers -= 1
    if (watchers <= 0 && timer !== undefined) { clearInterval(timer); timer = undefined }
  })
  return { bridgeStale, canvasSizeX, canvasSizeY, multiscaleLevels, poll: pollNapariStatus }
}
