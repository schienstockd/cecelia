import { ref, watch, type Ref } from 'vue'
import { resolveTrackValueName } from '../plots/trackDiagnostics'

/**
 * Load the (all, tracked, active) value-name triple for an image, and resolve the picker's choice.
 *
 * Extracted from `TrackSchemeView.vue:258` (`loadValueNames`) so the cockpit and the timeline pick
 * from the same list with the same fallback rule — a second implementation would drift, and a chip
 * naming a segmentation that the paths call did not use is the exact "picker lies about the data
 * behind it" bug the timeline already had to fix (docs/todo/TRACK_SCHEME_PLAN.md P0).
 *
 * Reactive: refetches whenever projectUid/imageUid change. `resolved` gives the picker's current
 * choice with fallback (prefers wanted → active → first tracked → first-of-anything).
 */
export function useTrackValueNames(
  projectUid: Ref<string>,
  imageUid: Ref<string>,
  wanted: Ref<string>,
) {
  const valueNames = ref<string[]>([])
  const trackedNames = ref<string[]>([])
  const activeName = ref<string>('')
  const loading = ref(false)

  async function load(): Promise<void> {
    if (!projectUid.value || !imageUid.value) {
      valueNames.value = []; trackedNames.value = []; activeName.value = ''
      return
    }
    loading.value = true
    try {
      const q = `projectUid=${projectUid.value}&imageUid=${imageUid.value}&popType=track` +
                (wanted.value ? `&valueName=${encodeURIComponent(wanted.value)}` : '')
      const r = await fetch(`/api/gating/channels?${q}`)
      if (!r.ok) return
      const d = await r.json() as { valueNames?: string[]; trackedValueNames?: string[]; valueName?: string }
      valueNames.value = d.valueNames ?? []
      trackedNames.value = d.trackedValueNames ?? []
      activeName.value = d.valueName ?? ''
    } catch { /* the caller's own load reports any failure the user actually needs */ }
    finally { loading.value = false }
  }

  watch([projectUid, imageUid], () => { void load() }, { immediate: true })

  return {
    valueNames, trackedNames, activeName, loading, reload: load,
    /** The picker's current value with fallback — wanted → active → first tracked → first-of-anything. */
    resolved: () => resolveTrackValueName(wanted.value, trackedNames.value, valueNames.value, activeName.value),
  }
}
