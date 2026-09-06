import { ref, shallowRef, watch, type Ref } from 'vue'
import type { CorrectionPlan, AcquisitionPresetSummary } from '../types/correctionPlan'

// Fetch the recommended plan for one image. This composable is passive: it watches (projectUid,
// imageUid, cardId, wizard) and re-POSTs whenever any of them changes. Slice 3a doesn't persist
// the plan — save/load lands in 3b — so a component using it always sees a freshly recommended plan
// for its current inputs.

export function useCorrectionPlan(input: {
  projectUid: Ref<string>
  imageUid: Ref<string | null>
  cardId?: Ref<string | null>
  wizard?: Ref<Record<string, string>>
}) {
  const plan = shallowRef<CorrectionPlan | null>(null)
  const loading = ref(false)
  const error = ref<string | null>(null)

  // A monotonic token so a slow response can't overwrite a newer one — same discipline as
  // debouncedLatest, done inline because there's exactly one request in flight per composable.
  let seq = 0

  async function reload(): Promise<void> {
    const uid = input.imageUid.value
    const proj = input.projectUid.value
    if (!uid || !proj) {
      plan.value = null
      error.value = null
      return
    }
    const mySeq = ++seq
    loading.value = true
    error.value = null
    try {
      const res = await fetch('/api/correction-plan/recommend', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          projectUid: proj,
          imageUid: uid,
          cardId: input.cardId?.value ?? null,
          wizard: input.wizard?.value ?? {},
        }),
      })
      if (mySeq !== seq) return
      if (!res.ok) {
        let msg = `HTTP ${res.status}`
        try { const j = await res.json(); if (j?.error) msg = j.error as string } catch { /* keep msg */ }
        throw new Error(msg)
      }
      plan.value = await res.json() as CorrectionPlan
    } catch (e) {
      if (mySeq !== seq) return
      error.value = e instanceof Error ? e.message : String(e)
      plan.value = null
    } finally {
      if (mySeq === seq) loading.value = false
    }
  }

  watch(
    () => [input.projectUid.value, input.imageUid.value, input.cardId?.value ?? null,
           JSON.stringify(input.wizard?.value ?? {})],
    () => { void reload() },
    { immediate: true },
  )

  return { plan, loading, error, reload }
}

// Presets are a constant registry — one fetch per session is enough. Reused across mount points.
let _presetsCache: Promise<AcquisitionPresetSummary[]> | null = null
export function fetchCorrectionPresets(): Promise<AcquisitionPresetSummary[]> {
  if (_presetsCache === null) {
    _presetsCache = fetch('/api/correction-plan/presets')
      .then(r => {
        if (!r.ok) throw new Error(`HTTP ${r.status}`)
        return r.json() as Promise<AcquisitionPresetSummary[]>
      })
      .catch(err => { _presetsCache = null; throw err })
  }
  return _presetsCache
}
