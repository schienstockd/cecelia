import { ref, shallowRef, watch, type Ref } from 'vue'
import type { CorrectionPlan, AcquisitionPresetSummary } from '../types/correctionPlan'

// Load-first fetch strategy — slice 3b. When the image changes:
//   1. GET /api/correction-plan/get. If plan.json exists, that's the source of truth (card the user
//      picked, wizard answers) and we show it verbatim, with `saved = true` + `stale` from the API.
//   2. Otherwise POST recommend. `saved = false` — no sidecar on disk yet.
// A card change is a `save(cardId)`: server recomputes with the new inputs AND persists, then the
// panel shows the returned plan. A `refresh()` bypasses the load and re-recommends WITHOUT saving,
// so a user can preview alternatives without overwriting the sidecar.

interface LoadResponse {
  plan: CorrectionPlan | null
  exists: boolean
  stale: boolean
}

export function useCorrectionPlan(input: {
  projectUid: Ref<string>
  imageUid: Ref<string | null>
}) {
  const plan = shallowRef<CorrectionPlan | null>(null)
  const saved = ref(false)   // was this plan loaded from plan.json (true) or freshly recommended (false)
  const stale = ref(false)   // plan.json's saturationFingerprint differs from the image's current meta
  const loading = ref(false)
  const error = ref<string | null>(null)

  let seq = 0

  async function _postJson(url: string, body: object): Promise<Response> {
    return fetch(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    })
  }

  function _errMsg(res: Response, fallback: string): Promise<string> {
    return res.json().then(j => (j?.error as string) ?? fallback).catch(() => fallback)
  }

  async function loadOrRecommend(): Promise<void> {
    const uid = input.imageUid.value
    const proj = input.projectUid.value
    if (!uid || !proj) {
      plan.value = null; saved.value = false; stale.value = false; error.value = null
      return
    }
    const mySeq = ++seq
    loading.value = true
    error.value = null
    try {
      const qs = `projectUid=${encodeURIComponent(proj)}&imageUid=${encodeURIComponent(uid)}`
      const getRes = await fetch(`/api/correction-plan/get?${qs}`)
      if (mySeq !== seq) return
      if (getRes.ok) {
        const got = await getRes.json() as LoadResponse
        if (got.exists && got.plan) {
          plan.value = got.plan
          saved.value = true
          stale.value = got.stale
          return
        }
        // Fall through: no plan on disk → recommend.
      } else {
        throw new Error(await _errMsg(getRes, `HTTP ${getRes.status}`))
      }

      const recRes = await _postJson('/api/correction-plan/recommend', {
        projectUid: proj, imageUid: uid,
      })
      if (mySeq !== seq) return
      if (!recRes.ok) throw new Error(await _errMsg(recRes, `HTTP ${recRes.status}`))
      plan.value = await recRes.json() as CorrectionPlan
      saved.value = false
      stale.value = false
    } catch (e) {
      if (mySeq !== seq) return
      error.value = e instanceof Error ? e.message : String(e)
      plan.value = null; saved.value = false; stale.value = false
    } finally {
      if (mySeq === seq) loading.value = false
    }
  }

  // Save = recommend at (cardId, wizard) AND persist to plan.json. The returned plan replaces state.
  async function save(cardId: string | null, wizard: Record<string, string> = {}): Promise<void> {
    const uid = input.imageUid.value
    const proj = input.projectUid.value
    if (!uid || !proj) return
    const mySeq = ++seq
    loading.value = true
    error.value = null
    try {
      const res = await _postJson('/api/correction-plan/save', {
        projectUid: proj, imageUid: uid, cardId, wizard,
      })
      if (mySeq !== seq) return
      if (!res.ok) throw new Error(await _errMsg(res, `HTTP ${res.status}`))
      plan.value = await res.json() as CorrectionPlan
      saved.value = true
      stale.value = false        // just wrote it — the fingerprint is by definition current
    } catch (e) {
      if (mySeq !== seq) return
      error.value = e instanceof Error ? e.message : String(e)
    } finally {
      if (mySeq === seq) loading.value = false
    }
  }

  // Recompute at the current inputs WITHOUT saving — a preview against the freshly recommended
  // plan. If plan.json is on disk, the panel keeps showing it (saved stays true) after this returns
  // to reflect what's actually persisted.
  async function refresh(): Promise<void> {
    await loadOrRecommend()
  }

  watch(
    () => [input.projectUid.value, input.imageUid.value],
    () => { void loadOrRecommend() },
    { immediate: true },
  )

  return { plan, saved, stale, loading, error, save, refresh }
}

// Presets are a constant registry — one fetch per session is enough.
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
