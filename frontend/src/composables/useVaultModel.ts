// Resolve a vault-model NAME to its kind + manifest without the caller having to know which vault
// holds it. The Model vault has a chip that switches which endpoint it browses; a plot that reads a
// model needs to dispatch on the same axis — but it doesn't share the vault's `kind` state (canvas
// panels are per-plot). Fetching both endpoints and looking the name up in either is the honest
// answer, and once fetched the two lists are cached at MODULE scope so every plot on the canvas
// hits one round-trip total instead of N.
//
// Returns:
//   kind:     'flow' | 'denoise' | null   — null while loading OR when the name isn't in either vault
//   manifest: the model's manifest, typed by kind (union)
//   loading:  true while either fetch is in flight
//   error:    a network/HTTP error if either endpoint failed
//
// Refreshing: `refresh()` re-fetches both. Wire it to `useDataRefresh` in the caller if the plot
// should react to a training-task completion. Not automatic here because a plot that never renames
// or retrains models doesn't need to.

import { computed, onScopeDispose, ref, watchEffect, type Ref } from 'vue'
import { endpointsFor, type VaultKind } from '../utils/modelVaultKinds'
import type { FlowManifest } from '../utils/flowManifest'
import type { DenoiseManifest } from '../utils/denoiseManifest'

// Row shape as returned by both vault endpoints (a superset — kind decides which manifest applies).
interface VaultRow {
  name: string
  stem: string
  hasManifest: boolean
  manifest: FlowManifest | DenoiseManifest | Record<string, unknown>
}

interface KindListing {
  rows: VaultRow[] | null   // null = not fetched yet
  err: string
}

// Module-scoped cache. Two entries — one per known kind — so a single fetch per kind serves every
// caller on the page. `refresh()` clears them.
const cache: Record<VaultKind, KindListing> = {
  flowModels:    { rows: null, err: '' },
  denoiseModels: { rows: null, err: '' },
}

// A single in-flight promise per kind — dedup concurrent callers so a page mount does not fire the
// same GET N times (one per plot). Cleared on refresh.
const inflight: Partial<Record<VaultKind, Promise<void>>> = {}

// Bump this to invalidate every reactive consumer. `refresh()` increments it; `watchEffect` reruns.
const cacheEpoch = ref(0)

async function _fetchKind(kind: VaultKind): Promise<void> {
  if (cache[kind].rows !== null || cache[kind].err) return
  const existing = inflight[kind]
  if (existing) return existing
  const p = (async () => {
    try {
      const r = await fetch(endpointsFor(kind).list)
      if (!r.ok) throw new Error(`HTTP ${r.status}`)
      const data = await r.json()
      cache[kind] = { rows: data.models ?? [], err: '' }
    } catch (e) {
      cache[kind] = { rows: [], err: e instanceof Error ? e.message : String(e) }
    } finally {
      delete inflight[kind]
      cacheEpoch.value += 1
    }
  })()
  inflight[kind] = p
  return p
}

export interface UseVaultModel {
  kind: Ref<'flow' | 'denoise' | null>
  manifest: Ref<FlowManifest | DenoiseManifest | null>
  loading: Ref<boolean>
  error: Ref<string>
  refresh: () => Promise<void>
}

/**
 * Reactive lookup for a model NAME across both vaults.
 *
 * `name` may be a ref or a getter — the composable rewires when it changes. An empty/undefined name
 * yields `kind: null, manifest: null, loading: false`.
 */
export function useVaultModel(nameRef: Ref<string> | (() => string)): UseVaultModel {
  const currentName = computed(() => typeof nameRef === 'function' ? nameRef() : nameRef.value)

  const loading = ref(false)
  const error = ref('')

  // Kick a fetch when the name is non-empty and either vault hasn't been fetched yet.
  watchEffect(() => {
    if (!currentName.value) {
      loading.value = false
      error.value = ''
      return
    }
    const needFlow    = cache.flowModels.rows === null && !cache.flowModels.err
    const needDenoise = cache.denoiseModels.rows === null && !cache.denoiseModels.err
    if (needFlow || needDenoise) {
      loading.value = true
      Promise.all([
        needFlow ? _fetchKind('flowModels') : Promise.resolve(),
        needDenoise ? _fetchKind('denoiseModels') : Promise.resolve(),
      ]).finally(() => {
        loading.value = false
        error.value = cache.flowModels.err || cache.denoiseModels.err
      })
    } else {
      loading.value = false
      error.value = cache.flowModels.err || cache.denoiseModels.err
    }
  })

  const kind = computed<'flow' | 'denoise' | null>(() => {
    void cacheEpoch.value   // reactive on cache refresh
    const n = currentName.value
    if (!n) return null
    if (cache.flowModels.rows?.some(r => r.name === n || r.stem === n))    return 'flow'
    if (cache.denoiseModels.rows?.some(r => r.name === n || r.stem === n)) return 'denoise'
    return null
  })

  const manifest = computed<FlowManifest | DenoiseManifest | null>(() => {
    void cacheEpoch.value
    const n = currentName.value
    if (!n) return null
    const k = kind.value
    if (k === null) return null
    const list = (k === 'flow' ? cache.flowModels.rows : cache.denoiseModels.rows) ?? []
    const row = list.find(r => r.name === n || r.stem === n)
    return (row?.manifest as FlowManifest | DenoiseManifest) ?? null
  })

  async function refresh() {
    cache.flowModels    = { rows: null, err: '' }
    cache.denoiseModels = { rows: null, err: '' }
    cacheEpoch.value += 1
    loading.value = true
    await Promise.all([_fetchKind('flowModels'), _fetchKind('denoiseModels')])
    loading.value = false
    error.value = cache.flowModels.err || cache.denoiseModels.err
  }

  // No side effects to unregister — the cache is module-scoped by design. Keep the hook for
  // symmetry with the other composables, so a future subscription can slot in without touching
  // every consumer.
  onScopeDispose(() => { /* no-op */ })

  return { kind, manifest, loading, error, refresh }
}

/** Clear the module cache — for tests. Production callers should use `refresh()`. */
export function _resetVaultModelCache() {
  cache.flowModels = { rows: null, err: '' }
  cache.denoiseModels = { rows: null, err: '' }
  cacheEpoch.value += 1
}
