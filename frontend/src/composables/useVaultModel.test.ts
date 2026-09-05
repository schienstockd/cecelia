// `useVaultModel` is the ONE composable a plot uses to resolve a picked model name to its kind +
// manifest across both vault endpoints. A plot that reads the model can't know which vault chip
// the user picked — the resolution is what makes the training-convergence plot work for a denoise
// model without every plot reimplementing "try flow first, else denoise" (which is how they'd
// drift and start disagreeing about which vault a name belongs to).
//
// Pure-logic test: mock global.fetch, drive a name through, assert (kind, manifest). Nothing here
// mounts a component; the composable's contract is the return value.

import { describe, it, expect, beforeEach, vi } from 'vitest'
import { effectScope, ref } from 'vue'
import { useVaultModel, _resetVaultModelCache } from './useVaultModel'

interface FetchStubMap {
  '/api/optical-flow/models'?: { ok: boolean; body: unknown }
  '/api/denoise/models'?:      { ok: boolean; body: unknown }
}

function stubFetch(map: FetchStubMap) {
  vi.stubGlobal('fetch', async (url: RequestInfo | URL) => {
    const key = String(url) as keyof FetchStubMap
    const entry = map[key]
    if (!entry) throw new Error(`unexpected fetch: ${url}`)
    return { ok: entry.ok, json: async () => entry.body } as Response
  })
}

async function flush() {
  // Two microtask ticks: one for `watchEffect` to fire the fetch promise, one for `finally` to
  // update the refs. Small and deterministic.
  await Promise.resolve()
  await Promise.resolve()
  await Promise.resolve()
}

describe('useVaultModel', () => {
  beforeEach(() => {
    _resetVaultModelCache()
    vi.unstubAllGlobals()
  })

  it('resolves a name found in the flow vault to kind=flow + the manifest', async () => {
    stubFetch({
      '/api/optical-flow/models': { ok: true, body: {
        models: [{ name: 'flowA.pt', stem: 'flowA', hasManifest: true,
                   manifest: { lossCurves: { total: [0.9, 0.6] } } }],
      }},
      '/api/denoise/models': { ok: true, body: { models: [] } },
    })
    const scope = effectScope()
    let out!: ReturnType<typeof useVaultModel>
    scope.run(() => { out = useVaultModel(ref('flowA.pt')) })
    await flush()
    expect(out.kind.value).toBe('flow')
    expect((out.manifest.value as { lossCurves?: unknown })?.lossCurves).toBeTruthy()
    scope.stop()
  })

  it('resolves a name found in the denoise vault to kind=denoise + the manifest', async () => {
    stubFetch({
      '/api/optical-flow/models': { ok: true, body: { models: [] } },
      '/api/denoise/models': { ok: true, body: {
        models: [{ name: 'sup.pt', stem: 'sup', hasManifest: true,
                   manifest: { kind: 'denoise-support', training: { epochLosses: [0.6, 0.5] } } }],
      }},
    })
    const scope = effectScope()
    let out!: ReturnType<typeof useVaultModel>
    scope.run(() => { out = useVaultModel(ref('sup')) })
    await flush()
    expect(out.kind.value).toBe('denoise')
    expect((out.manifest.value as { training?: { epochLosses?: number[] } })?.training?.epochLosses)
      .toEqual([0.6, 0.5])
    scope.stop()
  })

  it('kind is null for an empty name — no fetch, no loading spinner', async () => {
    stubFetch({
      '/api/optical-flow/models': { ok: true, body: { models: [] } },
      '/api/denoise/models': { ok: true, body: { models: [] } },
    })
    const scope = effectScope()
    let out!: ReturnType<typeof useVaultModel>
    scope.run(() => { out = useVaultModel(ref('')) })
    await flush()
    expect(out.kind.value).toBeNull()
    expect(out.manifest.value).toBeNull()
    expect(out.loading.value).toBe(false)
    scope.stop()
  })

  it('kind is null when the name is in neither vault (renamed/deleted)', async () => {
    stubFetch({
      '/api/optical-flow/models': { ok: true, body: {
        models: [{ name: 'flowA.pt', stem: 'flowA', hasManifest: true, manifest: {} }],
      }},
      '/api/denoise/models': { ok: true, body: {
        models: [{ name: 'sup.pt', stem: 'sup', hasManifest: true,
                   manifest: { kind: 'denoise-support' } }],
      }},
    })
    const scope = effectScope()
    let out!: ReturnType<typeof useVaultModel>
    scope.run(() => { out = useVaultModel(ref('ghost')) })
    await flush()
    expect(out.kind.value).toBeNull()
    expect(out.manifest.value).toBeNull()
    scope.stop()
  })

  it('caches across composable instances — one fetch per kind for the whole page', async () => {
    let flowCalls = 0
    let denoiseCalls = 0
    vi.stubGlobal('fetch', async (url: RequestInfo | URL) => {
      const key = String(url)
      if (key === '/api/optical-flow/models') { flowCalls++
        return { ok: true, json: async () => ({ models: [] }) } as Response }
      if (key === '/api/denoise/models') { denoiseCalls++
        return { ok: true, json: async () => ({ models: [] }) } as Response }
      throw new Error(`unexpected fetch: ${url}`)
    })
    // Two composable instances mounted concurrently should each end up with the cached listings
    // and only one round-trip per kind should hit the network.
    const scope = effectScope()
    scope.run(() => {
      useVaultModel(ref('a'))
      useVaultModel(ref('b'))
      useVaultModel(ref('c'))
    })
    await flush()
    expect(flowCalls).toBe(1)
    expect(denoiseCalls).toBe(1)
    scope.stop()
  })

  it('refresh() re-fetches both vaults', async () => {
    let calls = 0
    vi.stubGlobal('fetch', async (url: RequestInfo | URL) => {
      calls++
      const key = String(url)
      const models = key.includes('optical-flow')
        ? [{ name: `flow${calls}.pt`, stem: `flow${calls}`, hasManifest: true, manifest: {} }]
        : []
      return { ok: true, json: async () => ({ models }) } as Response
    })
    const scope = effectScope()
    let out!: ReturnType<typeof useVaultModel>
    scope.run(() => { out = useVaultModel(ref('flow1.pt')) })
    await flush()
    expect(out.kind.value).toBe('flow')
    await out.refresh()
    // First name is now stale — the fresh flow-model list has flow3.pt, not flow1.pt.
    expect(out.kind.value).toBeNull()
    scope.stop()
  })

  it('surfaces a network error without crashing (kind stays null)', async () => {
    stubFetch({
      '/api/optical-flow/models': { ok: false, body: {} },
      '/api/denoise/models':      { ok: true, body: { models: [] } },
    })
    const scope = effectScope()
    let out!: ReturnType<typeof useVaultModel>
    scope.run(() => { out = useVaultModel(ref('anything')) })
    // waitFor polls so a slow-arriving `.finally` (Promise.all → error propagate) doesn't race
    // the assertion. Without this the test flaked on 3-tick manual flushes.
    await vi.waitFor(() => expect(out.error.value).toBeTruthy())
    expect(out.kind.value).toBeNull()
    scope.stop()
  })
})
