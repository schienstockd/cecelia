// One dispatch table drives which vault the manager talks to. The rename PR (Phase C of
// DENOISE_INTEGRATION_PLAN.md) turned the flow-only vault into a two-kind one; a table with the
// wrong endpoint would silently rename the wrong model, so it is worth pinning.
import { describe, it, expect } from 'vitest'
import { endpointsFor, VAULT_KIND_OPTIONS, type VaultKind } from './modelVaultKinds'

describe('modelVaultKinds', () => {
  it('routes flow to the optical-flow api', () => {
    const e = endpointsFor('flowModels')
    expect(e.list).toBe('/api/optical-flow/models')
    expect(e.rename).toBe('/api/optical-flow/rename')
    expect(e.delete).toBe('/api/optical-flow/delete')
  })

  it('routes denoise to the denoise api', () => {
    const e = endpointsFor('denoiseModels')
    expect(e.list).toBe('/api/denoise/models')
    expect(e.rename).toBe('/api/denoise/rename')
    expect(e.delete).toBe('/api/denoise/delete')
  })

  it('chip options cover every kind — a new kind that forgets to add itself here is invisible', () => {
    const kinds = VAULT_KIND_OPTIONS.map(o => o.value as VaultKind).sort()
    expect(kinds).toEqual(['denoiseModels', 'flowModels'])
  })

  it('every chip option carries a tooltip (docs/ui/COPY.md — hover help is not optional)', () => {
    for (const o of VAULT_KIND_OPTIONS) {
      expect(o.tip, `${o.value} needs a tip`).toBeTruthy()
    }
  })
})
