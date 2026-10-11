// One kind selector, one endpoint map. `ModelVault.vue` dispatches list / rename / delete against
// this table so a new vault (custom cellpose training, next) is one entry and no branching in the
// component. A task spec's `optionsFrom` names the same vaults — mapped by `vaultKindForSource`
// below, so the vault picker and the vault manager read the same server route.

import type { ChipOption } from '../components/ChipSelect.vue'

export type VaultKind = 'flowModels' | 'denoiseModels'

export interface VaultEndpoints {
  list: string     // GET  — {models: [...], dir: string}
  rename: string   // POST — {name, newName}
  delete: string   // POST — {name}
}

const ENDPOINTS: Record<VaultKind, VaultEndpoints> = {
  flowModels: {
    list:   '/api/optical-flow/models',
    rename: '/api/optical-flow/rename',
    delete: '/api/optical-flow/delete',
  },
  denoiseModels: {
    list:   '/api/denoise/models',
    rename: '/api/denoise/rename',
    delete: '/api/denoise/delete',
  },
}

export function endpointsFor(kind: VaultKind): VaultEndpoints {
  return ENDPOINTS[kind]
}

// The vault behind a task picker's `optionsFrom`. `coastalModels` is the flow vault under its
// segmentation name; `cellposeModels` has no vault listing (built-ins + dropped-in files), so no entry.
// Not `RESOLVERS` (utils/vaultManifest.ts): that maps a source to a manifest→form mapper and is keyed by
// `flowModels`, the train task's name suggestions; this maps a PICKER's source to the listing it reads.
const SOURCE_KIND: Record<string, VaultKind> = {
  coastalModels: 'flowModels',
  denoiseModels: 'denoiseModels',
}
export const vaultKindForSource = (src: string | undefined): VaultKind | undefined =>
  src ? SOURCE_KIND[src] : undefined

// The chip row users see. Order matches "what came first": optical-flow shipped in v0, denoise in
// Phase A/B of DENOISE_INTEGRATION_PLAN.
export const VAULT_KIND_OPTIONS: ChipOption[] = [
  { value: 'flowModels',    label: 'Optical flow', tip: 'Optical-flow segmentation models' },
  { value: 'denoiseModels', label: 'Denoise',      tip: 'SUPPORT temporal denoise models' },
]
