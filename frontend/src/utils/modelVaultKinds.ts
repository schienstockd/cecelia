// One kind selector, one endpoint map. `ModelVault.vue` dispatches list / rename / delete against
// this table so a new vault (custom cellpose training, next) is one entry and no branching in the
// component. `optionsFrom` in a task spec uses the same string — that keeps the vault picker and
// the vault manager pointed at the same server route.

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

// The chip row users see. Order matches "what came first": optical-flow shipped in v0, denoise in
// Phase A/B of DENOISE_INTEGRATION_PLAN.
export const VAULT_KIND_OPTIONS: ChipOption[] = [
  { value: 'flowModels',    label: 'Optical flow', tip: 'Optical-flow segmentation models' },
  { value: 'denoiseModels', label: 'Denoise',      tip: 'SUPPORT temporal denoise models' },
]
