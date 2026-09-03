// Global vault → picker fallback. One place that knows which vault a `valueNameInput` points at and
// where its manifest lives, dispatched by the same `optionsFrom` key the server uses to build the
// picker (`_OPTION_SOURCES` in app/src/tasks/task.jl).
//
// The point of this file: `/api/tasks/funparams` is per-project, but a global vault is not — a model
// trained under one set applies to any set, and typing that name into the picker should restore its
// settings the same way `FlowModelVault`'s *Apply settings* does. Without this the two paths
// disagreed on the same input, and the vault UI was the only one that worked.
//
// **Adding a vault** (e.g. custom-cellpose training coming next): implement a `VaultResolver` — an
// async function that maps `(name, defParams)` to the form values — and add one row to
// `RESOLVERS`. That is the whole extension point. Registration is import-time so a resolver cannot
// exist without an entry here.

import type { ParamDef, ParamValues } from '../tasks/types'
import { flowManifestParams, type FlowModelEntry } from './flowModelParams'
import type { FlowManifest } from './flowManifest'

/**
 * Maps a vault name into the target task's form values, or `null` when the vault has no answer.
 * `defParams` is the CURRENT spec — resolvers may need it to reconstruct a chip selection against
 * options that have grown since the manifest was written (see `flowManifestParams`).
 *
 * `null` on any failure — network, missing manifest, orphan entry — because this is the SECONDARY
 * lookup: leaving the form alone is always safer than surfacing a vault error over it.
 */
export type VaultResolver = (name: string, defParams: ParamDef[] | undefined)
  => Promise<ParamValues | null>

// The `fetch` seam is exposed so tests can pass a canned response without touching global fetch;
// production always uses window.fetch.
type FetchLike = (input: RequestInfo | URL, init?: RequestInit) => Promise<Response>

async function fetchJson<T>(fetcher: FetchLike, url: string): Promise<T | null> {
  try {
    const res = await fetcher(url)
    if (!res.ok) return null
    return await res.json() as T
  } catch { return null }
}

// Flow-model vault. `FlowModelVault.vue` reads the same endpoint for its listing, and the details
// dialog reads the same `manifest` field — one source of truth for what a model was trained on.
const flowModels: VaultResolver = async (name, defParams) => {
  const d = await fetchJson<{ models?: FlowModelEntry[] }>(fetch, '/api/optical-flow/models')
  return flowManifestParams(d?.models, name, defParams)
}

/**
 * One resolver per `optionsFrom` value. A picker whose vault is not registered here answers `null`
 * from `vaultManifestParams`, which is the safe default: it looks exactly like "the vault has no
 * saved answer for this name", and the picker's per-project lookup still runs unchanged above it.
 */
export const RESOLVERS: Record<string, VaultResolver> = {
  flowModels,
}

/**
 * The manifest-derived form values for a globally-vaulted name, or `null`.
 *
 * Dispatches on `param.optionsFrom` — the same discriminator that names the picker's options — so
 * a new vault (custom cellpose training, next up) plugs in with one entry in `RESOLVERS` above and
 * one new resolver function. The caller does not need to know which vault it is talking to.
 */
export async function vaultManifestParams(
  param: ParamDef | undefined,
  defParams: ParamDef[] | undefined,
  name: string,
): Promise<ParamValues | null> {
  const src = typeof param?.optionsFrom === 'string' ? param.optionsFrom : ''
  const resolver = RESOLVERS[src]
  return resolver ? resolver(name, defParams) : null
}

// Kept for the test — a resolver map that a caller can inject, so the dispatch is verifiable
// without knowing which vaults the shipped one contains.
export async function vaultManifestParamsWith(
  resolvers: Record<string, VaultResolver>,
  param: ParamDef | undefined,
  defParams: ParamDef[] | undefined,
  name: string,
): Promise<ParamValues | null> {
  const src = typeof param?.optionsFrom === 'string' ? param.optionsFrom : ''
  const resolver = resolvers[src]
  return resolver ? resolver(name, defParams) : null
}

// re-exported so callers wiring a new vault do not have to hunt for it
export type { FlowManifest, FlowModelEntry }
