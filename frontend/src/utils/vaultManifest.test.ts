import { describe, it, expect } from 'vitest'
import { RESOLVERS, vaultManifestParamsWith, type VaultResolver }
  from './vaultManifest'
import type { ParamDef, ParamValues } from '../tasks/types'

// The convention the file exists to enforce: dispatch by `optionsFrom` keeps the caller vault-blind,
// and unregistered pickers are safe (they return `null`, not throw, so the primary lookup's result
// stays authoritative).
describe('vaultManifestParamsWith — dispatch by optionsFrom', () => {
  const cellposeCalls: string[] = []
  const cellpose: VaultResolver = async name => {
    cellposeCalls.push(name)
    return name === 'nuc-mtorch' ? { epochs: 300 } as ParamValues : null
  }
  const flow: VaultResolver = async () => ({ epochs: 30 } as ParamValues)
  const RS = { cellposeModels: cellpose, flowModels: flow }

  it('routes to the resolver named by optionsFrom', async () => {
    const p = { key: 'model', type: 'valueNameInput', optionsFrom: 'cellposeModels' } as ParamDef
    expect(await vaultManifestParamsWith(RS, p, [], 'nuc-mtorch')).toEqual({ epochs: 300 })
    expect(cellposeCalls).toEqual(['nuc-mtorch'])
  })

  it('is null for a picker with NO optionsFrom (an ordinary text input)', async () => {
    const p = { key: 'model', type: 'valueNameInput' } as ParamDef
    expect(await vaultManifestParamsWith(RS, p, [], 'anything')).toBeNull()
  })

  it('is null for a vault this map does not know about', async () => {
    const p = { key: 'model', type: 'valueNameInput', optionsFrom: 'coastalModels' } as ParamDef
    expect(await vaultManifestParamsWith(RS, p, [], 'anything')).toBeNull()
  })

  it('is null when the resolver itself has no answer — a name that is not in that vault', async () => {
    const p = { key: 'model', type: 'valueNameInput', optionsFrom: 'cellposeModels' } as ParamDef
    expect(await vaultManifestParamsWith(RS, p, [], 'not-there')).toBeNull()
  })

  it('is null for an undefined param (defensive: findParamByKey may miss)', async () => {
    expect(await vaultManifestParamsWith(RS, undefined, [], 'anything')).toBeNull()
  })
})

// The shipped registry — a smoke check that the one vault we support today is wired. When custom
// cellpose training lands, add its key here.
describe('RESOLVERS (shipped)', () => {
  it('has an entry for every vault we ship a manifest for', () => {
    expect(Object.keys(RESOLVERS).sort()).toEqual(['flowModels'])
  })
})
