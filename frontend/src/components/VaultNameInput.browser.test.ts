// A training task's "Model name" box over the vault: its suggestions are scoped like the Model vault
// manager (your models, this project) and widen with the same View toggles; a hidden name still marks
// as existing, because the vault is global and training onto it collides. Real DOM, real popover.
import { describe, expect, it } from 'vitest'
import { h, ref } from 'vue'
import { userEvent } from 'vitest/browser'
import VaultNameInput from './VaultNameInput.vue'
import { frames, mountInto } from '../test/browser/mount'
import { useAppControlStore } from '../stores/appControl'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { _resetVaultModelCache } from '../composables/useVaultModel'
import { endpointsFor } from '../utils/modelVaultKinds'

// the vault listing (`useVaultRows`) — the one network read, answered here; anything else still
// reaches the setup's guard and fails the test
const ROWS = [
  { name: 'mine.pt',  stem: 'mine',  hasManifest: false, manifest: {}, createdBy: 'alice', projectUid: 'P1' },
  { name: 'bobs.pt',  stem: 'bobs',  hasManifest: false, manifest: {}, createdBy: 'bob',   projectUid: 'P1' },
  { name: 'other.pt', stem: 'other', hasManifest: false, manifest: {}, createdBy: 'alice', projectUid: 'P2' },
]
function serveVault() {
  const guard = window.fetch
  window.fetch = ((input: RequestInfo | URL, init?: RequestInit) =>
    String(input).startsWith(endpointsFor('flowModels').list)
      ? Promise.resolve(new Response(JSON.stringify({ models: ROWS }), { status: 200 }))
      : guard(input, init)) as typeof fetch
}

async function setup() {
  serveVault()
  _resetVaultModelCache()
  const value = ref('')
  const host = mountInto({
    render: () => h(VaultNameInput, {
      kind: 'flowModels', options: ['mine', 'bobs', 'other'],
      modelValue: value.value, 'onUpdate:modelValue': (v: string) => { value.value = v },
    }),
  })
  useAppControlStore().activeProfileName = 'alice'
  useProjectMetaStore().current = { uid: 'P1' } as NonNullable<ReturnType<typeof useProjectMetaStore>['current']>
  const settings = useSettingsStore()
  settings.vaultOtherUsers = false
  settings.vaultOtherProjects = false
  await frames(3)
  const input = host.querySelector<HTMLInputElement>('input')!
  const offered = async () => {
    input.blur(); await frames(); await userEvent.click(input); await frames()
    return [...document.querySelectorAll('.si-item')].map(b => b.textContent)
  }
  return { input, settings, offered }
}

describe('VaultNameInput', () => {
  it('suggests only my models from this project by default', async () => {
    const { offered } = await setup()
    expect(await offered()).toEqual(['mine'])
  })

  it('the View toggles widen the suggestions', async () => {
    const { offered, settings } = await setup()
    settings.vaultOtherUsers = true
    expect(await offered()).toEqual(['mine', 'bobs'])
    settings.vaultOtherProjects = true
    expect(await offered()).toEqual(['mine', 'bobs', 'other'])
  })

  it('a hidden name still marks as existing', async () => {
    const { input } = await setup()
    await userEvent.fill(input, 'bobs')
    await frames()
    expect(input.classList.contains('si-existing')).toBe(true)
  })
})
