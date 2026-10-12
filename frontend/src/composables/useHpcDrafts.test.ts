import { describe, it, expect } from 'vitest'
import { ref, nextTick } from 'vue'
import { useHpcDrafts } from './useHpcDrafts'
import type { HpcSettings } from '../utils/hpcSettings'

const cls = () => ({ queue: '', account: '', qos: '', directives: '', preamble: '' })
const reply = (over: Partial<HpcSettings['site']> = {}): HpcSettings => ({
  available: true,
  schedulers: [{ name: 'slurm', label: 'SLURM', available: true }],
  site: { scheduler: 'slurm', host: 'login.x', port: 22, remoteDir: '~/cecelia', email: '',
          cpu: cls(), gpu: cls(), ...over },
  identity: { profile: 'default', user: 'alice', keyFile: '/k' },
  project: { projectUid: 'p', account: '' },
})

describe('useHpcDrafts', () => {
  it('seeds every field from the stored settings, nested classes included', () => {
    const src = ref<HpcSettings | null>(reply({ port: 2222 }))
    const d = useHpcDrafts(() => src.value)
    expect(d['site.host'].value).toBe('login.x')
    expect(d['site.port'].value).toBe('2222')
    expect(d['site.gpu.queue'].value).toBe('')
    expect(d['identity.user'].value).toBe('alice')
    expect(d['project.account'].value).toBe('')
  })

  it("a save of one field returning does not wipe another field's unsaved text", async () => {
    // The bug: the reply to saving `email` re-seeded the whole form, so the host the user was
    // still typing snapped back to the stored value.
    const src = ref<HpcSettings | null>(reply())
    const d = useHpcDrafts(() => src.value)
    d['site.host'].value = 'login.new-clus'           // typing, not committed yet
    d['site.cpu.preamble'].value = 'module load'      // and a nested one
    src.value = reply({ email: 'a@b.org' })           // the email save comes back
    await nextTick()
    expect(d['site.host'].value).toBe('login.new-clus')
    expect(d['site.cpu.preamble'].value).toBe('module load')
    expect(d['site.email'].value).toBe('a@b.org')     // the field that DID change takes the reply
  })

  it('a field whose stored value changes takes the new value', async () => {
    const src = ref<HpcSettings | null>(reply())
    const d = useHpcDrafts(() => src.value)
    src.value = reply({ host: 'login.normalised' })
    await nextTick()
    expect(d['site.host'].value).toBe('login.normalised')
  })

  it('before the first load every field is empty, then seeds', async () => {
    const src = ref<HpcSettings | null>(null)
    const d = useHpcDrafts(() => src.value)
    expect(d['site.host'].value).toBe('')
    src.value = reply()
    await nextTick()
    expect(d['site.host'].value).toBe('login.x')
  })
})
