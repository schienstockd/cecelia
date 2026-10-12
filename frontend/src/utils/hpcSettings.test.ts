import { describe, it, expect } from 'vitest'
import { hpcTestBlocker, hpcTestLines, keyPickerStart, type HpcSettings, type HpcTestResult } from './hpcSettings'

const cls = { queue: '', account: '', qos: '', directives: '', preamble: '' }
const settings = (over: Partial<HpcSettings> = {}): HpcSettings => ({
  available: true,
  schedulers: [{ name: 'slurm', label: 'SLURM', available: true }],
  site: { scheduler: 'slurm', host: 'login.x', port: 22, remoteDir: '~/cecelia', email: '', cpu: cls, gpu: cls },
  identity: { profile: 'default', user: 'alice', keyFile: '/home/alice/.ssh/id_ed25519' },
  project: null,
  ...over,
})
const result = (over: Partial<HpcTestResult> = {}): HpcTestResult => ({
  ok: true, reachable: true, fingerprint: 'ED25519 SHA256:abc', newHostKey: false,
  remoteDir: '/home/alice/cecelia', dirOk: true, scheduler: 'slurm 23.11.4', error: '', ...over,
})

describe('hpcTestBlocker', () => {
  it('is empty when everything the test needs is set', () => {
    expect(hpcTestBlocker(settings())).toBe('')
  })
  it('names the first missing piece, in the order the backend checks', () => {
    expect(hpcTestBlocker(null)).toBe('Loading')
    expect(hpcTestBlocker(settings({ available: false }))).toBe('Not available on Windows yet')
    expect(hpcTestBlocker(settings({ site: { ...settings().site, host: ' ' } }))).toBe('Set the host first')
    expect(hpcTestBlocker(settings({ identity: { profile: 'p', user: '', keyFile: 'k' } }))).toBe('Set your username first')
    expect(hpcTestBlocker(settings({ identity: { profile: 'p', user: 'u', keyFile: '' } }))).toBe('Select your SSH key first')
  })
})

describe('hpcTestLines', () => {
  it('a passing test: key, folder, scheduler', () => {
    expect(hpcTestLines(result({ newHostKey: true }))).toEqual([
      { ok: true, text: 'Host key ED25519 SHA256:abc (new, now trusted)' },
      { ok: true, text: 'Folder /home/alice/cecelia' },
      { ok: true, text: 'SLURM: slurm 23.11.4' },
    ])
  })
  it('unreachable: the fingerprint if the handshake got that far, then the error', () => {
    expect(hpcTestLines(result({ ok: false, reachable: false, error: 'Key rejected' }))).toEqual([
      { ok: true, text: 'Host key ED25519 SHA256:abc' },
      { ok: false, text: 'Key rejected' },
    ])
    expect(hpcTestLines(result({ ok: false, reachable: false, fingerprint: '', error: '' })))
      .toEqual([{ ok: false, text: 'Not reachable' }])
  })
  it('stops at the folder when it is not writable', () => {
    const l = hpcTestLines(result({ ok: false, dirOk: false, error: 'Remote folder not writable' }))
    expect(l.at(-1)).toEqual({ ok: false, text: 'Remote folder not writable' })
    expect(l).toHaveLength(2)
  })
  it('a missing scheduler is the failing line', () => {
    const l = hpcTestLines(result({ ok: false, scheduler: '', error: 'SLURM not found on the login node' }))
    expect(l.at(-1)).toEqual({ ok: false, text: 'SLURM not found on the login node' })
  })
})

describe('keyPickerStart', () => {
  it('opens ~/.ssh when no key is set', () => {
    expect(keyPickerStart('')).toBe('.ssh')
    expect(keyPickerStart('id_rsa')).toBe('.ssh')
  })
  it("opens the current key's folder", () => {
    expect(keyPickerStart('/home/a/.ssh/id_ed25519')).toBe('/home/a/.ssh')
    expect(keyPickerStart('~/keys/cluster')).toBe('keys')
    expect(keyPickerStart('C:\\Users\\a\\.ssh\\id')).toBe('C:\\Users\\a\\.ssh')
  })
})
