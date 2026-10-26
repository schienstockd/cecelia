import { describe, it, expect } from 'vitest'
import { hpcTestBlocker, hpcTestLines, keyPickerStart, hpcFieldPatch, hpcFieldValue, hpcEnvLine, HPC_DRAFT_FIELDS, type HpcSettings, type HpcTestResult } from './hpcSettings'

const cls = { queue: '', account: '', qos: '', directives: '', preamble: '' }
const settings = (over: Partial<HpcSettings> = {}): HpcSettings => ({
  available: true,
  schedulers: [{ name: 'slurm', label: 'SLURM', available: true }],
  site: { scheduler: 'slurm', host: 'login.x', port: 22, netQueue: '', remoteDir: '~/cecelia', email: '', cpu: cls, gpu: cls },
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
    expect(hpcTestBlocker(settings({ site: { ...settings().site, host: ' ' } }))).toBe('Set the cluster host first')
    expect(hpcTestBlocker(settings({ identity: { profile: 'p', user: '', keyFile: 'k' } }))).toBe('Set your username in Preferences → HPC first')
    expect(hpcTestBlocker(settings({ identity: { profile: 'p', user: 'u', keyFile: '' } }))).toBe('Select your SSH key in Preferences → HPC first')
    expect(hpcTestBlocker(settings({ site: { ...settings().site, remoteDir: '' } }))).toBe('Set the remote folder in Preferences → HPC first')
  })
  it('names the other surface only for a field that lives there', () => {
    expect(hpcTestBlocker(settings({ site: { ...settings().site, host: '' } }), 'user')).toBe('Set the cluster host in Settings → HPC first')
    expect(hpcTestBlocker(settings({ identity: { profile: 'p', user: '', keyFile: 'k' } }), 'user')).toBe('Set your username first')
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

describe('hpcFieldPatch / hpcFieldValue', () => {
  it('sends one field in the scope it is stored in', () => {
    expect(hpcFieldPatch('site.host', 'h', 'p')).toEqual({ site: { host: 'h' } })
    expect(hpcFieldPatch('site.port', '2222', 'p')).toEqual({ site: { port: 2222 } })
    expect(hpcFieldPatch('site.port', ' 2222 ', 'p')).toEqual({ site: { port: 2222 } })
    expect(hpcFieldPatch('site.port', 'abc', 'p')).toEqual({ site: { port: 0 } })   // refused by the range check
    expect(hpcFieldPatch('site.netQueue', 'copyq', 'p')).toEqual({ site: { netQueue: 'copyq' } })
    expect(hpcFieldPatch('site.gpu.qos', 'hi', 'p')).toEqual({ site: { gpu: { qos: 'hi' } } })
    expect(hpcFieldPatch('identity.keyFile', '/k', 'p')).toEqual({ identity: { keyFile: '/k' } })
    expect(hpcFieldPatch('project.account', 'g1', 'p')).toEqual({ project: { projectUid: 'p', account: 'g1' } })
  })
  it('reads every draft path, empty when absent', () => {
    const s = settings({ project: null })
    for (const p of HPC_DRAFT_FIELDS) expect(typeof hpcFieldValue(s, p)).toBe('string')
    expect(hpcFieldValue(s, 'site.port')).toBe('22')
    expect(hpcFieldValue(s, 'project.account')).toBe('')
    expect(hpcFieldValue(null, 'site.host')).toBe('')
  })
})

describe('hpcEnvLine', () => {
  const base = { configured: true, lock: 'abc123', installed: false, job: '', installing: false, error: '' }
  it('says what the cluster has, and offers Install only when it helps', () => {
    expect(hpcEnvLine(null)).toEqual({ text: 'Checking the cluster…', canInstall: false })
    expect(hpcEnvLine({ ...base, installed: true })).toEqual({ severity: 'ok', text: 'Environment ready (abc123)', canInstall: false })
    expect(hpcEnvLine(base)).toEqual({ severity: 'fail', text: 'Not installed for this version', canInstall: true })
    expect(hpcEnvLine({ ...base, installing: true }).canInstall).toBe(false)
    // A job from an earlier session: Install attaches to it rather than starting another.
    expect(hpcEnvLine({ ...base, job: '42' })).toEqual({ text: 'Installing on the cluster (job 42)', canInstall: true })
    expect(hpcEnvLine({ ...base, configured: false, error: 'Set the HPC host in Settings → HPC' }).canInstall).toBe(false)
    expect(hpcEnvLine({ ...base, error: 'Remote folder: Host not found' })).toEqual(
      { severity: 'fail', text: 'Remote folder: Host not found', canInstall: true })
  })
  it('says where a running install is, from its row first and the GET second', () => {
    const inst = { ...base, installing: true }
    expect(hpcEnvLine(inst).text).toBe('Installing — follow it in the Task Manager')
    expect(hpcEnvLine({ ...inst, stage: 'sync' }).text).toBe('Uploading the environment files')
    expect(hpcEnvLine({ ...inst, stage: 'queued', job: '7.pbs' }).text).toBe('Queued on the cluster (job 7.pbs)')
    // the row's stage moves without a re-fetch, so it wins over what the page loaded with
    expect(hpcEnvLine({ ...inst, stage: 'queued', job: '7.pbs' }, { stage: 'running', job: '7.pbs' }).text)
      .toBe('Installing on the cluster (job 7.pbs)')
  })
})
