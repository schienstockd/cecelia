import { describe, it, expect } from 'vitest'
import {
  vaultInScope, vaultMatchesQuery, vaultUserLabel, vaultProjectLabel, vaultOriginGroup,
  vaultPickerLayout, THIS_PROJECT, ORIGIN_UNKNOWN, type VaultOrigin,
} from './vaultScope'

const base = { profile: 'alice', projectUid: 'P1', otherUsers: false, otherProjects: false }

describe('vaultInScope', () => {
  it('default: only my models from the open project', () => {
    expect(vaultInScope({ createdBy: 'alice', projectUid: 'P1' }, base)).toBe(true)
    expect(vaultInScope({ createdBy: 'bob',   projectUid: 'P1' }, base)).toBe(false)
    expect(vaultInScope({ createdBy: 'alice', projectUid: 'P2' }, base)).toBe(false)
  })
  it('each toggle widens only its own axis', () => {
    const users = { ...base, otherUsers: true }
    expect(vaultInScope({ createdBy: 'bob', projectUid: 'P1' }, users)).toBe(true)
    expect(vaultInScope({ createdBy: 'bob', projectUid: 'P2' }, users)).toBe(false)
    const projects = { ...base, otherProjects: true }
    expect(vaultInScope({ createdBy: 'alice', projectUid: 'P2' }, projects)).toBe(true)
    expect(vaultInScope({ createdBy: 'bob',   projectUid: 'P2' }, projects)).toBe(false)
  })
  // the exception that keeps every model trained before the stamp visible
  it('an unrecorded creator or project is never out of scope', () => {
    expect(vaultInScope({}, base)).toBe(true)
    expect(vaultInScope({ createdBy: '', projectUid: 'P1' }, base)).toBe(true)
    expect(vaultInScope({ createdBy: 'alice', projectUid: '' }, base)).toBe(true)
  })
  it('no project open: the project axis does not filter', () => {
    expect(vaultInScope({ createdBy: 'alice', projectUid: 'P2' }, { ...base, projectUid: undefined }))
      .toBe(true)
  })
})

describe('vaultMatchesQuery', () => {
  const m = { stem: 'flow.cyto', label: 'flow.cyto (TdTom)', createdBy: 'bob',
              projectName: 'MerTK', projectUid: 'zolIMa' }
  it('empty query matches everything', () => expect(vaultMatchesQuery(m, '  ')).toBe(true))
  it.each(['CYTO', 'tdtom', 'bob', 'mertk', 'zolima'])('matches %s, case-insensitively', q =>
    expect(vaultMatchesQuery(m, q)).toBe(true))
  it('misses', () => expect(vaultMatchesQuery(m, 'denoise')).toBe(false))
  it('finds the default profile by its display name', () => {
    const d = { stem: 'x', createdBy: 'default' }
    expect(vaultMatchesQuery(d, vaultUserLabel(d).slice(0, 3))).toBe(true)
  })
})

describe('labels', () => {
  it('user: blank when unrecorded, never blank for the default profile', () => {
    expect(vaultUserLabel({})).toBe('')
    expect(vaultUserLabel({ createdBy: 'bob' })).toBe('bob')
    expect(vaultUserLabel({ createdBy: 'default' })).not.toBe('')
  })
  it('project: name, else uid (a deleted project), else blank', () => {
    expect(vaultProjectLabel({ projectUid: 'P1', projectName: 'Mine' })).toBe('Mine')
    expect(vaultProjectLabel({ projectUid: 'P1' })).toBe('P1')
    expect(vaultProjectLabel({})).toBe('')
  })
  it('origin group says when the project was recovered, and when nothing was recorded', () => {
    const g = vaultOriginGroup({ projectUid: 'P1', projectName: 'Mine', projectInferred: true })
    expect(g.fields).toEqual([
      { label: 'Trained by', value: 'not recorded' },
      { label: 'Project', value: 'Mine (from its source images)' },
    ])
    expect(vaultOriginGroup({ createdBy: 'bob', createdVia: 'claude' }).fields[0].value)
      .toBe('Claude for bob')
  })
})

describe('vaultPickerLayout', () => {
  const rows: Record<string, VaultOrigin> = {
    'mine.pt':   { createdBy: 'alice', projectUid: 'P1', projectName: 'Tcells' },
    'bobs.pt':   { createdBy: 'bob',   projectUid: 'P1', projectName: 'Tcells' },
    'other.pt':  { createdBy: 'alice', projectUid: 'P2', projectName: 'Bcells' },
    'old.pt':    {},
  }
  const opts = ['', 'mine.pt', 'bobs.pt', 'other.pt', 'old.pt', 'chain.pt']
    .map(v => ({ value: v, label: v || 'None' }))
  const lay = (s = base, sel = '') => vaultPickerLayout(opts, v => rows[v], s, sel)
  const titles = (l: ReturnType<typeof lay>) => l.groups.map(g => g.title)

  it('default: my models here, unrecorded last; no-row options loose and first', () => {
    const l = lay()
    expect(l.loose.map(o => o.value)).toEqual(['', 'chain.pt'])
    expect(titles(l)).toEqual([THIS_PROJECT, ORIGIN_UNKNOWN])
    expect(l.groups[0].options.map(o => o.value)).toEqual(['mine.pt'])
    expect(l.hidden).toBe(2)
  })
  it('other projects get their own heading, between this project and unrecorded', () => {
    expect(titles(lay({ ...base, otherProjects: true }))).toEqual([THIS_PROJECT, 'Bcells', ORIGIN_UNKNOWN])
  })
  it("another user's model is labelled with who trained it", () => {
    const l = lay({ ...base, otherUsers: true })
    expect(l.groups[0].options.map(o => o.label)).toEqual(['mine.pt', expect.stringMatching(/^bobs\.pt — /)])
  })
  it('the selected model stays listed even when out of scope', () => {
    const l = lay(base, 'other.pt')
    expect(titles(l)).toContain('Bcells')
    expect(l.hidden).toBe(1)
  })
})
