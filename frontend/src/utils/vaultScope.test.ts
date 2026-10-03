import { describe, it, expect } from 'vitest'
import {
  vaultInScope, vaultMatchesQuery, vaultUserLabel, vaultProjectLabel, vaultOriginGroup,
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
