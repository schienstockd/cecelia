import { describe, it, expect } from 'vitest'
import { threadReadout, threadTip, clampWorkers, cpuPhrase, type ThreadBudget } from './threadBudget'

const AUTO: ThreadBudget = { workers: 8, default: 8, max: 64, derived: true, cores: 32 }
const SET: ThreadBudget  = { workers: 4, default: 8, max: 64, derived: false, cores: 32 }

describe('threadReadout', () => {
  it('marks a derived value as auto, so 8 does not read as a choice someone made', () => {
    expect(threadReadout(AUTO)).toBe('auto · 8')
  })

  it('shows a configured value bare', () => {
    expect(threadReadout(SET)).toBe('4')
  })

  it('has a placeholder before the budget loads', () => {
    expect(threadReadout(null)).toBe('—')
  })
})

describe('threadTip', () => {
  it('says what auto resolved to and on what', () => {
    expect(threadTip(AUTO)).toContain('derived from 32 CPUs')
  })

  it('says what auto WOULD be, so lowering it is an informed choice', () => {
    expect(threadTip(SET)).toContain('auto would be 8')
  })

  it('always says the change lands on the next task, not the running one', () => {
    for (const b of [AUTO, SET]) expect(threadTip(b)).toContain('next task')
  })

  it('falls back to "this machine" when the core count is absent', () => {
    expect(threadTip({ ...AUTO, cores: undefined })).toContain('this machine')
  })
})

describe('cpuPhrase', () => {
  it('names one number on a workstation, where the two agree', () => {
    expect(cpuPhrase({ ...AUTO, cores: 32, machineCores: 32 })).toBe('32 CPUs')
  })

  it('names BOTH when the process is confined — the gap is the point on a cluster node', () => {
    // a 4-core PBS allocation on a 128-core node: a budget sized from 128 forks workers for CPUs
    // this process may not touch
    expect(cpuPhrase({ ...AUTO, cores: 4, machineCores: 128 })).toBe('4 of 128 CPUs usable here')
  })

  it('says nothing specific when the count is unknown, rather than guessing', () => {
    expect(cpuPhrase({ ...AUTO, cores: undefined })).toBe('this machine')
  })
})

describe('clampWorkers', () => {
  it('never goes below one — zero means "auto" and is a different control', () => {
    expect(clampWorkers(0, 64)).toBe(1)
    expect(clampWorkers(-5, 64)).toBe(1)
  })

  it('holds the ceiling the backend would apply', () => {
    expect(clampWorkers(999, 64)).toBe(64)
  })

  it('rounds, because a range input can hand back a float', () => {
    expect(clampWorkers(7.6, 64)).toBe(8)
  })
})
