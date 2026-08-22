import { describe, it, expect } from 'vitest'
import { openProjectFromStorageEvent } from './openProjectChannel'

const ev = (key: string | null, newValue: string | null) => ({ key, newValue })

describe('openProjectFromStorageEvent', () => {
  it('returns the uid to open when another window switched project', () => {
    expect(openProjectFromStorageEvent(ev('cc.openProject', 'B'), 'A')).toBe('B')
  })

  it('returns "" when the other window closed its project', () => {
    expect(openProjectFromStorageEvent(ev('cc.openProject', ''), 'A')).toBe('')
    expect(openProjectFromStorageEvent(ev('cc.openProject', null), 'A')).toBe('')
  })

  it('ignores the bounce — the follower publishes in turn, and we already show that project', () => {
    expect(openProjectFromStorageEvent(ev('cc.openProject', 'A'), 'A')).toBeNull()
  })

  it('ignores an event about any other key', () => {
    // the settings store writes a dozen of these on ordinary use
    expect(openProjectFromStorageEvent(ev('cc.tasksShowHistory', 'false'), 'A')).toBeNull()
  })

  it('ignores localStorage.clear() — key null is not "someone closed a project"', () => {
    expect(openProjectFromStorageEvent(ev(null, null), 'A')).toBeNull()
  })

  it('opens from nothing when this window has no project yet', () => {
    expect(openProjectFromStorageEvent(ev('cc.openProject', 'B'), null)).toBe('B')
    expect(openProjectFromStorageEvent(ev('cc.openProject', ''), null)).toBeNull()
  })
})
