import { describe, it, expect } from 'vitest'
import { resolveSetDestination, destinationParams } from './setDestination'

const SETS = [{ uid: 'aaa', name: 'Day 1' }, { uid: 'bbb', name: 'Day 2' }]

describe('resolveSetDestination', () => {
  it('takes the picked set when there is one', () => {
    expect(resolveSetDestination(SETS, 'bbb', 'ignored')).toEqual({ ok: true, toSetUid: 'bbb' })
  })

  it('creates from the trimmed name when no set is picked', () => {
    expect(resolveSetDestination(SETS, '', '  Day 3 ')).toEqual({ ok: true, newSetName: 'Day 3' })
  })

  it('rejects nothing picked and nothing typed', () => {
    expect(resolveSetDestination(SETS, '', '   ')).toEqual(
      { ok: false, error: 'Select a set or enter a new set name.' })
  })

  it('rejects a name that already exists — the backend would silently reuse that set', () => {
    expect(resolveSetDestination(SETS, '', 'Day 2')).toEqual(
      { ok: false, error: 'A set named "Day 2" already exists.' })
  })
})

describe('destinationParams', () => {
  it('emits the field the route expects for each mode', () => {
    expect(destinationParams(resolveSetDestination(SETS, 'aaa', ''))).toEqual({ toSetUid: 'aaa' })
    expect(destinationParams(resolveSetDestination(SETS, '', 'New'))).toEqual({ newSetName: 'New' })
  })

  it('emits nothing for an invalid destination', () => {
    expect(destinationParams(resolveSetDestination(SETS, '', ''))).toEqual({})
  })
})
