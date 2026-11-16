import { describe, it, expect } from 'vitest'
import { plotRequestFor } from './plotRequest'

const req = { type: 'plot:request', projectUid: 'P', plotId: 'cluster:3', requestId: 'plot-1', args: { maxSide: 800 } }

describe('plotRequestFor', () => {
  it('answers the request addressed to this panel', () => {
    expect(plotRequestFor(req, 'cluster:3', 'P')).toEqual({ requestId: 'plot-1', kind: 'image', maxSide: 800 })
  })
  it('ignores another panel or another project', () => {
    expect(plotRequestFor(req, 'cluster:4', 'P')).toBeNull()
    expect(plotRequestFor(req, 'cluster:3', 'Q')).toBeNull()
  })
  it('ignores a request with no id, and a panel with no key', () => {
    expect(plotRequestFor({ ...req, requestId: '' }, 'cluster:3', 'P')).toBeNull()
    expect(plotRequestFor(req, '', 'P')).toBeNull()
  })
  it('drops a maxSide that is not a positive number', () => {
    expect(plotRequestFor({ ...req, args: { maxSide: 'big' } }, 'cluster:3', 'P')).toEqual({ requestId: 'plot-1', kind: 'image' })
    expect(plotRequestFor({ ...req, args: undefined }, 'cluster:3', 'P')).toEqual({ requestId: 'plot-1', kind: 'image' })
  })
  it('a view request carries its settings', () => {
    expect(plotRequestFor({ ...req, kind: 'view', args: { settings: { colourBy: 'population' } } }, 'cluster:3', 'P'))
      .toEqual({ requestId: 'plot-1', kind: 'view', settings: { colourBy: 'population' } })
  })
})
