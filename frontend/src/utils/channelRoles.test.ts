import { describe, expect, it } from 'vitest'
import { asRoleMap, channelsWithRole, setChannelRole } from './channelRoles'

describe('channelRoles', () => {
  it('one channel, one role — setting a role replaces the old one', () => {
    let m = setChannelRole({}, 'AF', 'af', ['SHG', 'CART', 'AF'])
    m = setChannelRole(m, 'AF', 'competing', ['SHG', 'CART', 'AF'])
    expect(m).toEqual({ AF: 'competing' })
  })

  it("'' clears a channel, and channels the image lacks are dropped", () => {
    const m = setChannelRole({ CART: 'correct', OLD: 'af' }, 'CART', '', ['CART', 'AF'])
    expect(m).toEqual({})
  })

  it('lists channels by role in image order', () => {
    const m = { AF: 'af', TOM: 'correct', GFP: 'correct' }
    expect(channelsWithRole(m, 'correct', ['GFP', 'TOM', 'AF'])).toEqual(['GFP', 'TOM'])
    expect(channelsWithRole(m, 'af')).toEqual(['AF'])
  })

  it('anything that is not a map reads as empty', () => {
    expect(asRoleMap(null)).toEqual({})
    expect(asRoleMap(['CART'])).toEqual({})
    expect(asRoleMap({ CART: 3, AF: 'af' })).toEqual({ AF: 'af' })
  })
})
