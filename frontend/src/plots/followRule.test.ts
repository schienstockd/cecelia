import { describe, it, expect } from 'vitest'
import type { GateSpec } from '../stores/gating'
import { canFollow, follows, toggleFollow, followTip } from './followRule'

const base = { x_channel: 'x', y_channel: 'y', x_transform: { kind: 'linear' }, y_transform: { kind: 'linear' } } as const
const rect: GateSpec = { ...base, kind: 'rectangle', x_min: 1, x_max: 2, y_min: 1, y_max: 2 }

describe('follow rule', () => {
  it('only a rectangle or polygon can follow its cloud', () => {
    expect(canFollow(rect)).toBe(true)
    expect(canFollow({ ...base, kind: 'polygon', vertices: [[0, 0], [1, 0], [0, 1]] })).toBe(true)
    expect(canFollow({ ...base, kind: 'bisector', cut: 1 })).toBe(false)
    // a bisector's valley rule is not a follow rule
    expect(follows({ ...base, kind: 'bisector', cut: 1, rule: { kind: 'valley', lo: 0, hi: 2 } })).toBe(false)
  })
  it('toggles on keeping the shape, and off dropping the rule and its flag where it is shown', () => {
    const on = toggleFollow(rect)
    expect(on).toEqual({ ...rect, rule: { kind: 'follow' } })
    expect(toggleFollow({ ...on, x_min: 1.5, rule_ok: true })).toEqual({ ...rect, x_min: 1.5 })
    const bis: GateSpec = { ...base, kind: 'bisector', cut: 1 }
    expect(toggleFollow(bis)).toBe(bis)
  })
  it('says what a double-click does, and when no cloud was found', () => {
    expect(followTip(rect)).toMatch(/^Double-click: follow/)
    expect(followTip({ ...rect, rule: { kind: 'follow' }, rule_ok: true })).toMatch(/^Follows its cloud/)
    expect(followTip({ ...rect, rule: { kind: 'follow' }, rule_ok: false })).toMatch(/stays where drawn/)
  })
})
