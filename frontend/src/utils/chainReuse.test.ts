import { describe, expect, it } from 'vitest'
import { liveStatusText, readReusePref, reusedOf, reusedTip, withReuse } from './chainReuse'

describe('chainReuse', () => {
  it('the toggle is off unless it was saved on', () => {
    expect(readReusePref(null)).toBe(false)
    expect(readReusePref('false')).toBe(false)
    expect(readReusePref('true')).toBe(true)
  })

  it('a run payload carries reuse only when it is on', () => {
    const base = { type: 'chain:run', projectUid: 'p', chain: 'c', imageUids: ['a'] }
    expect(withReuse(base, false)).toEqual(base)
    expect('reuse' in withReuse(base, false)).toBe(false)
    expect(withReuse(base, true)).toEqual({ ...base, reuse: true })
  })

  it('a done node that reused a past run reads reused, anything else its status', () => {
    expect(liveStatusText('done', 'tid1')).toBe('reused')
    expect(liveStatusText('done')).toBe('done')
    expect(liveStatusText('done', '')).toBe('done')
    expect(liveStatusText('failed', 'tid1')).toBe('failed')
    expect(reusedTip('tid1')).toContain('tid1')
    expect(reusedTip(undefined)).toBeUndefined()
  })

  it('reads one node off the run route map', () => {
    const m = { img1: { n1: 'tid1' } }
    expect(reusedOf(m, 'img1', 'n1')).toBe('tid1')
    expect(reusedOf(m, 'img1', 'n2')).toBeUndefined()
    expect(reusedOf(m, 'img2', 'n1')).toBeUndefined()
    expect(reusedOf(undefined, 'img1', 'n1')).toBeUndefined()
  })
})
