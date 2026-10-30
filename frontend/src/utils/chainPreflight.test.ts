import { describe, it, expect } from 'vitest'
import { groupByImage, type PreflightProblem } from './chainPreflight'

const p = (imageUid: string, level: 'fail' | 'warn', message = 'x'): PreflightProblem =>
  ({ imageUid, nodeId: 'n', fn: 'f', level, message })

describe('groupByImage', () => {
  it('groups per image and puts images with a failure first', () => {
    const g = groupByImage([p('a', 'warn'), p('b', 'fail'), p('a', 'warn', 'y')], u => `img-${u}`)
    expect(g.map(x => x.name)).toEqual(['img-b', 'img-a'])
    expect(g[1].problems.map(x => x.message)).toEqual(['x', 'y'])
    expect(g[0].fails).toBe(true)
  })
  it('is empty with nothing to report', () => {
    expect(groupByImage([])).toEqual([])
  })
})
