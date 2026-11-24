import { describe, expect, it } from 'vitest'
import { snapPosition, SNAP_PX } from './panelSnap'

const A = { x: 100, y: 100, w: 300, h: 300 }

describe('snapPosition', () => {
  it('nothing near → exactly where the pointer put it', () => {
    expect(snapPosition({ x: 600, y: 600, w: 200, h: 200 }, [A])).toEqual({ x: 600, y: 600 })
  })
  it('dropped just right of a plot → touches it, tops lined up', () => {
    expect(snapPosition({ x: 407, y: 95, w: 300, h: 300 }, [A])).toEqual({ x: 400, y: 100 })
  })
  it('dropped just left of a plot → its right edge touches the plot', () => {
    expect(snapPosition({ x: 2 + 100 - 250 + 5, y: 100, w: 250, h: 300 }, [A]).x).toBe(100 - 250)
  })
  it('dropped just below → touches the bottom, left edges lined up', () => {
    expect(snapPosition({ x: 104, y: 409, w: 300, h: 300 }, [A])).toEqual({ x: 100, y: 400 })
  })
  it('the canvas edges pull too', () => {
    expect(snapPosition({ x: 7, y: 5, w: 300, h: 300 }, [])).toEqual({ x: 0, y: 0 })
  })
  it('a plot far away on the other axis does not line things up', () => {
    // same left edge as A (± a few px) but 500 px below it — no neighbour on that row
    expect(snapPosition({ x: 105, y: 900, w: 300, h: 300 }, [A]).x).toBe(105)
  })
  it('just outside the tolerance → no snap', () => {
    expect(snapPosition({ x: 400 + SNAP_PX + 1, y: 100, w: 300, h: 300 }, [A]).x).toBe(400 + SNAP_PX + 1)
  })
  it('the nearest candidate wins', () => {
    const B = { x: 410, y: 100, w: 300, h: 300 }
    expect(snapPosition({ x: 408, y: 100, w: 300, h: 300 }, [A, B]).x).toBe(410)
  })
})
