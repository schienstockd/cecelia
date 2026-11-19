import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { slowPaint, SLOW_PAINT_MS } from './slowPaint'

// Hand-driven frames + clock: the rules under test are "announce before a known-slow draw, after the
// announcement could paint" and "only the latest request draws" — not the browser's cadence.
let queued: Array<() => void> = []
let now = 0
const flushMicro = async () => { for (let i = 0; i < 5; i++) await Promise.resolve() }
async function frame() { const cbs = queued; queued = []; cbs.forEach(cb => cb()); await flushMicro() }

beforeEach(() => {
  queued = []; now = 0
  vi.stubGlobal('requestAnimationFrame', (cb: () => void) => { queued.push(cb); return queued.length })
  vi.stubGlobal('performance', { now: () => now })
})
afterEach(() => { vi.unstubAllGlobals() })

/** a draw that "takes" `ms` of main-thread time */
const drawTaking = (ms: number, log: string[]) => () => { log.push('draw'); now += ms }

describe('slowPaint', () => {
  it('a fast plot never raises the busy flag', async () => {
    const log: string[] = []
    const p = slowPaint(drawTaking(20, log), b => log.push(b ? 'busy' : 'idle'))
    await p.run(); await p.run(); await p.run()
    expect(log).toEqual(['draw', 'draw', 'draw'])
  })

  it('after a slow draw, the next one shows busy and waits two frames before drawing', async () => {
    const log: string[] = []
    const p = slowPaint(drawTaking(SLOW_PAINT_MS + 500, log), b => log.push(b ? 'busy' : 'idle'))
    await p.run()                                   // first draw: nothing known yet, no announcement
    expect(log).toEqual(['draw'])
    const done = p.run()
    await flushMicro()
    expect(log).toEqual(['draw', 'busy'])           // announced, NOT yet drawn — so the wheel can paint
    await frame()
    expect(log).toEqual(['draw', 'busy'])           // one frame is not enough to be sure it painted
    await frame()
    await done
    expect(log).toEqual(['draw', 'busy', 'draw', 'idle'])
  })

  it('a draw that gets fast again stops announcing', async () => {
    let ms = SLOW_PAINT_MS + 100
    const log: string[] = []
    const p = slowPaint(() => { log.push('draw'); now += ms }, b => log.push(b ? 'busy' : 'idle'))
    await p.run()
    ms = 5
    const second = p.run(); await frame(); await frame(); await second
    log.length = 0
    await p.run()
    expect(log).toEqual(['draw'])
  })

  it('a request made while one waits for its frames supersedes it — one draw, busy until it is done', async () => {
    const log: string[] = []
    const p = slowPaint(drawTaking(SLOW_PAINT_MS, log), b => log.push(b ? 'busy' : 'idle'))
    await p.run()
    log.length = 0
    const a = p.run()
    await flushMicro()
    const b = p.run()                               // e.g. the canvas grew right after the toggle
    await frame(); await frame()
    await Promise.all([a, b])
    expect(log).toEqual(['busy', 'draw', 'idle'])
  })

  it('clears the busy flag even when the draw throws', async () => {
    const log: string[] = []
    let fail = false
    const p = slowPaint(() => { now += SLOW_PAINT_MS; if (fail) throw new Error('x') }, b => log.push(b ? 'busy' : 'idle'))
    await p.run()
    fail = true
    const r = p.run(); await frame(); await frame()
    await expect(r).rejects.toThrow('x')
    expect(log).toEqual(['busy', 'idle'])
  })
})
