// Runs before every browser test file (vitest.browser.config.ts → setupFiles).
//
//  - The app stylesheets, so tokens (`--cc-*`) and the primitives' classes exist as in the app.
//  - NO NETWORK. The tests mount components that persist through `profileStorage`, which PATCHes
//    /api/profile/settings — and a browser test must never reach a backend (the maintainer's live one
//    is on :8080). `fetch` and `WebSocket` are replaced for each test; any call fails it, by URL.
//  - A "ResizeObserver loop" error fails the test that caused it: a callback resized what it observes
//    (utils/roLoopTrace.ts is the app's dev-time attribution of the same event).
import '../../icons.css'
import '../../style.css'
import { afterEach, beforeEach, expect } from 'vitest'
import { unmountAll } from './mount'

let netCalls: string[] = []
let roLoops: string[] = []
const realFetch = window.fetch
const RealWebSocket = window.WebSocket

const onError = (e: ErrorEvent) => {
  if (e.message && /ResizeObserver loop/.test(e.message)) { roLoops.push(e.message); e.preventDefault() }
}

beforeEach(() => {
  netCalls = []
  roLoops = []
  window.fetch = (input: RequestInfo | URL) => {
    const url = typeof input === 'string' ? input : input instanceof URL ? input.href : input.url
    netCalls.push(`fetch ${url}`)
    return Promise.reject(new Error(`browser test reached the network: ${url}`))
  }
  window.WebSocket = class {
    constructor(url: string | URL) {
      netCalls.push(`ws ${String(url)}`)
      throw new Error(`browser test opened a WebSocket: ${String(url)}`)
    }
  } as unknown as typeof WebSocket
  window.addEventListener('error', onError)
})

afterEach(() => {
  unmountAll()
  window.fetch = realFetch
  window.WebSocket = RealWebSocket
  window.removeEventListener('error', onError)
  expect(netCalls, 'a browser test reached the network').toEqual([])
  expect(roLoops, 'a ResizeObserver loop (a callback resized what it observes)').toEqual([])
})
