import { describe, it, expect } from 'vitest'

// Module pages are kept alive under App.vue's <KeepAlive> (composables/useKeepAlive.ts). A hidden page is
// not unmounted, so a key listener added in onMounted / removed in onBeforeUnmount keeps firing while
// the user is on another page — Esc cancelled a hidden plot's selection, and Ctrl+Z in a hidden page's
// population manager undid on a tree the user wasn't looking at. `useWindowListener` attaches only
// while the page is visible; every other raw key listener must say why it cannot leak.
const KEY_LISTENER_EXEMPT: Record<string, string> = {
  'composables/useKeepAlive.ts': 'IS the fix (useWindowListener)',
  'components/TeleportPopover.vue': 'attached only while open, and it closes on deactivate',
  'components/GuideBubble.vue': 'app-level (App.vue), never inside a module page',
  'composables/usePlotFullscreen.ts': 'one app-wide listener for the one app-wide maximised flag',
  'modules/ViewerWindow.vue': 'the viewer popout window — a bare route, never kept alive',
  'modules/MoviesModule.vue': 'not a kept-alive page (App.vue KEPT_ALIVE_PAGES)',
  'directives/tooltip.ts': 'attached only while a tip shows, which hides when its target leaves the document',
}

const RAW = import.meta.glob('/src/**/*.{vue,ts}', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
const sources = Object.entries(RAW)
  .map(([path, text]) => ({ path: path.replace('/src/', ''), text }))
  .filter(s => !s.path.endsWith('.test.ts'))
const KEY_LISTENER = /(window|document)\.addEventListener\(\s*['"]key(down|up|press)['"]/

describe('key listeners on kept-alive pages', () => {
  it('go through useWindowListener unless exempt', () => {
    const offenders = sources.filter(s => KEY_LISTENER.test(s.text) && !(s.path in KEY_LISTENER_EXEMPT))
      .map(s => s.path)
    expect(offenders).toEqual([])
  })
  it('the exemption list stays honest — every entry still adds a key listener', () => {
    const stale = Object.keys(KEY_LISTENER_EXEMPT).filter(p => {
      const s = sources.find(x => x.path === p)
      return !s || !(KEY_LISTENER.test(s.text) || p === 'composables/useKeepAlive.ts')
    })
    expect(stale).toEqual([])
  })
})

// Kiwi's Share-plots button targets ONE registered canvas (stores/shareTarget.ts). Registering on mount
// and unregistering only on dispose left the last-MOUNTED kept-alive page as the target: back on an
// earlier page, Share-plots opened its selection overlay on the hidden page and nothing happened.
describe('canvas share host on kept-alive pages', () => {
  it('re-registers on activate and unregisters on deactivate', () => {
    const src = RAW['/src/stores/shareTarget.ts']
    const body = src.slice(src.indexOf('export function useCanvasShareHost'))
    expect(body).toMatch(/onActivated\(\s*\(\)\s*=>\s*store\.registerCanvasHost\(host\)\s*\)/)
    expect(body).toMatch(/onDeactivated\(\s*\(\)\s*=>\s*store\.unregisterCanvasHost\(host\)\s*\)/)
  })
  // Same single-winner shape: two kept-alive canvases share an exporter key (Whiteboard QC + Segment).
  it('panel exporters re-register on activate and unregister on deactivate', () => {
    const src = RAW['/src/stores/canvasPanelExports.ts']
    const body = src.slice(src.indexOf('export function usePanelExport'))
    expect(body).toMatch(/onActivated\(\s*\(\)\s*=>\s*store\.register\(lastKey, exporter\)\s*\)/)
    expect(body).toMatch(/onDeactivated\(\s*\(\)\s*=>\s*store\.unregister\(lastKey, exporter\)\s*\)/)
  })
})
