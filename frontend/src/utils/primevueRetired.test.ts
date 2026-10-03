import { describe, it, expect } from 'vitest'
import pkg from '../../package.json'

// PrimeVue's next major moved to a proprietary, key-gated licence, so the app dropped it
// (docs/todo/PRIMEVUE_RETIRE_PLAN.md). The toast and tooltip are our own now, behind the same
// call-site API. This keeps it gone: a `primevue` or `@primeuix/*` import or dependency fails here,
// because a copy-pasted `import Dialog from 'primevue/dialog'` would quietly pull it back in.
// `primeicons` too: the icons are vendored Lucide now (src/icons.css), behind the same `pi-*` classes.
const RAW = import.meta.glob('/src/**/*.{vue,ts,css}', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
const sources = Object.entries(RAW)
  .map(([path, text]) => ({ path: path.replace('/src/', ''), text }))
  .filter(s => !s.path.endsWith('.test.ts'))
const VENDOR = /(?:from\s+|import\s*\(\s*|import\s+|@import\s+)['"](?:primevue|@primeuix|@primevue|primeicons)(?:\/[^'"]*)?['"]/

describe('PrimeVue stays retired', () => {
  it('no source imports primevue, @primevue, @primeuix or primeicons', () => {
    expect(sources.filter(s => VENDOR.test(s.text)).map(s => s.path)).toEqual([])
  })
  it('package.json does not depend on them', () => {
    const deps = { ...pkg.dependencies, ...pkg.devDependencies } as Record<string, string>
    expect(Object.keys(deps).filter(d => /^(primevue|primeicons|@primeuix\/|@primevue\/)/.test(d))).toEqual([])
  })
  it('the detector catches the shapes an import takes', () => {
    for (const line of [`import Dialog from 'primevue/dialog'`, `import Aura from "@primeuix/themes/aura"`,
                        `const T = () => import('primevue/toast')`, `import 'primevue/resources/x.css'`])
      expect(VENDOR.test(line)).toBe(true)
    expect(VENDOR.test(`import 'primeicons/primeicons.css'`)).toBe(true)
    expect(VENDOR.test(`import './icons.css'`)).toBe(false)
  })
})
