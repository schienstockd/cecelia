import { describe, it, expect, beforeEach, vi } from 'vitest'
import { computed } from 'vue'
import { setActivePinia, createPinia } from 'pinia'

// The stores read localStorage at setup; a Map-backed one per test.
function stubStorage() {
  const m = new Map<string, string>()
  vi.stubGlobal('localStorage', {
    getItem: (k: string) => m.get(k) ?? null,
    setItem: (k: string, v: string) => { m.set(k, String(v)) },
    removeItem: (k: string) => { m.delete(k) },
    clear: () => m.clear(),
    key: (i: number) => [...m.keys()][i] ?? null,
    get length() { return m.size },
  })
}

const { drawSpecOf, readDrawSpec } = await import('./drawSpec')
const { useSettingsStore } = await import('../../stores/settings')
const { useViewerStore } = await import('../../stores/viewer')

const src = (over: Record<string, unknown> = {}) => ({
  imageUid: 'img1', setUid: 'set1', labelNames: ['cpSAM', 'flowTom'], trackNames: ['flowTom'],
  branchNames: [], channelNames: ['DAPI'], nZ: 0, version: 'raw', viewState: null,
  gating: { valueName: '', popType: '' }, ...over,
})

describe('drawSpecOf — the viewer’s live read (DRAW_SPEC_PLAN D2)', () => {
  beforeEach(() => { stubStorage(); setActivePinia(createPinia()) })

  it('reads the settings the viewer drew from', () => {
    const settings = useSettingsStore()
    settings.setLabelVisibility('img1', { cpSAM: false, flowTom: true })
    settings.setPopVisible('set1', 'clust', true)
    settings.setPointSize('set1', 11)
    const spec = drawSpecOf(src())
    expect(spec.maskValueName).toBe('flowTom')
    expect(spec.popVisible.clust).toBe(true)
    expect(spec.popVisible.flow).toBe(false)
    expect(spec.pointSize).toBe(11)
  })

  it('a field tracks only the settings it reads: a global style change does not recompute a pop type', () => {
    const settings = useSettingsStore()
    const spec = drawSpecOf(src())
    let runs = 0
    const flow = computed(() => { runs++; return spec.popVisible.flow })
    expect(flow.value).toBe(false)
    settings.viewerLabelOpacity = 0.7
    settings.viewerSteps = 128
    expect(flow.value).toBe(false)
    expect(runs).toBe(1)
    settings.setPopVisible('set1', 'flow', true)
    expect(flow.value).toBe(true)
    expect(runs).toBe(2)
  })
})

describe('readDrawSpec — a recording’s snapshot (DRAW_SPEC_PLAN D3)', () => {
  beforeEach(() => { stubStorage(); setActivePinia(createPinia()) })
  const img = { uid: 'img1', labels: { cpSAM: [], other: [] }, channelNames: ['DAPI'] }

  it('picks the mask from the names the open viewer draws from, not the registry’s', () => {
    const viewer = useViewerStore()
    viewer.setOpenImage({ projectUid: 'p', imageUid: 'img1', valueName: 'raw', zarrPath: '', taskDir: '',
                          labelNames: ['onDisk'], trackNames: ['onDisk'] })
    expect(readDrawSpec(img, 'set1', true).maskValueName).toBe('onDisk')
    expect(readDrawSpec(img, 'set1', false).maskValueName).toBe('cpSAM')
  })

  it('is plain data — the pop types are values, not getters', () => {
    useSettingsStore().setPopVisible('set1', 'flow', true)
    const spec = readDrawSpec(img, 'set1', false)
    expect(Object.getOwnPropertyDescriptor(spec.popVisible, 'flow')?.get).toBeUndefined()
    expect(JSON.parse(JSON.stringify(spec.popVisible)).flow).toBe(true)
    expect(Object.getOwnPropertyDescriptor(spec, 'pointSize')?.get).toBeUndefined()
  })
})
