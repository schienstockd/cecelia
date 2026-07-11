import { ref, computed, watch, onMounted, onUnmounted, type Ref } from 'vue'
import { useWsStore } from '../stores/ws'
import { useDataRefresh } from './useDataRefresh'
import { useViewState } from './useViewState'
import { tkey } from '../plots/series'
import { defaultVis, type VisProps } from '../plots/plot'
import type { PlotSpec, PlotSeries, SegmentationPops } from '../plots/types'

// Data + shared view-state for a summary-plot surface — the part that is IDENTICAL whether the plots
// float freely (SummaryCanvas, per-module) or sit in a grid (LayoutCanvas, /analysis). Extracted so the
// two hosts share ONE implementation (see feedback_use_existing_framework): plot-spec registry,
// populations-by-segmentation, image attributes + the compare cluster, and the canvas-level shared bag
// (compare mode / scope / global eye-selection + vis / pool). The PANEL vs SLOT model stays in the host.
//
// `shared` is the per-canvas persisted bag (from useCanvasPanels for SummaryCanvas, from the layout
// store for LayoutCanvas) — canvas-level options live in it via useViewState so they survive navigation.
export function useSummaryData(opts: {
  projectUid: Ref<string>
  imageUids: Ref<string[]>
  setUid: Ref<string | null>
  module: string | null | undefined
  shared: Ref<Record<string, unknown>>
  // OPTIONAL: the id of the ACTIVE slot's spec (the universal board). When set, the population picker's
  // popType/granularity follow the ACTIVE plot's spec instead of the first registered spec — so a mixed
  // board (flow / live / clust / trackclust) surfaces the RIGHT pops for whichever plot is selected.
  // Omitted on the per-module canvases (their specs share a popType, so specs[0] is correct).
  activeSpecId?: Ref<string | null>
}) {
  const { projectUid, imageUids, setUid, module } = opts
  const ws = useWsStore()
  const imageUid = computed(() => imageUids.value[0] ?? null)

  const { compareMode, compareAttr, compareAttr2, scope, sel: gSel, vis: gVis, poolGroups } =
    useViewState(opts.shared, {
      compareMode: 'image' as 'image' | 'per_image' | 'summarised' | 'by_attr',
      compareAttr: '' as string,
      compareAttr2: '' as string,
      scope: 'global' as 'global' | 'local',
      sel: [] as string[],
      vis: defaultVis() as VisProps,
      poolGroups: false as boolean,
    })

  const canCompare = computed(() => !!setUid.value && imageUids.value.length > 1)
  const crossImage = computed(() => compareMode.value !== 'image' && canCompare.value)
  const panelSetUid = computed(() => crossImage.value ? setUid.value : null)
  const panelImageUids = computed(() => crossImage.value ? imageUids.value : undefined)
  const panelScope = computed<'per_image' | 'summarised'>(() =>
    compareMode.value === 'summarised' ? 'summarised' : 'per_image')

  // image attributes available across the set (for "by attribute" compare); the chosen one is sent as
  // groupAttr so images sharing a value pool into one series labelled by the value.
  const setAttrs = ref<{ name: string; values: string[] }[]>([])
  async function loadAttrs() {
    if (!setUid.value || !canCompare.value) { setAttrs.value = []; return }
    const p = new URLSearchParams({ projectUid: projectUid.value, setUid: setUid.value })
    if (imageUids.value.length) p.set('imageUids', imageUids.value.join(','))
    try { setAttrs.value = (await (await fetch(`/api/plots/attrs?${p}`)).json()).attrs ?? [] }
    catch { setAttrs.value = [] }
  }
  watch([compareMode, setAttrs, compareAttr], () => {
    if (compareMode.value === 'by_attr' && !compareAttr.value && setAttrs.value.length)
      compareAttr.value = setAttrs.value[0].name
    if (compareAttr2.value && (compareAttr2.value === compareAttr.value
        || !setAttrs.value.some(a => a.name === compareAttr2.value)))
      compareAttr2.value = ''
  })
  const panelGroupAttr = computed<string[]>(() =>
    compareMode.value === 'by_attr' && crossImage.value
      ? [compareAttr.value, compareAttr2.value].filter(Boolean) : [])
  const attrOptions2 = computed(() => setAttrs.value.filter(a => a.name !== compareAttr.value))

  // available plot specs (per-module registry; null module = universal → all specs)
  const specs = ref<PlotSpec[]>([])
  const specById = computed(() => Object.fromEntries(specs.value.map(s => [s.id, s])))
  // active plot's spec (board only; each spec carries ONE popType because the plots are split per
  // module page) — its popType/granularity drive the picker so a mixed board surfaces the right pops.
  const activeSpec = computed(() => opts.activeSpecId?.value ? specById.value[opts.activeSpecId.value] : undefined)
  const popType = computed(() => activeSpec.value?.dataSource.popType ?? specs.value[0]?.dataSource.popType ?? 'live')
  const granularity = computed(() =>
    activeSpec.value?.dataSource.granularity
      ?? (specs.value.some(s => s.dataSource.granularity === 'track') ? 'track' : 'cell'))
  async function loadSpecs() {
    const q = module ? `?module=${encodeURIComponent(module)}` : ''
    try { specs.value = await (await fetch(`/api/plots/definitions${q}`)).json() } catch { specs.value = [] }
  }

  // populations across the selected images, grouped by segmentation
  const segPops = ref<SegmentationPops[]>([])
  const popColors = computed(() => {
    const m = new Map<string, string>()
    for (const g of segPops.value) for (const p of g.populations) m.set(`${g.valueName}${p.path}`, p.colour)
    return m
  })
  async function loadPops() {
    if (!imageUid.value && !imageUids.value.length) { segPops.value = []; return }
    const p = new URLSearchParams({ projectUid: projectUid.value, popType: popType.value, granularity: granularity.value })
    if (setUid.value) { p.set('setUid', setUid.value); if (imageUids.value.length) p.set('imageUids', imageUids.value.join(',')) }
    else if (imageUid.value) p.set('imageUid', imageUid.value)
    try { segPops.value = await (await fetch(`/api/plots/populations?${p}`)).json() }
    catch { segPops.value = [] }
  }

  // series colour = the POPULATION colour so a population reads identically across images
  function seriesColor(s: PlotSeries): string { return popColors.value.get(s.pop) ?? '#7c93b8' }

  // live updates: the server broadcasts gating:popmap after any gate mutation — refetch pops + bump a
  // token so panels re-pull data (membership may change with no prop change). Prune vanished selections.
  const reloadToken = ref(0)
  function onPopmap(d: unknown) {
    const m = d as { imageUid?: string }
    if (m.imageUid && imageUids.value.length && !imageUids.value.includes(m.imageUid)) return
    loadPops(); reloadToken.value++
  }
  // a task finishing on one of THESE images → refetch (pop list may have new pops; data may have
  // changed in place). Same mechanism as the gate popmap above; targeted per-image via useDataRefresh.
  useDataRefresh(() => imageUids.value, () => { loadPops(); reloadToken.value++ })
  // the set of currently-valid target keys (for pruning host selections)
  const validSelKeys = computed(() => {
    const exist = new Set<string>()
    for (const g of segPops.value) for (const p of g.populations) exist.add(tkey(p.popType, g.valueName, p.path))
    return exist
  })

  watch([() => imageUids.value.join(','), popType, setUid], () => { loadPops(); loadAttrs() })
  // prune the selection to populations that still exist — but ONLY once we actually have populations.
  // segPops is transiently [] during load / image-switch / a failed fetch; pruning then would wipe a
  // restored selection (and it would save back empty). Guard so an empty segPops never clears gSel.
  watch(segPops, () => { if (segPops.value.length) gSel.value = gSel.value.filter(k => validSelKeys.value.has(k)) })
  onMounted(async () => { ws.on('gating:popmap', onPopmap); await loadSpecs(); await loadPops(); await loadAttrs() })
  onUnmounted(() => ws.off('gating:popmap', onPopmap))

  return {
    // data
    specs, specById, popType, granularity, segPops, popColors, setAttrs, seriesColor, reloadToken,
    validSelKeys, loadSpecs, loadPops, loadAttrs,
    // shared view-state
    compareMode, compareAttr, compareAttr2, scope, gSel, gVis, poolGroups,
    // compare-derived
    canCompare, crossImage, panelSetUid, panelImageUids, panelScope, panelGroupAttr, attrOptions2,
  }
}
