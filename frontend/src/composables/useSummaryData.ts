import { ref, computed, watch, onMounted, onUnmounted, type Ref } from 'vue'
import { useWsStore } from '../stores/ws'
import { useDataRefresh } from './useDataRefresh'
import { useViewState } from './useViewState'
import { tkey, parseTkey } from '../plots/series'
import { resolvePopType, granularityFor, popTypeOptions } from '../plots/popTypes'
import { defaultVis, type VisProps } from '../plots/plot'
import { fetchImageAttrs, type ImageAttr } from './useImageAttrs'
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
  // The id of the ACTIVE slot's/panel's spec. The population picker's popType/granularity follow the
  // ACTIVE plot's spec rather than the first registered one, so a canvas hosting a MIX of popTypes
  // (flow / live / clust / trackclust / region) surfaces the RIGHT pops for whichever plot is selected.
  // Passed by BOTH hosts (LayoutCanvas and SummaryCanvas) — it was board-only while every module page's
  // specs happened to share a popType, which the per-poptype population summaries broke. Still optional
  // so a host with a single spec need not thread it; `specs[0]` is then correct by definition.
  activeSpecId?: Ref<string | null>
  // The ACTIVE panel's chosen pop type, for a spec that offers several (the collapsed "Population
  // summary"). This is what makes the population manager a view of the active plot's family: the plot
  // owns the choice, the manager follows it. Ignored when the active spec offers only one.
  activePopType?: Ref<string | null>
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
  // groupAttr so images sharing a value pool into one series labelled by the value. Fetch via the
  // shared helper (same impl as the UMAP colour/facet-by-attribute picker).
  const setAttrs = ref<ImageAttr[]>([])
  async function loadAttrs() {
    setAttrs.value = canCompare.value
      ? await fetchImageAttrs(projectUid.value, setUid.value, imageUids.value) : []
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
  // The pop type the population picker lists = the ACTIVE plot's. For a spec offering a choice that is
  // the panel's own pick (resolved against what this page offers, so a pick carried over from the board
  // can't ask for a family this page hasn't got); for a single-family spec it is simply that family.
  // Granularity always follows the pop type — never the spec — because they differ per family.
  const effSpec = computed(() => activeSpec.value ?? specs.value[0])
  const popType = computed(() =>
    effSpec.value ? resolvePopType(effSpec.value, opts.activePopType?.value ?? null) : 'live')
  const granularity = computed(() =>
    effSpec.value ? granularityFor(effSpec.value, popType.value)
      : (specs.value.some(s => s.dataSource.granularity === 'track') ? 'track' : 'cell'))
  async function loadSpecs() {
    const q = module ? `?module=${encodeURIComponent(module)}` : ''
    try { specs.value = await (await fetch(`/api/plots/definitions${q}`)).json() } catch { specs.value = [] }
  }

  // populations across the selected images, grouped by segmentation (drives the picker — holds ONLY the
  // active popType on the mixed board, since it follows the active slot).
  const segPops = ref<SegmentationPops[]>([])
  // Population colours, ACCUMULATING across popTypes (keyed `${valueName}${path}`; colours are stable).
  // A plain computed(segPops) would drop every OTHER popType's colours the moment the active slot
  // switches — so the non-active slots (and a full-board PDF export) render grey. We instead MERGE each
  // load into a persistent map so once a popType's colours have loaded they stay available for its
  // slots even while another popType is active; popmap reload re-merges to refresh any changed colour.
  const popColors = ref(new Map<string, string>())
  function mergeColors(groups: SegmentationPops[]) {
    if (!groups.length) return
    const m = new Map(popColors.value)
    for (const g of groups) for (const p of g.populations) m.set(`${g.valueName}${p.path}`, p.colour)
    popColors.value = m
  }
  function popsUrl(pt: string, gran: string) {
    const p = new URLSearchParams({ projectUid: projectUid.value, popType: pt, granularity: gran })
    if (setUid.value) { p.set('setUid', setUid.value); if (imageUids.value.length) p.set('imageUids', imageUids.value.join(',')) }
    else if (imageUid.value) p.set('imageUid', imageUid.value)
    return `/api/plots/populations?${p}`
  }
  async function loadPops() {
    if (!imageUid.value && !imageUids.value.length) { segPops.value = []; return }
    try { segPops.value = await (await fetch(popsUrl(popType.value, granularity.value))).json() }
    catch { segPops.value = [] }
    mergeColors(segPops.value)
    warmColors()
  }
  // Pre-warm colours for EVERY (popType, granularity) the specs can offer — the board hosts a mix, but
  // loadPops only fetches the ACTIVE slot's popType. Without this, a slot never made active has no
  // colours, so a full-board PDF export paints it grey. Colours are stable → a single seed each is
  // enough. Enumerates each spec's FULL option list (not just its default), so a page whose one spec
  // offers several families — Phenotype's gated + cell clusters — is warmed for all of them.
  async function warmColors() {
    if (!imageUid.value && !imageUids.value.length) return
    const combos = new Map<string, { pt: string; gran: string }>()
    for (const s of specs.value)
      for (const o of popTypeOptions(s))
        combos.set(`${o.popType}|${o.granularity}`, { pt: o.popType, gran: o.granularity })
    if (combos.size <= 1) return
    await Promise.all([...combos.values()].map(async c => {
      try { mergeColors(await (await fetch(popsUrl(c.pt, c.gran))).json()) } catch { /* ignore */ }
    }))
  }

  // series colour = the POPULATION colour so a population reads identically across images. A COMPUTED
  // returning the resolver (not a plain function) so its IDENTITY changes whenever popColors updates →
  // the panel's buildOpts (which captures colorOf) recomputes and the plot re-renders. A stable
  // function reference would leave colours grey until the next data change — the "grey on load until I
  // toggle a population" bug (colours arrive from loadPops AFTER the first render).
  const seriesColor = computed(() => {
    const m = popColors.value
    return (s: PlotSeries): string => m.get(s.pop) ?? '#7c93b8'
  })

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
  // popType-AWARE: only prune keys of the CURRENTLY-LOADED popType. On the mixed board (popType follows
  // the active slot), segPops holds only one popType at a time — pruning blindly would drop the OTHER
  // plots' selections (e.g. selecting a trackclust pop-summary slot wiped the track-measure plots'
  // live/track pops). Keep any key whose popType isn't the current one; it belongs to another slot.
  watch(segPops, () => {
    if (!segPops.value.length) return
    const valid = validSelKeys.value, pt = popType.value
    gSel.value = gSel.value.filter(k => parseTkey(k).popType !== pt || valid.has(k))
  })
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
