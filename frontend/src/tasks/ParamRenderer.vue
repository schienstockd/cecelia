<!--
  Renders a single parameter from a TaskDef params array.
  The parent owns the values object and passes it as modelValue.
  Supports nested section (collapsible box) with recursive rendering.
-->
<script setup lang="ts">
import { computed, ref, watch, onUnmounted } from 'vue'
import type { ParamDef, ParamValues } from './types'
import type { CciaImage } from '../stores/project'
import { SEVERITY } from '../lib/severity'
import { paramAdvisor, type ParamAdvisor, type ParamAdvisory, type AdvisorContext,
         type AdvisorParam } from './paramAdvisors'
import { debouncedLatest } from '../utils/debouncedLatest'
import InlineNote from '../components/InlineNote.vue'
import SuggestInput from '../components/SuggestInput.vue'
import { selectedOptionHelp } from '../utils/optionHelp'
import { isChosenValueName, preferredValueName, valueNameOptions, showIfSatisfied,
         scopeValueName, groupOrderKeys, newEntryDefaults } from './paramValues'
import { groupPopulations, type PopGroupDef, type RawGroup } from '../utils/popGroups'
import { measureGroups } from '../utils/measureGroups'
import { consumerField, type ValueNameNamespace } from '../utils/taskOutput'
import ChipSelect, { type ChipOption } from '../components/ChipSelect.vue'
import VisualAid from '../components/VisualAid.vue'
import FloatingPanel from '../components/FloatingPanel.vue'
import { paramVisColumns, uniformWarning } from './paramVis'
import CcToggle from '../components/CcToggle.vue'
import FileBrowser from '../components/FileBrowser.vue'

type GroupValues = Record<string, ParamValues>

export interface ParamContext {
  images: CciaImage[]
  projectUid?: string        // popSelection: needed to query the gating popmap
  values?: ParamValues       // sibling param values (popSelection reads valueName)
  params?: ParamDef[]        // the WHOLE task's params, so a widget can find a sibling by TYPE rather
                             // than by a hardcoded key. See `scopeValueName` in paramValues.ts —
                             // resolving by name silently scoped two specs to the wrong segmentation.
  extraValueNames?: string[] // valueNameSelection: value_names not (yet) on disk — e.g. the output
                             // of an upstream whiteboard node ("cpCorrected") that only exists once
                             // the chain runs. Merged into the option list so it can be selected.
}

const props = defineProps<{
  param: ParamDef
  modelValue: unknown
  context?: ParamContext
}>()

const emit = defineEmits<{
  (e: 'update:modelValue', v: unknown): void
  // A `valueNameInput` the user has FINISHED entering — blur, or picking a suggestion (SuggestInput
  // dispatches a native `change` on accept). Deliberately NOT `update:modelValue`, which fires per
  // keystroke: reloading the form from a half-typed name would swap every other field while the user
  // is still deciding, and typing "Tcell2" passes through "Tcell" on the way.
  (e: 'commit', key: string, v: unknown): void
  // Which entries of a repeatable group run, and in what order — a SIBLING key (`<groupKey>Order`),
  // not this param's own value, so it cannot go through `update:modelValue`. Bubbled to the form
  // like `commit` is, because a repeatable group can sit inside a section and the renderer recurses.
  (e: 'update:groupOrder', key: string, v: string[]): void
}>()

// Two ways a param can not apply, and they are deliberately separate:
//   `hidden`  — the SERVER ruled it out, from something only it can see (the file you picked is an
//               XML export, which has no columns). Set by `_inject_dynamic_options!`.
//   `showIf`  — the SPEC ruled it out, from the form alone (`{ "mode": "attach" }`). No Julia.
// Either one means it renders nowhere rather than sitting there empty and looking broken.
const notApplicable = computed(() =>
  props.param.hidden === true || !showIfSatisfied(props.param.showIf, props.context?.values))

// section (collapsible box) state
const sectionOpen = ref(!props.param.collapsed)

// dirPath: the folder picker modal. Opened per param row, so each destination field owns its own.
const showDirBrowser = ref(false)
const showFileBrowser = ref(false)


const val = computed({
  get: () => props.modelValue ?? props.param.default,
  set: (v) => emit('update:modelValue', v),
})

// valueNameSelection: intersection of available names across selected images.
// Which field is used depends on param.field:
//   'labels'   → img.labels keys  (segmentation label sets)
//   'filepath' or absent → img.filepaths keys  (image versions)
// `field` names a CciaImage field (stores/project.ts): 'filepaths' (image versions, the default),
// 'labels' (segmentation label sets), 'spatialGraphs'. See `VALUE_NAME_FIELDS` in paramValues.ts —
// an unrecognised name used to degrade silently, and the suite now rejects one.

// Which image field the NAME LIST comes from. A `valueNameSelection` says so directly (`field`); a
// `valueNameInput` says which namespace it writes into and the field follows from that, so a spec
// never states the same thing twice. `null` = a namespace with no image-payload field yet (clusters,
// stats, models, obsCols) — the input still works, it just offers no suggestions. See
// docs/todo/VALUE_NAME_INPUT_PLAN.md → Phase 3.
// THREE-valued, and the difference matters. `undefined` means image VERSIONS — most task JSON omits
// `field`, so that is the common case — while `null` means there is genuinely no source, which only a
// `valueNameInput` whose namespace has no image field can be. Collapsing the two emptied the version
// picker on six specs; `valueNameOptions` is where that distinction is now enforced and tested.
const nameSourceField = computed<string | null | undefined>(() =>
  props.param.type === 'valueNameInput'
    ? consumerField(props.param.namespace as ValueNameNamespace)
    : props.param.field)

// A GLOBAL namespace has no image to read from — `models` is the vault, shared across projects — so
// its suggestions ride the spec's OPTIONS, injected by the definitions route the same way the coastal
// model picker's are. `null` field + no options = a plain input, which is the honest fallback for a
// namespace nothing can enumerate yet.
const globalNameOptions = computed<string[]>(() =>
  props.param.type === 'valueNameInput' && nameSourceField.value === null
    ? (props.param.options ?? []).map(o => String(o.value)).filter(Boolean)
    : [])

const availableValueNames = computed(() => valueNameOptions(
  props.context?.images ?? [],
  nameSourceField.value,
  // chain-propagated names (an upstream node's output, not on disk yet) + a GLOBAL namespace's
  // injected spec options, which is how `models` gets its list without an image to read from
  [...globalNameOptions.value, ...(props.context?.extraValueNames ?? [])]))

// When images change, auto-select an appropriate value name.
// For filepath fields: prefer the active value name.
// For other fields (e.g. labels): just pick the first available option.
watch(() => props.context?.images, (images) => {
  if (props.param.type !== 'valueNameSelection') return
  if (!images || images.length === 0) return
  // Keep an already-valid selection — notably an edge-propagated chain value like
  // "cpCorrected" — rather than resetting it to the active/first name on every image change.
  // `isChosenValueName` excludes the spec's OWN default: it is what the form started with, not a
  // pick, and since every task JSON declares `"default": "default"` this guard used to fire on
  // first render everywhere and the prefer-the-active-version line below never ran at all.
  if (isChosenValueName(props.modelValue, props.param.default)
      && availableValueNames.value.includes(props.modelValue as string)) return
  emit('update:modelValue', preferredValueName(
    availableValueNames.value, props.param.field, images[0].activeValueName))
}, { immediate: true })

// channelSelection: intersection of channel names across selected images
const availableChannels = computed(() => {
  const images = props.context?.images ?? []
  if (images.length === 0) return []
  const nameLists = images.map(img => img.channelNames ?? [])
  if (nameLists[0].length === 0) return []
  return nameLists[0].filter(n => nameLists.every(ns => ns.includes(n)))
})

// popSelection — two modes:
//  • single (default): a dropdown of flow populations for ONE segmentation (the sibling
//    `valueName`), plus "NONE (whole segmentation)". Used by e.g. tracking.bayesianTracking.
//  • multi / acrossSegmentations: a chip multi-select listing populations from EVERY segmentation,
//    each value prefixed with its value_name ("A/_tracked", "B/_tracked", …). `pop_df` pools these
//    (the prefix names the segmentation), so behaviour/HMM fits tracked A, B, C across images at once.
//    Source: /api/plots/populations (popType=live) — the same cross-segmentation picker the summary
//    canvas uses, which injects the DERIVED `/_tracked` pop (track_id>0, not stored in a gating file)
//    alongside the stored gates. The flat popmap endpoint would miss `_tracked`.
interface PopNode { name: string; children?: PopNode[] }
const popOptions = ref<{ label: string; value: string }[]>([{ label: 'NONE (whole segmentation)', value: 'NONE' }])
// grouped chip list (Decision 14): [{ title: "Cells · Gated", opts: [...] }]. Legacy/ungrouped
// callers get a single untitled group. `popMultiOptions` is the flat union, for the count guard.
const popMultiGroups = ref<PopGroupDef[]>([])
const popMultiOptions = computed(() => popMultiGroups.value.flatMap(g => g.opts))

const popAcross = computed(() => props.param.type === 'popSelection'
  && (props.param.multiple === true || props.param.acrossSegmentations === true))

function flattenPopTree(nodes: PopNode[] | undefined, parent: string, out: string[]) {
  for (const n of nodes ?? []) {
    const path = parent === '' ? `/${n.name}` : `${parent}/${n.name}`
    out.push(path)
    flattenPopTree(n.children, path, out)
  }
}

async function fetchPopPaths(img: CciaImage, projectUid: string, vn: string, popType: string): Promise<string[]> {
  try {
    const q = `projectUid=${projectUid}&imageUid=${img.uid}&valueName=${encodeURIComponent(vn)}&popType=${popType}`
    const res = await fetch(`/api/gating/popmap?${q}`)
    if (!res.ok) return []
    const d = await res.json() as { tree?: { populations?: PopNode[] } }
    const paths: string[] = []
    flattenPopTree(d.tree?.populations, '', paths)
    return paths
  } catch { return [] }
}

interface PopGroup { valueName: string; populations: { path: string; name: string; colour?: string; popType?: string; granularity?: string; category?: string }[] }

async function loadPops() {
  if (props.param.type !== 'popSelection') return
  const img = props.context?.images?.[0]
  const projectUid = props.context?.projectUid
  // popScope (cells|tracks) is the defined module-function scope; the backend resolves sources +
  // cell/track filtering + existence-checked roots. Falls back to the raw popType picker when absent.
  // accepts (explicit pop_type allow-list, Decision 14) supersedes popScope; both resolve the
  // sources + cell/track tagging server-side. popType is the legacy raw path.
  const accepts = props.param.accepts
  const popScope = props.param.popScope
  const popType = props.param.popType ?? (popScope === 'cells' ? 'flow' : popScope === 'tracks' ? 'live' : 'flow')
  if (popAcross.value) {
    // populations across every segmentation, value_name-prefixed (incl. the derived /_tracked),
    // grouped by "<granularity> · <category>" (the tags the backend now sends).
    popMultiGroups.value = []
    if (!img || !projectUid) return
    try {
      let q = `projectUid=${projectUid}&imageUid=${img.uid}`
      let structured = true
      if (accepts && accepts.length) {
        q += `&accepts=${encodeURIComponent(accepts.join(','))}`
      } else if (popScope) {
        q += `&popScope=${popScope}`
        if (props.param.includeClusters === false) q += `&includeClusters=false`
      } else {
        q += `&popType=${popType}`; structured = false
      }
      const res = await fetch(`/api/plots/populations?${q}`)
      if (!res.ok) return
      const groups = await res.json() as PopGroup[]
      if (structured) {
        // accepts / popScope: backend tags each pop (granularity/category) AND returns existence-checked
        // roots (all-cells "/" for cells, the guarded "/_tracked" for tracks) — group them under headers.
        popMultiGroups.value = groupPopulations(groups as RawGroup[])
      } else {
        // legacy popType path: fabricate the whole-segmentation root client-side, one untitled group.
        const opts: { label: string; value: string }[] = []
        for (const g of groups) {
          if (props.param.includeRoot) opts.push({ label: `${g.valueName} · all`, value: `${g.valueName}/` })
          for (const p of g.populations) opts.push({ label: `${g.valueName}${p.path}`, value: `${g.valueName}${p.path}` })
        }
        popMultiGroups.value = opts.length ? [{ title: '', opts }] : []
      }
    } catch { /* gating may not exist yet */ }
    return
  }
  // single mode — scoped to the sibling segmentation, found by TYPE (a spec may call it anything)
  const valueName = scopeValueName(props.context?.params, props.context?.values,
                                   Object.keys(img?.labels ?? {}))
  popOptions.value = [{ label: 'NONE (whole segmentation)', value: 'NONE' }]
  if (!img || !projectUid) return
  popOptions.value.push(...(await fetchPopPaths(img, projectUid, valueName, popType)).map(p => ({ label: p, value: p })))
}

// reload when the image or the chosen segmentation changes
watch(() => [props.context?.images?.[0]?.uid,
             scopeValueName(props.context?.params, props.context?.values,
                            Object.keys(props.context?.images?.[0]?.labels ?? {}))],
  () => { loadPops() }, { immediate: true })

// Multi-select chip lists (pops / measure cols / channels) all edit the same flat string[] `val`.
// A GROUPED list edits only its own slice, so on update merge the group's new selection back with the
// other groups' selections, reconstructed in the full option order so the array stays stable.
function chipArr(): string[] { return Array.isArray(val.value) ? (val.value as string[]) : [] }
function chipGroupSel(groupValues: string[]): string[] { const cur = chipArr(); return groupValues.filter(v => cur.includes(v)) }
function chipGroupUpdate(allValues: string[], groupValues: string[], next: string[]) {
  const keep = new Set([...chipArr().filter(v => !groupValues.includes(v)), ...next])
  emit('update:modelValue', allValues.filter(v => keep.has(v)))
}
const popAllValues = computed(() => popMultiOptions.value.map(o => o.value))

// The value_name a measure picker reads from: the segmentation of the first SELECTED population
// (pops carry it as a prefix, "A/_tracked" → "A"); falls back to a sibling valueName, else the
// image's first segmentation. This is why the picker now lists A/B/C measures (and HMM-state
// columns) correctly — previously it was hardcoded to "default", which the tracked sets don't have.
function resolveColValueName(): string {
  return scopeValueName(props.context?.params, props.context?.values,
                        Object.keys(props.context?.images?.[0]?.labels ?? {}))
}

// Map a var intensity column to its channel name, mirroring Julia `_channel_label`
// (population_manager.jl): "mean_intensity_0" → channelNames[0]; "nuc_mean_intensity_0" → "nuc_<name>".
// The stored VALUE stays the raw column; only the displayed LABEL changes.
function channelLabel(col: string, chans: string[]): string {
  const m = col.match(/^(?:([a-z]+)_)?(?:mean|median)_intensity_(\d+)$/)
  if (!m) return col
  const idx = parseInt(m[2], 10)
  if (idx >= chans.length) return col
  return m[1] ? `${m[1]}_${chans[idx]}` : chans[idx]
}

// labelPropsColsSelection: multi-select of per-cell measure columns for the chosen image +
// segmentation. Source: /api/gating/channels. Grouped by family via the shared `measureGroups`
// (utils/measureGroups.ts — also the gate axis pickers and the population manager): whole-track
// motility, then Morphology and Channels (the intensities, by CHANNEL NAME), then Behaviour (obs
// `live.*`). The one "Object measures" heading used to hold the shape descriptors AND the markers.
// `trimPrefix` collapses to one flat group filtered to that prefix with the label trimmed (the
// transitions HMM-state picker shows just the suffix).
const COL_DENYLIST = new Set(['label', 'track_id', 'track_parent', 'track_root', 'track_state',
  'track_generation', 'cell_id', 'pop', 'value_name'])
const colGroups = ref<{ title: string; opts: { label: string; value: string }[] }[]>([])

async function loadCols() {
  if (props.param.type !== 'labelPropsColsSelection') return
  const img = props.context?.images?.[0]
  const projectUid = props.context?.projectUid
  const valueName = resolveColValueName()
  // popType matters: 'track'/'trackclust' returns the PER-TRACK feature universe (whole-track
  // motility + aggregatable cell measures), not the cell columns. Was hardcoded to flow, so the
  // track-clustering picker never showed the whole-track measures.
  const popType = props.param.popType ?? 'flow'
  colGroups.value = []
  if (!img || !projectUid) return
  try {
    const q = `projectUid=${projectUid}&imageUid=${img.uid}&valueName=${encodeURIComponent(valueName)}&popType=${popType}`
    const res = await fetch(`/api/gating/channels?${q}`)
    if (!res.ok) return
    const d = await res.json() as { columns?: string[]; obsColumns?: string[]; channelNames?: string[]
      channels?: string[]; cellMeasures?: string[]; cellObsMeasures?: string[]; trackAggregates?: string[] }
    const vars = (d.columns ?? []).filter(c => !COL_DENYLIST.has(c))
    const obs = (d.obsColumns ?? []).filter(c => !COL_DENYLIST.has(c))
    const chans = d.channelNames ?? []
    const trim = props.param.trimPrefix ?? ''
    if (trim) {
      const seen = new Set<string>()
      const opts = [...obs, ...vars].filter(c => c.startsWith(trim) && !seen.has(c) && seen.add(c))
        .map(c => ({ label: c.slice(trim.length), value: c }))
      colGroups.value = opts.length ? [{ title: '', opts }] : []
      return
    }
    // Both branches pick BASE measures (like old R); for tracks the task aggregates each to ALL
    // per-track stats (mean/median/…; categorical → frequencies) automatically — so we DON'T list
    // *.mean/*.median. The FAMILIES and their order come from the shared `measureGroups`, so Cluster
    // cells and Cluster tracks read consistently: the only difference is that Cluster tracks
    // additionally has a "Track measures" (whole-track motility) group.
    //  • track: motility in `columns`, the per-track-aggregatable cell vars in `cellMeasures`
    //  • flow:  the cell vars are `columns`; behaviour is the cell obs `live.*`
    const track = popType === 'track' || popType === 'trackclust'
    const groups = measureGroups(track
      ? { trackColumns: vars, columns: (d.cellMeasures ?? []).filter(c => !COL_DENYLIST.has(c)),
          channels: d.channels,
          obsColumns: (d.cellObsMeasures ?? []).filter(c => !COL_DENYLIST.has(c) && c.startsWith('live.')) }
      : { columns: vars, channels: d.channels, obsColumns: obs.filter(c => c.startsWith('live.')) })
    colGroups.value = groups.map(grp => ({
      title: grp.title, opts: grp.cols.map(c => ({ label: channelLabel(c, chans), value: c })) }))
  } catch { /* no columns available yet */ }
}

// reload when the image, the sibling valueName, or the selected populations change
watch(() => [props.context?.images?.[0]?.uid, props.context?.values?.valueName,
             JSON.stringify(props.context?.values?.pops)],
  () => { loadCols() }, { immediate: true })

const colAllValues = computed(() => colGroups.value.flatMap(g => g.opts).map(o => o.value))

// ── param advisory (generic) ───────────────────────────────────────────────────────────────────
// A one-line "know this before you run" readout under the control, for params that are easy to set
// wrongly. The judgement lives in `paramAdvisors.ts` (pure + unit-tested); this only fetches when the
// advisor needs the backend, and renders. Was hand-rolled per-param for motionDimsSelection; adding
// the anisotropy grid-size readout would have been a second copy, so it is one mechanism now.
const advisor = computed(() => paramAdvisor(props.param))
const advisory = ref<ParamAdvisory | null>(null)
const advisoryLoading = ref(false)

const advisoryCtx = computed<AdvisorContext>(() => ({
  projectUid: props.context?.projectUid,
  images: props.context?.images,
  values: props.context?.values,
}))

// Coalesced, latest-wins (docs/UI.md → "Continuous controls"): an advisor may hit the backend
// (`/api/tracking/motion-dims`, `/api/images/geometry`) and its trigger is a SLIDER, which emits an
// event per pixel of travel. The old hand-rolled sequence token discarded the stale REPLIES but still
// sent every request, so a drag fired a request per step at the API. `debouncedLatest` collapses the
// burst into one, never runs two at once, and hands the run an `isCurrent()` for the same stale guard.
const advisoryRun = debouncedLatest<{
  advisor: ParamAdvisor | undefined; value: unknown; ctx: AdvisorContext; param: AdvisorParam
}>(
  async ({ advisor: a, value, ctx, param }, isCurrent) => {
    if (!a) return
    advisoryLoading.value = true
    try {
      const r = await a.advise(value, ctx, param)
      if (isCurrent()) advisory.value = r
    } finally {
      if (isCurrent()) advisoryLoading.value = false
    }
  },
  { wait: 200, onError: () => { advisoryLoading.value = false } },
)
function loadAdvisory() {
  advisory.value = null           // clear immediately — a stale readout is worse than none
  advisoryRun.cancel()            // …and stop an in-flight run applying its (now older) answer over it
  advisoryLoading.value = false   // a cancelled run skips its own `finally`, so reset the flag here
  if (!advisor.value) return
  // the param goes WITH the run, not read at apply time: a type-registered advisor behaves
  // differently per param (`valueNameSelection`'s `field`), so a stale reply must not be shaped by
  // whichever param the renderer happens to be on when it lands
  advisoryRun.schedule({
    advisor: advisor.value, value: val.value, ctx: advisoryCtx.value, param: props.param,
  })
}
// `val` is in the key list on purpose: an async advisor still depends on the CURRENT value (the grid
// estimate changes as the slider moves), and `reloadOn` only covers the context. The fetch itself is
// the expensive part, so advisors whose fetch does not depend on the value should cache — today both
// are cheap enough (one metadata read) that correctness wins over a caching layer nobody needs yet.
watch(() => [props.param.key, val.value, advisor.value?.reloadOn?.(advisoryCtx.value)],
  () => { loadAdvisory() }, { immediate: true, deep: true })
onUnmounted(() => advisoryRun.cancel())

// The order row over this group's own entries. Its value does NOT live in `val` — that is the
// group's entries — but in a sibling key, `<groupKey>Order`, which `_apply_group_order` (Julia)
// resolves away before any runner sees it. An unset value means "all of them, in entry order": a
// task saved before this control existed, a chain node and a REPL call all carry nothing, and each
// must keep running everything (see `groupOrderKeys`).
const orderKey = computed(() => `${props.param.key}Order`)

const groupOrderOptions = computed(() => groupEntries.value.map((e, i) => {
  const raw = props.param.labelKey ? e.vals[props.param.labelKey] : undefined
  const named = Array.isArray(raw) ? raw[0] : raw
  return { value: e.key, label: named ? String(named) : `${i + 1}` }
}))

// µm per pixel for the strip's pixel captions — only when every selected image AGREES on it. A batch
// spanning two objectives has no single answer, and printing one of them would state a scale that is
// wrong for the others; the strip falls back to form units and says so.
const groupPxSize = computed<number | null>(() => {
  const sizes = (props.context?.images ?? [])
    .map(i => i.physicalSizeX).filter((v): v is number => typeof v === 'number' && v > 0)
  if (!sizes.length) return null
  return sizes.every(v => v === sizes[0]) ? sizes[0] : null
})

// The figure is FLOATING, not inline. Eleven rows above the entry list pushed the whole form down
// and was the first thing Dominik said about it — a reference you consult while tuning wants to sit
// beside the controls, not between them. `FloatingPanel` remembers where you put it.
const figureOpen = ref(false)
const groupVis = computed(() =>
  paramVisColumns(props.param, (val.value as GroupValues) ?? {}, groupOrderValue.value,
                  groupPxSize.value))
const groupVisNote = computed(() => uniformWarning(groupVis.value))

const groupVisHeadings = computed<string[]>(() =>
  groupOrderValue.value.map(k => {
    const raw = props.param.labelKey ? (val.value as GroupValues)?.[k]?.[props.param.labelKey] : undefined
    const named = Array.isArray(raw) ? raw[0] : raw
    // The same heading the entry itself carries, so the strip and the list cannot disagree about
    // which column is which.
    return named ? String(named) : String(Number(k) + 1)
  }))

const groupOrderValue = computed<string[]>(() =>
  groupOrderKeys(Object.fromEntries(groupEntries.value.map(e => [e.key, e.vals])),
                 props.context?.values?.[orderKey.value]))

// group helpers — value is Record<string, ParamValues> keyed by "0", "1", ...
const groupEntries = computed(() => {
  if (props.param.type !== 'group') return []
  const v = (val.value ?? {}) as GroupValues
  return Object.keys(v).sort((a, b) => Number(a) - Number(b)).map(k => ({ key: k, vals: v[k] ?? {} }))
})

function addGroupEntry() {
  const v = { ...((val.value ?? {}) as GroupValues) }
  const nextKey = String(groupEntries.value.length === 0
    ? 0
    : Math.max(...groupEntries.value.map(e => Number(e.key))) + 1)
  // POSITION, not key: what a new entry should start as depends on where it sits in the run order,
  // and after a removal the next key is not the next position. See `newEntryDefaults` for why a
  // second entry must not be born as a copy of the first.
  v[nextKey] = newEntryDefaults(props.param, groupEntries.value.length,
                                groupEntries.value[0]?.vals)
  val.value = v
}

// Section open/close state inside group entries — keyed [entryKey][sectionKey]
const groupSectionStates = ref<Record<string, Record<string, boolean>>>({})

function isGroupSectionOpen(entryKey: string, sectionKey: string, collapsed: boolean): boolean {
  return groupSectionStates.value[entryKey]?.[sectionKey] ?? !collapsed
}

function toggleGroupSection(entryKey: string, sectionKey: string, collapsed: boolean) {
  const cur = isGroupSectionOpen(entryKey, sectionKey, collapsed)
  groupSectionStates.value = {
    ...groupSectionStates.value,
    [entryKey]: { ...(groupSectionStates.value[entryKey] ?? {}), [sectionKey]: !cur },
  }
}

function removeGroupEntry(key: string) {
  const v = { ...((val.value ?? {}) as GroupValues) }
  delete v[key]
  val.value = v
}

function updateGroupEntry(entryKey: string, paramKey: string, newVal: unknown) {
  const v = (val.value ?? {}) as GroupValues
  val.value = { ...v, [entryKey]: { ...(v[entryKey] ?? {}), [paramKey]: newVal } }
}

// channelSelection toggle helpers
const channelOptions = computed<ChipOption[]>(() => availableChannels.value.map(ch => ({ value: ch, label: ch })))
// Per-option guidance for a `select` — see `utils/optionHelp.ts` for why this is not a `tip` and not
// an advisory.
const optionHelp = computed(() =>
  props.param.type === 'select' ? selectedOptionHelp(props.param.options, val.value) : '')

// channelSelection stores an array even when single (`multiple === false`) — so route through a handler
// that keeps only the newly-added value in the single case, preserving the old replace-on-click behaviour.
function onChannelUpdate(next: string[]) {
  if (props.param.multiple === false) {
    const added = next.find(v => !chipArr().includes(v))
    val.value = added ? [added] : []
  } else {
    val.value = next
  }
}

// range for slider display
const pct = computed(() => {
  if (props.param.type !== 'int' && props.param.type !== 'float') return 0
  const min = props.param.min ?? 0
  const max = props.param.max ?? 100
  return (((val.value as number) - min) / (max - min)) * 100
})
</script>

<template>
  <!-- `hidden` — a param the task itself has ruled out for the CURRENT form state, so it renders
       nowhere rather than sitting there empty and looking broken. Set by `_inject_dynamic_options!`,
       which already re-runs on every `triggersOptions` edit and already sees the form, so nothing new
       has to reach the frontend. The driving case: pick a TrackMate XML and the "Column mapping"
       section had no columns to offer — because that export has none — but still drew five empty
       dropdowns. "Not applicable" and "failed to load" looked identical.

       Guarded here, at the component root, so it holds for every caller at once: TaskRunner's list,
       ChainModule's list, and section/group sub-params, which each iterate separately. -->
  <template v-if="notApplicable" />

  <!-- Not for `section`/`group`: each renders its own heading below (the collapsible's toggle, the
       group's title), so the generic row put the label on screen TWICE — a plain "Advanced" sitting
       above a collapsible headed "ADVANCED". They are siblings of this row, not children of it, so
       the row was contributing a duplicate label and an empty body. -->
  <div v-else-if="param.type !== 'section' && param.type !== 'group'" class="param-row">
    <label class="param-label" v-tooltip.left="param.tip">
      {{ param.label }}
      <i v-if="param.tip" class="pi pi-info-circle tip-icon" />
    </label>

    <!-- int / float → slider + number display -->
    <div v-if="param.type === 'int' || param.type === 'float'" class="slider-wrap">
      <input
        type="range"
        class="slider"
        :min="param.min ?? 0"
        :max="param.max ?? 100"
        :step="param.step ?? (param.type === 'int' ? 1 : 0.01)"
        :value="val as number"
        @input="val = param.type === 'int'
          ? parseInt(($event.target as HTMLInputElement).value)
          : parseFloat(($event.target as HTMLInputElement).value)"
        :style="`--pct: ${pct}%`"
        v-tooltip.right="`${val} (range ${param.min}–${param.max})`"
      />
      <span class="slider-val">{{ val }}</span>
    </div>

    <!-- bool → shared toggle switch. No `v-tooltip`: `param-label` above already carries `param.tip`
         on its info icon, and repeating it here renders the tooltip ON TOP of the switch you were
         about to click. See `HEADING_COVERED` / `duplicateTooltips` in utils/uiCopy.ts. -->
    <CcToggle v-else-if="param.type === 'bool'" :aria-label="param.label"
      :model-value="val as boolean" @update:model-value="val = $event" />

    <!-- text — with SUGGESTIONS when the spec carries options (a datalist, so the field stays free
         text). Deliberately not a `select`/`chipSelect`: those validate the value against the spec's
         options (task.jl), and options injected from the current form are absent at validation time
         (`_task_spec` resolves without form state), so a picked value would fail to validate. This is
         the same shape as `valueNameInput`'s suggestions — offer, never constrain. -->
    <template v-else-if="param.type === 'text'">
      <input
        type="text" class="text-input"
        :value="val as string"
        :list="param.options?.length ? `dl-${param.key}` : undefined"
        @input="val = ($event.target as HTMLInputElement).value"
        v-tooltip.right="param.tip"
      />
      <datalist v-if="param.options?.length" :id="`dl-${param.key}`">
        <option v-for="o in param.options" :key="o.value" :value="o.value" />
      </datalist>
    </template>

    <!-- valueNameInput: the name this task WRITES under. Free text, with the names already in that
         namespace offered as you type. `valueNameSelection` (a strict <select>) is the INPUT-side
         twin: correct where the name must already exist, wrong here because you could never name a
         new one. -->
    <SuggestInput v-else-if="param.type === 'valueNameInput'"
      :model-value="(val as string) ?? ''"
      :options="availableValueNames"
      :placeholder="param.placeholder"
      :tip="param.tip"
      mark-existing
      @update:model-value="val = $event"
      @change="emit('commit', param.key, ($event.target as HTMLInputElement).value)"
    />

    <!-- dirPath: a folder on the machine running the server. Still typeable — a remembered path is
         faster to paste than to browse to — but Browse opens the shared FileBrowser in dir mode, the
         same picker the .ccbundle project export uses. A destination that has to be typed exactly is
         a task that fails after doing all its work. -->
    <div v-else-if="param.type === 'dirPath'" class="cc-row cc-row-tight dir-path">
      <input type="text" class="text-input" :value="val as string" :placeholder="param.placeholder"
        @input="val = ($event.target as HTMLInputElement).value"
        v-tooltip.right="param.tip" />
      <button type="button" class="cc-btn cc-btn-ghost" @click="showDirBrowser = true"
        v-tooltip.top="'Browse for a folder'">
        <i class="pi pi-folder-open" />
      </button>
    </div>

    <!-- filePath: one file on the machine running the server. Same shape as dirPath — still typeable,
         because a remembered path is faster to paste than to browse to — but Browse opens the shared
         FileBrowser in file mode, filtered to the param's `extensions`. A path that has to be typed
         exactly is a task that fails after the user has filled in everything else. -->
    <div v-else-if="param.type === 'filePath'" class="cc-row cc-row-tight dir-path">
      <input type="text" class="text-input" :value="val as string" :placeholder="param.placeholder"
        @input="val = ($event.target as HTMLInputElement).value"
        v-tooltip.right="param.tip" />
      <button type="button" class="cc-btn cc-btn-ghost" @click="showFileBrowser = true"
        v-tooltip.top="'Browse for a file'">
        <i class="pi pi-folder-open" />
      </button>
    </div>

    <!-- chipSelect: multi-pick from a fixed set. A raw text field for something like "1,2,4,8" is a
         parse error waiting to happen and reads as unfinished; ChipSelect is the canonical primitive
         for "pick from a set" (docs/UI.md). -->
    <ChipSelect v-else-if="param.type === 'chipSelect'"
      :options="(param.options ?? []).map(o => ({ value: String(o.value), label: o.label }))"
      :model-value="(Array.isArray(val) ? val : []).map(String)"
      multiple select-all
      :aria-label="param.label"
      @update:model-value="v => val = v as string[]"
    />

    <!-- select, as a segmented control. Same param type and so the SAME validation (value ∈ options)
         — only the rendering differs, opted into per param with `variant: "chips"`. For a short,
         closed set the chips show every choice at once, where a dropdown hides all but one and makes
         a binary look like a list that might be long. Kept opt-in rather than auto-applied by option
         count, so no existing dropdown silently changes shape. -->
    <ChipSelect v-else-if="param.type === 'select' && param.variant === 'chips'"
      variant="segmented"
      :options="(param.options ?? []).map(o => ({ value: String(o.value), label: o.label }))"
      :model-value="String(val ?? '')"
      :aria-label="param.label"
      @update:model-value="v => val = v as string"
    />

    <!-- select -->
    <select v-else-if="param.type === 'select'"
      class="select-input"
      :value="val as string"
      @change="val = ($event.target as HTMLSelectElement).value"
      v-tooltip.right="param.tip"
    >
      <option v-for="opt in param.options" :key="opt.value" :value="opt.value">
        {{ opt.label }}
      </option>
    </select>

    <!-- valueNameSelection: dropdown of available filepath keys from selected images -->
    <select v-else-if="param.type === 'valueNameSelection'"
      class="select-input"
      :value="val as string"
      @change="val = ($event.target as HTMLSelectElement).value"
      v-tooltip.right="param.tip"
    >
      <option v-for="name in availableValueNames" :key="name" :value="name">{{ name }}</option>
      <option v-if="availableValueNames.length === 0" value="" disabled>— no versions available —</option>
    </select>

    <!-- popSelection (multi / across segmentations): chip list of value_name-prefixed populations -->
    <div v-else-if="param.type === 'popSelection' && popAcross" class="channel-select-wrap"
      v-tooltip.right="param.tip">
      <div v-if="popMultiOptions.length === 0" class="channel-empty cc-muted">
        No populations — select an image first.
      </div>
      <div v-for="grp in popMultiGroups" v-else :key="grp.title" class="col-group">
        <div v-if="grp.title" class="col-group-title cc-eyebrow cc-fs-2xs">{{ grp.title }}</div>
        <ChipSelect multiple select-all :options="grp.opts"
          :model-value="chipGroupSel(grp.opts.map(o => o.value))"
          @update:model-value="v => chipGroupUpdate(popAllValues, grp.opts.map(o => o.value), v as string[])" />
      </div>
    </div>

    <!-- popSelection (single): NONE (whole segmentation) + flow population paths for this image -->
    <select v-else-if="param.type === 'popSelection'"
      class="select-input"
      :value="(val as string) ?? 'NONE'"
      @change="val = ($event.target as HTMLSelectElement).value"
      v-tooltip.right="param.tip"
    >
      <option v-for="opt in popOptions" :key="opt.value" :value="opt.value">{{ opt.label }}</option>
    </select>

    <!-- labelPropsColsSelection: grouped (Tracking / Object) multi-select chip lists -->
    <div v-else-if="param.type === 'labelPropsColsSelection'" class="channel-select-wrap"
      v-tooltip.right="param.tip">
      <div v-if="colGroups.length === 0" class="channel-empty cc-muted">
        No measures — select a population first.
      </div>
      <div v-for="g in colGroups" :key="g.title" class="col-group">
        <div v-if="g.title" class="col-group-title cc-eyebrow cc-fs-2xs">{{ g.title }}</div>
        <ChipSelect multiple select-all :options="g.opts"
          :model-value="chipGroupSel(g.opts.map(o => o.value))"
          @update:model-value="v => chipGroupUpdate(colAllValues, g.opts.map(o => o.value), v as string[])" />
      </div>
    </div>

    <!-- motionDimsSelection: auto/2D/3D + the auto recommendation & warning -->
    <div v-else-if="param.type === 'motionDimsSelection'" class="motion-dims">
      <select class="select-input" :value="(val as string) ?? 'auto'"
              @change="val = ($event.target as HTMLSelectElement).value"
              v-tooltip.right="param.tip">
        <option value="auto">Auto (recommended)</option>
        <option value="2D">2D (in-plane)</option>
        <option value="3D">3D</option>
      </select>
    </div>

    <!-- channelSelection: togglable chip list from image context -->
    <!-- No tooltip on the wrapper: it anchors over the CHIPS, hiding the things you are about to
         click, and `param-label` above already carries the same tip on its info icon. See
         `HEADING_COVERED` in utils/uiCopy.ts. -->
    <div v-else-if="param.type === 'channelSelection'" class="channel-select-wrap">
      <div v-if="availableChannels.length === 0" class="channel-empty cc-muted">
        No channels — select images first.
      </div>
      <ChipSelect v-else multiple select-all :options="channelOptions"
        :model-value="chipArr()" @update:model-value="v => onChannelUpdate(v as string[])" />
    </div>

    <!-- fallback -->
    <div v-else class="picker-placeholder"
      v-tooltip.right="`${param.type} — populated from image metadata`">
      <i class="pi pi-spinner pi-spin" style="font-size:var(--cc-fs-xs)" />
      {{ param.type }}
    </div>

    <!-- ONE advisory block for every param that registers an advisor (paramAdvisors.ts). A note
         under the control, whatever the control is.
         MUST sit AFTER the widget chain closes, never inside it: a `v-if` placed mid-chain starts a
         NEW chain, so every `v-else-if`/`v-else` below it re-parents onto this condition — which is
         exactly what happened, and the `v-else` fallback above then rendered its "unsupported type"
         spinner under every param on the page. -->
    <div v-if="advisoryLoading" class="param-advisory cc-muted">checking…</div>
    <!-- `InlineNote` hangs the tooltip off the TEXT, not the row, which is what the data-quality flag
         in the slot needs: a row-level tip fires on top of the flag's own (docs/UI.md → nested tooltips) -->
    <InlineNote v-else-if="advisory" class="param-advisory"
                :severity="advisory.severity" :short="advisory.message" :detail="advisory.tip">
      <!-- optional second signal: how good the DATA is, as distinct from how concerning the
           recommendation is. Own colour + own tooltip; colour is never the only cue. -->
      <i v-if="advisory.flag" class="pi param-advisory-flag" :class="SEVERITY[advisory.flag.severity].icon"
         :style="{ color: SEVERITY[advisory.flag.severity].color }"
         v-tooltip.right="advisory.flag.tip" />
    </InlineNote>
    <!-- Per-OPTION guidance for a select: what this choice means and when to pick it. Deliberately NOT
         an advisory — nothing about the user's data was consulted, and borrowing `severity: ok` would
         render a green check claiming a verdict nobody reached. -->
    <InlineNote v-if="optionHelp" class="param-advisory" :short="optionHelp" placement="bottom" />
  </div>

  <!-- section rendered outside .param-row so it spans full width -->
  <div v-if="param.type === 'section' && !notApplicable" class="param-section">
    <button class="section-toggle cc-section-toggle cc-eyebrow cc-fs-sm" @click="sectionOpen = !sectionOpen"
      v-tooltip.left="sectionOpen ? 'Collapse advanced parameters' : 'Expand advanced parameters'">
      <i :class="['pi', sectionOpen ? 'pi-chevron-down' : 'pi-chevron-right']" />
      {{ param.label }}
    </button>
    <div v-if="sectionOpen" class="section-body">
      <ParamRenderer
        v-for="p in param.params"
        :key="p.key"
        :param="p"
        :modelValue="(val as ParamValues)?.[p.key]"
        @update:modelValue="val = { ...(val as ParamValues ?? {}), [p.key]: $event }"
        :context="context"
        @commit="(k, v) => emit('commit', k, v)"
        @update:groupOrder="(k, v) => emit('update:groupOrder', k, v)"
      />
    </div>
  </div>

  <!-- group: repeatable set of sub-params keyed by string index -->
  <div v-if="param.type === 'group' && !notApplicable" class="param-group">
    <div class="group-header">
      <span class="group-title cc-eyebrow cc-fs-sm">{{ param.label }}</span>
      <!-- Both buttons in ONE right-aligned group. The header is `space-between`, so as separate
           children a second button parked the first in the middle of the row — reading as a label
           with a stray control after it. `+` stays rightmost, where it was when it was alone. -->
      <span class="group-actions cc-row cc-row-tight">
        <button v-if="groupVis.rows.length" class="group-fig-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-micro"
          type="button" :class="{ 'cc-btn-on': figureOpen }" @click="figureOpen = !figureOpen"
          v-tooltip.left="'Show these settings as a figure'">
          <i class="pi pi-chart-bar" />
        </button>
        <button v-if="param.repeatable" class="group-add-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-micro" type="button"
          @click="addGroupEntry()"
          v-tooltip.left="'Add another entry'">
          <i class="pi pi-plus" />
        </button>
      </span>
    </div>

    <!-- Which entries run, and in what order. Every `repeatable` group gets this — it is not a
         spec-authored field, because the reason it exists is a property of repeatable groups
         themselves: entries are applied in turn and each fills only what an earlier one left, so
         the order is semantic and "run this one but not that one" is a per-run choice, not a
         reason to delete an entry and retype its parameters.
         Only from two entries up: with one there is nothing to order, and a lone chip that can be
         switched off is a worse way to say "don't run this task".
         The control is the shared ChipSelect in its multi-select + reorderable mode, which already
         means exactly "an ordered pick". -->
    <!-- The tip is on the LABEL, not on the chips. `docs/ui/COPY.md` has this exact case: a tooltip
         anchored to a `ChipSelect` renders on top of the control, so the hover help hides the thing
         you were about to click — and in a narrow column `.right` flips upward onto the chips, which
         is what Dominik saw. A tipped label preceding it in the same row is how chips are covered. -->
    <div v-if="param.repeatable && groupEntries.length > 1" class="group-order-row cc-row cc-row-tight">
      <span class="group-order-label cc-muted cc-fs-2xs"
        v-tooltip.right="param.entriesTip
          ? `${param.entriesTip}; drag to reorder`
          : 'Drag to reorder'">Order</span>
      <ChipSelect
        class="group-order"
        :options="groupOrderOptions"
        :model-value="groupOrderValue"
        multiple reorderable
        :aria-label="`Order of ${param.label}`"
        @update:model-value="v => emit('update:groupOrder', orderKey, v as string[])"
      />
    </div>

    <!-- HOW the entries combine. Shown, not tucked into a tooltip: a user who adds a second entry has
         no way to guess that the entries are not independent — that the first one claims pixels and
         the second only fills what it left — and the consequence of not knowing is a second pass
         configured like the first, which costs double and contributes almost nothing. Same condition
         as the order row, because with one entry there is nothing to combine. Text comes from the
         spec (`entriesTip`); a task whose entries ARE independent simply omits it and shows no line.
         `InlineNote` is the canonical short-line-plus-reasoning primitive (docs/ui/PRIMITIVES.md);
         an icon plus a span plus a tooltip by hand is the variant it exists to delete. -->
    <InlineNote v-if="param.repeatable && groupEntries.length > 1 && param.entriesTip"
      class="group-entries-note cc-fs-2xs" placement="bottom"
      :short="param.entriesTip"
      detail="Entries are applied in turn, so the first has first claim on every pixel and later ones
              fill only what it left. Two entries configured alike therefore do the same work twice." />

    <!-- A floating figure of what these numbers MEAN, one column per entry. Off by default and
         remembered per user: it is a reference you consult while tuning, so it belongs beside the
         controls rather than wedged between them. Only offered when the spec gives some param a
         `vis` role, so a task with none shows no button. -->
    <FloatingPanel v-if="figureOpen" :title="`${param.label} — at a glance`"
      storage-key="param-figure" icon="pi-chart-bar" :default-w="330" :default-h="420"
      @close="figureOpen = false">
      <VisualAid :vis="groupVis" :headings="groupVisHeadings"
        :note="groupVisNote" note-severity="warn" />
    </FloatingPanel>

    <div v-if="groupEntries.length === 0" class="group-empty cc-muted">
      No entries — click + to add one.
    </div>

    <div v-for="entry in groupEntries" :key="entry.key" class="group-entry">
      <div class="group-entry-hdr">
        <span class="group-entry-num">
          {{ param.labelKey && (entry.vals[param.labelKey] as string[])?.[0]
             ? (entry.vals[param.labelKey] as string[])[0]
             : Number(entry.key) + 1 }}
        </span>
        <button v-if="param.repeatable && groupEntries.length > 1"
          class="group-remove-btn" type="button"
          @click="removeGroupEntry(entry.key)"
          v-tooltip.right="'Remove this entry'">
          <i class="pi pi-times" />
        </button>
      </div>
      <div class="group-entry-body">
        <template v-for="p in param.params" :key="p.key">
          <!-- Section inside group: collapsible visual box, flat data storage -->
          <template v-if="p.type === 'section'">
            <button
              class="group-section-toggle cc-section-toggle cc-eyebrow"
              @click="toggleGroupSection(entry.key, p.key, p.collapsed ?? false)"
            >
              <i :class="['pi', isGroupSectionOpen(entry.key, p.key, p.collapsed ?? false)
                ? 'pi-chevron-down' : 'pi-chevron-right']" />
              {{ p.label }}
            </button>
            <div
              v-if="isGroupSectionOpen(entry.key, p.key, p.collapsed ?? false)"
              class="group-section-body"
            >
              <ParamRenderer
                v-for="sp in p.params"
                :key="sp.key"
                :param="sp"
                :modelValue="entry.vals[sp.key]"
                @update:modelValue="updateGroupEntry(entry.key, sp.key, $event)"
                :context="context"
        @commit="(k, v) => emit('commit', k, v)"
        @update:groupOrder="(k, v) => emit('update:groupOrder', k, v)"
              />
            </div>
          </template>
          <ParamRenderer
            v-else
            :param="p"
            :modelValue="entry.vals[p.key]"
            @update:modelValue="updateGroupEntry(entry.key, p.key, $event)"
            :context="context"
        @commit="(k, v) => emit('commit', k, v)"
        @update:groupOrder="(k, v) => emit('update:groupOrder', k, v)"
          />
        </template>
      </div>
    </div>
  </div>

  <!-- dirPath: teleported to <body>. BaseModal is `position: fixed`, which is enough at the page
       level (ManageImagesModule mounts its browser outside ModuleLayout for exactly this reason) but
       NOT from here: a param row sits deep inside the scrolled task form, and any ancestor with a
       transform/filter/will-change becomes the containing block for a fixed child and traps it.
       Teleport removes the dependency on what happens to be above this row. -->
  <Teleport to="body">
    <FileBrowser v-if="showFileBrowser" mode="file" :extensions="param.extensions ?? []"
      :title="param.label ? `Select ${param.label.toLowerCase()}` : ''"
      @select="(paths: string[]) => { if (paths[0]) val = paths[0]; showFileBrowser = false }"
      @close="showFileBrowser = false" />
    <FileBrowser v-if="showDirBrowser" mode="dir"
      @select="(paths: string[]) => { if (paths[0]) val = paths[0]; showDirBrowser = false }"
      @close="showDirBrowser = false" />
  </Teleport>

</template>

<style scoped>
/* The order chips sat flush against the group heading above and the first entry below, with no rule
   of their own — they read as part of whichever neighbour you looked at first. */
.group-order-row { margin: 0.35rem 0 0.5rem; }
.group-order-label { flex: 0 0 auto; }
.group-entries-note { margin: 0 0 0.4rem; }
.param-row {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  padding: 0.5rem 0;
  border-bottom: 1px solid var(--cc-border);
}
.param-row:last-child { border-bottom: none; }

.param-label {
  font-size: var(--cc-fs-sm);
  font-weight: 500;
  color: var(--cc-text-dim);
  display: flex;
  align-items: center;
  gap: 0.3rem;
  cursor: default;
}
.tip-icon { font-size: var(--cc-fs-2xs); opacity: 0.6; }

/* slider */
.slider-wrap {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}
.slider {
  flex: 1;
  appearance: none;
  height: 4px;
  border-radius: var(--cc-radius-xs);
  background: linear-gradient(to right,
    var(--cc-accent) 0%, var(--cc-accent) var(--pct, 0%),
    var(--cc-surface-2) var(--pct, 0%), var(--cc-surface-2) 100%);
  cursor: pointer;
}
.slider::-webkit-slider-thumb {
  appearance: none;
  width: 13px; height: 13px;
  border-radius: var(--cc-radius-pill);
  background: var(--cc-accent);
  cursor: pointer;
  box-shadow: 0 0 4px #a78bfa55;
}
.slider-val {
  font-size: var(--cc-fs-sm);
  font-family: var(--cc-mono);
  color: var(--cc-text);
  min-width: 28px;
  text-align: right;
}

/* bool → shared CcToggle switch (see components/CcToggle.vue) */

/* text / select — visual styling comes from the global form base (style.css) */
.text-input, .select-input { width: 100%; }

/* placeholder */
/* section */
.param-section { border-bottom: 1px solid var(--cc-border); }
/* + cc-section-toggle (row) + cc-eyebrow (label tier). Only the padding and the one-step-larger
   size are this site's: an advanced-params header sits above body text, not inside dense chrome. */
.section-toggle { padding: 0.45rem 0; }
.section-body { padding-left: 0.5rem; border-left: 2px solid var(--cc-border); margin-left: 0.25rem; }

.picker-placeholder {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  font-size: var(--cc-fs-sm);
  color: var(--cc-text-dim);
  background: var(--cc-surface-2);
  border: 1px dashed var(--cc-border);
  border-radius: var(--cc-radius-sm);
  padding: 0.35rem 0.5rem;
}

/* channel selection */
.channel-select-wrap { width: 100%; }
.channel-empty { font-style: italic; padding: 0.2rem 0; }
/* Generic param advisory note. Colour comes from the validated severity palette and is never the
   sole cue — a shape-distinct icon rides along (see lib/severity.ts). */
/* no font-size here: `.cc-muted` on the element already sets --cc-fs-sm, and repeating it is a
   no-op the cssScenarios shadowing detector (rightly) fails on. */
/* layout only — `InlineNote` owns the icon/text/gap and the severity colour */
.param-advisory { display: flex; }
.param-advisory-flag { margin-left: 0.1rem; }

/* motion-dims selector + recommendation note (gap keeps the note off the dropdown) */
.motion-dims { display: flex; flex-direction: column; gap: 0.4rem; width: 100%; }
.md-note { display: inline-flex; align-items: center; gap: 0.3rem; }
.md-note .pi { font-size: var(--cc-fs-sm); }
.md-note.warn { color: #fbbf24; }
/* traffic-light flag: how usable is the z-axis (ok real 3D · warn borderline · fail jitter). A
   shape-distinct severity icon (canonical severity model) — colour is never the sole cue. */
.md-flag { font-size: var(--cc-fs-sm); flex: none; }

.col-group { margin-bottom: 0.4rem; }
.col-group:last-child { margin-bottom: 0; }
.col-group-title { margin: 0.15rem 0 0.25rem; }
/* group */
.param-group {
  border-bottom: 1px solid var(--cc-border);
  padding-bottom: 0.25rem;
}
.group-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.45rem 0 0.3rem;
}

.group-actions { flex: 0 0 auto; }
.group-add-btn { transition: background 0.1s, border-color 0.1s; }   /* + cc-btn cc-btn-ghost cc-btn-icon cc-btn-micro */
.group-add-btn:hover { background: var(--cc-accent); border-color: var(--cc-accent); color: #fff; }
.group-empty { font-style: italic; padding: 0.3rem 0; }
.group-entry {
  margin-bottom: 0.4rem;
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm);
  overflow: hidden;
}
.group-entry-hdr {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.2rem 0.4rem;
  background: var(--cc-surface-2);
  border-bottom: 1px solid var(--cc-border);
}
.group-entry-num {
  font-size: var(--cc-fs-xs);
  font-weight: 700;
  font-family: var(--cc-mono);
  color: var(--cc-accent);
}
.group-remove-btn {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 14px; height: 14px;
  border-radius: var(--cc-radius-pill);
  border: none;
  background: none;
  color: var(--cc-text-dim);
  cursor: pointer;
  font-size: var(--cc-fs-3xs);
  transition: color 0.1s;
}
.group-remove-btn:hover { color: #f87171; }
.group-entry-body { padding: 0 0.4rem; }
.group-entry-body .param-row:last-child { border-bottom: none; }

/* + cc-section-toggle (row) + cc-eyebrow (label tier) — the divider is this site's own */
.group-section-toggle { padding: 0.3rem 0; border-top: 1px solid var(--cc-border); }

.group-section-body {
  padding-left: 0.4rem;
  border-left: 2px solid var(--cc-border);
  margin-left: 0.1rem;
  margin-bottom: 0.2rem;
}

/* + cc-row cc-row-tight (the flex row) — this site owns only the field/button split.
   `.text-input` is width:100%, and cc-row wraps, so a flex-basis of `auto` made the field claim the
   whole line and pushed the Browse button onto a second row. Basis 0 + width auto lets the field
   take the leftover space instead; nowrap because a field and its own button are one control. */
.dir-path { flex-wrap: nowrap; }
.dir-path .text-input { flex: 1 1 0; width: auto; min-width: 0; }
.dir-path .cc-btn { flex: 0 0 auto; }
</style>
