<!--
  Renders a single parameter from a TaskDef params array.
  The parent owns the values object and passes it as modelValue.
  Supports nested section (collapsible box) with recursive rendering.
-->
<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import type { ParamDef, ParamValues } from './types'
import type { CciaImage } from '../stores/project'
import { SEVERITY } from '../lib/severity'
import { paramAdvisor, type ParamAdvisory, type AdvisorContext } from './paramAdvisors'
import { preferredValueName } from './paramValues'
import { groupPopulations, type PopGroupDef, type RawGroup } from '../utils/popGroups'
import ChipSelect, { type ChipOption } from '../components/ChipSelect.vue'
import CcToggle from '../components/CcToggle.vue'

type GroupValues = Record<string, ParamValues>

export interface ParamContext {
  images: CciaImage[]
  projectUid?: string        // popSelection: needed to query the gating popmap
  values?: ParamValues       // sibling param values (popSelection reads valueName)
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
}>()

// section (collapsible box) state
const sectionOpen = ref(!props.param.collapsed)

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
function imageFieldKeys(img: CciaImage, field: string | undefined): string[] {
  if (field === 'labels') return Object.keys(img.labels ?? {})
  // spatial neighbour graphs (spatialAnalysis.cellNeighbours), keyed by run suffix — the intersection
  // across the selected images is exactly the set of graphs a pooled analysis can run over.
  if (field === 'spatialGraphs') return Object.keys(img.spatialGraphs ?? {})
  return Object.keys(img.filepaths ?? { default: '' })
}

const availableValueNames = computed(() => {
  const extra = props.context?.extraValueNames ?? []
  const images = props.context?.images ?? []
  // Base names: intersection of what exists on the selected images (or just "default" if none).
  const base = images.length === 0
    ? ['default']
    : (() => {
        const field = props.param.field
        const sets = images.map(img => new Set(imageFieldKeys(img, field)))
        return [...sets[0]].filter(k => sets.every(s => s.has(k)))
      })()
  // Union in chain-propagated names (upstream node outputs), de-duplicated, preserving order.
  return [...new Set([...base, ...extra])]
})

// When images change, auto-select an appropriate value name.
// For filepath fields: prefer the active value name.
// For other fields (e.g. labels): just pick the first available option.
watch(() => props.context?.images, (images) => {
  if (props.param.type !== 'valueNameSelection') return
  if (!images || images.length === 0) return
  // Keep an already-valid selection — notably an edge-propagated chain value like
  // "cpCorrected" — rather than resetting it to the active/first name on every image change.
  if (props.modelValue && availableValueNames.value.includes(props.modelValue as string)) return
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
  // single mode — scoped to the sibling valueName
  const valueName = (props.context?.values?.valueName as string) ?? 'default'
  popOptions.value = [{ label: 'NONE (whole segmentation)', value: 'NONE' }]
  if (!img || !projectUid) return
  popOptions.value.push(...(await fetchPopPaths(img, projectUid, valueName, popType)).map(p => ({ label: p, value: p })))
}

// reload when the image or the chosen segmentation changes
watch(() => [props.context?.images?.[0]?.uid, props.context?.values?.valueName],
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
  const pops = props.context?.values?.pops
  if (Array.isArray(pops) && pops.length) {
    const first = String(pops[0])
    if (!first.startsWith('/')) {
      const idx = first.indexOf('/')
      if (idx > 0) return first.slice(0, idx)
    }
  }
  const vn = props.context?.values?.valueName as string | undefined
  if (vn) return vn
  const keys = Object.keys(props.context?.images?.[0]?.labels ?? {})
  return keys[0] ?? 'default'
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
// segmentation. Source: /api/gating/channels. Two groups (matching the R UI): TRACKING measures
// (obs `live.*`) and OBJECT measures (var columns — intensities shown by CHANNEL NAME, plus shape).
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
      cellMeasures?: string[]; cellObsMeasures?: string[]; trackAggregates?: string[] }
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
    const groups: { title: string; opts: { label: string; value: string }[] }[] = []
    if (popType === 'track' || popType === 'trackclust') {
      // TRACK: pick BASE measures (like old R); the task aggregates each to ALL per-track stats
      // (mean/median/…; categorical → frequencies) automatically — so we DON'T list *.mean/*.median.
      //  • Track measures = whole-track motility (used directly)
      //  • Object measures = cell vars (channels/morphology), aggregated per track
      //  • Behaviour = cell obs (live.* incl. HMM state/transitions), aggregated per track
      const motility = vars.map(c => ({ label: c, value: c }))
      if (motility.length) groups.push({ title: 'Track measures', opts: motility })
      const object = (d.cellMeasures ?? []).filter(c => !COL_DENYLIST.has(c))
        .map(c => ({ label: channelLabel(c, chans), value: c }))
      if (object.length) groups.push({ title: 'Object measures', opts: object })
      const behaviour = (d.cellObsMeasures ?? []).filter(c => !COL_DENYLIST.has(c) && c.startsWith('live.'))
        .map(c => ({ label: c, value: c }))
      if (behaviour.length) groups.push({ title: 'Behaviour', opts: behaviour })
      colGroups.value = groups
      return
    }
    // FLOW/cell: object vars + behaviour obs (live.*). Group names + order match the track picker
    // (Object measures, then Behaviour) so Cluster cells and Cluster tracks read consistently — the
    // only difference is Cluster tracks additionally has a "Track measures" (whole-track) group.
    const object = vars.map(c => ({ label: channelLabel(c, chans), value: c }))
    if (object.length) groups.push({ title: 'Object measures', opts: object })
    const behaviour = obs.filter(c => c.startsWith('live.')).map(c => ({ label: c, value: c }))
    if (behaviour.length) groups.push({ title: 'Behaviour', opts: behaviour })
    colGroups.value = groups
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

let advisorySeq = 0
async function loadAdvisory() {
  const a = advisor.value
  advisory.value = null
  if (!a) return
  const seq = ++advisorySeq          // only the latest run may write; a slider drag races otherwise
  advisoryLoading.value = true
  try {
    const r = await a.advise(val.value, advisoryCtx.value)
    if (seq === advisorySeq) advisory.value = r
  } finally {
    if (seq === advisorySeq) advisoryLoading.value = false
  }
}
// `val` is in the key list on purpose: an async advisor still depends on the CURRENT value (the grid
// estimate changes as the slider moves), and `reloadOn` only covers the context. The fetch itself is
// the expensive part, so advisors whose fetch does not depend on the value should cache — today both
// are cheap enough (one metadata read) that correctness wins over a caching layer nobody needs yet.
watch(() => [props.param.key, val.value, advisor.value?.reloadOn?.(advisoryCtx.value)],
  () => { loadAdvisory() }, { immediate: true, deep: true })

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
  const defaults: ParamValues = {}
  for (const p of props.param.params ?? []) {
    if (p.type === 'section') {
      // Section sub-params are stored flat in the entry dict
      for (const sp of p.params ?? []) {
        if (sp.default !== undefined) defaults[sp.key] = sp.default
      }
    } else if (p.default !== undefined) {
      defaults[p.key] = p.default
    }
  }
  v[nextKey] = defaults
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
  <div class="param-row">
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

    <!-- bool → shared toggle switch -->
    <CcToggle v-else-if="param.type === 'bool'" v-tooltip.right="param.tip"
      :model-value="val as boolean" @update:model-value="val = $event" />

    <!-- text -->
    <input v-else-if="param.type === 'text'"
      type="text" class="text-input"
      :value="val as string"
      @input="val = ($event.target as HTMLInputElement).value"
      v-tooltip.right="param.tip"
    />

    <!-- chipSelect: multi-pick from a fixed set. A raw text field for something like "1,2,4,8" is a
         parse error waiting to happen and reads as unfinished; ChipSelect is the canonical primitive
         for "pick from a set" (docs/UI.md). -->
    <ChipSelect v-else-if="param.type === 'chipSelect'"
      :options="(param.options ?? []).map(o => ({ value: String(o.value), label: o.label }))"
      :model-value="(Array.isArray(val) ? val : []).map(String)"
      multiple
      :aria-label="param.label"
      v-tooltip.right="param.tip"
      @update:model-value="v => val = v as string[]"
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
        <ChipSelect multiple :options="grp.opts"
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
        <ChipSelect multiple :options="g.opts"
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

    <!-- section / group: rendered outside .param-row below -->
    <template v-else-if="param.type === 'section' || param.type === 'group'"><!-- handled below --></template>

    <!-- channelSelection: togglable chip list from image context -->
    <div v-else-if="param.type === 'channelSelection'" class="channel-select-wrap"
      v-tooltip.right="param.tip">
      <div v-if="availableChannels.length === 0" class="channel-empty cc-muted">
        No channels — select images first.
      </div>
      <ChipSelect v-else multiple :options="channelOptions"
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
    <div v-else-if="advisory" class="param-advisory cc-muted"
         :class="`sev-${advisory.severity}`" v-tooltip.right="advisory.tip">
      <i class="pi" :class="SEVERITY[advisory.severity].icon" />
      {{ advisory.message }}
      <!-- optional second signal: how good the DATA is, as distinct from how concerning the
           recommendation is. Own colour + own tooltip; colour is never the only cue. -->
      <i v-if="advisory.flag" class="pi param-advisory-flag" :class="SEVERITY[advisory.flag.severity].icon"
         :style="{ color: SEVERITY[advisory.flag.severity].color }"
         v-tooltip.right="advisory.flag.tip" />
    </div>
  </div>

  <!-- section rendered outside .param-row so it spans full width -->
  <div v-if="param.type === 'section'" class="param-section">
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
      />
    </div>
  </div>

  <!-- group: repeatable set of sub-params keyed by string index -->
  <div v-if="param.type === 'group'" class="param-group">
    <div class="group-header">
      <span class="group-title cc-eyebrow cc-fs-sm">{{ param.label }}</span>
      <button v-if="param.repeatable" class="group-add-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-micro" type="button"
        @click="addGroupEntry()"
        v-tooltip.right="'Add another entry'">
        <i class="pi pi-plus" />
      </button>
    </div>

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
              />
            </div>
          </template>
          <ParamRenderer
            v-else
            :param="p"
            :modelValue="entry.vals[p.key]"
            @update:modelValue="updateGroupEntry(entry.key, p.key, $event)"
            :context="context"
          />
        </template>
      </div>
    </div>
  </div>
</template>

<style scoped>
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
.param-advisory { display: flex; align-items: center; gap: 0.3rem; }
.param-advisory.sev-warn { color: var(--cc-sev-warn); }
.param-advisory.sev-fail { color: var(--cc-sev-fail); }
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
</style>
