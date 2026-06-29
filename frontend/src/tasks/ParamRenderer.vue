<!--
  Renders a single parameter from a TaskDef params array.
  The parent owns the values object and passes it as modelValue.
  Supports nested section (collapsible box) with recursive rendering.
-->
<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import type { ParamDef, ParamValues } from './types'
import type { CciaImage } from '../stores/project'

type GroupValues = Record<string, ParamValues>

export interface ParamContext {
  images: CciaImage[]
  projectUid?: string        // popSelection: needed to query the gating popmap
  values?: ParamValues       // sibling param values (popSelection reads valueName)
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
function imageFieldKeys(img: CciaImage, field: string | undefined): string[] {
  if (field === 'labels') return Object.keys(img.labels ?? {})
  return Object.keys(img.filepaths ?? { default: '' })
}

const availableValueNames = computed(() => {
  const images = props.context?.images ?? []
  if (images.length === 0) return ['default']
  const field = props.param.field
  const sets = images.map(img => new Set(imageFieldKeys(img, field)))
  const first = sets[0]
  return [...first].filter(k => sets.every(s => s.has(k)))
})

// When images change, auto-select an appropriate value name.
// For filepath fields: prefer the active value name.
// For other fields (e.g. labels): just pick the first available option.
watch(() => props.context?.images, (images) => {
  if (props.param.type !== 'valueNameSelection') return
  if (!images || images.length === 0) return
  const field = props.param.field
  const first = availableValueNames.value[0] ?? 'default'
  const preferred = (field === undefined || field === 'filepath')
    ? (images[0].activeValueName ?? first)
    : first
  const target = availableValueNames.value.includes(preferred) ? preferred : first
  emit('update:modelValue', target)
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
const popMultiOptions = ref<{ label: string; value: string }[]>([])

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

interface PopGroup { valueName: string; populations: { path: string; name: string }[] }

async function loadPops() {
  if (props.param.type !== 'popSelection') return
  const img = props.context?.images?.[0]
  const projectUid = props.context?.projectUid
  const popType = props.param.popType ?? 'flow'
  if (popAcross.value) {
    // populations across every segmentation, value_name-prefixed (incl. the derived /_tracked)
    popMultiOptions.value = []
    if (!img || !projectUid) return
    try {
      const q = `projectUid=${projectUid}&imageUid=${img.uid}&popType=${popType}`
      const res = await fetch(`/api/plots/populations?${q}`)
      if (!res.ok) return
      const groups = await res.json() as PopGroup[]
      const opts: { label: string; value: string }[] = []
      for (const g of groups) {
        for (const p of g.populations) {
          const value = `${g.valueName}${p.path}`   // "A" + "/_tracked" → "A/_tracked"
          opts.push({ label: value, value })
        }
      }
      popMultiOptions.value = opts
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

function isPopSelected(value: string): boolean {
  return Array.isArray(val.value) && (val.value as string[]).includes(value)
}
function togglePop(value: string) {
  const cur = Array.isArray(val.value) ? [...(val.value as string[])] : []
  const i = cur.indexOf(value)
  if (i >= 0) cur.splice(i, 1); else cur.push(value)
  emit('update:modelValue', cur)
}

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
  colGroups.value = []
  if (!img || !projectUid) return
  try {
    const q = `projectUid=${projectUid}&imageUid=${img.uid}&valueName=${encodeURIComponent(valueName)}`
    const res = await fetch(`/api/gating/channels?${q}`)
    if (!res.ok) return
    const d = await res.json() as { columns?: string[]; obsColumns?: string[]; channelNames?: string[] }
    const obs = (d.obsColumns ?? []).filter(c => !COL_DENYLIST.has(c))
    const vars = (d.columns ?? []).filter(c => !COL_DENYLIST.has(c))
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
    const track = obs.filter(c => c.startsWith('live.')).map(c => ({ label: c, value: c }))
    if (track.length) groups.push({ title: 'Tracking', opts: track })
    const object = vars.map(c => ({ label: channelLabel(c, chans), value: c }))
    if (object.length) groups.push({ title: 'Object', opts: object })
    colGroups.value = groups
  } catch { /* no columns available yet */ }
}

// reload when the image, the sibling valueName, or the selected populations change
watch(() => [props.context?.images?.[0]?.uid, props.context?.values?.valueName,
             JSON.stringify(props.context?.values?.pops)],
  () => { loadCols() }, { immediate: true })

function isColSelected(value: string): boolean {
  return Array.isArray(val.value) && (val.value as string[]).includes(value)
}
function toggleCol(value: string) {
  const cur = Array.isArray(val.value) ? [...(val.value as string[])] : []
  const i = cur.indexOf(value)
  if (i >= 0) cur.splice(i, 1); else cur.push(value)
  emit('update:modelValue', cur)
}

// motionDimsSelection: auto/2D/3D for track measures. In 'auto' we fetch the backend's z-assessment
// (cached by h5ad mtime, cheap) for the selected image+segmentation and show the recommendation +
// warning BEFORE the user runs — they can still override to 3D. Value sent: "auto" | "2D" | "3D".
interface MotionDims { dims: number; zUsed: boolean; confidence: string; reason: string
  metrics?: { nSteps?: number; autocorrX?: number; autocorrY?: number; autocorrZ?: number; persist2D?: number; persist3D?: number } }
const motionDims = ref<MotionDims | null>(null)
const motionLoading = ref(false)
async function loadMotionDims() {
  if (props.param.type !== 'motionDimsSelection') return
  const img = props.context?.images?.[0]
  const projectUid = props.context?.projectUid
  const valueName = (props.context?.values?.valueName as string) ?? 'default'
  motionDims.value = null
  if (!img || !projectUid) return
  motionLoading.value = true
  try {
    const q = `projectUid=${projectUid}&imageUid=${img.uid}&valueName=${encodeURIComponent(valueName)}`
    const res = await fetch(`/api/tracking/motion-dims?${q}`)
    if (res.ok) motionDims.value = await res.json() as MotionDims
  } catch { /* leave null */ } finally { motionLoading.value = false }
}
watch(() => [props.context?.images?.[0]?.uid, props.context?.values?.valueName],
  () => { loadMotionDims() }, { immediate: true })
// brief one-liner (full detector reason goes in the tooltip)
const motionWarn = computed(() => {
  const m = motionDims.value; if (!m) return false
  return (val.value ?? 'auto') === 'auto' && (m.dims === 2 || m.confidence === 'low')
})
const motionMsg = computed(() => {
  const m = motionDims.value; if (!m) return ''
  if ((val.value ?? 'auto') !== 'auto') return `using ${val.value} (auto: ${m.dims}D)`
  if (m.confidence === 'low') return `${m.dims}D — uncertain, review`
  return `${m.dims}D recommended`
})
// traffic-light flag: how usable is the z-axis? green = real 3D motion; yellow = borderline /
// uncertain; red = z is clearly jitter (anti-persistent). Only shown for the 'auto' recommendation.
const motionFlag = computed<'green' | 'yellow' | 'red' | ''>(() => {
  const m = motionDims.value; if (!m || (val.value ?? 'auto') !== 'auto') return ''
  if (m.dims === 3) return m.confidence === 'high' ? 'green' : 'yellow'
  const aZ = m.metrics?.autocorrZ                         // dims === 2: severity from z autocorrelation
  return (typeof aZ === 'number' && aZ <= 0) ? 'red' : 'yellow'   // ≤0 = reversing (bad); else just under cutoff
})
// plain-language explanation (replaces the technical detector `reason` in the tooltip): what we
// tested, the result, and by how far the z-axis missed the migration cutoff.
const motionTip = computed(() => {
  const m = motionDims.value; if (!m) return ''
  const mt = m.metrics ?? {}
  const f = (x?: number) => typeof x === 'number' ? x.toFixed(2) : '?'
  const aZ = mt.autocorrZ, p2 = mt.persist2D, p3 = mt.persist3D, n = mt.nSteps
  if (typeof n === 'number' && n < 50)
    return `Only ${n} track steps — too few to judge the z-axis reliably. Kept 3D to be safe; review.`
  const straight = (typeof p2 === 'number' && typeof p3 === 'number')
    ? ` Path straightness is ${f(p2)} in-plane vs ${f(p3)} once z is included.` : ''
  if (m.dims === 2)
    return `Tested whether vertical (z) motion is real or just jitter. Its step-to-step direction is `
      + `${typeof aZ === 'number' && aZ <= 0 ? 'random/reversing' : 'weak'} `
      + `(z consistency ${f(aZ)}; real migration needs > 0.10).${straight} `
      + `So z looks like jitter — measures use the flat in-plane (2D) path.`
  if (m.dims === 3 && m.confidence === 'high')
    return `Tested whether vertical (z) motion is real or just jitter. The z direction is consistent `
      + `(z consistency ${f(aZ)}, comparable to in-plane).${straight} z is real motion — full 3D kept.`
  return `The z-axis signal is borderline (z consistency ${f(aZ)}; cutoff is 0.10).${straight} `
      + `Kept 3D to be safe — review, or force 2D if you know the z spacing is coarse.`
})

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
function isChannelSelected(ch: string): boolean {
  const v = (val.value ?? []) as string[]
  return v.includes(ch)
}

function toggleChannel(ch: string) {
  const v = [...((val.value ?? []) as string[])]
  const idx = v.indexOf(ch)
  if (idx >= 0) {
    v.splice(idx, 1)
    val.value = v
  } else {
    // multiple=false: replace selection; multiple=true (or unset): append
    val.value = props.param.multiple === false ? [ch] : [...v, ch]
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
    <label class="param-label" v-tooltip.left="param.tip ?? ''">
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

    <!-- bool → toggle checkbox -->
    <label v-else-if="param.type === 'bool'" class="toggle-wrap"
      v-tooltip.right="param.tip ?? ''">
      <input type="checkbox" class="toggle-input"
        :checked="val as boolean"
        @change="val = ($event.target as HTMLInputElement).checked"
      />
      <span class="toggle-track">
        <span class="toggle-thumb" />
      </span>
    </label>

    <!-- text -->
    <input v-else-if="param.type === 'text'"
      type="text" class="text-input"
      :value="val as string"
      @input="val = ($event.target as HTMLInputElement).value"
      v-tooltip.right="param.tip ?? ''"
    />

    <!-- select -->
    <select v-else-if="param.type === 'select'"
      class="select-input"
      :value="val as string"
      @change="val = ($event.target as HTMLSelectElement).value"
      v-tooltip.right="param.tip ?? ''"
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
      v-tooltip.right="param.tip ?? 'Select an image version (valueName).'"
    >
      <option v-for="name in availableValueNames" :key="name" :value="name">{{ name }}</option>
      <option v-if="availableValueNames.length === 0" value="" disabled>— no versions available —</option>
    </select>

    <!-- popSelection (multi / across segmentations): chip list of value_name-prefixed populations -->
    <div v-else-if="param.type === 'popSelection' && popAcross" class="channel-select-wrap"
      v-tooltip.right="param.tip ?? 'Select populations across segmentations.'">
      <div v-if="popMultiOptions.length === 0" class="channel-empty">
        No populations — select an image first.
      </div>
      <div v-else class="channel-chips">
        <button
          v-for="opt in popMultiOptions"
          :key="opt.value"
          class="channel-chip"
          :class="{ active: isPopSelected(opt.value) }"
          @click="togglePop(opt.value)"
          type="button"
        >{{ opt.label }}</button>
      </div>
    </div>

    <!-- popSelection (single): NONE (whole segmentation) + flow population paths for this image -->
    <select v-else-if="param.type === 'popSelection'"
      class="select-input"
      :value="(val as string) ?? 'NONE'"
      @change="val = ($event.target as HTMLSelectElement).value"
      v-tooltip.right="param.tip ?? 'Track the whole segmentation or a gated population.'"
    >
      <option v-for="opt in popOptions" :key="opt.value" :value="opt.value">{{ opt.label }}</option>
    </select>

    <!-- labelPropsColsSelection: grouped (Tracking / Object) multi-select chip lists -->
    <div v-else-if="param.type === 'labelPropsColsSelection'" class="channel-select-wrap"
      v-tooltip.right="param.tip ?? 'Select measurement columns.'">
      <div v-if="colGroups.length === 0" class="channel-empty">
        No measures — select a population first.
      </div>
      <div v-for="g in colGroups" :key="g.title" class="col-group">
        <div v-if="g.title" class="col-group-title">{{ g.title }}</div>
        <div class="channel-chips">
          <button
            v-for="opt in g.opts"
            :key="opt.value"
            class="channel-chip"
            :class="{ active: isColSelected(opt.value) }"
            @click="toggleCol(opt.value)"
            type="button"
          >{{ opt.label }}</button>
        </div>
      </div>
    </div>

    <!-- motionDimsSelection: auto/2D/3D + the auto recommendation & warning -->
    <div v-else-if="param.type === 'motionDimsSelection'" class="motion-dims">
      <select class="select-input" :value="(val as string) ?? 'auto'"
              @change="val = ($event.target as HTMLSelectElement).value"
              v-tooltip.right="param.tip ?? 'Compute measures in-plane (2D) or full 3D.'">
        <option value="auto">Auto (recommended)</option>
        <option value="2D">2D (in-plane)</option>
        <option value="3D">3D</option>
      </select>
      <div v-if="motionLoading" class="md-note">checking z…</div>
      <div v-else-if="motionDims" class="md-note"
           :class="{ warn: motionWarn }" v-tooltip.right="motionTip">
        <i :class="['pi', motionWarn ? 'pi-exclamation-triangle' : 'pi-check-circle']" />
        {{ motionMsg }}
        <span v-if="motionFlag" class="md-flag" :class="motionFlag"
              v-tooltip.right="motionFlag === 'green' ? 'z carries real migration' : motionFlag === 'red' ? 'z is clearly jitter — 2D strongly advised' : 'borderline — only just decided'" />
      </div>
    </div>

    <!-- section / group: rendered outside .param-row below -->
    <template v-else-if="param.type === 'section' || param.type === 'group'"><!-- handled below --></template>

    <!-- channelSelection: togglable chip list from image context -->
    <div v-else-if="param.type === 'channelSelection'" class="channel-select-wrap"
      v-tooltip.right="param.tip ?? 'Select channels to process.'">
      <div v-if="availableChannels.length === 0" class="channel-empty">
        No channels — select images first.
      </div>
      <div v-else class="channel-chips">
        <button
          v-for="ch in availableChannels"
          :key="ch"
          class="channel-chip"
          :class="{ active: isChannelSelected(ch) }"
          @click="toggleChannel(ch)"
          type="button"
        >{{ ch }}</button>
      </div>
    </div>

    <!-- fallback -->
    <div v-else class="picker-placeholder"
      v-tooltip.right="`${param.type} — populated from image metadata`">
      <i class="pi pi-spinner pi-spin" style="font-size:0.7rem" />
      {{ param.type }}
    </div>
  </div>

  <!-- section rendered outside .param-row so it spans full width -->
  <div v-if="param.type === 'section'" class="param-section">
    <button class="section-toggle" @click="sectionOpen = !sectionOpen"
      v-tooltip.left="sectionOpen ? 'Collapse advanced parameters.' : 'Expand advanced parameters.'">
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
      <span class="group-title">{{ param.label }}</span>
      <button v-if="param.repeatable" class="group-add-btn" type="button"
        @click="addGroupEntry()"
        v-tooltip.right="'Add another entry'">
        <i class="pi pi-plus" />
      </button>
    </div>

    <div v-if="groupEntries.length === 0" class="group-empty">
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
              class="group-section-toggle"
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
  font-size: 0.75rem;
  font-weight: 500;
  color: var(--cc-text-dim);
  display: flex;
  align-items: center;
  gap: 0.3rem;
  cursor: default;
}
.tip-icon { font-size: 0.65rem; opacity: 0.6; }

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
  border-radius: 2px;
  background: linear-gradient(to right,
    var(--cc-accent) 0%, var(--cc-accent) var(--pct, 0%),
    var(--cc-surface-2) var(--pct, 0%), var(--cc-surface-2) 100%);
  cursor: pointer;
  outline: none;
}
.slider::-webkit-slider-thumb {
  appearance: none;
  width: 13px; height: 13px;
  border-radius: 50%;
  background: var(--cc-accent);
  cursor: pointer;
  box-shadow: 0 0 4px #a78bfa55;
}
.slider-val {
  font-size: 0.78rem;
  font-family: var(--cc-mono);
  color: var(--cc-text);
  min-width: 28px;
  text-align: right;
}

/* toggle */
.toggle-wrap { display: inline-flex; align-items: center; gap: 0.5rem; cursor: pointer; }
.toggle-input { display: none; }
.toggle-track {
  width: 32px; height: 17px;
  border-radius: 999px;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  position: relative;
  transition: background 0.15s, border-color 0.15s;
}
.toggle-input:checked ~ .toggle-track {
  background: var(--cc-accent);
  border-color: var(--cc-accent);
}
.toggle-thumb {
  position: absolute;
  width: 11px; height: 11px;
  border-radius: 50%;
  background: #fff;
  top: 2px; left: 2px;
  transition: left 0.15s;
}
.toggle-input:checked ~ .toggle-track .toggle-thumb { left: 17px; }

/* text / select — visual styling comes from the global form base (style.css) */
.text-input, .select-input { width: 100%; }

/* placeholder */
/* section */
.param-section { border-bottom: 1px solid var(--cc-border); }
.section-toggle {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  width: 100%;
  padding: 0.45rem 0;
  background: none;
  border: none;
  cursor: pointer;
  font-size: 0.72rem;
  font-weight: 600;
  color: var(--cc-text-dim);
  text-transform: uppercase;
  letter-spacing: 0.07em;
  text-align: left;
}
.section-toggle:hover { color: var(--cc-text); }
.section-body { padding-left: 0.5rem; border-left: 2px solid var(--cc-border); margin-left: 0.25rem; }

.picker-placeholder {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  font-size: 0.75rem;
  color: var(--cc-text-dim);
  background: var(--cc-surface-2);
  border: 1px dashed var(--cc-border);
  border-radius: 0.3rem;
  padding: 0.35rem 0.5rem;
}

/* channel selection */
.channel-select-wrap { width: 100%; }
.channel-empty {
  font-size: 0.72rem;
  color: var(--cc-text-dim);
  font-style: italic;
  padding: 0.2rem 0;
}
.channel-chips {
  display: flex;
  flex-wrap: wrap;
  gap: 0.25rem;
}
/* motion-dims selector + recommendation note (gap keeps the note off the dropdown) */
.motion-dims { display: flex; flex-direction: column; gap: 0.4rem; width: 100%; }
.md-note { display: inline-flex; align-items: center; gap: 0.3rem; font-size: 0.72rem; color: var(--cc-text-dim); }
.md-note .pi { font-size: 0.72rem; }
.md-note.warn { color: #fbbf24; }
/* traffic-light flag: how usable is the z-axis (green real 3D · yellow borderline · red jitter) */
.md-flag { width: 0.55rem; height: 0.55rem; border-radius: 50%; flex: none; }
.md-flag.green { background: #34d399; }
.md-flag.yellow { background: #fbbf24; }
.md-flag.red { background: #f87171; }

.col-group { margin-bottom: 0.4rem; }
.col-group:last-child { margin-bottom: 0; }
.col-group-title {
  font-size: 0.62rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--cc-text-dim);
  margin: 0.15rem 0 0.25rem;
}
.channel-chip {
  font-size: 0.7rem;
  padding: 0.15rem 0.45rem;
  border-radius: 999px;
  border: 1px solid var(--cc-border);
  background: var(--cc-surface-2);
  color: var(--cc-text-dim);
  cursor: pointer;
  transition: background 0.1s, color 0.1s, border-color 0.1s;
  white-space: nowrap;
}
.channel-chip:hover { border-color: var(--cc-accent); color: var(--cc-text); }
.channel-chip.active {
  background: var(--cc-accent);
  border-color: var(--cc-accent);
  color: #fff;
}

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
.group-title {
  font-size: 0.72rem;
  font-weight: 600;
  color: var(--cc-text-dim);
  text-transform: uppercase;
  letter-spacing: 0.06em;
}
.group-add-btn {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 18px; height: 18px;
  border-radius: 50%;
  border: 1px solid var(--cc-border);
  background: var(--cc-surface-2);
  color: var(--cc-text-dim);
  cursor: pointer;
  font-size: 0.6rem;
  transition: background 0.1s, border-color 0.1s;
}
.group-add-btn:hover { background: var(--cc-accent); border-color: var(--cc-accent); color: #fff; }
.group-empty {
  font-size: 0.72rem;
  color: var(--cc-text-dim);
  font-style: italic;
  padding: 0.3rem 0;
}
.group-entry {
  margin-bottom: 0.4rem;
  border: 1px solid var(--cc-border);
  border-radius: 0.3rem;
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
  font-size: 0.68rem;
  font-weight: 700;
  font-family: var(--cc-mono);
  color: var(--cc-accent);
}
.group-remove-btn {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 14px; height: 14px;
  border-radius: 50%;
  border: none;
  background: none;
  color: var(--cc-text-dim);
  cursor: pointer;
  font-size: 0.55rem;
  transition: color 0.1s;
}
.group-remove-btn:hover { color: #f87171; }
.group-entry-body { padding: 0 0.4rem; }
.group-entry-body .param-row:last-child { border-bottom: none; }

.group-section-toggle {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  width: 100%;
  padding: 0.3rem 0;
  background: none;
  border: none;
  border-top: 1px solid var(--cc-border);
  cursor: pointer;
  font-size: 0.68rem;
  font-weight: 600;
  color: var(--cc-text-dim);
  text-transform: uppercase;
  letter-spacing: 0.06em;
  text-align: left;
}
.group-section-toggle:hover { color: var(--cc-text); }
.group-section-toggle .pi { font-size: 0.6rem; }

.group-section-body {
  padding-left: 0.4rem;
  border-left: 2px solid var(--cc-border);
  margin-left: 0.1rem;
  margin-bottom: 0.2rem;
}
</style>
