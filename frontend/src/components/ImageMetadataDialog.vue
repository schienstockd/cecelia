<!--
  ImageMetadataDialog — "everything we know about this image" view, opened from the info icon on
  each ImageTable row. Its headline is the original source file location (oriPath): the raw file
  this image was converted from, which is otherwise not visible anywhere in the UI. Built on
  BaseModal like every other dialog. Editing physical size / timing / channels lives elsewhere
  (PhysicalSizeDialog, Metadata page). One mutation lives here: renaming a segmentation's
  value_name — the operation is scoped to the label set the user is looking at (POST
  /api/images/labels/rename → app/src/storage.jl:rename_value_name!).
-->
<script setup lang="ts">
import { computed, defineAsyncComponent, ref, nextTick, onMounted } from 'vue'
import { acquisitionRows, acquisitionNotice, microscopeLine } from '../utils/acquisitionSettings'
import { acquisitionSupportRequestUrl } from '../lib/links'
import { acquisitionTechnology } from '../utils/acquisitionLightPath'
import { hasDataPath } from '../utils/acquisitionDataPath'
import AcquisitionDataPath from './AcquisitionDataPath.vue'
import InlineNote from './InlineNote.vue'
import AcquisitionLightPath from './AcquisitionLightPath.vue'
import AcquisitionCardDialog from './AcquisitionCardDialog.vue'
import { hasAcquisitionCard } from '../utils/acquisitionCard'
import CollapsibleSection from './CollapsibleSection.vue'
import { useToast } from '../composables/useToast'
import BaseModal from './BaseModal.vue'
import type { CciaImage } from '../stores/project'
import { useProjectStore } from '../stores/project'
import { useCopyFlash } from '../composables/useCopyFlash'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useLogStore } from '../stores/log'
import { formatBytes } from '../utils/storage'
import { storeFormatFacts, storeFormatTitle, storeLevelRows,
         type StoreEncoding } from '../utils/storeFormat'
import { formatPhysicalSize, formatTimeIncrement } from '../utils/physicalSize'
import { buildLineageForest, flattenLineage } from '../utils/versionLineage'
import { cropSourceLine } from '../utils/cropSource'
import { useTaskDefsStore } from '../stores/taskDefs'
import { fetchAcqCheck, type AcqCheckResult } from '../utils/acqCheckApi'
import { findingsFor } from '../utils/acqCheck'
// lazily: the set's check dialog opens this one for a movie, so a static import would be circular
const AcqCheckDialog = defineAsyncComponent(() => import('./AcqCheckDialog.vue'))

const props = defineProps<{ image: CciaImage }>()
defineEmits<{ (e: 'close'): void }>()

const img = computed(() => props.image)

// One line pointing at the set's acquisition check (the check is set-level: its own dialog)
const acqSet = computed(() => project.sets.find(s => s.images.some(i => i.uid === props.image.uid)) ?? null)
const acq = ref<AcqCheckResult | null>(null)
const showAcq = ref(false)
const acqLine = computed(() => {
  if (!acq.value) return null
  const mine = findingsFor(acq.value, props.image.uid).filter(f => f.level === 'warn').length
  const set = findingsFor(acq.value, 'set').filter(f => f.level === 'warn').length
  return mine + set ? `Acquisition check: ${mine} for this movie, ${set} across the set` : null
})
const projectMeta = useProjectMetaStore()
const project = useProjectStore()
const log = useLogStore()
const toast = useToast()

// "12 × 512 × 512" style formatting is overkill here — we show each dimension as its own row so a
// missing one reads as "—" rather than a silently-absent factor.
function num(v: number | null | undefined): string {
  return v === null || v === undefined ? '—' : String(v)
}

// Calibration readouts share the ImageTable's formatting (`0.346 µm`, not `0.3459441507762987
// micrometer`) — one shared helper, `formatPhysicalSize`, so the two surfaces stay in sync.
// Fill-only: adds what the store or the source file records and ccid.json lacks (size, timing,
// acquisition settings); never overwrites a value already set (`resync_ome_meta!`). Also runs the
// import's pixel probe when any `SATURATION_CURRENT_KEYS` field is missing (saturation.jl) — one
// image, so the pixel pass is affordable here.
const resyncing = ref(false)
async function resync() {
  const projectUid = projectMeta.current?.uid
  if (!projectUid || resyncing.value) return
  resyncing.value = true
  try {
    await project.resyncImageMeta(projectUid, [img.value.uid], true)
    log.info(`Re-read metadata from file for ${img.value.name}.`, { source: 'manageImages' })
  } catch (e) {
    log.error(`Failed to re-read metadata: ${e instanceof Error ? e.message : String(e)}`, { source: 'manageImages' })
  } finally {
    resyncing.value = false
  }
}

const physical = computed(() => {
  const i = img.value
  const u = i.physicalSizeUnit
  return {
    x: formatPhysicalSize(i.physicalSizeX, u),
    y: formatPhysicalSize(i.physicalSizeY, u),
    z: formatPhysicalSize(i.physicalSizeZ, u),
  }
})

const timeStr = computed(() => formatTimeIncrement(img.value.timeIncrement, img.value.timeIncrementUnit))

const channels = computed(() => img.value.channelNames?.filter(c => c && c.length) ?? [])
// valueName → filename, active first. The active version is the zarr the app currently reads.
const versions = computed(() => Object.entries(img.value.filepaths ?? {}))

// Version lineage — one row per stored version, indented under whichever version was used as
// INPUT to produce it (derived from the automatic run log; see utils/versionLineage.ts). Only
// meaningful when there are two or more versions AND at least one was produced from another; a lone
// `default` (or a set with no run-log edges) collapses to nothing rather than a "flow" of a single
// leaf.
const taskDefs = useTaskDefsStore()
const lineageRows = computed(() => {
  const names = versions.value.map(([vn]) => vn)
  const rows = flattenLineage(buildLineageForest(names, img.value.runLog))
  return rows
})
const hasLineageEdges = computed(() => lineageRows.value.some(r => r.node.edge))
// The label the caption shows for a producing task ('cleanupImages.driftCorrect' → 'Drift correct').
// Task defs load asynchronously, so labelFor falls back to the fun's last segment while it warms.
const funLabel = (fun: string) => taskDefs.labelFor(fun)

// What each stored version IS on disk: how its pixels are ENCODED, and how much space it takes.
// Fetched rather than stored: the codec is a property of the store (which can be re-landed on a
// different one by rechunk_zarr.py without anything in ccid.json changing), and the size can only be
// known by walking it. Awaited AFTER the modal is on screen, never before — the walk is the expensive
// part (a few hundred ms per store warm, seconds cold on a multi-GB version), so the sizes fill in a
// moment later rather than delaying the dialog. Failure is silent — this dialog is read-only
// information and must still open if a store is missing.
type StoreInfo = { bytes: number, label?: string } & StoreEncoding
const stores = ref<{ versions: Record<string, StoreInfo | null>, labels: Record<string, StoreInfo> }>(
  { versions: {}, labels: {} })
onMounted(async () => {
  const uid = projectMeta.current?.uid
  if (uid && acqSet.value) acq.value = await fetchAcqCheck(uid, acqSet.value.uid)
})
onMounted(async () => {
  // Warm the task-defs cache so lineage captions carry the pretty task label ('Drift correct')
  // rather than the fun's last segment ('driftCorrect'). Awaited alongside the store walk since
  // both are display-only enrichments the modal must still open without.
  taskDefs.ensureLoaded()
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  try {
    const res = await fetch(`/api/images/stores?projectUid=${encodeURIComponent(projectUid)}`
                            + `&imageUid=${encodeURIComponent(img.value.uid)}`)
    if (res.ok) {
      const j = await res.json()
      stores.value = { versions: j.versions ?? {}, labels: j.labels ?? {} }
    }
  } catch { /* display-only */ }
})
// 0 bytes means "not measured yet / store missing", not an empty store — show it as unknown.
const size = (s: StoreInfo | null | undefined) => (s && s.bytes > 0 ? formatBytes(s.bytes) : '—')
const labels = computed(() => Object.entries(img.value.labels ?? {}))
const attrs = computed(() => Object.entries(img.value.attr ?? {}).filter(([, v]) => v && v.length))
const extra = computed(() => Object.entries(img.value.extraMeta ?? {}))
const acquisition = computed(() => acquisitionRows(img.value.acquisition))
// Every technology with stages (two-photon, confocal, Airyscan, widefield) gets the light-path diagram,
// which places the same values at their stage.
const lightPath = computed(() => acquisitionTechnology(img.value.acquisition) != null)
// The diagram has no rows of its own for the system, so name it above (the plain rows already do).
const scope = computed(() => (lightPath.value ? microscopeLine(img.value.acquisition) : null))
const acqNotice = computed(() =>
  acquisitionNotice(img.value.acquisition, img.value.acquisitionMissing, lightPath.value, acquisitionSupportRequestUrl))
// Per-channel pixel numbers from the import probe — any image probed with the current probe.
const dataPath = computed(() => hasDataPath(img.value))
// The printable settings card: same rule as the diagram; elsewhere the notice above offers
// "Request your microscope" instead (docs/todo/ACQUISITION_CARD_PLAN.md D10).
const cardAvailable = computed(() => hasAcquisitionCard(img.value))
const showCard = ref(false)
// A crop names its source image (by name when it's in the open project) + version + pixel box.
const cropLine = computed(() => {
  const cs = img.value.cropSource
  return cropSourceLine(cs, cs ? project.imageByUid(cs.uid)?.name : null)
})

// copy-to-clipboard for path-like values — shared helper, keyed per field
const { isCopied, copy: copyValue } = useCopyFlash()
const copy = (key: string, value: string) => copyValue(value, key)

// Which stored versions have their Pyramid section expanded. Default collapsed — a project with N
// stored versions (default + drift-corrected + AF-corrected + …) would otherwise show N per-level
// tables the moment the modal opens, and only one is usually of interest. Local ref, not persisted:
// the modal reopens fresh from an info-icon click, so persisting a per-open toggle would over-index
// on a decision the user makes once per look.
const expandedLevels = ref<Set<string>>(new Set())
const togglePyramid = (vn: string) => {
  const s = new Set(expandedLevels.value)
  s.has(vn) ? s.delete(vn) : s.add(vn)
  expandedLevels.value = s
}
const levelsFor = (vn: string) => storeLevelRows(stores.value.versions[vn])

// Rename a label set's value_name. Inline: one row at a time swaps its `labels · {vn}` heading for a
// text input + save/cancel. The backend guards enforce uniqueness, reserved suffixes and the
// running-task / cross-image clustering blocks — the frontend only validates the shape (non-empty, no
// path separators, not already on this image) so the user gets an immediate answer for typos and the
// server stays authoritative. Local ref, not persisted — the modal reopens fresh from an info click.
const editingVn = ref<string | null>(null)
const editingValue = ref('')
const editingBusy = ref(false)
// Template ref inside a `v-for` — Vue populates it as an array of the currently-mounted
// elements (only one row renders the input at a time, guarded by `editingVn === vn`).
const editInputRef = ref<HTMLInputElement[]>([])

function startEditVn(vn: string) {
  editingVn.value = vn
  editingValue.value = vn
  nextTick(() => editInputRef.value[0]?.select())
}
function cancelEditVn() {
  editingVn.value = null
  editingValue.value = ''
}
function editVnError(): string | null {
  const to = editingValue.value.trim()
  if (!to) return 'Enter a name'
  if (to === editingVn.value) return null   // no-op — the save is disabled
  if (/[/\\]/.test(to) || to.startsWith('.')) return 'No slashes or leading dot'
  if (to.endsWith('__tracks') || to.endsWith('__branch')) return 'Reserved suffix'
  const existing = Object.keys(img.value.labels ?? {})
  if (existing.includes(to)) return 'Name already used on this image'
  return null
}
async function saveEditVn() {
  const from = editingVn.value
  const to = editingValue.value.trim()
  const projectUid = projectMeta.current?.uid
  if (!from || !projectUid || from === to || editVnError()) return
  editingBusy.value = true
  try {
    const res = await fetch('/api/images/labels/rename', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUid: img.value.uid, from, to }),
    })
    const body = await res.json().catch(() => ({})) as { error?: string, image?: unknown }
    if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
    if (body.image) project.updateImageMeta(img.value.uid, body.image as Partial<CciaImage>)
    toast.add({ severity: 'success', summary: 'Renamed', life: 2500,
                detail: `Label set '${from}' → '${to}'` })
    cancelEditVn()
  } catch (e) {
    const msg = e instanceof Error ? e.message : String(e)
    log.error(`Rename failed: ${msg}`, { source: 'imageMetadata' })
    toast.add({ severity: 'error', summary: 'Rename failed', life: 4000, detail: msg })
  } finally {
    editingBusy.value = false
  }
}
</script>

<template>
  <BaseModal width="720px" @close="$emit('close')">
    <template #title>
      <i class="pi pi-info-circle" /> Image metadata
    </template>

    <div class="md-body">
      <div class="md-head cc-row">
        <p class="md-name">{{ img.name }}</p>
        <button class="cc-btn cc-btn-bare cc-btn-icon" :disabled="resyncing" @click="resync"
          v-tooltip.left="'Refresh metadata from the store and source file'">
          <i :class="['pi', resyncing ? 'pi-spin pi-spinner' : 'pi-sync']" />
        </button>
      </div>

      <div v-if="acqLine" class="md-acq cc-row cc-fs-sm">
        <InlineNote :short="acqLine" severity="warn" />
        <button class="cc-btn cc-btn-ghost cc-btn-dense cc-fs-xs" @click="showAcq = true">Open check</button>
      </div>
      <AcqCheckDialog v-if="showAcq && acqSet && projectMeta.current" :project-uid="projectMeta.current.uid"
                      :set-uid="acqSet.uid" :set-name="acqSet.name" @close="showAcq = false" />

      <!-- headline: the original source file this image was converted from -->
      <section class="md-section">
        <h4 class="md-h cc-eyebrow">Original file</h4>
        <div v-if="img.oriPath" class="md-path">
          <code class="md-code">{{ img.oriPath }}</code>
          <button class="md-copy cc-btn cc-btn-bare cc-btn-icon" @click="copy('ori', img.oriPath!)"
            v-tooltip.left="isCopied('ori') ? 'Copied!' : 'Copy path'">
            <i :class="isCopied('ori') ? 'pi pi-check' : 'pi pi-copy'" />
          </button>
        </div>
        <p v-else class="md-none cc-muted">Not recorded — imported before source paths were tracked, or created in-app.</p>
        <p v-if="cropLine" class="md-crop cc-muted">{{ cropLine }}</p>
      </section>

      <section class="md-section">
        <h4 class="md-h cc-eyebrow">Identity</h4>
        <div class="md-grid">
          <span class="md-k">UID</span><span class="md-v md-mono">{{ img.uid }}</span>
          <span class="md-k">Status</span><span class="md-v">{{ img.status }}</span>
        </div>
      </section>

      <CollapsibleSection label="Dimensions & calibration" tip="Size and calibration" accordion="cc.imageMeta.section" section="dimensions" default-open max-height="none">
        <div class="md-section md-cs">
        <div class="md-grid">
          <span class="md-k">Channels (C)</span><span class="md-v">{{ num(img.sizeC) }}</span>
          <span class="md-k">Z-slices (Z)</span><span class="md-v">{{ num(img.sizeZ) }}</span>
          <span class="md-k">Frames (T)</span><span class="md-v">{{ num(img.sizeT) }}</span>
          <span class="md-k">Pixel size X</span><span class="md-v">{{ physical.x }}</span>
          <span class="md-k">Pixel size Y</span><span class="md-v">{{ physical.y }}</span>
          <span class="md-k">Voxel depth Z</span><span class="md-v">{{ physical.z }}</span>
          <span class="md-k">Frame interval</span><span class="md-v">{{ timeStr }}</span>
        </div>
        </div>
      </CollapsibleSection>

      <!-- Microscope settings read from the source file (laser power, Bright Z, scan, detectors) —
           values Bio-Formats drops. Always shown: without settings (or without a diagram) it says why,
           so users of an untested microscope learn it exists and can ask for it. -->
      <CollapsibleSection label="Acquisition" tip="Microscope settings from the source file" accordion="cc.imageMeta.section" section="acquisition" max-height="none">
        <div class="md-section md-cs">
        <div v-if="scope || cardAvailable" class="md-scope cc-row cc-fs-sm">
          <span v-if="scope"><span class="cc-muted">Microscope</span> {{ scope }}</span>
          <button v-if="cardAvailable && projectMeta.current" class="cc-btn cc-btn-ghost cc-btn-dense cc-fs-xs md-card-btn"
                  v-tooltip="'Printable card to acquire this image again'" @click="showCard = true">
            <i class="pi pi-print" /> Settings card
          </button>
        </div>
        <AcquisitionCardDialog v-if="showCard && projectMeta.current" :image="img" :project-uid="projectMeta.current.uid"
                               @close="showCard = false" />
        <AcquisitionLightPath v-if="lightPath" :image="img" />
        <div v-else-if="acquisition.length" class="md-grid">
          <template v-for="(r, i) in acquisition" :key="i">
            <span class="md-k">{{ r.k }}</span><span class="md-v">{{ r.v }}</span>
          </template>
        </div>
        <div v-if="acqNotice" class="md-acq-note cc-row">
          <InlineNote :short="acqNotice.text" />
          <a v-if="acqNotice.action" class="cc-btn cc-btn-ghost cc-btn-dense cc-fs-xs" :href="acqNotice.action.url"
             target="_blank" rel="noopener" v-tooltip="'Opens a GitHub issue: attach a sample file'">
            {{ acqNotice.action.label }} <i class="pi pi-external-link" />
          </a>
        </div>
        </div>
      </CollapsibleSection>

      <CollapsibleSection v-if="dataPath" label="Acquired data" tip="What landed in the file, per channel" accordion="cc.imageMeta.section" section="data" max-height="none">
        <div class="md-section md-cs">
          <AcquisitionDataPath :image="img" />
        </div>
      </CollapsibleSection>

      <CollapsibleSection v-if="channels.length" label="Channels" tip="Channel names" accordion="cc.imageMeta.section" section="channels" max-height="none">
        <div class="md-section md-cs">
        <ol class="md-chips cc-row cc-row-tight">
          <li v-for="(c, i) in channels" :key="i" class="md-chip">{{ c }}</li>
        </ol>
        </div>
      </CollapsibleSection>

      <CollapsibleSection label="Stored files" tip="Versions, segmentations and their sizes" accordion="cc.imageMeta.section" section="stored" max-height="none">
        <div class="md-section md-cs">
        <div class="md-grid">
          <span class="md-k">Active version</span><span class="md-v">{{ img.activeValueName || '—' }}</span>
        </div>
        <!-- Provenance flow: which version was used as INPUT to produce which. Rebuilt from the
             per-image run log (see utils/versionLineage.ts). Only rendered when there is at least
             one edge — for a project with a lone `default` (or a set with no run-log entries) a
             "flow" of one node is noise. Compact tree: each version is one line; a producing task
             sits on the line ABOVE its output as "└─ Drift correct →" so the read order is
             input-then-task-then-output, matching a data-flow diagram left→right/top-down. -->
        <div v-if="hasLineageEdges" class="md-lineage cc-card">
          <ol class="md-flow">
            <li v-for="row in lineageRows" :key="row.node.version"
                class="md-flow-row" :style="{ '--depth': row.depth }">
              <span v-if="row.node.edge" class="md-flow-via cc-muted cc-fs-xs">
                <i class="pi pi-arrow-down md-flow-arrow" />
                {{ funLabel(row.node.edge.fun) }}
              </span>
              <span class="md-flow-vn"
                    :class="{ 'md-flow-active': row.node.version === img.activeValueName }">
                {{ row.node.version }}
                <span v-if="!row.node.edge" class="cc-muted cc-fs-2xs">imported</span>
              </span>
            </li>
          </ol>
        </div>
        <!-- One CARD per stored thing, not one grid of rows. Each version carries six technical facts
             (codec, zarr format, NGFF version, chunk, shard, chunk keys) on top of its name, filename
             and size — as columns that is unreadably dense, and as one appended sentence it is a
             run-on; both were tried. A card gives each entry its own block: what it IS on the head row
             (name · format · size), what it is MADE OF underneath. Sizes still compare down the page —
             every card is full width, so the right-aligned size lands in the same column. -->
        <ul class="md-stores">
          <li v-for="[vn, fn] in versions" :key="'v-' + vn" class="md-store cc-card">
            <div class="md-store-head">
              <span class="md-store-vn">{{ vn }}</span>
              <!-- The format sits with the NAME, not among the facts below: it is what this entry IS,
                   and it is what differs between two versions of one image. It also keeps the facts to
                   a single uncrowded line. NGFF bracketed, and absent when the store declares none. -->
              <span v-if="storeFormatTitle(stores.versions[vn])" class="cc-muted cc-fs-xs">
                {{ storeFormatTitle(stores.versions[vn]) }}
              </span>
              <span class="md-size cc-readout cc-fs-xs">{{ size(stores.versions[vn]) }}</span>
            </div>
            <code class="md-code">{{ fn }}</code>
            <!-- Chunking and codec: answerable here rather than by reading `zarr.json` in a terminal,
                 since v2 and v3 stores coexist permanently (no converter). An em dash when the store
                 could not be read at all — a different answer from "unsharded". -->
            <div v-if="storeFormatFacts(stores.versions[vn]).length" class="cc-row cc-row-loose">
              <span v-for="f in storeFormatFacts(stores.versions[vn])" :key="f.k" class="cc-row-group">
                <span class="cc-muted cc-fs-xs">{{ f.k }}</span>
                <span class="md-fact-v">{{ f.v }}</span>
              </span>
            </div>
            <span v-else class="cc-muted cc-fs-xs">—</span>
            <!-- Pyramid layout: per-level XY shape, XY chunk, and tile grid at that level. Collapsed by
                 default (see `expandedLevels` — one card per stored version could otherwise mean N tables
                 open on the first look). One row is a valid answer for a store with `nscales=1` (drift/AF
                 corrections default to that), which is exactly the state the toggle is here to surface. -->
            <template v-if="levelsFor(vn).length">
              <button class="md-pyr-toggle cc-section-toggle"
                      @click="togglePyramid(vn)"
                      v-tooltip.top="expandedLevels.has(vn) ? 'Hide pyramid layout' : 'Show pyramid layout'">
                <i :class="['pi', expandedLevels.has(vn) ? 'pi-chevron-down' : 'pi-chevron-right']" />
                <span class="cc-eyebrow cc-fs-2xs">Pyramid</span>
                <span class="cc-muted cc-fs-2xs">{{ levelsFor(vn).length }} level{{ levelsFor(vn).length === 1 ? '' : 's' }}</span>
              </button>
              <table v-if="expandedLevels.has(vn)" class="md-pyr">
                <thead>
                  <tr>
                    <th class="cc-muted cc-fs-2xs">level</th>
                    <th class="cc-muted cc-fs-2xs">XY</th>
                    <th class="cc-muted cc-fs-2xs">chunk XY</th>
                    <th class="cc-muted cc-fs-2xs">grid</th>
                  </tr>
                </thead>
                <tbody>
                  <tr v-for="row in levelsFor(vn)" :key="row.level">
                    <td class="md-pyr-lvl">{{ row.level }}</td>
                    <td class="md-fact-v">{{ row.xy }}</td>
                    <td class="md-fact-v">{{ row.chunk }}</td>
                    <td class="md-fact-v">{{ row.grid }}</td>
                  </tr>
                </tbody>
              </table>
            </template>
          </li>
          <!-- A label set's size is the sum of its files (base + nuc). No codec facts: it is written
               with the fixed `labels` compressor, which nothing here asks the user to choose.
               The pencil affordance opens an inline rename: someone who named a segmentation "T cells"
               where a downstream chain expects "default" can fix it here without a re-run. -->
          <li v-for="[vn, fns] in labels" :key="'l-' + vn" class="md-store cc-card">
            <div class="md-store-head">
              <template v-if="editingVn === vn">
                <span class="cc-muted cc-fs-xs">labels ·</span>
                <input ref="editInputRef" v-model="editingValue"
                       class="md-vn-input cc-input-xs"
                       :disabled="editingBusy"
                       v-tooltip.top="editVnError() ?? 'New name'"
                       @keydown.enter.prevent="saveEditVn"
                       @keydown.escape.prevent="cancelEditVn" />
                <button class="cc-btn cc-btn-bare cc-btn-icon"
                        :disabled="editingBusy || !!editVnError() || editingValue.trim() === vn"
                        @click="saveEditVn"
                        v-tooltip.top="editVnError() ?? 'Rename'">
                  <i class="pi pi-check" />
                </button>
                <button class="cc-btn cc-btn-bare cc-btn-icon"
                        :disabled="editingBusy"
                        @click="cancelEditVn"
                        v-tooltip.top="'Cancel'">
                  <i class="pi pi-times" />
                </button>
              </template>
              <template v-else>
                <span class="md-store-vn">labels · {{ vn }}</span>
                <button class="cc-btn cc-btn-bare cc-btn-icon md-vn-edit"
                        :disabled="editingVn !== null"
                        @click="startEditVn(vn)"
                        v-tooltip.top="'Rename this label set'">
                  <i class="pi pi-pencil" />
                </button>
                <span class="md-size cc-readout cc-fs-xs">{{ size(stores.labels[vn]) }}</span>
              </template>
            </div>
            <code class="md-code">{{ fns.join(', ') }}</code>
          </li>
        </ul>
        </div>
      </CollapsibleSection>

      <CollapsibleSection v-if="attrs.length" label="Attributes" tip="Image attributes" accordion="cc.imageMeta.section" section="attributes" max-height="none">
        <div class="md-section md-cs">
        <div class="md-grid">
          <template v-for="[k, v] in attrs" :key="'a-' + k">
            <span class="md-k">{{ k }}</span><span class="md-v">{{ v }}</span>
          </template>
        </div>
        </div>
      </CollapsibleSection>

      <CollapsibleSection v-if="extra.length" label="Other metadata" tip="Other recorded metadata" accordion="cc.imageMeta.section" section="other" max-height="none">
        <div class="md-section md-cs">
        <div class="md-grid">
          <template v-for="[k, v] in extra" :key="'e-' + k">
            <span class="md-k">{{ k }}</span><span class="md-v">{{ v }}</span>
          </template>
        </div>
        </div>
      </CollapsibleSection>

      <CollapsibleSection v-if="img.note" label="Note" tip="Note on this image" accordion="cc.imageMeta.section" section="note" max-height="none">
        <div class="md-section md-cs">
        <p class="md-note-text">{{ img.note }}</p>
        </div>
      </CollapsibleSection>
    </div>
  </BaseModal>
</template>

<style scoped>
/* Shell (overlay/box/header/footer) lives in BaseModal; only dialog-specific styles here. */
.md-body { display: flex; flex-direction: column; gap: 1rem; }   /* padding from BaseModal */
.md-name { margin: 0; font-size: var(--cc-fs-md); font-weight: 600; color: var(--cc-text); }

.md-section { display: flex; flex-direction: column; gap: 0.4rem; }
.md-h { margin: 0; }
.md-head { justify-content: space-between; flex-wrap: nowrap; }
.md-cs { padding: 0.2rem 0 0.4rem; }
.md-scope, .md-acq-note { margin: 0; }
.md-card-btn { margin-left: auto; }

.md-grid {
  display: grid; grid-template-columns: max-content 1fr;
  gap: 0.25rem 1rem; font-size: var(--cc-fs-md); align-items: baseline;
}
.md-k { color: var(--cc-text-dim); white-space: nowrap; }
.md-v { color: var(--cc-text); word-break: break-word; }
.md-mono, .md-v.md-mono { font-family: var(--cc-mono); font-size: var(--cc-fs-sm); }

.md-path { display: flex; align-items: flex-start; gap: 0.4rem; }
.md-code {
  flex: 1; min-width: 0; font-family: var(--cc-mono); font-size: var(--cc-fs-sm);
  color: var(--cc-text); background: var(--cc-surface-2); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm); padding: 0.3rem 0.45rem; word-break: break-all;
}
/* .md-copy → cc-btn cc-btn-bare cc-btn-icon */
.md-copy:hover { color: var(--cc-text); background: var(--cc-surface-2); }
.md-none { margin: 0; font-style: italic; }
.md-crop { margin: 0.3rem 0 0; }

.md-chips { list-style: none; margin: 0; padding: 0; counter-reset: ch; }
.md-chip {
  font-size: var(--cc-fs-sm); color: var(--cc-text);
  background: var(--cc-surface-2); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-pill); padding: 0.12rem 0.5rem;
}
.md-chip::before { counter-increment: ch; content: counter(ch) '· '; color: var(--cc-text-dim); }

/* One card per stored version / label set (surface + border + radius from .cc-card). */
.md-stores { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.4rem; }
.md-store { display: flex; flex-direction: column; gap: 0.3rem; padding: 0.45rem 0.5rem; }
.md-store-head { display: flex; align-items: baseline; gap: 0.5rem; }
/* The value name is the entry's title — the one thing you scan the list by, so it is text, not dim. */
.md-store-vn { color: var(--cc-text); font-weight: 600; font-size: var(--cc-fs-sm); overflow-wrap: anywhere; }
/* pushed right (rather than space-between, which would centre the format) so the numbers line up down
   the page — the only way sizes compare at a glance */
.md-size { white-space: nowrap; margin-left: auto; }
/* Values in mono: these are shapes and identifiers, and `1×1×1×1024×1024` only lines up in mono. */
.md-fact-v { font-family: var(--cc-mono); font-size: var(--cc-fs-sm); color: var(--cc-text); }

.md-note-text { margin: 0; font-size: var(--cc-fs-md); color: var(--cc-text); white-space: pre-wrap; }

/* Pyramid section — bare inline toggle inside a store card. `.cc-section-toggle` gives us the row
   affordance without CollapsibleSection's panel-bar chrome (which would fight the card border). */
.md-pyr-toggle { padding: 0.15rem 0; gap: 0.4rem; }
.md-pyr {
  border-collapse: collapse; margin-top: 0.15rem;
  font-size: var(--cc-fs-sm);
}
.md-pyr th, .md-pyr td { text-align: left; padding: 0.1rem 0.65rem 0.1rem 0; }
.md-pyr td { font-family: var(--cc-mono); }
.md-pyr th { font-weight: 400; }
.md-pyr-lvl { color: var(--cc-text-dim); font-family: var(--cc-mono); }

/* Version lineage — a compact indented tree. Each row is one stored version; its inbound "via"
   caption sits above it on the same row (arrow + task label). Depth is a CSS variable so nested
   children inherit their parent's indent without a nested <ul>, which would break the top-to-bottom
   read order the tree relies on. */
.md-lineage { padding: 0.45rem 0.6rem; }
.md-flow { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.15rem; }
.md-flow-row {
  display: flex; flex-direction: column; gap: 0.05rem;
  padding-left: calc(var(--depth, 0) * 1rem);
}
.md-flow-via { display: inline-flex; align-items: center; gap: 0.3rem; }
/* colour inherits from the .cc-muted wrapper via currentColor */
.md-flow-arrow { font-size: var(--cc-fs-2xs); }
.md-flow-vn { color: var(--cc-text); font-family: var(--cc-mono); font-size: var(--cc-fs-sm); }
.md-flow-active { font-weight: 600; }

/* Inline rename input on a labels row — layout only. Size + border + colour come from the input
   base + `cc-input-xs` (see style.css → Density steps for form controls); a scoped re-statement is
   what findRestatedInputBase in cssScenarios.test.ts refuses. Font-weight matches the surrounding
   `md-store-vn` (600) so the row height doesn't jump between view and edit modes. */
.md-vn-input { flex: 1; min-width: 0; font-weight: 600; }
/* The pencil is dim until the row is hovered — the rename is a rare op, and a bright icon on every
   label row would fight the size readout for attention. */
.md-vn-edit { opacity: 0.5; }
.md-store:hover .md-vn-edit:not(:disabled) { opacity: 1; }
.md-acq { gap: 0.4rem; padding: 0.3rem 0; }
</style>
